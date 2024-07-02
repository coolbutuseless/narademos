
# The stream contains flags, palette-data and vertex-/polygon-data for 1800 frames.
# The stream is segmented in 64KB blocks.
# A frame never crosses such a 64KB boundary.
# Polygons can have 3-15 vertices.
# Polygons can cover each other, so a non-convex polygon filler is needed.
# Polygons are rendered to a 256×200 pixel screen in 4 bit color depth.
# The palette (16 out of 512 colors) can change from frame to frame.
# Words are 2 bytes in big endian order.
# Colors are stored as words in Atari-ST format 00000RRR0GGG0BBB (512 possible colors).
# 
# 
# # Every frame stores the following data:
# 
# 1 byte - Flags 
#   Bit 0: Frame needs to clear the screen.
#   Bit 1: Frame contains palette data.
#   Bit 2: Frame is stored in indexed mode.
# 
# If frame contains palette data {
#   1 word Bitmask
#   
#   For every set bit in the Bitmask (0-15) {
#     1 word - Color 
#        The color has to be copied into the palette at the reverse 
#        index of the actual bit, because the bitmask is stored in reverse order.
#        In other words: If bit 15 of mask is set -> update color 0 of palette,
#        if bit 0 of mask is set -> update color 15 of palette.
#   }
# }
# 
# If frame is stored in indexed mode {
#   1 byte - Number of vertices (0-255)
#   
#   For every Vertex {
#     1 byte X-position
#     1 byte Y-position
#   }
#   
#   While (…) {
#     1 byte Poly-descriptor Contains: 
#       hi-nibble – 4 bits color-index
#       lo-nibble – 4 bits number of polygon vertices
#     
#       Some special cases are encoded in the descriptor byte:
#         $ff = End of frame
#         $fe = End of frame and the stream skips to the next 64KB block
#         $fd = End of stream (we are done \o/)
#     
#     For every vertex of the polygon {
#       1 byte Vertex-id (0-255)
#     }
#   }
# } Else if frame is stored in non-indexed mode {
#   while (…) {
#     1 byte Poly-descriptor (See indexed mode)
#     
#     For every vertex of the polygon {
#       1 byte X-position
#       1 byte Y-position
#     }
#   }
# }

if (!requireNamespace('nara', quietly = TRUE)) {
  remotes::install_github('coolbutuseless/nara')
}
if (!requireNamespace('governor', quietly = TRUE)) {
  remotes::install_github('coolbutuseless/governor')
}
library(nara)
library(governor)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Open a fast graphics device
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
x11(type = 'dbcairo', width = 7, height = 7)
dev.control(displaylist = 'inhibit')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Prepare some helpers for bitwise operation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'%&%'  <- bitwAnd
'%>>%' <- bitwShiftR
'%<<%' <- bitwShiftL
lo_nibble <- 15

# Print a 'word' as bits
pw <- function(i) {
  intToBits(i)[1:16] |> rev()
}

# Print a byte as bites
pb <- function(i) {
  intToBits(i)[1:8] |> rev()
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# The data stream to read/unpack. See top of this file for structure
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
stdata <- file("data/scene1.bin", open = 'rb')

# read a byte from the stream
read_byte <- function() {
  readBin(stdata, integer(), n = 1, size = 1, signed = FALSE)
}

# Read a 'word' from the stream
read_word <- function() {
  readBin(stdata, integer(), n = 1, size = 2, signed = FALSE, endian = 'big')
}

# Advance data stream to start of next 64kB block
next_block <- function() {
  pos <- ceiling(seek(stdata) / (64 * 1024)) * 64 * 1024 
  invisible(seek(stdata, pos))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Setup drawing canvas
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
nr <- nr_new(256, 200, 'black')
palette <- rep('hotpink', 16)

# Control the framerate
gov <- governor::gov_init(interval = 1/20)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Parse the data stream until the end
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
while (TRUE) {
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # First byte has some flags in it
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  flag <- read_byte()
  clear_screen <- as.logical(flag %&% 0x01)
  has_palette  <- as.logical(flag %&% 0x02)
  indexed_mode <- as.logical(flag %&% 0x04)
  
  nr_fill(nr, 'black')
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Unpack the palette if provided
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (has_palette) {
    bitmask <- read_word()
    idxs <- rev(17L - which(intToBits(bitmask) > 0))
    
    for (idx in idxs) {
      ## 00000RRR0GGG0BBB
      col   <- read_word()
      blue  <- (col %&% 7) %<<% 5
      col   <- col %>>% 4
      green <- (col %&% 7) %<<% 5
      col   <- col %>>% 4
      red   <- (col %&% 7) %<<% 5
      
      palette[idx] <- rgb(red, green, blue, maxColorValue = 255)
    }
  }
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Indexed mode gives 
  #   - a list of vertices
  #   - a list of indexes into the vertex list
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (indexed_mode) {
    
    nvertices <- read_byte()
    xs <- integer(nvertices)
    ys <- integer(nvertices)
    
    for (i in seq(nvertices)) {
      xs[i] <- read_byte()
      ys[i] <- read_byte()
    }
    
    while(TRUE) {
      desc <- read_byte()
      if (desc == 0xff) {
        break
      } else if (desc == 0xfe) {
        next_block()
        break;
      } else if (desc == 0xfd) {
        stop("0xfd - end of stream")
        break;
      } else {
        nverts  <- desc %&% lo_nibble
        pal_idx <- (desc %>>% 4) %&% lo_nibble
        colour  <- palette[pal_idx + 1]
        
        vert_idx <- integer(nverts)
        for (i in seq(nverts)) {
          vert_idx[i] <- read_byte()
        }
        
        nr_polygon(nr, xs[vert_idx + 1], ys[vert_idx + 1], fill = colour)
      }
    }
    
  } else {
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Non-indexed mode. Just an ordered list of vertices to draw
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    while(TRUE) {
      desc <- read_byte()
      if (desc == 0xff) {
        break
      } else if (desc == 0xfe) {
        next_block()
        break;
      } else if (desc == 0xfd) {
        stop("0xfd - end of stream")
        break;
      } else {
        nverts  <- desc %&% lo_nibble
        pal_idx <- (desc %>>% 4) %&% lo_nibble
        colour  <- palette[pal_idx + 1]
        
        xs <- integer(nverts)
        ys <- integer(nverts)
        for (i in seq(nverts)) {
          xs[i] <- read_byte()
          ys[i] <- read_byte()
        }
        
        nr_polygon(nr, xs, ys, fill = colour)
      }
    }
    
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Draw the frame, and use the governor to wait a bit in order to 
  # maintain the requested framerate
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  dev.hold()
  plot(nr, T)
  dev.flush()
  governor::gov_wait(gov)
}

close(stdata)













