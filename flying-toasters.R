
if (!requireNamespace('nara', quietly = TRUE)) {
  remotes::install_github('coolbutuseless/nara')
}
library(nara)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load the spritemap as a native raster
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if (!file.exists("image/toasters.png")) {
  dir.create("image", showWarnings = FALSE)
  download.file("https://www.spriters-resource.com/resources/sheets/203/206245.png", "image/toasters.png")
}
toasters_map <- png::readPNG("image/toasters.png", native = TRUE)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Replace the current background color with transparent white
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
nr_replace(toasters_map, toasters_map[1,1], 'transparent') 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Cut out the sprites from the map into a list of sprites
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
toasters <- lapply(0:4, function(i) {
  nr_crop(toasters_map, x = i * 64, y = 0, w = 64, h = 59)
})



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Open a window to draw on. 'dbcairo' = double-buffered window
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
x11(type = 'dbcairo')
dev.control(displaylist = 'inhibit')


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Set each toaster to start at a different index in the 5-frame animation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Nframes <- 300

# If you want to save the frames and output an animation at the end
save_anim <- FALSE
if (save_anim) {
  frames <- vector('list', Nframes)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Setup toaster information
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Nflying <- 10
blit_config <- data.frame(
  idx   = sample(length(toasters), Nflying, TRUE), # index into 'src' list of nativerasters
  x     = runif(Nflying, 0, 600),   # position on 'dst'
  y     = runif(Nflying, 0, 100),   # position on 'dst'
  x0    = 0L,                       # start of blit on 'src'
  y0    = 0L,                       # start of blit on 'src'
  w     = -1L,                      # width of blit on 'src'
  h     = -1L,                      # height of blit on 'src'
  hjust = 0.5,                      # position of handle on 'src' (0.5 = middle)
  vjust = 0.5,                      # position of handle on 'src' (0.5 = middle)
  respect_alpha = TRUE,             # respect alpha when drawing
  render = TRUE                     # do we want to draw this item?
)

blit_config$vx <- -sample(c(4, 6, 8), Nflying, TRUE)
blit_config$vy <- -0.5 * blit_config$vx


nr <- nr_new(400, 400, 'black')

for (frame in seq(Nframes)) {

  nr_fill(nr, 'black')
  
  nr_blit_bulk(dst = nr, src = toasters, config = blit_config)
  
  # increment animation frame
  blit_config$idx <- ifelse(blit_config$idx == 5L, 1L, blit_config$idx + 1L)
  
  # set new position
  blit_config$x <- blit_config$x + blit_config$vx
  blit_config$y <- blit_config$y + blit_config$vy
  
  # if off-screen then reset to new position 
  offscreen <- blit_config$x < -70 | blit_config$y > 600
  blit_config$x <- ifelse(offscreen, runif(1, 0, 700), blit_config$x)
  blit_config$y <- ifelse(offscreen,              -64, blit_config$y)
  
  # Draw to screen
  dev.hold()
  plot(nr)
  dev.flush()
  
  # Keep frame if saving animation at end
  if (save_anim) frames[[frame]] <- nr_duplicate(nr)
  
  # Slow down the framerate otherwise it'll render too fast!
  Sys.sleep(0.1)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create an animation to post to Mastodon :)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if (save_anim) {
  nrs_to_gif(frames, gif_name = "toasters.gif")
  nrs_to_mp4(frames, mp4_name = "toasters.mp4")
}

