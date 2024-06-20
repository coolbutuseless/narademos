
library(nara)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load the spritemap as a native raster
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if (!file.exists("image/toasters.png")) {
  download.file("https://www.spriters-resource.com/resources/sheets/203/206245.png", "image/toasters.png")
}
toasters <- png::readPNG("image/toasters.png", native = TRUE)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Replace the current background color with transparent white
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
nr_replace(toasters, toasters[1,1], 'transparent') 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create a location data.frame for the spritemap giving the coordinates
# and size of each of the sprites
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
w <- 64
h <- 59
loc <- data.frame(
  x = (0:4) * w,
  y = 0,
  w = w,
  h = h
)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Open a window to draw on. 'dbcairo' = double-buffered window
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
x11(type = 'dbcairo')
dev.control(displaylist = 'inhibit')


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Setup some initial positions / velocities for the toasters
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Ntoasters <- 10

x <- runif(Ntoasters, 0, 600)
y <- runif(Ntoasters, 0, 100)

vx <- -sample(c(4, 6, 8), Ntoasters, TRUE)
vy <-  -vx/2

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Set each toaster to start at a different index in the 5-frame animation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
idx <- sample(5, Ntoasters, TRUE)

Nframes <- 300

# If you want to save the frames and output an animation at the end
save_anim <- FALSE
if (save_anim) {
  frames <- vector('list', Nframes)
}


nr <- nr_new(400, 400, 'black')

for (frame in seq(Nframes)) {

  nr_fill(nr, 'black')
  
  nr_blit2(
    nr, 
    x   = x,          # Vector of destination coordinates
    y   = y, 
    src = toasters, 
    loc = loc,        # the data.frame of all sprite locations
    idx = idx         # the index of the sprite to draw at this location
  )
  
  # increment animation frame
  idx <- ifelse(idx == 5L, 1L, idx + 1L)
  
  # set new position
  x <- x + vx
  y <- y + vy
  
  # if off-screen then reset to new position 
  offscreen <- x < -70 | y > 600
  x <- ifelse(offscreen, runif(1, 0, 700), x)
  y <- ifelse(offscreen, -64, y)
  
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

