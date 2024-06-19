

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Copyright 2024 Mike Cheng
# MIT License
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(nara)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load the spritemap as a native raster
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if (!file.exists("toasters.png")) {
  download.file("https://www.spriters-resource.com/resources/sheets/203/206245.png", "toasters.png")
}
toasters <- png::readPNG("toasters.png", native = TRUE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Set the background to transparent
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
nr_replace(toasters, -15658735L, 'transparent') 
plot(toasters)


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
# Open a window to draw on
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
frames <- vector('list', Nframes)
for (frame in seq(Nframes)) {
  nr <- nr_new(400, 400, 'black')
  frames[[frame]] <- nr
  
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
  
  Sys.sleep(0.1)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create an animation to post to Mastodon :)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
nrs_to_gif(frames, gif_name = "toasters.gif")
nrs_to_mp4(frames, mp4_name = "toasters.mp4")




