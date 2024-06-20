
library(nara)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load the logo image as a native raster
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logo <- png::readPNG(system.file("img", "Rlogo.png", package="png"), native = TRUE)
lw <- ncol(logo)
lh <- nrow(logo)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Screen size
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
w <- 800
h <- 600

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Open a window to draw on. 'dbcairo' = double-buffered window
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
x11(type = 'dbcairo')
dev.control(displaylist = 'inhibit')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Initialise position and velocity of logos
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Nlogos <- 10

x <- sample(0:(w - lw), Nlogos, replace = TRUE)
y <- sample(0:(h - lh), Nlogos, replace = TRUE)

vx <- runif(Nlogos, -5, 5)
vy <- runif(Nlogos, -5, 5)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Initialise screen
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
screen <- nr_new(w, h, 'black')
Nframes <- 600

# If you want to save the frames and output an animation at the end
save_anim <- FALSE
if (save_anim) {
  frames <- vector('list', Nframes)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Run the animation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
for (frame in seq(Nframes)) {
  nr_fill(screen, 'black')
  nr_blit(screen, x, y, logo)
  
  # Update position
  x <- x + vx
  y <- y + vy
  
  # If logo has hit edge of screen, reverse velocity
  vy <- ifelse(y > h - lh | y < 0, -vy, vy)
  vx <- ifelse(x > w - lw | x < 0, -vx, vx)

  # Draw to screen
  dev.hold()
  plot(screen)
  dev.flush()
  
  # Keep frame if saving animation at end
  if (save_anim) frames[[frame]] <- nr_duplicate(screen)
  
  Sys.sleep(0.03)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create an animation to post to Mastodon :)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if (save_anim) {
  nrs_to_gif(frames, gif_name = "image/logo-bounce.gif")
  nrs_to_mp4(frames, mp4_name = "image/logo-bounce.mp4")
}
