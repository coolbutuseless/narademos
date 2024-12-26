
if (!requireNamespace('nara', quietly = TRUE)) {
  remotes::install_github('coolbutuseless/nara')
}
library(nara) # remotes::install_github('coolbutuseless/nara')
library(displease) # install.packages('displease')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Open a window to draw on. 'dbcairo' = double-buffered window
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
x11(type = 'dbcairo')
dev.control(displaylist = 'inhibit')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# If you want to save the frames and output an animation at the end
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
save_anim <- FALSE
if (save_anim) {
  frames <- vector('list', Nframes)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Number of frames
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Nframes <- 200

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create the canvas
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
w  <- 800 * 2
h  <- 600 * 2
nr <- nr_new(width = w, height = h, 'black')
src <- fastpng::read_png(system.file("img/Rlogo.png", package = "png"), type = 'nativeraster')

set.seed(1)
Nboxes <- 100
x      <- runif(Nboxes, 0, w-1)
y      <- runif(Nboxes, 0, h-1)
vx     <- rnorm(Nboxes) * 5
vy     <- rnorm(Nboxes) * 5
theta  <- rep(0, Nboxes)
omega  <- rnorm(Nboxes) / 5
scale  <- runif(Nboxes, 0.5, 2)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Prepare to render
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
while(dev.flush()) {}
gov <- governor::gov_init(1/30) # 30 fps
skip <- FALSE


for (frame in seq(Nframes)) {

  theta <- theta + omega
  x <- x + vx
  y <- y + vy
  
  # bounce back if reaches boundary
  oob <- x < 0 | x >= w 
  vx[oob] <- -vx[oob]
  oob <- y < 0 | y >= h
  vy[oob] <- -vy[oob]
  
  nr_fill(nr, 'white')
  nr_blit_rotozoom(nr, src, x, y, theta, scale = scale)
  
  # Draw to screen
  if (!skip) {
    dev.hold()
    plot(nr)
    dev.flush()
  } else {
    cat(".")
  }
  
  # Keep frame if saving animation at end
  if (save_anim) frames[[frame]] <- nr_duplicate(nr)
  
  skip <- governor::gov_wait(gov)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create an animation to post to Mastodon :)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if (save_anim) {
  message("Rendering anims")
  nrs_to_gif(frames, filename = "rotozoom-multiple.gif")
  nrs_to_mp4(frames, filename = "rotozoom-multiple.mp4", framerate = 30)
}

