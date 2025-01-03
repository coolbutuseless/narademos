
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
nr <- nr_new(400, 400, 'black')
src <- fastpng::read_png(system.file("img/Rlogo.png", package = "png"), type = 'nativeraster')

thetas <- c(seq(0, 4 * pi, length.out = Nframes)) 
sfs <- c(
  displease::seq_ease(1, 4.75, Nframes/2, direction = 'in'), 
  displease::seq_ease(4.75, 1, Nframes/2, direction = 'out')
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Prepare to render
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
while(dev.flush()) {}
gov <- governor::gov_init(1/30) # 30 fps
skip <- FALSE


for (frame in seq(Nframes)) {

  nr_fill(nr, 'white')
  nr_blit(nr, src, 200, 200, angle = thetas[frame], scale = sfs[frame])
  
  # Draw to screen
  if (!skip) {
    dev.hold()
    plot(nr)
    dev.flush()
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
  nrs_to_gif(frames, filename = "rotozoom-simple.gif")
  nrs_to_mp4(frames, filename = "rotozoom-simple.mp4", framerate = 30)
}

