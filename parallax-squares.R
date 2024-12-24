
if (!requireNamespace('nara', quietly = TRUE)) {
  remotes::install_github('coolbutuseless/nara')
}
library(nara)

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
Nframes <- 600


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This is all a bit of brute force to generate large checkerboards
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cols <- viridisLite::cividis(4)

d <- 20
nr1 <- nr_new(d, d, cols[[1]])
pos1 <- rbind(
  expand.grid(x = seq(-500    , 700, by = d*2), y = seq(-500    , 700, by = d*2)),
  expand.grid(x = seq(-500 + d, 700, by = d*2), y = seq(-500 + d, 700, by = d*2))
)

d <- 50
nr2 <- nr_new(d, d, cols[[2]])
pos2 <- rbind(
  expand.grid(x = seq(-500    , 700, by = d*2), y = seq(-500    , 700, by = d*2)),
  expand.grid(x = seq(-500 + d, 700, by = d*2), y = seq(-500 + d, 700, by = d*2))
)


d <- 100
nr3 <- nr_new(d, d, cols[[3]])
pos3 <- rbind(
  expand.grid(x = seq(-500    , 700, by = d*2), y = seq(-500    , 700, by = d*2)),
  expand.grid(x = seq(-500 + d, 700, by = d*2), y = seq(-500 + d, 700, by = d*2))
)


d <- 150
nr4 <- nr_new(d, d, cols[[4]])
pos4 <- rbind(
  expand.grid(x = seq(-900    , 1100, by = d*2), y = seq(-900    , 1100, by = d*2)),
  expand.grid(x = seq(-900 + d, 1100, by = d*2), y = seq(-900 + d, 1100, by = d*2))
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create the canvas
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
nr <- nr_new(400, 400, 'black')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Prepare to render
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
while(dev.flush()) {}
gov <- governor::gov_init(1/30) # 30 fps



for (frame in seq(Nframes)) {

  nr_fill(nr, 'black')
  
  # Offsets at each frame
  xoff <- 103 * cos(frame / 120 * pi)
  yoff <-  67 * sin(frame / 75 * pi + pi / 3)
  
  # blit each layer with offsets
  nr_blit(nr, nr1, pos1$x + xoff    , pos1$y + yoff    )
  nr_blit(nr, nr2, pos2$x + xoff * 2, pos2$y + yoff * 2)
  nr_blit(nr, nr3, pos3$x + xoff * 4, pos3$y + yoff * 4)
  nr_blit(nr, nr4, pos4$x + xoff * 8, pos4$y + yoff * 8)
  
  # Draw to screen
  dev.hold()
  plot(nr)
  dev.flush()
  
  # Keep frame if saving animation at end
  if (save_anim) frames[[frame]] <- nr_duplicate(nr)
  
  governor::gov_wait(gov)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create an animation to post to Mastodon :)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if (save_anim) {
  message("Rendering anims")
  nrs_to_gif(frames, filename = "parallax-squares.gif")
  nrs_to_mp4(frames, filename = "parallax-squares.mp4", framerate = 30)
}

