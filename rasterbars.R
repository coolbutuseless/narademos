
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
Nframes <- 400

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create the canvas
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
w  <- 400
h  <- 400
nr <- nr_new(w, h)

Nlines <- 20

set.seed(1)
create_bars <- function(...) {
  col <- sample(colours(), 3)
  cols <- c(
    displease::seq_color(col[1], col[2], Nlines/4, colorspace = 'lab'),
    displease::seq_color(col[2], col[3], Nlines/4, colorspace = 'lab')
  )
  cols <- c(cols, rev(cols))
}

bars <- lapply(1:10, create_bars)

Nmotion <- 100
motion <- sin(seq(0, 2*pi, length.out = 100)) * 100

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Prepare to render
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
while(dev.flush()) {}
gov <- governor::gov_init(1/30) # 30 fps
skip <- FALSE


for (frame in seq(Nframes)) {

  nr_fill(nr, 'black')
  
  for (i in seq_along(bars)) {
    yoff <- 200
    moff <- i * 5
    nr_line(nr, 
            0, yoff + seq(-9, 10) + motion[((frame + moff) %% Nmotion) + 1], 
            w, yoff + seq(-9, 10) + motion[((frame + moff) %% Nmotion) + 1], color = bars[[i]])
  }
  
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
  nrs_to_gif(frames, filename = "rasterbars.gif")
  nrs_to_mp4(frames, filename = "rasterbars.mp4", framerate = 30)
}

