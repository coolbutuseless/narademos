
# 'processing' code from the tweet: 
# https://x.com/yuruyurau/status/1226846058728177665
# By twitter user "https://x.com/yuruyurau" 12:31 PM · Feb 10, 2020
#
# W=540; 
# N=200;
# x, y, t = 0, 0, 0
# 
# def setup():
#   size(W,W);
#   noStroke()
# 
# def draw():
#   global t;
#   clear();
#   [F(i,j) for i in range(N) for j in range(N)];
#   t += .1
# 
# def F(i, c):
#   global x, y;
#   r = TAU/N;
#   u = sin(i + y) + sin(ri + x);
#   v = cos(i + y) + cos(ri + x);
#   x = u + t;
#   y = v;
#   fill(i, c, 99);
#   circle(uN/2 + W/2, yN/2 + W/2, 2)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Direct translation into R with nativeRasters
# Small modification to set 'blue' channel to dynamic value (rather than '99')
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if (!requireNamespace('nara', quietly = TRUE)) {
  remotes::install_github('coolbutuseless/nara')
}
library(nara)

# Open a fast display
x11(type = 'dbcairo')
dev.control(displaylist = 'inhibit')

# Init 
W   <- 400
N   <- W/2 - 40
tau <- 2 * pi
r   <- tau/N
x   <- 0
y   <- 0

nr  <- nr_new(W, W, 'black')

for (t in seq(0, 3, 0.02)) {
  nr_fill(nr, 'black')
  
  xs <- double(N)
  ys <- double(N)
  for (i in seq(N)) {
    for (j in seq(N)) {
      u <- sin(i + y) + sin(r*i + x)
      v <- cos(i + y) + cos(r*i + x)
      x <- u + t
      y <- v
      xs[j] <- u
      ys[j] <- y
    }
    col <- rgb(i, seq(N), (ys + 2) * N/4, maxColorValue = N)
    nr_point(nr, xs * N/2 + W/2, ys * N/2 + W/2, color = col)
  }
  
  dev.hold()
  plot(nr)
  dev.flush()
  
  # uncomment/adapt next line if animation runs too fast
  # Sys.sleep(0.02) 
}
