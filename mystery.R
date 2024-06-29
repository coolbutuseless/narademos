
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Install pkgs from github
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if (!requireNamespace('nara', quietly = TRUE)) {
  remotes::install_github('coolbutuseless/nara')
}

if (!requireNamespace('governor', quietly = TRUE)) {
  remotes::install_github('coolbutuseless/governor')
}

library(nara)
library(governor)
library(audio)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Check audio drivers
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if (nrow(audio::audio.drivers()) == 0) {
  warning("No audio drivers found")
  warning("For linux users:\napt-get install portaudio19-dev\nThen re-install {audio} package")
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Open a fast display
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
x11(type = 'dbcairo')
dev.control(displaylist = 'inhibit')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Setup audio
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rate <- 8000 # Ha
len  <- 15   # seconds
beat <- readBin("data/mystery-beat.u1", raw(), n = file.size("data/mystery-beat.u1"))
beat <- as.double(beat[1:(len * rate)]) / 255 - 0.5

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Setup video playback
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
nrs     <- readRDS("data/mystery-frames.dat")
fps     <- 12
Nframes <- len * fps
gov     <- governor::gov_init(1/fps)

audio::play(beat, rate = rate)
for (i in seq(Nframes)) {
  idx <- (i %% length(nrs)) + 1L
  dev.hold(); plot(nrs[[idx]]); dev.flush()
  governor::gov_wait(gov)
}

# Run this to reset the device if interrupted by ctrl-c
while(dev.flush()) {}
