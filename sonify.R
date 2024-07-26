# Script to test sonifying data using USGS discharge data as
# an example.


source('setup.R')


# set wave player - Note this is for mac. 
setWavPlayer("afplay")

# Select USGS gage site_no
site_no <- "06752260"

# Download discharge
stream_discharge <- 
  dataRetrieval::readNWISdv(siteNumbers = site_no,
                            parameterCd = c('00060','00065')) %>%
  dplyr::rename_with(~c("discharge", "code"), 
                     c("X_00060_00003", "X_00060_00003_cd")) %>%
  mutate(date_num = as.numeric(Date),
         log_dis = ifelse(discharge>0,log10(discharge),0))

# Plot

plot(stream_discharge$Date, stream_discharge$discharge, type = "l")

# Select columns necessary for sound only
sd <- stream_discharge %>%
  dplyr::select(date_num,discharge,log_dis)


# Sonify using sonify package
stream_sound <- sonify(#x = stream_discharge$date_num, 
                       y = sd$discharge, 
                       waveform = "square",    #c("sine", "square", "triangle","sawtooth"), 
                       interpolation = "constant", #c("spline", "linear", "constant"),
                       duration = 10, 
                       flim = c(10,440), 
                       ticks = NULL, 
                       tick_len = 1,
                       pulse_len = 0, 
                       pulse_amp = 0.5, 
                       noise_interval = c(0, 0),
                       noise_amp = 0, 
                       amp_level = .99, 
                       na_freq = 300, 
                       stereo = FALSE,
                       smp_rate = 2000, 
                       play = TRUE, player = NULL, player_args = NULL)

# Will auto play with above command but can also call just play:
play(stream_sound)

# Extract part or full wave
SS1 <- extractWave(stream_sound, from=1, to=40000)
# Extract just left channel (note above command has identical left and right but can vary...)
SS1m <- mono(Wave(SS1), "left")
# Calculating periodograms of sections each consisting of 64 observations,
# overlapping by 32 observations:
SS1_obj <- periodogram(SS1m, normalize = TRUE, width = 32, overlap = 0)
# Plot the first periodogram:
plot(SS1_obj, xlim = c(0, 2000), which = 1)
# or a spectrogram
image(SS1_obj, ylim = c(0, 1000))
# calculate the fundamental frequency of each periodogram
ff <- FF(SS1_obj)
plot(ff)
# derive note from FF given diapason a'=440
notes <- noteFromFF(ff, 440)
plot(notes)
# smooth the notes:
snotes <- smoother(notes)
# Convert notes back to frequency
hz_notes <- (2^(snotes/12))*440
# Sonify new frequency (notes)
w2 <- sonify(hz_notes, 
             flim = c(min(hz_notes, na.rm = TRUE),max(hz_notes, na.rm = TRUE)), 
             waveform = "triangle",    #c("sine", "square", "triangle","sawtooth"), 
             interpolation = "constant", #c("spline", "linear", "constant"),
             smp_rate = 8000, 
             duration = 30)

# outcome should be 0 for diapason "a'" and -12 (12 halftones lower) for "a"
plot(snotes)
# plot melody and energy of the sound:
melodyplot(SS1_obj, snotes)
# apply some quantization (into 8 parts):
qnotes <- quantize(snotes, SS1_obj@energy, parts = 64)
# an plot it, 4 parts a bar (including expected values):
quantplot(qnotes, expected = rep(c(0, -12), each = 2), bars = 10)
# now prepare for LilyPond
qlily <- quantMerge(snotes, 4, 4, 2)


