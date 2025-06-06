#' Function to sonify data using TuneR package
#' 
#' @param data_array_to_sonify
#' @param ref_freq
#' @param to_plot TRUE or FALSE, if TRUE returns list with second object containing plots
#' @param raw_min
#' @param raw_max
#' @param total_seconds 
#' @param samp_width
#' 
sonify_data <- function(data_array_to_sonify, ref_freq, to_plot = FALSE,
                        raw_min = 10, raw_max = 1000, total_seconds = 30,
                        samp_width = 4*64,
                        wave_type = c("square","sine","triangle","sawtooth")) {
  
  #data_array_to_sonify = sd$value
  data_array_to_sonify = data_array_to_sonify[!is.na(data_array_to_sonify)]
  ref_freq = 440
  to_plot = TRUE
  raw_min = 16
  raw_max = 500
  total_seconds <- 20
  note_length <- 0.1 # seconds
  samp_width <- 64*4
  
  
  if (to_plot == TRUE) {
    plots <- vector(mode = "list", length = 8)
  }
  
  # Start by converting raw data to frequency
  data_sound <- sonify(#x = stream_discharge$date_num, 
    y = data_array_to_sonify, 
    waveform = "triangle",    #c("sine", "square", "triangle","sawtooth"), 
    interpolation = "constant", #c("spline", "linear", "constant"),
    duration = 20, 
    flim = c(raw_min,raw_max),  # C2 to C6 
    ticks = NULL, 
    tick_len = 1,
    pulse_len = 0, 
    pulse_amp = 0.5, 
    noise_interval = c(0, 0),
    noise_amp = 0, 
    amp_level = .99, 
    na_freq = 300, 
    stereo = TRUE,
    smp_rate = 3000, 
    play = FALSE, player = NULL, player_args = NULL)
  
  # Extract part or full wave
  SS1 <- extractWave(data_sound, from=1, to=nrow(data_sound))
  
  # Extract just left channel (note above command has identical left and right but can vary...)
  SS1m <- mono(Wave(SS1), "left")
  
  # Calculating periodograms of sections each consisting of 64 observations,
  # overlapping by 32 observations:
  SS1_obj <- periodogram(SS1m, normalize = TRUE, 
                         width = samp_width, 
                         overlap = 2) # 1024/2
  
  # Plot the first periodogram:
  if (to_plot == TRUE) {
    plot(SS1_obj, xlim = c(0, 2000), which = 1, col = dark_pal[4])
    grid(nx = NULL, ny = NULL,
         lty = 1,      # Grid line type
         col = "gray90", # Grid line color
         lwd = 1) 
    plots[[1]] <- recordPlot()
  }
  
  # or a spectrogram
  if (to_plot == TRUE) {
    image(SS1_obj, ylim = c(0, 1000), col = c(dark_pal[5],light_pal))
    plots[[2]] <- recordPlot()
  }
  
  # calculate the fundamental frequency of each periodogram
  ff <- FF(SS1_obj)
  ff[ff<raw_min] <- NA
  ff <- zoo::na.approx(ff)
  
  if (to_plot == TRUE) {
    plot(ff)
    plots[[3]] <- recordPlot()
  }
  
  # derive note from FF given diapason a'=440
  notes <- noteFromFF(ff, ref_freq)
  #plot(notes)
  
  notes[is.na(notes)] <- floor(mean(notes, na.rm = TRUE))
  
  # smooth the notes:
  #snotes <- smoother(notes)
  #plot(snotes)
  
  snotes <- note_to_major7th(notes)
  #snotes <- note_to_minor(notes)
  
  if (to_plot == TRUE) {
    plot(snotes)
    a <- recordPlot()
    plots[[4]] <- a
  }
  
  # Convert notes back to frequency
  hz_notes <- note_num_to_freq(snotes, ref = ref_freq)
  
  # Sonify new frequency (notes)
  w2 <- sonify(hz_notes, 
               flim = c(min(hz_notes, na.rm = TRUE),max(hz_notes, na.rm = TRUE)), 
               waveform = wave_type,    #c("sine", "square", "triangle","sawtooth"), 
               interpolation = "constant", #c("spline", "linear", "constant"),
               smp_rate = 44100, 
               duration = total_seconds)
  
  
  
  # plot melody and energy of the sound:
  if (to_plot == TRUE) {
    melodyplot(SS1_obj[1:length(snotes)], snotes)
    plots[[5]] <- recordPlot()
  }
  
  
  qnotes <- quantize(snotes, SS1_obj@energy[1:length(snotes)], parts = 64)
  
  if (to_plot == TRUE) {
    quantplot(qnotes, expected = rep(c(0, -12), each = 2), bars = 10)
    plots[[6]] <- recordPlot()
    # an plot it, 4 parts a bar (including expected values):
  }
  
  if (to_plot == TRUE) {
    # now prepare for LilyPond
    qlily <- quantMerge(snotes, 4, 4, 2)
    plots[[7]] <- recordPlot()
  }
  
  if (to_plot == TRUE) {
    return(list(w2, plots))
  } else {
    return(list(w2))
  }
  
}
