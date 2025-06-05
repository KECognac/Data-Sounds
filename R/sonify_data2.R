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
sonify_data2 <- function(data_array_to_sonify, 
                        ref_freq, 
                        amp = 1,
                        to_plot = FALSE,
                        total_seconds = 30,
                        #samp_width = 4*64,
                        octaves = 3,
                        pulse = 0,
                        pulse_amp = .2,
                        note_length = 0.1,
                        wave_type = c("square","sine","triangle","sawtooth"),
                        scale = c("major","minor","mixolydian"),
                        to_play = TRUE) {
  
  #data_array_to_sonify <- stream_discharge$discharge %>% log10()
  #ref_freq <- ref_freq
  #to_plot <- TRUE
  #octaves <- 2
  #note_length <- .2
  #total_seconds <- 120
  #wave_type <- "sine" # square, triangle, sawtooth, sine
  #scale <- "minor"
  #to_play <- FALSE
  
  # remove NA values
  data <- data_array_to_sonify[!is.na(data_array_to_sonify)]
  
  # Resample to desired # of notes
  tot_notes <- total_seconds / note_length
  if(length(data) > tot_notes) {
    data2 <- resample_array(data, tot_notes)
  } else if (length(data) < tot_notes) {
    data2 <- interp1(1:length(data), data, 1:tot_notes)
  }
  
  # Remap to set the range to specified # of octaves
  new_range <- note_num_to_freq(c(-floor(6*octaves),floor(6*octaves)), ref = ref_freq) # C3
  data3 <- map_to_freq_range(data2,new_range[1],new_range[2])

  
  # derive note from FF given diapason 
  notes <- noteFromFF(data3, ref_freq)
  
  # remove NA if necessary (shouldn't be)
  notes[is.na(notes)] <- floor(mean(notes, na.rm = TRUE))
  
  # convert to desired scale - hardwired for major 7th right now.
  snotes <- scale_dict[[scale]](notes)  
  
  # Convert new notes back to frequency
  hz_notes <- note_num_to_freq(snotes, ref = ref_freq)
  
  # Sonify new frequency (notes)
  w2 <- sonify(hz_notes, 
               flim = c(min(hz_notes, na.rm = TRUE),max(hz_notes, na.rm = TRUE)), 
               waveform = wave_type,    #c("sine", "square", "triangle","sawtooth"), 
               interpolation = "constant", #c("spline", "linear", "constant"),
               smp_rate = 44100, 
               duration = total_seconds, 
               play = to_play,
               amp_level = amp,
               pulse_len = pulse,
               pulse_amp = pulse_amp,
               stereo = FALSE)
  
  # Extract info from sonified notes
  SS1 <- extractWave(w2, from=1, to=nrow(w2))
  SS1m <- mono(Wave(SS1), "left")
  # Calculating periodograms of sections each consisting of 64 observations,
  # overlapping by 32 observations:
  SS1_obj <- periodogram(SS1m, width = 2^12, overlap = 64)#, normalize = TRUE, 

  ff <- FF(SS1_obj)
  notes2 <- noteFromFF(ff, ref_freq)
  
  if (to_plot == TRUE) {
    plots <- vector(mode = "list", length = 6)
    
    plot(snotes)
    plots[[1]] <- recordPlot()
    
    # Plot hertz
    plot(hz_notes)
    plots[[2]] <- recordPlot()
  
  # plot melody and energy of the sound:
    melodyplot(SS1_obj, notes2)
    plots[[3]] <- recordPlot()

    qnotes <- quantize(notes2, SS1_obj@energy, parts = 128)
  
    quantplot(qnotes, expected = rep(c(0, -12), each = 2), bars = 10)
    plots[[4]] <- recordPlot()
    # an plot it, 4 parts a bar (including expected values):

    qlily <- quantMerge(notes2, 4, 4, 2)
    plots[[5]] <- recordPlot()
    
    plot_dat <- data.frame(time = seq(note_length,total_seconds, by = note_length),
                           values = data2,
                           hertz = hz_notes)
    
    plots[[6]] <- plot_dat
    
    #animate(p, fps = 1/note_length) 
      
  }
  
  if (to_plot == TRUE) {
    return(list(w2, plots))
  } else {
    return(list(w2))
  }
  
}


wave_fun <- function(amplitude, frequency, total_time, sample_rate = 44100, waveform_type) {
  
  amplitude <- 5
  frequency <- 220
  sample_rate <- 44100
  waveform_type <- "square"
  time <- seq(0,total_time, by = 1/sample_rate)

  if (waveform_type == "sine") {
    waveform <- amplitude * sin(2 * pi * frequency * time)
  } else if (waveform_type == "square") {
    waveform <- amplitude * sin(7 * frequency * time) / (7 * pi)
  } else if (waveform_type == "sawtooth") {
    waveform <- amplitude * (2 * (time %% (1 / frequency)) / (1 / frequency) - 1)
  } else {
    stop("Invalid waveform type. Please choose 'sine', 'square', or 'sawtooth'.")
  }
}
  
a <- wave_fun(5,220,1,"square")

