# Note Conversion Functions

# SCALES


note_to_minor <- function(notes) {
  semitones <- c(2, 1, 2, 2, 1, 2, 2)
  minor <- c(-rev(cumsum(rep(rev(semitones),4))),0, cumsum(rep(semitones,4)))
  #major7th <- c(-44, -41, -37, -34, -29, -26, -22, -19, -14, -11, -7, -4, 0, 3, 6, 9, 13, 16, 19, 22, 26, 29, 32, 35, 39, 42, 45, 48, 52, 55, 58, 61, 65, 68, 71, 74, 78, 81, 84)
  nearest_note <- function(x, reference_notes) {
    reference_notes[which.min(abs(x - reference_notes))]
  }
  notes_minor <- notes %>%
    map_dbl(., ~ nearest_note(.x, minor))
}


note_to_major <- function(notes) {
  semitones <- c(2, 2, 1, 2, 2, 2, 1)
  major <- c(-rev(cumsum(rep(rev(semitones),4))),0, cumsum(rep(semitones,4)))
  #major7th <- c(-44, -41, -37, -34, -29, -26, -22, -19, -14, -11, -7, -4, 0, 3, 6, 9, 13, 16, 19, 22, 26, 29, 32, 35, 39, 42, 45, 48, 52, 55, 58, 61, 65, 68, 71, 74, 78, 81, 84)
  nearest_note <- function(x, reference_notes) {
    reference_notes[which.min(abs(x - reference_notes))]
  }
  notes_major <- notes %>%
    map_dbl(., ~ nearest_note(.x, major))
}


# Mixolydian mode
# This mode has a minor seventh scale degree, which can create a slightly 
# melancholic or wistful mood, reminiscent of the quiet power of a river.
note_to_mixolydian <- function(notes) {
  semitones <- c(2,1,1,3,2,1)
  mixolydian <- c(-rev(cumsum(rep(rev(semitones),4))),0, cumsum(rep(semitones,4)))
  #major7th <- c(-44, -41, -37, -34, -29, -26, -22, -19, -14, -11, -7, -4, 0, 3, 6, 9, 13, 16, 19, 22, 26, 29, 32, 35, 39, 42, 45, 48, 52, 55, 58, 61, 65, 68, 71, 74, 78, 81, 84)
  nearest_note <- function(x, reference_notes) {
    reference_notes[which.min(abs(x - reference_notes))]
  }
  notes_mixolydian <- notes %>%
    map_dbl(., ~ nearest_note(.x, mixolydian))
}


# Scale dictionary
scale_dict <- c(
  "major" = note_to_major,
  "mixolydian" = note_to_mixolydian,
  "minor" = note_to_minor
)


note_num_to_freq <- function(note, ref = 440) {
  (2^(note/12))*ref
}


note_to_freq <- function (note, ref = 440, octave = 3) {
  ref <- 440
  note <- "A#"
  octave <- 3
  
  if (is.character(note)) {
    n <- any(nchar(note))
    if (n > 2) 
      stop("'note' cannot be a character vector with more than 2 characters")
    if (any(note == c("E#", "Fb", "B#", "Cb"))) 
      stop("This note does not exist")
    if (n == 2) {
      notesplit <- unlist(strsplit(note, split = NULL))
      if (notesplit[2] == "b") 
        names <- c("C", "Db", "D", "Eb", "E", "F", "Gb", 
                   "G", "Ab", "A", "Bb", "B")
      if (notesplit[2] == "#") 
        names <- c("C", "C#", "D", "D#", "E", "F", "F#", 
                   "G", "G#", "A", "A#", "B")
    }
    else names <- c("C", "C#", "D", "D#", "E", "F", "F#", 
                    "G", "G#", "A", "A#", "B")
    note2 <- which(names == note)
  }
  f <- ref * 2^((octave - 3) + ((note2 - 10)/12))
  return(f)
  
}

map_to_freq_range <- function(vals, min_freq, max_freq) {
  old_min <- min(vals, na.rm = TRUE)
  old_max <- max(vals, na.rm = TRUE)
  new_value <- (vals - old_min) / (old_max - old_min) * (max_freq - min_freq) + min_freq
  return(new_value)
}


resample_array <- function(array, n) {
  # Calculate the sample size
  sample_size <- length(array)
  
  # Calculate the step size for resampling
  step_size <- floor(sample_size / n)
  
  # Initialize the resampled array
  resampled_array <- numeric(n)
  
  # Iterate over the resampled indices
  for (i in 1:n) {
    start_index <- (i - 1) * step_size + 1
    end_index <- min(i * step_size, sample_size)
    resampled_array[i] <- mean(array[start_index:end_index])
  }
  
  return(resampled_array)
}


add_resonance <- function(wave, resonance_freq, resonance_bandwidth, gain) {
  
  #wave <- test[[1]]
  #resonance_freq <- 400
  #resonance_bandwidth <- 100
  #gain <- 0.5
  
  # Extract the envelope
  envelope <- seewave::env(wave)
  
  start_freq <- resonance_freq - resonance_bandwidth/2 
  end_freq <- resonance_freq + resonance_bandwidth/2
  
  # Create a resonant filter
  filtered_wave <- seewave::fir(wave = wave,
                                bandpass = TRUE, 
                                f = resonance_freq, 
                                from = start_freq,
                                to = end_freq,
                                channel = 2)
  
  # Apply the filter to the wave
  #filtered_wave <- filter(wave, filt = filter)
  
  # Combine the filtered wave with the original wave
  new_wave <- wave 
  new_wave@.Data[,2] <- new_wave@.Data[,2] + (filtered_wave * gain)
  #new_wave <- tuneR::normalize(new_wave)
  
  return(new_wave)
}



# Function to add feedback
add_feedback <- function(wave, feedback_gain, delay_samples) {

  
  wave_dat <- wave@.Data[,1]
  
  # Get the number of samples
  num_samples <- length(wave_dat)
  
  # Create a delayed version of the wave
  delayed_wave_dat <- c(rep(0, delay_samples), wave_dat[1:(num_samples - delay_samples)])
  delayed_wave <- wave
  delayed_wave@.Data[,2] <- delayed_wave_dat
  
  # Add the delayed wave to the original wave with feedback gain
  new_wave <- wave + delayed_wave * feedback_gain
  
  # normalize wave
  new_wave <- normalize(new_wave, unit = "16")
  
  # Clip the signal to prevent clipping
  #new_wave <- pmax(pmin(new_wave, 1), -1)
  
  # Create a new Wave object
  #new_wave_obj <- Wave(new_wave, samp.rate = wave@samp.rate, bit = wave@bit)
  
  return(new_wave)
}


clear_viewer_pane <- function() {
  dir <- tempfile()
  dir.create(dir)
  TextFile <- file.path(dir, "blank.html")
  writeLines("", con = TextFile)
  rstudioapi::viewer(TextFile) 
}

animate_sound <- function(plot_dat, note_length, total_seconds) {
  
  plot_dat <- main[[2]][[6]]
  p <- ggplot() +
    geom_path(data = plot_dat, aes(x = time, y = hertz), color = "#002EA3", alpha = 0.5) +
    geom_point(data = plot_dat, aes(x = time, y = hertz, group = time), color = "#002EA3", size = 4) + 
    geom_point(data = plot_dat, aes(x = time, y = hertz), color = light_pal[2], size = 3) +
    theme_bw() +
    #shadow_wake(wake_length = 1, size = 2, alpha = FALSE, colour = 'grey92') +
    labs(x = "Time (Seconds)", y = "Frequency (hz)", title = "Time:{frame_time}") #
  # view_follow()
  p <- p + 
    transition_reveal(along = time) +
    labs(title = "Time: {frame_time}") 
  clear_viewer_pane()
  animate(p, fps = 1/note_length, duration = total_seconds) 
}


