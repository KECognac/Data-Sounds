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





#' A4 (A in the 4th octave) is standardized at 440 Hz. This is the common 
#' reference point. The frequency ratio between two consecutive notes in 
#' Western equal temperament is the 12th root of 2 (approximately 1.059463). 
#' This is because there are 12 semitones in an octave, and each octave doubles 
#' the frequency.
#'Steps:Define Note Mapping: Create a mapping from note names 
#'(C, C#, D, D#, E, F, F#, G, G#, A, A#, B) to their semitone position 
#'relative to C. Calculate Semitone Offset from A4: Determine the total number 
#'of semitones away from A4 (the 440 Hz reference). Calculate the frequency of 
#'a musical note.
#'
#' This function takes a musical note (e.g., "A", "C#", "Eb") and an octave
#' number and returns its frequency in Hertz (Hz), based on A4 = 440 Hz
#' and 12-tone equal temperament. It covers the range of a standard grand piano
#' (A0 to C8).
#'
#' @param note A character string representing the musical note.
#'   Valid values are "C", "C#", "Db", "D", "D#", "Eb", "E", "F", "F#",
#'   "Gb", "G", "G#", "Ab", "A", "A#", "Bb", "B". Case-insensitive.
#' @param octave An integer representing the octave number.
#'   Valid range for a standard grand piano is typically 0 to 8.
#' @return The frequency of the note in Hertz (numeric). Returns NA if
#'   the note or octave is invalid or out of grand piano range.
#' @examples
#' note_to_frequency("A", 4)  # Should return 440
#' note_to_frequency("C", 4)  # Middle C
#' note_to_frequency("C", 8)  # Highest C on piano
#' note_to_frequency("A", 0)  # Lowest A on piano
#' note_to_frequency("G#", 3)
#' note_to_frequency("Bb", 2)
#' note_to_frequency("X", 4)  # Invalid note
#' note_to_frequency("C", 9)  # Octave out of range
note_to_freq <- function(note, octave, ref = 440) {
  # Standard reference: A4 = 440 Hz
  A4_FREQ <- ref
  # Semitone index for A4 (where C is 0)
  A4_SEMITONE_INDEX <- 9 # C=0, C#=1, D=2, D#=3, E=4, F=5, F#=6, G=7, G#=8, A=9, A#=10, B=11
  
  # Define semitone values relative to C in an octave (0-11)
  note_map <- c(
    "C" = 0, "C#" = 1, "DB" = 1, "D" = 2, "D#" = 3, "EB" = 3, "E" = 4,
    "F" = 5, "F#" = 6, "GB" = 6, "G" = 7, "G#" = 8, "AB" = 8, "A" = 9,
    "A#" = 10, "BB" = 10, "B" = 11
  )
  
  # Convert input note to uppercase for case-insensitivity and remove any accidental characters
  note_upper <- toupper(note)
  # Handle flat notation by replacing 'B' with 'BB' and '#' with 'SHARP' for lookup
  note_upper <- gsub("B$", "BB", note_upper) # Ensure Eb, Ab, Gb are treated as Eb, Ab, Gb
  note_upper <- gsub("#", "SHARP", note_upper) # Use a distinct string for sharp
  
  # Adjust note_map keys to match simplified input (e.g., "DB" for "Db", "BB" for "Bb", "A#SHARP" for "A#")
  # A more robust way to handle this:
  # Create a consistent lookup for accidentals
  clean_note_map <- c(
    "C" = 0, "C#" = 1, "D" = 2, "D#" = 3, "E" = 4,
    "F" = 5, "F#" = 6, "G" = 7, "G#" = 8, "A" = 9,
    "A#" = 10, "B" = 11
  )
  # Add flat equivalents
  clean_note_map["DB"] <- 1
  clean_note_map["EB"] <- 3
  clean_note_map["GB"] <- 6
  clean_note_map["AB"] <- 8
  clean_note_map["BB"] <- 10
  
  # Get the semitone index for the given note in its octave (0-11)
  note_semitone_in_octave <- clean_note_map[note_upper]
  
  # Validate note
  if (is.na(note_semitone_in_octave)) {
    warning("Invalid note: ", note, ". Please use valid note names (e.g., 'C', 'C#', 'Eb').")
    return(NA_real_)
  }
  
  # Validate octave (standard grand piano range: A0 to C8)
  # A0 is C0 + 9 semitones
  # C8 is C0 + 8 * 12 = 96 semitones
  # So, the lowest key is at index (0*12 + 9) = 9 relative to C0
  # The highest key is at index (8*12 + 0) = 96 relative to C0 (for C8)
  # The highest actual key is B7, which is (7*12 + 11) = 95 relative to C0
  # Grand piano range: A0 to C8.
  # Lowest MIDI note is 21 (A0), highest is 108 (C8).
  # C4 is MIDI 60.
  # So, MIDI note number = (octave + 1) * 12 + semitone_index (where C is 0 in the octave)
  midi_note_num <- (octave + 1) * 12 + note_semitone_in_octave
  
  # Check against standard MIDI range for grand piano (A0 to C8)
  # A0 is MIDI note 21
  # C8 is MIDI note 108
  if (midi_note_num < 21 || midi_note_num > 108) {
    warning("Note and octave combination (", note, octave, ") is outside the standard grand piano range (A0-C8).")
    return(NA_real_)
  }
  
  # Calculate total semitones from A4 (MIDI note 69)
  # A4 has MIDI note number 69
  semitones_from_A4 <- midi_note_num - 69
  
  # Calculate frequency using the equal temperament formula
  frequency <- A4_FREQ * 2^(semitones_from_A4 / 12)
  
  return(frequency)
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

