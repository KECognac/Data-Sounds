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


#' A4 (A in the 4th octave) is standardized at 440 Hz. This is the common reference point.
#'The frequency ratio between two consecutive notes in Western equal temperament is the 12th root of 2 (approximately 1.059463). This is because there are 12 semitones in an octave, and each octave doubles the frequency.
#'Grand Piano Range:
#'A standard grand piano typically spans 88 keys, from A0 (the lowest note) to C8 (the highest note).
#'Steps:
#'Define Note Mapping: Create a mapping from note names (C, C#, D, D#, E, F, F#, G, G#, A, A#, B) to their semitone position relative to C.
#'Calculate Semitone Offset from A4: Determine the total number of semitones away from A4 (the 440 Hz reference).

#' Calculate the frequency of a musical note.
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
#' @param ref A numeric value for the reference frequency of A4. Defaults to 440 Hz.
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
  
  # Define semitone values relative to C in an octave (0-11)
  # Using a comprehensive lookup that handles sharps and flats consistently
  clean_note_map <- c(
    "C" = 0, "C#" = 1, "D" = 2, "D#" = 3, "E" = 4,
    "F" = 5, "F#" = 6, "G" = 7, "G#" = 8, "A" = 9,
    "A#" = 10, "B" = 11,
    # Add flat equivalents explicitly
    "DB" = 1, "EB" = 3, "GB" = 6, "AB" = 8, "BB" = 10
  )
  
  # Convert input note to uppercase for case-insensitivity
  note_upper <- toupper(note)
  # For flat notes, replace 'b' with 'B' to match lookup (e.g., "Db" -> "DB")
  note_upper <- gsub("B$", "BB", note_upper) # Handles cases like "Bb" -> "BB"
  note_upper <- gsub("B$", "B", note_upper) # Ensure a lone 'B' remains 'B'
  note_upper <- gsub("B$", "B", note_upper) # Handles cases like "Ab" -> "AB" etc.
  note_upper <- gsub("B$", "B", note_upper) # Handles cases like "Gb" -> "GB" etc.
  note_upper <- gsub("B$", "B", note_upper) # Handles cases like "Eb" -> "EB" etc.
  note_upper <- gsub("B$", "B", note_upper) # Handles cases like "Db" -> "DB" etc.
  
  
  # A simpler and more robust way to handle the input note string
  # Create a unified lookup for both sharps and flats directly
  # The original 'note_map' is sufficient if we don't mess with the input string 'note_upper'
  # in a way that breaks the lookup. Let's rely on the more explicit 'clean_note_map' keys.
  # The 'gsub("B$", "BB", note_upper)' etc. lines are problematic for actual 'B' notes.
  
  # Simpler way to clean up input note for lookup:
  # Just convert to upper and then handle the accidental symbol lookup.
  # The previous gsub lines were causing issues.
  # Let's rebuild the clean_note_map and directly lookup.
  clean_note_map <- c(
    "C" = 0, "C#" = 1, "D" = 2, "D#" = 3, "E" = 4,
    "F" = 5, "F#" = 6, "G" = 7, "G#" = 8, "A" = 9,
    "A#" = 10, "B" = 11,
    # Flat equivalents
    "DB" = 1, "EB" = 3, "GB" = 6, "AB" = 8, "BB" = 10 # Uppercased flat symbols
  )
  
  # Ensure the input 'note' is uppercased.
  # For 'b' (flat) notation, replace 'b' with 'B' in the input string to match lookup table keys like "DB", "EB"
  # This makes "Db" become "DB", "Eb" becomes "EB" etc.
  processed_note <- toupper(gsub("b", "B", note))
  
  
  # Get the semitone index for the given note in its octave (0-11)
  note_semitone_in_octave <- clean_note_map[processed_note]
  
  # Validate note
  if (is.na(note_semitone_in_octave)) {
    warning("Invalid note: '", note, "'. Please use valid note names (e.g., 'C', 'C#', 'Eb').")
    return(NA_real_)
  }
  
  # Calculate MIDI note number (where C0 is MIDI note 12)
  # This formula is standard: MIDI note = (octave + 1) * 12 + semitone_index (where C is 0 in its octave)
  midi_note_num <- (octave + 1) * 12 + note_semitone_in_octave
  
  # Check against standard grand piano range (A0 to C8)
  # A0 is MIDI note 21
  # C8 is MIDI note 108
  if (midi_note_num < 21 || midi_note_num > 108) {
    warning("Note and octave combination ('", note, octave, "') is outside the standard grand piano range (A0-C8, MIDI notes 21-108).")
    return(NA_real_)
  }
  
  # Calculate total semitones from A4 (MIDI note 69)
  # A4 has MIDI note number 69
  semitones_from_A4 <- midi_note_num - 69
  
  # Calculate frequency using the equal temperament formula
  frequency <- A4_FREQ * 2^(semitones_from_A4 / 12)
  
  return(frequency)
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


