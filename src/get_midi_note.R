#' @title Calculate MIDI Note from Note Name and Octave
#' @description This function converts a standard musical note name (e.g., "C", "A#")
#' and an octave number to its corresponding MIDI note number.
#' @param note A character string representing the note name. Valid names include
#' "C", "C#", "Db", "D", "D#", "Eb", "E", "F", "F#", "Gb", "G", "G#", "Ab",
#' "A", "A#", "Bb", and "B". The function is case-insensitive.
#' @param octave An integer representing the piano octave number (e.g., 4 for Middle C).
#' @return The MIDI note number (an integer from 0 to 127).
#' @examples
#' # Calculate the MIDI note for Middle C (C4)
#' get_midi_note("C", 4)
#'
#' # Calculate the MIDI note for G5
#' get_midi_note("G", 5)
#'
#' # Calculate the MIDI note for B-flat in the 3rd octave
#' get_midi_note("Bb", 3)
#'
#' # Note names are case-insensitive
#' get_midi_note("a#", 3)
get_midi_note <- function(note, octave) {
  
 # note <- "A"
 # octave <- 3
  
  # Define the base MIDI note for C4 (Middle C) and the semitone offsets
  # from C within a single octave.
  SEMITONE_OFFSETS <- list(
    "C" = 0,
    "C#" = 1, "Db" = 1,
    "D" = 2,
    "D#" = 3, "Eb" = 3,
    "E" = 4,
    "F" = 5,
    "F#" = 6, "Gb" = 6,
    "G" = 7,
    "G#" = 8, "Ab" = 8,
    "A" = 9,
    "A#" = 10, "Bb" = 10,
    "B" = 11
  )
  
  # Normalize the input note to upperrcase for consistent lookup
  note_upper <- toupper(note)
  
  # Check if the note is valid
  if (!(note_upper %in% names(SEMITONE_OFFSETS))) {
    stop("Invalid note name provided. Please use a valid note (e.g., 'C', 'Db', 'G#').")
  }
  
  # Get the semitone offset for the given note
  semitone_offset <- SEMITONE_OFFSETS[[note_upper]]
  
  # Calculate the MIDI note number using C4 (MIDI note 60) as the reference
  # Each octave above or below C4 adds or subtracts 12 semitones.
  midi_note <- 60 + (12 * (octave - 4)) + semitone_offset
  
  # Return the calculated MIDI note
  return(midi_note)
}

