# Note Conversion Functions


note_to_minor7th <- function(notes) {
  minor7th <- c(-32,-29,-26,-23,-19,-16,-13,-10,-6,-3,0,3,7,10,13,16,20,23,26,29,33,36,39)
  nearest_note <- function(x, reference_notes) {
    reference_notes[which.min(abs(x - reference_notes))]
  }
  notes_minor7th <- notes %>%
    map_dbl(., ~ nearest_note(.x, minor7th))
}


note_to_minor <- function(notes) {
  minor <- c(0,2,3,5,7,8,10,12)
  minor_scale <- c(minor-26, minor-13,minor,minor+13, minor + 26)
  nearest_note <- function(x, reference_notes) {
    reference_notes[which.min(abs(x - reference_notes))]
  }
  notes_minor <- notes %>%
    map_dbl(., ~ nearest_note(.x, minor_scale))
}


note_to_major7th <- function(notes) {
  major7th <- major7th <- c(-44, -41, -37, -34, -29, -26, -22, -19, -14, -11, -7, -4, 0, 3, 6, 9, 13, 16, 19, 22, 26, 29, 32, 35, 39, 42, 45, 48, 52, 55, 58, 61, 65, 68, 71, 74, 78, 81, 84)
  nearest_note <- function(x, reference_notes) {
    reference_notes[which.min(abs(x - reference_notes))]
  }
  notes_major7th <- notes %>%
    map_dbl(., ~ nearest_note(.x, major7th))
}

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

