# Function to take in notes and convert to midi
#' @param notes an array of semitones to convert
#' @param tonic the tonic midi note of the key the notes are centered around
#'              default set to 60 which is middle C

notes_to_midi <- function(relative_notes, tonic = 60) {

relative_notes <- snotes
  
# 2. Convert relative intervals to absolute MIDI notes (offsets from tonic)
# Add the tonic to all values to get the absolute MIDI pitch number.
absolute_notes <- tonic + relative_notes


# 3. Define the parameters for each note event.
# Time: The time of each note event in "ticks" (a unit of musical time).
# We'll make each note start at a new time, following the previous note.
# A simple way is to use a cumulative sum.
durations_ticks <- rep(480, length(absolute_notes)) # 480 ticks per quarter note
start_times_ticks <- cumsum(c(0, durations_ticks[-length(durations_ticks)]))

# Type: The event type. 144 is "Note On", 128 is "Note Off".
# We need both an "on" and "off" event for each note.
event_types <- rep(c(144, 128), each = length(absolute_notes))

# Note: The absolute MIDI note number (0-127).
# We need to repeat the notes for both "on" and "off" events.
note_pitches <- rep(absolute_notes, 2)

# Velocity: How "hard" the note is played (0-127). 0 is an "off" event.
# The velocity for "Note On" is typically between 60-120. We'll use 100.
# The velocity for "Note Off" is always 0.
velocities <- c(rep(100, length(absolute_notes)), rep(0, length(absolute_notes)))

# Times: The timestamp of each event.
# "Note Off" events happen after the duration of the note.
on_times <- start_times_ticks
off_times <- start_times_ticks + durations_ticks
event_times <- c(on_times, off_times)

duration <- .1

# 2. Combine the vectors into a data frame and sort by time.
midi_df <- data.frame(
  time = event_times,
  duration = duration,
  type = event_types,
  note = note_pitches,
  velocity = velocities,
  channel = 1 # MIDI channel
)

# Sort the events by their time to ensure a proper sequence.
midi_df <- midi_df[order(midi_df$time), ]

}
