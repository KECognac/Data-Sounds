# This script contains a function to manually write a MIDI file
# from a data frame of MIDI events, without using the 'tuneR' writeMidi() function.

# --- Helper function for Variable-Length Quantity (VLQ) encoding ---
# MIDI uses VLQ to encode time deltas. Each byte has a continuation bit (most significant bit).
# If the bit is 1, another byte follows. If it's 0, it's the last byte.
#' @title Encode an integer as a Variable-Length Quantity (VLQ)
#' @description This function takes an integer and converts it to a sequence of
#' raw bytes according to the MIDI VLQ standard.
#' @param value An integer.
#' @return A raw vector containing the VLQ encoded value.
encode_vlq <- function(value) {
  if (value < 0) {
    stop("VLQ value must be non-negative.")
  }
  
  # Special case for 0
  if (value == 0) {
    return(as.raw(0))
  }
  
  bytes <- raw()
  
  # Start from the least significant bits and build the byte sequence
  while (value > 0) {
    byte <- value %% 128 # Get the lowest 7 bits
    value <- floor(value / 128)
    
    # If there are more bytes to come, set the MSB (most significant bit) to 1
    if (value > 0) {
      byte <- byte + 128
    }
    
    bytes <- c(as.raw(byte), bytes) # Prepend to the vector
  }
  
  return(bytes)
}

# --- Main function to manually write the MIDI file ---
#' @title Manually Write a MIDI File from a Data Frame
#' @description This function takes a data frame of MIDI events and manually
#' writes a Type 1 MIDI file to the specified path. The input data frame
#' should contain one row per MIDI event (Note On, Note Off, etc.).
#' @param midi_df A data frame containing columns 'time', 'type', 'note',
#' 'velocity', and 'channel'. Times should be in ticks (e.g., 480 per
#' quarter note).
#' @param output_path A character string for the output file path.
#' @return The function invisibly returns the output path on success.
write_midi_manual <- function(midi_df, output_path) {
  
  con <- file(output_path, "wb")
  
  # --- 1. Write the MIDI Header Chunk ('MThd') ---
  # Chunk ID ('MThd' as a raw vector)
  writeBin(as.raw(c(0x4d, 0x54, 0x68, 0x64)), con)
  # Chunk Length (6 bytes)
  writeBin(as.integer(6), con, size = 4, endian = "big")
  # Format (Type 1, multiple tracks)
  writeBin(as.integer(1), con, size = 2, endian = "big")
  # Number of Tracks (1 track)
  writeBin(as.integer(1), con, size = 2, endian = "big")
  # Time Division (e.g., 480 ticks per quarter note)
  writeBin(as.integer(480), con, size = 2, endian = "big")
  
  # --- 2. Write the MIDI Track Chunk ('MTrk') ---
  # We'll write the data to a raw vector first to get the track length
  track_data <- raw()
  
  # Sort all events by time before processing
  sorted_events <- midi_df[order(midi_df$time), ]
  
  # --- Calculate delta times using a robust manual approach ---
  delta_times <- vector("integer", nrow(sorted_events))
  last_time <- 0
  for (i in 1:nrow(sorted_events)) {
    current_time <- sorted_events$time[i]
    delta_times[i] <- current_time - last_time
    last_time <- current_time
  }
  
  # Iterate through all events and write the data
  for (i in 1:nrow(sorted_events)) {
    event <- sorted_events[i, ]
    
    # Encode and append the delta time
    track_data <- c(track_data, encode_vlq(delta_times[i]))
    
    # Encode and append the MIDI event data (type, note, velocity)
    # The 'type' is a status byte (e.g., 0x90 for Note On on channel 1)
    status_byte <- as.raw(event$type + (event$channel - 1))
    
    track_data <- c(track_data, status_byte, as.raw(event$note), as.raw(event$velocity))
  }
  
  # --- 3. Add the End of Track Meta Event ---
  # This is a standard event to signal the end of a track.
  # Delta time (0), Meta event (0xFF), End of Track (0x2F), length (0)
  track_data <- c(track_data, as.raw(c(0x00, 0xFF, 0x2F, 0x00)))
  
  # Now that we have the track data, we can write the Track Chunk header.
  # Chunk ID ('MTrk' as a raw vector)
  writeBin(as.raw(c(0x4d, 0x54, 0x72, 0x6b)), con)
  # Chunk Length (length of the track data in bytes)
  writeBin(as.integer(length(track_data)), con, size = 4, endian = "big")
  
  # --- 4. Write the final track data to the file ---
  writeBin(track_data, con)
  
  # Close the connection
  close(con)
  
  cat(paste("Successfully wrote MIDI file to:", output_path, "\n"))
  return(invisible(output_path))
}

# --- Sample Usage ---
# Create a sample data frame of notes. We need to create both Note On (type 144)
# and Note Off (type 128) events.
note_ons <- data.frame(
  time = c(0, 480, 960, 1440, 1920, 2400, 2880, 3360),
  note = c(60, 62, 64, 65, 67, 69, 71, 72),
  velocity = 100,
  channel = 1,
  type = 144, # Note On
  stringsAsFactors = FALSE
)

note_offs <- data.frame(
  time = c(240, 720, 1200, 1680, 2160, 2640, 3120, 3600),
  note = c(60, 62, 64, 65, 67, 69, 71, 72),
  velocity = 0, # Velocity for Note Off is 0
  channel = 1,
  type = 128, # Note Off
  stringsAsFactors = FALSE
)

# Combine and sort all events by time to create the final data frame
sample_midi_df <- rbind(note_ons, note_offs)
sample_midi_df <- sample_midi_df[order(sample_midi_df$time), ]

# Specify the output file path
output_file <- "/Users/kcognac/Desktop/Poudre Sounds Grant/c_major_scale.mid"

# Write the MIDI file
write_midi_manual(sample_midi_df, output_file)
