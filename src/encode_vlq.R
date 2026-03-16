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


