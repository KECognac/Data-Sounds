# TuneR Notes Example
# Another example playing specific notes based on frequency in Hz

source("setup.R")

sr <- 8000 # sample rate
bits <- 16 # bits
secs <- 1. # seconds
amp <- 1.  # amplitude
t <- seq(0, secs, 1/sr)  # time vector

C0 <- 16.35  # Notes
G3 <- 196
A5 <- 880

# Create note sine wave for length t
C0 <- floor(2^(bits-2)*(amp*sin(2*pi*C0*t)))
G3 <- floor(2^(bits-2)*(amp*sin(2*pi*G3*t)))
A5 <- floor(2^(bits-2)*(amp*sin(2*pi*A5*t)))

# Concatenate
u <- tuneR::Wave(c(C0,G3,A5), samp.rate=sr, bit=bits)

play(u)
