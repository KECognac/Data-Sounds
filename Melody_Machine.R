# Melody Machine
# from: https://stackoverflow.com/questions/32857065/is-it-possible-to-code-music-in-r-and-play-it-back-mac-os-x

source("setup.R")

### Create a melody from notes and frequencies

melodymachine <- function(nbnotes, seed = NULL) {
  if(!is.null(seed)){set.seed(seed)}
  muss = NULL
  for (i in 1:nbnotes) {
    yo <- abs(round(rnorm(nbnotes,400,200)))
    lengthhtime <- abs(rnorm(nbnotes,0.0,0.2))
    f <- yo[i]                 #frequency of A4 note
    titi <- lengthhtime[i]
    sr<-1000
    bits=16
    secs <- titi                #length of the note set to 2
    amp <- 0.99
    t <- seq(from = 0, to = secs, by = 1/sr)
    y <-  amp*sin(2*pi*f*t)  #make a sinewave with above attributes
    s <- floor(2^(bits-2)*y) #floor it to make it an integer value
    muss <- c(muss,s)
  }
  return(muss)
}

mel1 = melodymachine(6,seed = 1)
mel2 = melodymachine(6,seed = 1)
mel3 = melodymachine(6,seed = 1)
mel4 = melodymachine(6,seed = 1)
mel5 = melodymachine(6,seed = 1)
u=Wave(left =  c(mel1,mel2,mel3,mel4,mel5),
       right = rev(c(mel1,mel2,mel3,mel4,mel5)), 
       samp.rate=1000, bit=16)  #make a wave structure

tuneR::play(u)


plot(u, 
     info = TRUE, 
     xunit = c("time"), 
     ylim = NULL, main = "My random melody", 
     sub = "made by me", 
     xlab = NULL, ylab = NULL, 
     simplify = FALSE, nr = 2500, 
     axes = TRUE, yaxt = par("yaxt"), 
     las = 1, 
     center = TRUE)

