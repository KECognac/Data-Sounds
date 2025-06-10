# Script to test sonifying data using USGS discharge data as
# an example.


# TO DO

# 1. Notes Library
# 2. Curating an emotional experience
# 3. Key
#    - happy - major
#    - sad - minor
#    - 
# 4. Temp, DO, Flow, Turbidity


source('setup.R')
light_pal <- c("#002EA3","#E70870","#745CFB","#256BF5","#1E4D2B","#56104E")
dark_pal <- c("#000000","#FFFFFF","#1E4D2B","#745CFB","#FFCA3A","#56104E")

# set wave player - Note this is for mac. 
setWavPlayer("afplay")


# For this example, we'll pull data from the Poudre Water Quality Network.

data <- arrow::read_parquet("data/all_pwqn_data.parquet")
sites <- unique(data$site)
parameters <- unique(data$parameter)


# load PWQN data
all_PWQN <- load_psn()

# Select subset necessary for sound only
sites <- c("archery virridy","archery","boxcreek","boxelder","cbri",              
           "chd", "joei",  "lbea",  "legacy",  "lincoln",           
           "pbd", "penn", "pfal", "prospect virridy", "prospect",          
           "river bluffs", "sfm",  "springcreek", "tamasag", "timberline virridy",
           "timberline")

# Select site
site_sel <- "prospect virridy"

# Filter all data to site only
site_PWQN <-  all_PWQN %>%
  dplyr::filter(site == site_sel) 

# Plot
ggplot(site_PWQN, aes(x = as.Date(DT_join), y = clean_mean, color = parameter)) + 
  geom_point(size = .5) +
  theme_bw() +
  scale_color_manual("", values = c(light_pal, dark_pal)) +
  facet_wrap(~parameter, scales = "free")

# Now, select parameter to sonify
pars <- c(5) # can also type string here
sd1 <- site_PWQN %>% 
  dplyr::filter(site == site_sel, 
                parameter %in% unique(parameter)[pars]) %>%
  dplyr::mutate(date = as_datetime(DT_join)) %>%
 # dplyr::filter(date < as.Date("2023-12-12")) #%>%
  dplyr::select(c(date, parameter, value = clean_mean)) %>%
  dplyr::filter(date <= min(date) + weeks(4)) #%>%
 # pivot_wider(names_from = "parameter", values_from = "value")

# Define name for saving subsequent files
fname <- paste0(site_sel,"_",paste(sd1$parameter %>% unique(), collapse = "_"))

pg <- ggplot(sd1, aes(x = date, y = value, color = parameter)) + 
  geom_point(size = 0.5) + theme_bw() + 
  scale_color_manual("", values = light_pal) +
  facet_wrap(~parameter, scales = "free") 
pg

# if multiple parameters are selected, pivot table. 
sd <- sd1 %>%
 pivot_wider(names_from = "parameter", values_from = "value") %>%
  drop_na()

# Save figure of data to be sonified
ggsave(paste0("/Users/kcognac/Desktop/KEC_Docs/PSN_Sounds/",fname,"_data.jpg"),
       plot = pg, dpi = 600, height = 3, width = 6, units = "in")  



############################## SONIFY DATA  ####################################

# Some frequencies for reference - C3 indicates C, third octave
# Each time you divide the frequency by 2, you go down an octave. 
# Each note is 2^(1/12) because there are 12 half steps in an octave.
# Frequency (Hz)
#.    C3 130.81
#.    C4 (Middle C)	261.63
#.    C#4/Db4	277.18
#.    D4	293.66
#.    D#4/Eb4	311.13
#.    E4	329.63
#.    F4	349.23
#.    F#4/Gb4	369.99
#.    G4	392.00
#.    G#4/Ab4	415.30
#.    A4	440.00
#.    A#4/Bb4	466.16
#.    B4	493.88

# Set some parameters for output sound
total_seconds <- 60 # total length of "song"
note_len <- .1 # length of each note
ref_freq <- 98 # 
n_octaves <- 4

main <- sonify_data2(data_array_to_sonify = sd[,2],
                    #ref_freq = ref_freq,
                    to_plot = FALSE,
                    octaves = n_octaves,
                    note_length = note_len,
                    total_seconds = total_seconds,
                    wave_type = "sine", # square, triangle, sawtooth, sine
                    scale = "minor",
                    to_play = FALSE) 

# Play raw wave
play(main[[1]])

# Add feedback
feedback_wave <- add_feedback(main[[1]], feedback_gain = 1, delay_samples = 44100 * .8) #%>%
    add_feedback(feedback_gain = 1, delay_samples = floor(44100 * .7)) %>%
    add_feedback(feedback_gain = 1, delay_samples = 44100 * .5) %>%
    add_feedback(feedback_gain = .5, delay_samples = 44100 * .2) 
    
play(feedback_wave)

writeWave(feedback_wave, paste0("/Users/kcognac/Desktop/KEC_Docs/PSN_Sounds/",fname,"_slow.wav"))

jpeg(paste0("/Users/kcognac/Desktop/KEC_Docs/PSN_Sounds/",fname,"_notes.jpg"), 
     width = 6, height = 6, res = 200, units = "in")
print(main[[2]][[5]])

dev.off()

# Now, join to a second wave
#a <- animate_sound(main[[2]][[6]], .2, 30)


base <- sonify_data2(data_array_to_sonify = sd[,2],
                     ref_freq = ref_freq/2, # C2                                                                                                                                             
                     to_plot = TRUE,
                     octaves = 4,
                     amp = .1,
                     note_length = 1,
                     total_seconds = total_seconds,
                     wave_type = "square", # square, triangle, sawtooth, sine
                     scale = "minor",
                     to_play = FALSE) 
# Now, join to other wave
joined_wave <- feedback_wave
joined_wave@.Data[,1] <- joined_wave@.Data[,1] + base[[1]]@.Data[,1]
joined_wave@.Data[,2] <- joined_wave@.Data[,2] + base[[1]]@.Data[,2]
joined_wave <- tuneR::normalize(joined_wave, unit = "16")

play(base[[1]])

# Play joined wave
play(joined_wave)


# save joined wave
writeWave(joined_wave, paste0("/Users/kcognac/Desktop/KEC_Docs/PSN_Sounds/",fname,".wav"))


#feedback_wave <- add_feedback(feedback_wave, feedback_gain = .5, delay_samples = 44100 * .1)  # 0.2 second delay

# Play the wave with feedback
play(feedback_wave)

# Save final figure with notes.
jpeg(paste0("/Users/kcognac/Desktop/KEC_Docs/PSN_Sounds/",fname,"_notes.jpg"), 
     width = 6, height = 6, res = 200, units = "in")

print(test[[2]][[6]])

dev.off()



##### Playing around with a bunch of functions - in development


# envelope filter concept.
apply_envelope_filter <- function(waveform, note_duration, attack, decay, release, base, sustain, overlap_factor = 0.5) {
 
  #waveform <- main[[1]]
  #sr <- 44100
  #note_duration <- 0.2
  ##attack <-  .2
  #decay <- .3
  #release <- 0.4
  #base <- 0.5
  #sustain <- 0.7
  #overlap_factor <- 0.5
  # ADD OVERLAP 
  
   # Calculate the number of samples per note
  sr <- waveform@samp.rate
  samples_per_note <- note_duration * sr
  overlap_samples <- overlap_factor * samples_per_note
  num_notes <- floor(length(waveform) / (note_duration * sr))
  sustain_time <- 1 - (attack + decay + release)
  

  create_envelope <- function(attack, decay, sustain, release, samples_per_note) {
    # Calculate the number of samples for each stage
    attack_samples <- attack * samples_per_note
    decay_samples <- decay * samples_per_note
    sustain_samples <- sustain_time * samples_per_note
    release_samples <- release * samples_per_note
    
    # Create the envelope using cosine curves for smooth transitions
    attack_envelope <- (1-base) * sin(pi/2 * seq(0, 1, length.out = attack_samples)) + base
    decay_envelope <- (1-sustain)/2 * cos(pi * seq(0, 1, length.out = decay_samples)) + (sustain + (1-sustain)/2)
    sustain_envelope <- rep(sustain, sustain_samples)
    release_envelope <- (sustain-base) * cos(pi/2 * seq(0,1, length.out = release_samples)) + base
    # Combine the envelope segments, ensuring a smooth transition
    envelope <- c(attack_envelope, decay_envelope, sustain_envelope, release_envelope)
    
    return(envelope)
  }
  
  envelope <- create_envelope(attack, decay, sustain, release, samples_per_note)
  plot(envelope)
  
  
  # Overlap and Smooth Envelope
  overlap_samples <- overlap_factor * samples_per_note
  num_notes <- ceiling(length(waveform) / (samples_per_note - overlap_samples))
  
  envelope_list <- list()
  for (i in 1:num_notes) {
    start_idx <- (i - 1) * (samples_per_note - overlap_samples) + 1
    end_idx <- min(start_idx + samples_per_note - 1, length(waveform))
    
    # Create a smoothed envelope with overlap
    envelope <- create_envelope(attack, decay, sustain, release, overlap_samples)
    envelope_list[[i]] <- envelope[1:(end_idx - start_idx + 1)]
  }
  
  # Combine envelopes, ensuring smooth transitions
  combined_envelope <- do.call(c, envelope_list)
  combined_envelope <- combined_envelope[1:length(waveform)]
  
  
  
  env_ar <- rep(envelope, num_notes)
  
  waveform@.Data[,1] <- waveform@.Data[,1] * env_ar %>% round()
  waveform@.Data[,2] <- waveform@.Data[,2] * env_ar %>% round()
  
  waveform <- normalize(waveform)
  
  play(waveform)
  
  return(waveform)
}


# Trying to recreate instruments using envelope filters + harmonics
create_clarinet_sound <- function(frequency, duration, sample_rate, harmonics, amplitudes, envelope) {
  
  duration <- 30
  frequency <- 440
  sample_rate <- 44100
  harmonics <- c(1,3,5)
  amplitudes <- c(1,0.5,0.3)
  envelope <- c(seq(0, 1, length.out = 0.1 * 44100),
    seq(1, 0.8, length.out = 0.2 * 44100),
    rep(0.8, 0.5 * 44100),
    seq(0.8, 0, length.out = 0.2 * 44100))
  
  
  # Time vector
  time <- seq(0, duration, 1/sample_rate)
  
  # Create the waveform
  waveform <- 0
  for (i in 1:length(harmonics)) {
    waveform <- waveform + amplitudes[i] * sin(2 * pi * harmonics[i] * frequency * time)
  }

  
  # Apply the envelope
  waveform <- waveform * envelope
  
  # Create a wave object
  wave_obj <- WaveMC(data = data.frame(FR = round(waveform), FL = round(waveform)), samp.rate = sample_rate, bit = 16)
  play(wave_obj)
  
  wave_obj <- normalize(wave_obj, unit = "16")
  
  return(wave_obj)
}

# Example usage:
clarinet_sound <- create_clarinet_sound(440, 1, 44100, 
                                        c(1, 3, 5, 7, 9), 
                                        c(1, 0.5, 0.3, 0.2, 0.1), 
                                        c(seq(0, 1, length.out = 0.1 * 44100),
                                          seq(1, 0.8, length.out = 0.2 * 44100),
                                          rep(0.8, 0.5 * 44100),
                                          seq(0.8, 0, length.out = 0.2 * 44100)))

play(clarinet_sound)

s5 = reverb(waveform,
            echoDelay = 850, echoLevel = -40)


apply_reverb <- function(waveform, delay_ms, decay_ms, mix) {
  
  waveform <- main[[1]]
  outwave <- waveform
  ldat <- waveform@.Data[,1]
  rdat <- waveform@.Data[,1]
  samp_rate <- waveform@samp.rate
  Delay <- 400
  Spread <- 1500
  Level <- -15
  Density <- 100
  eDelay <- NULL#c(50,100)
  eLevel <- NULL #c(-4, -6)

  lr <- reverb(ldat, samplingRate = samp_rate, 
               echoDelay = eDelay,
               echoLevel = eLevel,
              reverbDelay = Delay, 
              reverbSpread = Spread,
              reverbLevel = Level, 
              reverbDensity = Density)
  
  rr <- reverb(rdat, samplingRate = samp_rate, 
               echoDelay = eDelay,
               echoLevel = eLevel,
               reverbDelay = Delay, 
               reverbSpread = Spread,
               reverbLevel = Level, 
               reverbDensity = Density)
  
  outwave@.Data[,1] <- lr[1:length(ldat)]
  outwave@.Data[,2] <- rr[1:length(rdat)]
  
  normalize(outwave)
  
  play(outwave)
  
  return(outwave)

}

