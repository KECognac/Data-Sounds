#Sonify nwis

# Get USGS Data
# Select USGS gage site_no
site_no <- "06752260" #CACHE LA POUDRE RIVER AT FORT COLLINS, CO

# Download discharge
stream_discharge <- 
  dataRetrieval::readNWISdv(siteNumbers = site_no,
                            parameterCd = c('00060','00065')) %>%
  dplyr::rename_with(~c("discharge", "code"), 
                     c("X_00060_00003", "X_00060_00003_cd")) %>%
  mutate(date_num = as.numeric(Date),
         log_dis = ifelse(discharge>0,log10(discharge),0)) %>%
  dplyr::filter(year(Date) > 2010)

# Plot

ggplot(stream_discharge, aes(x = Date, y = discharge)) +
  geom_line(color = dark_pal[4], lwd = 1) +
  theme_light() +
  labs(x = "", y = "Discharge (cfs)") +
  scale_y_log10() +
  labs(title = "Cache la Poudre River in Fort Collins, CO")


total_seconds <- 120
note_len <- .2
ref_freq <- 329.63/2
#ref_freq <- 369.994/2
ref_freq = 440/2

sonified_dat <- sonify_data2(data_array_to_sonify = stream_discharge$discharge %>% log10(),
                     ref_freq = ref_freq,
                     to_plot = TRUE,
                     octaves = 3,
                     note_length = note_len,
                     total_seconds = total_seconds,
                     wave_type = "triangle", # square, triangle, sawtooth, sine
                     scale = "minor",
                     to_play = FALSE) 

sonified_dat2 <- add_feedback(sonified_dat[[1]], feedback_gain = .7, delay_samples = 44100 * .8) #%>%
  #add_feedback(feedback_gain = 1, delay_samples = floor(44100 * .7)) %>%
  #add_feedback(feedback_gain = 1, delay_samples = 44100 * .5) %>%
  #add_feedback(feedback_gain = .5, delay_samples = 44100 * .2) 

play(sonified_dat2)

sd2 <- sonify_data2(data_array_to_sonify = stream_discharge$discharge %>% log10(),
             ref_freq = ref_freq,
             to_plot = TRUE,
             octaves = 2,
             note_length = note_len,
             total_seconds = total_seconds,
             wave_type = "square", # square, triangle, sawtooth, sine
             scale = "minor",
             to_play = FALSE) 

fin <- sd2[[1]]
fin@.Data[,1] <- fin@.Data[,1] + sonified_dat[[1]]@.Data[,1] 
fin@.Data[,2] <- fin@.Data[,2] + sonified_dat[[1]]@.Data[,2] 

fin <- normalize(fin)

# Example usage:
feedback_wave <- fin %>%
  add_feedback(feedback_gain = 1, delay_samples = 44100 * .8) %>%
  add_feedback(feedback_gain = .1, delay_samples = floor(44100 * .7)) %>%
  add_feedback(feedback_gain = .4, delay_samples = 44100 * .5) %>%
  add_feedback(feedback_gain = .3, delay_samples = 44100 * .2) 

play(feedback_wave)

