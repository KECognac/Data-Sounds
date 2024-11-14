# Script to test sonifying data using USGS discharge data as
# an example.


source('setup.R')
light_pal <- c("#002EA3","#E70870","#745CFB","#256BF5","#1E4D2B","#56104E")
dark_pal <- c("#000000","#FFFFFF","#1E4D2B","#745CFB","#FFCA3A","#56104E")

# set wave player - Note this is for mac. 
setWavPlayer("afplay")


# Alt, get poudre sonde network data

verified_data <- load_psn()


# Select subset necessary for sound only

sites <- c("archery virridy","archery","boxcreek","boxelder","cbri",              
           "chd", "joei",  "lbea",  "legacy",  "lincoln",           
           "pbd", "penn", "pfal", "prospect virridy", "prospect",          
           "river bluffs", "sfm",  "springcreek", "tamasag", "timberline virridy",
           "timberline")

n <- 18
par <- 2
sd <- verified_data %>% 
  dplyr::filter(site == sites[n],
                #parameter == "FDOM Fluorescence") %>%
                #parameter == "Specific Conductivity") %>%
                #parameter == "ORP") %>%
                #parameter == "pH") %>%
                parameter == unique(parameter)[par]) %>%
  dplyr::mutate(date = as.Date(DT_join)) %>%
 # dplyr::filter(date < as.Date("2023-12-12")) #%>%
  dplyr::select(c(date, parameter, value = clean_mean))

fname <- paste0(sites[n],"_",sd$parameter[par])

pg <- ggplot(sd, aes(x = date, y = value)) + geom_point() + theme_bw() + 
  labs(title = paste0("Site: ",sites[n],"\nWav file: ",fname), y = sd$parameter[par])

pg

ggsave(paste0("/Users/kcognac/Desktop/KEC_Docs/PSN_Sounds/",fname,"_data.jpg"),
       plot = pg, dpi = 600, height = 3, width = 6, units = "in")  

#ref_freq <- 261.63 #C
#ref_freq <- 440 #A
#Note	Frequency (Hz)
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

test <- sonify_data(data_array_to_sonify = note_num_to_freq(sd$value[!is.na(sd$value)]),
                    ref_freq = 261.63,
                    to_plot = TRUE,
                    raw_min = 1,
                    raw_max = 600,
                    total_seconds = 20,
                    samp_width = 4*64) 


writeWave(test[[1]], paste0("/Users/kcognac/Desktop/KEC_Docs/PSN_Sounds/",fname,".wav"))



jpeg(paste0("/Users/kcognac/Desktop/KEC_Docs/PSN_Sounds/",fname,"_notes.jpg"), 
     width = 6, height = 6, res = 200, units = "in")

print(test[[2]][[6]])

dev.off()






