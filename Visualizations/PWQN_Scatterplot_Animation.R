library(sf)
library(nhdplusTools)
library(tidyverse)
library(gstat)      
library(stars)      
library(gganimate)  
library(lubridate)
library(future.apply) # For parallel processing
library(sp)
library(ggfx)
library(ggplot2)


########################## Colors from Anika ###################################
# Colors
colors <- c("#40e4ee",# (bright blue)
            "#79a357", # (light green)
            #"#293529", #(dark green) removed b/c too dark
            #"#236866", #(dark blue) removed b/c too dark
            "#ffe286", # (yellow)
            "#f78032" # (orange)
)

color_func <- colorRampPalette(colors)

############################ Get Observation Data ##############################

all_PWQN <- arrow::read_parquet("data/sharing/all_pwqn_data.parquet") %>%
 # filter(between(DT_round, as.Date("2024-05-01"), as.Date("2025-07-01")))
  filter(year(DT_round) == 2024)
  
all_PWQN_wide <- all_PWQN %>%
  pivot_wider(names_from = parameter, values_from = clean_mean)

# Preview
ggplot(all_PWQN_wide, aes(x = Temperature, y = log10(`Specific Conductivity`), color = site)) + 
  geom_point() +
  theme_bw()

########################## Animation ###########################################

# 1. Select which sites to plot
sites <- all_PWQN$site %>% 
  unique() %>%
  head(16) # 16 = all sites but can modify
  
# Calculate some animation parameters
n_frames <- floor(unique(all_PWQN_wide$DT_round) %>% length() / 4)
wake_length <- 1/floor(n_frames*15/1000)

# 2. Create the animation plot
anim <- ggplot(all_PWQN_wide %>% 
                 filter(site %in% sites), 
               aes(x = Temperature, y = DO, color = site)) + 
  
  # Add Glow
  with_outer_glow(
    geom_point(size = 5),
    colour = "white", sigma = 8
  ) +
  
  # Dark Theme + no legend / text exept axis labels
  theme_void() + 
  theme(
    plot.background = element_rect(fill = "black", color = NA),
    panel.background = element_rect(fill = "black", color = NA),
    legend.position = "none",
    #panel.spacing.x = unit(-1, "inch"), 
    strip.background = element_blank(),
    strip.text = element_blank(),
    axis.title.x = element_text(color = "white", size = 14, margin = margin(t = 10)),
    axis.title.y = element_text(color = "white", size = 14, angle = 90, margin = margin(r = 10)),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) +
  
  # Neon Color Palette
  scale_color_manual(values = color_func(length(sites))) +
  labs(x = "Temperature", y = "Oxygen") +
  
  # Make it fade using transition_time + dense shadow_wake 
  transition_time(DT_round) + 
  # wake_length: 0.1 (10% of animation)
  # size = TRUE makes it taper off
  # alpha = TRUE makes it fade out
  shadow_wake(wake_length = wake_length, alpha = TRUE, size = TRUE, wrap = TRUE) #+
#facet_wrap(~site) # you could also facet so there are multiple plots

# Render the animation 
# note, to make points closer together, increase nframes. Currently, raw data
# are in 15 minute increments. So, dividing before essentially makes the data
# hourly. Alternatively, you could summarize the data ahead of time. 
# NOTE - This step takes a bit of time - for 10 months of hourly data, 12 mins
rend_anim <- animate(anim, nframes = n_frames, fps = 15, 
                     width = 800, height = 450, 
                     renderer = gifski_renderer())

# Watch animation
rend_anim

# 3. Save animation to directory (Hardcoded for now)
save_path <- "/Users/kcognac/Library/CloudStorage/OneDrive-SharedLibraries-Colostate/Ross,Matthew - ROSS Lab Shared Files/Comms/Art+Sci/kinetic/"
anim_save(paste0(save_path,
                 "poudre_temp_turbidity.gif"), 
          animation = rend_anim)





