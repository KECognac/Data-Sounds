library(sf)
library(nhdplusTools)
library(gstat)      
library(stars)      
library(gganimate)  
library(lubridate)
library(future.apply) # For parallel processing
library(sp)
library(ggfx)
library(tidyverse)

############################## Define Functions ################################

download_nhd_flowlines_by_aoi <- function(aoi_polygon) {
  if (!inherits(aoi_polygon, "sf") || sf::st_geometry_type(aoi_polygon) != "POLYGON") {
    stop("Input 'aoi_polygon' must be an sf object with POLYGON geometry type.")
  }
  message("Starting NHD data retrieval...")
  aoi_wgs84 <- st_transform(aoi_polygon, crs = 4326)
  nhd_list <- tryCatch({
    get_nhdplus(AOI = aoi_wgs84, realization = "flowline", streamorder = TRUE)
  }, error = function(e) {
    stop(paste("Failed to download NHD data. Error:", conditionMessage(e)))
  })
  if (!is.null(nhd_list)) { return(nhd_list) } else { return(st_sf(st_sfc(), crs = 4326)) }
}

######################## Get Flowlines ########################

set_crs <- 2232
huc8 <- get_huc(id="10190007", type = "huc08") 
nhd_flowlines <- download_nhd_flowlines_by_aoi(huc8) %>% st_transform(set_crs)

############################ Get Observation Data ##############################

all_PWQN <- arrow::read_parquet("data/sharing/all_pwqn_data.parquet") %>%
  filter(between(DT_round, as.Date("2024-05-01"), as.Date("2024-06-01")))

gauge_sites <- read.csv("data/metadata/sonde_location_metadata.csv") %>%
  mutate(lat = as.numeric(str_split(lat_long, ",", simplify = TRUE)[,1]),
         lng = as.numeric(str_split(lat_long, ",", simplify = TRUE)[,2])) %>%
  st_as_sf(coords = c("lng","lat"), crs = 4326) %>%
  mutate(site = tolower(Site)) %>%
  st_transform(crs = set_crs)

gauge_sites_cid <- 
  st_join(st_buffer(gauge_sites, 100), 
          select(nhd_flowlines, comid), 
          join = st_intersects) %>% 
  drop_na(site) %>% distinct(site, .keep_all = TRUE) %>% 
  st_centroid()

par <- "Specific Conductivity"

wq_weekly <- all_PWQN %>%
  filter(parameter == par) %>%
  mutate(week = floor_date(DT_round, unit = "day")) %>%
  group_by(site, week) %>%
  summarize(mean_val = max(log10(clean_mean), na.rm = TRUE), .groups = "drop") %>%
  drop_na(mean_val)

obs_spatial <- gauge_sites_cid %>% 
  select(site, comid) %>% 
  inner_join(wq_weekly, by = "site")

############################ Grid and Interpolation ############################

# Create Grid and Mask efficiently
bbox <- st_bbox(nhd_flowlines)

grid_template <- 
  st_as_stars(bbox, dx = 3000, dy = 3000) %>% 
  st_set_crs(set_crs) %>% 
  st_as_sf()

flowline_buffer <- 
  st_buffer(filter(nhd_flowlines, streamleve %in% c(4,5)), dist = 100)

# Fast Sparse Intersection
grid_masked <- 
  grid_template[lengths(st_intersects(grid_template, flowline_buffer)) > 0, ]

# Pre-calculate Coordinates for Matrix Math
grid_coords <- as.data.frame(st_coordinates(st_centroid(grid_masked)))
names(grid_coords) <- c("x", "y")

# Setup Parallel Processing
plan(multisession) 

# Faster "Zero-Object" Version
interpolate_week_fast <- function(current_week, obs_spatial, grid_masked, grid_coords) {
  # 1. Load gstat inside the worker
  library(gstat)
  library(sf)
  
  # 2. Filter data for the week
  week_data <- obs_spatial[obs_spatial$week == current_week, ]
  if(nrow(week_data) < 3) return(NULL)
  
  # 3. Create a clean dataframe with EXPLICIT names
  # This avoids the "Object X not found" error
  coords <- st_coordinates(week_data)
  df <- data.frame(
    val = week_data$mean_val,
    lon = coords[,1], 
    lat = coords[,2]
  )
  
  # 4. Run IDW using those explicit names
  idw_mod <- gstat(formula = val ~ 1, locations = ~lon + lat, data = df, nmax = 5)
  
  # 5. Predict onto the grid_coords (which must also have lon/lat names)
  # We rename grid_coords on the fly to match the model
  colnames(grid_coords) <- c("lon", "lat")
  preds <- predict(idw_mod, newdata = grid_coords, debug.level = 0)
  
  # 6. Attach results to the grid geometry
  out <- grid_masked
  out$var1.pred <- preds$var1.pred
  out$week <- current_week
  return(out)
}

# --- Prep the grid_coords before running ---
grid_coords_clean <- as.data.frame(st_coordinates(st_centroid(grid_masked)))
colnames(grid_coords_clean) <- c("lon", "lat")

# Run the parallel process
all_weeks <- unique(obs_spatial$week)

interpolated_series <- future_lapply(
  all_weeks, 
  interpolate_week_fast, 
  obs_spatial = obs_spatial, 
  grid_masked = grid_masked, 
  grid_coords = grid_coords_clean,
  future.seed = TRUE # Needed for random number stability in parallel
) %>% bind_rows()

all_weeks <- unique(obs_spatial$week)
message("Starting parallel interpolation...")
interpolated_series <- future_lapply(all_weeks, interpolate_week_fast, 
                                     obs_spatial = obs_spatial, 
                                     grid_masked = grid_masked, 
                                     grid_coords = grid_coords) %>% 
  bind_rows()

############################ Animation #########################################



# 1. Develop the animation

# Version 1 - normal
anim_plot <- ggplot() +
  geom_sf(data = nhd_flowlines, color = "grey40", size = 0.1) +
  geom_sf(data = interpolated_series, aes(fill = var1.pred, color = var1.pred)) +
  geom_sf(data = gauge_sites, color = "white") +
  scale_fill_viridis_c(name = paste("Log", par, " (NTU)"), option = "mako") +
  scale_color_viridis_c(option = "mako", guide = "none") +
  theme_minimal() +
  labs(title = paste("Daily", par, "along the Poudre River"),
       # This formats the date to show only Year-Month-Day
       subtitle = "Date: {format(frame_time, '%Y-%m-%d')}",
       caption = "Interpolated along major flowlines (Stream Level 4-5)") +
  transition_time(week) +
  ease_aes('linear')


# Version2 - black background glow


anim_plot <- ggplot() +
  # 1. Dark background and white text theme
  theme_void() + # Removes axes/grids for a cleaner "map" look
  theme(
    plot.background = element_rect(fill = "black", color = NA),
    panel.background = element_rect(fill = "black", color = NA),
    text = element_text(color = "white"),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    legend.position = "right",
    legend.text = element_text(color = "white"),
    legend.title = element_text(color = "white")
  ) +
  
  # 2. Subtle background flowlines
  geom_sf(data = nhd_flowlines, color = "white", size = 0.5) +
  
  # 3. GLOWING Interpolated Grid
  # 'with_outer_glow' creates the neon effect
  with_outer_glow(
    geom_sf(data = interpolated_series, aes(fill = var1.pred, color = var1.pred)),
    colour = "cyan", # Glow color
    sigma = 10,      # Blur intensity
    expand = 2       # How far the glow spreads
  ) +
  
  # 4. Sensor sites as bright white "nodes"
  #geom_sf(data = gauge_sites, color = "white", size = 1.5, shape = 18) +
  
  # 5. High-contrast "Magma" or "Rocket" palette (brighter than Mako)
  scale_fill_viridis_c(
    name = paste("Log", par), 
    option = "magma", # Rocket is very bright/energetic
    begin = 0.2,       # Avoids too-dark colors at the bottom
    end = 1
  ) +
  scale_color_viridis_c(option = "mako", guide = "none", begin = 0.2) +
  
  # 6. Labels and Animation
  labs(
    title = paste("Poudre River:", par),
    subtitle = "Date: {format(frame_time, '%Y-%m-%d')}",
    caption = "Interpolated along major flowlines (Stream Level 4-5)"
  ) +
  transition_time(week) +
  ease_aes('linear')



# Version 3 - no text, black background

anim_plot <- ggplot() +
  # 1. Dark background and remove all text/legends via theme
  theme_void() + 
  theme(
    plot.background = element_rect(fill = "black", color = NA),
    panel.background = element_rect(fill = "black", color = NA),
    legend.position = "none" # Removes the legend
  ) +
  
  # 2. Subtle background flowlines
  #geom_sf(data = nhd_flowlines, color = "white", size = 0.5) +
  
  # 3. GLOWING Interpolated Grid
  with_outer_glow(
    geom_sf(data = interpolated_series, aes(fill = var1.pred, color = var1.pred)),
    colour = "cyan", 
    sigma = 10,      
    expand = 2       
  ) +
  
  # 4. Color palettes (removed 'name' as legend is hidden)
  scale_fill_viridis_c(
    option = "magma", 
    begin = 0.2,       
    end = 1
  ) +
  scale_color_viridis_c(option = "mako", guide = "none", begin = 0.2) +
  
  # 5. Animation (Labs removed to ensure no text appears)
  transition_time(week) +
  ease_aes('linear')



# Version 4 - no text, white background

anim_plot <- ggplot() +
  # 1. Dark background and remove all text/legends via theme
  theme_void() + 
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.position = "none" # Removes the legend
  ) +
  
  # 2. Subtle background flowlines
  #geom_sf(data = nhd_flowlines, color = "black", size = 0.5) +
  
  # 3. GLOWING Interpolated Grid
  with_outer_glow(
    geom_sf(data = interpolated_series, aes(fill = var1.pred, color = var1.pred)),
    colour = "#fca9d8", 
    sigma = 10,      
    expand = 2       
  ) +
  
  # 4. Color palettes (removed 'name' as legend is hidden)
  scale_fill_viridis_c(
    option = "magma", 
    begin = 0.2,       
    end = 1
  ) +
  scale_color_viridis_c(option = "mako", guide = "none", begin = 0.2) +
  
  # 5. Animation (Labs removed to ensure no text appears)
  transition_time(week) +
  ease_aes('linear')





# 2. Render the animation
rendered_anim <- animate(anim_plot, nframes = length(all_weeks)*2, fps = 5, width = 800, height = 600)

rendered_anim

# 3. Save to your working directory
anim_save(paste0("/Users/kcognac/Desktop/Poudre Sounds Grant/kinetic/poudre_",par,"_daily_black_nostream2.gif"), 
          animation = rendered_anim)







