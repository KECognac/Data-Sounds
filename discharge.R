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
  dplyr::filter(year(Date)==2020)

# Plot

ggplot(stream_discharge, aes(x = Date, y = discharge)) +
  geom_line(color = dark_pal[4], lwd = 1) +
  theme_light() +
  labs(x = "", y = "Discharge (cfs)") +
  scale_y_log10() +
  labs(title = "Cache la Poudre River in Fort Collins, CO")

sd <- stream_discharge %>%
  dplyr::filter(year(Date) == 2020) %>%
  dplyr::select(date_num,discharge,log_dis)