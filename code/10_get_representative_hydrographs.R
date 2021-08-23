# 10_Get Representative sites and Look at hydrographs

# Libraries -------------------------------------------------------------

library(tidyverse)
library(glue)
library(viridis)
library(lubridate)
library(sf)
library(plotly)
library(patchwork)
library(mapview)
mapviewOptions(fgb = FALSE)

# Data --------------------------------------------------------------------

df_final <- read_rds("output/wavelet_csci_colwell_final.rds")
df_final %>% distinct(.keep_all = TRUE) %>% 
  group_by(gagetype) %>% tally() # ALT = 305, REF=117

# Pull Actual Flow Data ---------------------------------------------------

load("data/usgs_Q_daily_ref_gages_trim.rda") # usgs_flows_ref
load("data/usgs_Q_daily_alt_gages_trim.rda") # usgs_flows_alt

usgs_flows_ref_trim %>% ungroup() %>% distinct(site_no) %>% tally()
usgs_flows_alt_trim %>% ungroup() %>% distinct(site_no) %>% tally()

# filter to filtered CSCI Paired sites
refdat <- usgs_flows_ref_trim %>% 
  filter(site_no %in% unique(df_final$site_id)) 

altdat <- usgs_flows_alt_trim %>% 
  filter(site_no %in% unique(df_final$site_id)) 

length(unique(altdat$site_no))
length(unique(refdat$site_no))


# Mean Daily Flow Plots ---------------------------------------------------



altdat %>% 
  group_by(site_no, DOWY) %>% 
  summarize(meanFlow = mean(Flow, na.rm=TRUE)) %>%
  left_join(., df_final[,c("site_id", "csci", "MP_metric")], by=c("site_no"="site_id")) %>%
  left_join(., df_wav_max, by=c("site_no"="site_id")) %>%
  ggplot() + 
  theme_bw() +
  # switch color: Power.avg or MP_Metric
  geom_line(aes(x=DOWY, y=log(meanFlow), group=site_no, color=MP_metric)) + 
  #geom_line(aes(x=DOWY, y=meanFlow, group=site_no, color=MP_metric)) +
  #scale_color_viridis(limits=c(0,10)) + # for Power.avg
  scale_color_viridis(limits=c(0,1)) + # for MP
  labs(subtitle = "Alt: Mean Annual Discharge (cfs) [n=140], 6 month peak") -> 