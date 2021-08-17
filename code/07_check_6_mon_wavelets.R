# 07: pull out seasonality metrics from wavelet that are 6 months


# Libraries -------------------------------------------------------------

library(tidyverse)
library(fs)
library(purrr)
library(glue)
library(viridis)
library(lubridate)
library(sf)

# Data --------------------------------------------------------------------

df_col <- read_rds("output/wavelet_csci_colwell_final.rds")
load("output/wavelet_combined_period_power_outputs.rda")
rm(df_wav_12, df_wav_max)

# get metadata
# read in:
usgs_daily_alt <- read_csv("data/usgs_metadata_alt_gages.csv")
usgs_daily_ref <- read_csv("data/usgs_metadata_ref_gages.csv") %>% 
  mutate(site_id = as.character(site_id))
gages_meta <- bind_rows(usgs_daily_alt, usgs_daily_ref)

rm(usgs_daily_alt, usgs_daily_ref)

# Filter to 6months -------------------------------------------------------

df_wav_6 <- df_wav %>% 
  group_by(site_id, gagetype) %>% 
  mutate(Period_rnd = round(Period, 1)) %>% 
  filter(Period_rnd > 5.9 & Period_rnd < 6.1) %>% 
  slice_max(Power.avg, n = 1)


# Calculate Quantiles and Filter to Those Gages ---------------------------

# take quantiles
round(quantile(df_wav_6$Power.avg),2)
# 0%   25%   50%   75%  100% 
# 0.12  1.61  2.37  3.83 16.15

round(quantile(df_wav_6$Power.avg, 
               probs = c(0.1,.25, 0.5, .75, 0.8, 0.9)), 2)
# 10%  25%  50%  75%  80%  90% 
# 1.10 1.61 2.37 3.83 4.54 6.58 

# filter to gages in top 90%
df_wav_6 %>% filter(Power.avg>=6.58) %>% ungroup() %>% tally()
df_wav_6 %>% filter(Power.avg>=6.58) %>% 
ggplot() + 
  geom_point(aes(x=site_id, y=Power.avg, fill=gagetype), size=4, pch=21) +
  scale_fill_viridis_d()

# how many unique gages?
df_wav_6 %>% filter(Power.avg>=6.58) %>% ungroup() %>% 
  distinct(site_id) %>% tally()

# Join these With Metadata ------------------------------------------------

# make spatial and plot
df_meta <- df_wav_6 %>% filter(Power.avg>=6.58) %>%
  left_join(., gages_meta, by=c("site_id")) %>% 
  st_as_sf(coords=c("dec_long_va","dec_lat_va"), crs=4269, remove=FALSE)

library(mapview)
mapviewOptions(fgb = FALSE)

mapview(df_meta, zcol="gagetype.y")

# Get StreamClasses -------------------------------------------------------

ceff_strmclass <- st_read("data/eflows_final_classification_9CLASS/Final_Classification_9CLASS.shp")

# crosswalk
strmclass_xwalk <- tibble("CLASS"=c(1,2,3,4,5,6,7,8,9), "CLASS_NAME"=c("snowmelt","high-volume snowmelt and rain", "low-volume snowmelt and rain", "winter storms","groundwater","perennial groundwater and rain","flashy, ephemeral rain","rain and seasonal groundwater","high elevation low precipitation"))

# join with class names
ceff_strmclass <- left_join(ceff_strmclass, strmclass_xwalk)
st_crs(ceff_strmclass)$epsg

# Spatial Join ------------------------------------------------------------

#mapview(ceff_strmclass) + mapview(df_meta, col.regions="orange")

# make diff proj
df_meta <- df_meta %>% st_transform(3310)
ceff_strmclass <- ceff_strmclass %>% st_transform(3310) %>% 
  st_zm()
df_meta_class <- st_join(df_meta, ceff_strmclass, st_is_within_distance, dist = 100)
summary(df_meta_class)
mapview(df_meta_class, zcol="CLASS_NAME")


# Pull Actual Flow Data ---------------------------------------------------

devtools::install_github("ryanpeek/wateRshedTools")

load("data/usgs_Q_daily_ref_gages_rev.rda") # usgs_flows_ref
load("data/usgs_Q_daily_alt_gages_rev.rda") # usgs_flows_alt

usgs_flows_ref %>% ungroup() %>% distinct(site_no) %>% tally()
usgs_flows_alt %>% ungroup() %>% distinct(site_no) %>% tally()

# now filter to data:
refdat <- usgs_flows_ref %>% 
  filter(site_no %in% unique(df_meta_class$site_id)) %>% 
  wateRshedTools::add_WYD("date")

# now filter to data:
altdat <- usgs_flows_alt %>% 
  filter(site_no %in% unique(df_meta_class$site_id)) %>% 
  wateRshedTools::add_WYD("date")

# Plot
(g1 <- ggplot() + geom_line(data=altdat, aes(x=DOWY, y=Flow, group=site_no), color="steelblue") + labs(subtitle="Alt"))

(g2 <- ggplot() + geom_line(data=refdat, aes(x=DOWY, y=Flow, group=site_no), color="cyan4") + labs(subtitle="Ref"))

library(patchwork)
g1 + g2

# density of mean annual?
altdat %>% 
  group_by(site_no, DOWY) %>% 
  summarize(meanFlow = mean(Flow, na.rm=TRUE)) %>%
  left_join(., df_wav_6, by=c("site_no"="site_id")) %>% #View()
  ggplot() + 
  theme_bw() +
  geom_line(aes(x=DOWY, y=meanFlow, group=site_no, color=Power.avg)) +
  scale_color_viridis(limits=c(6,17)) +
  labs(subtitle = "Alt: Mean Annual Discharge (cfs)") -> gm1
gm1

refdat %>% 
  group_by(site_no, DOWY) %>% 
  summarize(meanFlow = mean(Flow, na.rm=TRUE)) %>%
  left_join(., df_wav_6, by=c("site_no"="site_id")) %>% #View()
  ggplot() + 
  theme_bw() +
  geom_line(aes(x=DOWY, y=meanFlow, group=site_no, color=Power.avg)) +
  scale_color_viridis(limits=c(6, 17)) +
  labs(subtitle = "Ref: Mean Annual Discharge (cfs)") -> gm2
gm2

# plot together
gm1 / gm2

ggsave(filename = "figures/mean_ann_flow_alt_ref.png", 
       width = 11, height = 8, dpi=300)

ggsave(filename = "figures/mean_ann_logflow_alt_ref.png", 
       width = 11, height = 8, dpi=300)
