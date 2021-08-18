# 07: pull out seasonality metrics from wavelet that are 6 months


# Libraries -------------------------------------------------------------

library(tidyverse)
library(fs)
library(glue)
library(viridis)
library(lubridate)
library(sf)
library(patchwork)

# Data --------------------------------------------------------------------

df_col <- read_rds("output/wavelet_csci_colwell_final.rds")
load("output/wavelet_combined_period_power_outputs.rda")
#rm(df_wav_12, df_wav_max)

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
length(unique(df_wav_6$site_id))

df_wav_12 <- df_wav %>% 
  group_by(site_id, gagetype) %>% 
  mutate(Period_rnd = round(Period, 1)) %>% 
  filter(Period_rnd > 11.9 & Period_rnd <= 12.1) %>% 
  slice_max(Power.avg, n = 1)

length(unique(df_wav_12$site_id))

# Calculate Quantiles and Filter  ------------------------------------------

# take quantiles (6 mon)
round(quantile(df_wav_6$Power.avg, 
               probs = c(0, 0.1,.25, 0.5, .75, 0.8, 0.9, 1)), 2)
# 0%   10%   25%   50%   75%   80%   90%  100% 
# 0.12  1.10  1.61  2.37  3.83  4.54  6.58 16.15

# take quantiles for 12 mon
round(quantile(df_wav_12$Power.avg, 
               probs = c(0, 0.1,.25, 0.5, .75, 0.8, 0.9, 1)), 2)
# 0%   10%   25%   50%   75%   80%   90%  100% 
# 0.06  1.59  3.28  6.64 11.31 13.30 17.96 43.71 

## Filter to 6 Months Quantiles --------------------------------------------

# filter to gages in top 90%
df_wav_6 %>% filter(Power.avg>=6.58) %>% ungroup() %>% tally()
df_wav_6 %>% filter(Power.avg>=6.58) %>% 
ggplot() + 
  geom_boxplot(aes(x=gagetype, y=Power.avg, fill=gagetype), notch = TRUE,
               notchwidth = 0.3) +
  scale_fill_viridis_d(option = "B")

# how many unique gages?
df_wav_6 %>% filter(Power.avg>=6.58) %>% ungroup() %>% 
  distinct(site_id) %>% tally()

## Filter to 12 Months Quantiles --------------------------------------------

# filter to gages in top 90%
df_wav_12 %>% filter(Power.avg>=17.96) %>% ungroup() %>% tally()
df_wav_12 %>% filter(Power.avg>=17.96) %>% 
  ggplot() + 
  geom_boxplot(aes(x=gagetype, y=Power.avg, fill=gagetype), notch = TRUE,
               notchwidth = 0.3) +
  scale_fill_viridis_d(option = "B")

# how many unique gages?
df_wav_12 %>% filter(Power.avg>=17.96) %>% ungroup() %>% 
  distinct(site_id) %>% tally()


# Join these With Metadata ------------------------------------------------

# make spatial and plot
df_meta6 <- df_wav_6 %>% filter(Power.avg>=6.58) %>%
  left_join(., gages_meta, by=c("site_id")) %>% 
  st_as_sf(coords=c("dec_long_va","dec_lat_va"), crs=4269, remove=FALSE)

df_meta12 <- df_wav_12 %>% filter(Power.avg>=17.96) %>%
  left_join(., gages_meta, by=c("site_id")) %>% 
  st_as_sf(coords=c("dec_long_va","dec_lat_va"), crs=4269, remove=FALSE)

library(mapview)
mapviewOptions(fgb = FALSE)

mapview(df_meta6, zcol="gagetype.y", col.regions=RColorBrewer::brewer.pal(2,"Set1")) + mapview(df_meta12, zcol="gagetype.y")


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
df_meta6 <- df_meta6 %>% st_transform(3310)
ceff_strmclass <- ceff_strmclass %>% st_transform(3310) %>% 
  st_zm()
df_meta6_class <- st_join(df_meta6, ceff_strmclass, st_is_within_distance, dist = 100)
summary(df_meta6_class)
mapview(df_meta6_class, zcol="CLASS_NAME")

df_meta12 <- df_meta12 %>% st_transform(3310)
df_meta12_class <- st_join(df_meta12, ceff_strmclass, st_is_within_distance, dist = 100)
summary(df_meta12_class)
mapview(df_meta12_class, zcol="CLASS_NAME")

# Pull Actual Flow Data ---------------------------------------------------

#devtools::install_github("ryanpeek/wateRshedTools")

load("data/usgs_Q_daily_ref_gages_rev.rda") # usgs_flows_ref
load("data/usgs_Q_daily_alt_gages_rev.rda") # usgs_flows_alt

usgs_flows_ref %>% ungroup() %>% distinct(site_no) %>% tally()
usgs_flows_alt %>% ungroup() %>% distinct(site_no) %>% tally()

# now filter to data:
refdat6 <- usgs_flows_ref %>% 
  filter(site_no %in% unique(df_meta6_class$site_id)) %>% 
  wateRshedTools::add_WYD("date")
altdat6 <- usgs_flows_alt %>% 
  filter(site_no %in% unique(df_meta6_class$site_id)) %>% 
  wateRshedTools::add_WYD("date")

refdat12 <- usgs_flows_ref %>% 
  filter(site_no %in% unique(df_meta12_class$site_id)) %>% 
  wateRshedTools::add_WYD("date")
altdat12 <- usgs_flows_alt %>% 
  filter(site_no %in% unique(df_meta12_class$site_id)) %>% 
  wateRshedTools::add_WYD("date")


# PLOT 6MON: ---------------------------------------------------------------


# g1 <- ggplot() + geom_line(data=altdat6, aes(x=DOWY, y=Flow, group=site_no), color="steelblue") + labs(subtitle="Alt")
# g2 <- ggplot() + geom_line(data=refdat6, aes(x=DOWY, y=Flow, group=site_no), color="cyan4") + labs(subtitle="Ref")
# g1 + g2

# mean annual
# altdat6 %>% distinct(site_no) %>% nrow() n=71
altdat6 %>% 
  group_by(site_no, DOWY) %>% 
  summarize(meanFlow = mean(Flow, na.rm=TRUE)) %>%
  left_join(., df_wav_6, by=c("site_no"="site_id")) %>% #View()
  ggplot() + 
  theme_bw() +
  geom_line(aes(x=DOWY, y=log(meanFlow), group=site_no, color=Power.avg)) +
  #geom_line(aes(x=DOWY, y=meanFlow, group=site_no, color=Power.avg)) +
  scale_color_viridis(limits=c(6,17)) +
  labs(subtitle = "Alt: Mean Annual Discharge (cfs) [n=71]") -> g6_a
g6_a

#refdat6 %>% distinct(site_no) %>% nrow() n=26
refdat6 %>% 
  group_by(site_no, DOWY) %>% 
  summarize(meanFlow = mean(Flow, na.rm=TRUE)) %>%
  left_join(., df_wav_6, by=c("site_no"="site_id")) %>% #View()
  ggplot() + 
  theme_bw() +
  geom_line(aes(x=DOWY, y=log(meanFlow), group=site_no, color=Power.avg)) +
  #geom_line(aes(x=DOWY, y=meanFlow, group=site_no, color=Power.avg)) +
  scale_color_viridis(limits=c(6, 17)) +
  labs(subtitle = "Ref: Mean Annual Discharge (cfs) [n=26]") -> g6_b
g6_b

# plot together
g6_a / g6_b

ggsave(filename = "figures/mean_ann_flow_alt_ref_6mon_period.png", 
       width = 11, height = 8, dpi=300)

ggsave(filename = "figures/mean_ann_logflow_alt_ref_6mon_period.png", 
       width = 11, height = 8, dpi=300)


# PLOT 12MON ---------------------------------------------------------------

# mean annual
#altdat12 %>% distinct(site_no) %>% nrow() #n=85
altdat12 %>% 
  group_by(site_no, DOWY) %>% 
  summarize(meanFlow = mean(Flow, na.rm=TRUE)) %>%
  left_join(., df_wav_12, by=c("site_no"="site_id")) %>% #View()
  ggplot() + 
  theme_bw() +
  geom_line(aes(x=DOWY, y=log(meanFlow), group=site_no, color=Power.avg)) +
  #geom_line(aes(x=DOWY, y=meanFlow, group=site_no, color=Power.avg)) +
  #scale_color_viridis(limits=c(6,17)) +
  scale_color_viridis() +
  labs(subtitle = "Alt: Mean Annual Discharge (cfs) [n=85]") -> g12_a
g12_a

#refdat12 %>% distinct(site_no) %>% nrow() #n=12
refdat12 %>% 
  group_by(site_no, DOWY) %>% 
  summarize(meanFlow = mean(Flow, na.rm=TRUE)) %>%
  left_join(., df_wav_6, by=c("site_no"="site_id")) %>% #View()
  ggplot() + 
  theme_bw() +
  geom_line(aes(x=DOWY, y=log(meanFlow), group=site_no, color=Power.avg)) +
  #geom_line(aes(x=DOWY, y=meanFlow, group=site_no, color=Power.avg)) +
  #scale_color_viridis(limits=c(6, 17)) +
  scale_color_viridis() +
  labs(subtitle = "Ref: Mean Annual Discharge (cfs) [n=12]") -> g12_b
g12_b

# plot together
g12_a / g12_b

ggsave(filename = "figures/mean_ann_flow_alt_ref_12mon_period.png", 
       width = 11, height = 8, dpi=300)

ggsave(filename = "figures/mean_ann_logflow_alt_ref_12mon_period.png", 
       width = 11, height = 8, dpi=300)
