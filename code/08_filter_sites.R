# 08: pull out seasonality metrics from wavelet that are 6 months

# Libraries -------------------------------------------------------------

library(tidyverse)
library(fs)
library(glue)
library(viridis)
library(lubridate)
library(sf)
library(patchwork)
library(mapview)
mapviewOptions(fgb = FALSE)

# Data --------------------------------------------------------------------

# Get Colwell
df_col <- read_rds("output/wavelet_csci_colwell_final.rds")

# Get Wavelets
load("output/wavelet_combined_period_power_outputs.rda")
# df_wav_max: max power no matter the period
# df_wav: raw data, all points
# df_wav_12: max power at 12 months
# df_wav_6: max power at 6 months

# get GAGE metadata
usgs_daily_alt <- read_csv("data/usgs_metadata_alt_gages.csv") 
usgs_daily_ref <- read_csv("data/usgs_metadata_ref_gages.csv") %>% 
  mutate(site_id = as.character(site_id))
gages_meta <- bind_rows(usgs_daily_alt, usgs_daily_ref) # 969
table(gages_meta$gagetype) # 221 ref

rm(usgs_daily_alt, usgs_daily_ref)

## Get StreamClasses -------------------------------------------------------

ceff_strmclass <- st_read("data/eflows_final_classification_9CLASS/Final_Classification_9CLASS.shp")

# crosswalk
strmclass_xwalk <- tibble("CLASS"=c(1,2,3,4,5,6,7,8,9), "CLASS_NAME"=c("snowmelt","high-volume snowmelt and rain", "low-volume snowmelt and rain", "winter storms","groundwater","perennial groundwater and rain","flashy, ephemeral rain","rain and seasonal groundwater","high elevation low precipitation"))

# join with class names
ceff_strmclass <- left_join(ceff_strmclass, strmclass_xwalk)
st_crs(ceff_strmclass)$epsg

# check with FFC Ref list?
ref_223 <- read_csv("https://raw.githubusercontent.com/ceff-tech/bmi_ffm_links/main/data/usgs/gages_ref_223_period_record.csv") %>% 
  separate(stream_class, into = c("class","CLASS"), sep="-", remove=T) %>% 
  select(-c(class, stat, data)) %>% 
  mutate(CLASS = as.numeric(CLASS),
         site_id = as.character(gage))

# join:
ref_223 <- left_join(ref_223, strmclass_xwalk)
table(ref_223$CLASS)

# join with gage metadata
ref_223_meta <- left_join(ref_223, gages_meta) %>% 
  select(CLASS, gage, site_id, minYr, maxYr, YrRange, yr_begin, yr_end, yr_total, gagetype)

write_csv(ref_223_meta, file = "output/ref_gages_period_of_rec_comparison.csv")

# Calculate Wavelet Quantiles and Filter  ----------------------------------

length(unique(df_wav_6$site_id))
length(unique(df_wav_12$site_id))

# view quantiles (6 mon)
round(quantile(df_wav_6$Power.avg, 
               probs = c(0, 0.1,.25, 0.5, .75, 0.8, 0.9, 1)), 2)
# 0%   10%   25%   50%   75%   80%   90%  100% 
# 0.12  1.10  1.61  2.37  3.83  4.54  6.58 16.15

# set percentile
wav6_90 <- round(quantile(df_wav_6$Power.avg, 
                           probs = c(0.9)), 2)

# take quantiles for 12 mon
round(quantile(df_wav_12$Power.avg, 
               probs = c(0, 0.1,.25, 0.5, .75, 0.8, 0.9, 1)), 2)
# 0%   10%   25%   50%   75%   80%   90%  100% 
# 0.06  1.59  3.28  6.64 11.31 13.30 17.96 43.71 
wav12_90 <- round(quantile(df_wav_12$Power.avg, 
                           probs = c(0.9)), 2)


# take quantiles for wav_max
round(quantile(df_wav_max$Power.avg, 
               probs = c(0, 0.1,.25, 0.5, .75, 0.8, 0.9, 1)), 2)
# 0%   10%   25%   50%   75%   80%   90%  100% 
#  3.41  4.67  5.37  7.37 11.53 13.37 17.95 43.71
wavmx_90 <- round(quantile(df_wav_max$Power.avg, 
                           probs = c(0.9)), 2)

## Filter to 6 Months Quantiles --------------------------------------------

# filter to gages in top 90%
df_wav_6 %>% filter(Power.avg>=wav6_90) %>% ungroup() %>% tally() # n=97
df_wav_6 %>% filter(Power.avg>=wav6_90) %>% 
ggplot() + 
  geom_boxplot(aes(x=gagetype, y=Power.avg, fill=gagetype), notch = TRUE,
               notchwidth = 0.3) +
  scale_fill_viridis_d(option = "B")

# how many unique gages?
df_wav_6 %>% filter(Power.avg>=wav6_90) %>% ungroup() %>% 
  distinct(site_id) %>% tally()

## Filter to 12 Months Quantiles --------------------------------------------

# filter to gages in top 90%
df_wav_12 %>% filter(Power.avg>=wav12_90) %>% ungroup() %>% tally() # n=97
df_wav_12 %>% filter(Power.avg>=wav12_90) %>% 
  ggplot() + 
  geom_boxplot(aes(x=gagetype, y=Power.avg, fill=gagetype), notch = TRUE,
               notchwidth = 0.3) +
  scale_fill_viridis_d(option = "B")

# how many unique gages?
df_wav_12 %>% filter(Power.avg>=wav12_90) %>% ungroup() %>% 
  distinct(site_id) %>% tally()

# Join these With Metadata ------------------------------------------------

df_meta <- df_wav_max %>% 
  left_join(., gages_meta, by=c("site_id")) %>% 
  st_as_sf(coords=c("dec_long_va","dec_lat_va"), crs=4269, remove=FALSE)


# make spatial and plot
df_meta6 <- df_wav_6 %>% filter(Power.avg>=wav6_90) %>%
  left_join(., gages_meta, by=c("site_id")) %>% 
  st_as_sf(coords=c("dec_long_va","dec_lat_va"), crs=4269, remove=FALSE)

df_meta12 <- df_wav_12 %>% filter(Power.avg>=wav12_90) %>%
  left_join(., gages_meta, by=c("site_id")) %>% 
  st_as_sf(coords=c("dec_long_va","dec_lat_va"), crs=4269, remove=FALSE)

mapview(df_meta6, zcol="gagetype.y", 
        col.regions=viridis::viridis(2,option = "C", direction = -1),
        #col.regions=RColorBrewer::brewer.pal(2,"Set1"), 
        layer.name="GageType (6m)", cex=2.5) + 
  mapview(df_meta12, zcol="gagetype.y", layer.name="GageType (12m)",
          col.regions=viridis::viridis(2,option = "C", direction = -1))


# FILTER OUT CANALS -------------------------------------------------------

# need to filter out a lot of the highly altered / diversion type gages
df_meta %>% 
  filter(grepl("CN|TAILRACE|DIV|INTAKE|WEIR|CONDUIT| PP| PH", station_nm)) %>% 
    distinct(site_id, .keep_all = TRUE) %>% 
    #View() # n=248, 
    #but need to add TUO early Intake sites (11276600, 11276900), 11403530 bucks ck, 11230530 bear ck
    # mapview(., zcol="Power.avg")
    st_drop_geometry() %>% 
    filter(!site_id %in% c("11276600", "11276900", "11403530", "11230530")) %>% 
    select(site_id, station_nm) -> df_canals_to_drop  

df_meta_filt <- df_meta %>% 
  filter(!site_id %in% df_canals_to_drop$site_id) %>% 
  # add a few spots
  filter(!site_id %in% c("11299996", "10290500"))

mapview(df_meta_filt, zcol="Power.avg", shape="gagetype.y", layer.name="GageType (Max)")  

# Add Full colwells ----------------------------------------------------------

df_colwell_all_meta <- read_rds("output/usgs_gages_colwells_metric.rds") %>% 
  select(site_no, MP_metric)

df_meta_filt <- df_meta_filt %>% 
  left_join(., df_colwell_all_meta, by=c("site_id"="site_no")) %>% 
  select(site_id, MP_metric, Power.avg:dec_long_va, alt_va, huc8, date_begin:gagetype.y)


## Spatial Join by StreamClass -----------------------------------------------

# make diff proj
# df_meta_filt_class <- df_meta_filt %>% st_transform(3310)
# ceff_strmclass <- ceff_strmclass %>% st_transform(3310) %>%
#   st_zm()
# df_meta_filt_class <- st_join(df_meta_filt_class, ceff_strmclass, st_is_within_distance, dist = 100)
# 
# table(df_meta_filt_class$CLASS, useNA = "ifany")



# FINAL DATASET: Filtered Meta with Colwells --------------------------------

# this is the selected gages with stream class and colwell, 
# needs to be filtered by bunk gages
df_col %>% 
  filter(!site_id %in% df_canals_to_drop$site_id) %>% 
  # add a few spots
  filter(!site_id %in% c("11299996", "10290500")) %>% 
  st_as_sf(coords=c("dec_long_va", "dec_lat_va"), crs=4326, remove=FALSE) -> df_final_filt

# map
mapview(df_final_filt, zcol="Power.avg", layer.name="Wavelet Power") + 
  mapview(df_final_filt, zcol="MP_metric", cex=3.5, layer.name="Colwell")

# SAVE --------------------------------------------------------------------

# final csci dataset
write_rds(df_final_filt, file = "output/csci_wavelet_col_finalsites_filtered.rds")
write_rds(df_meta_filt, file = "output/wavelet_colwell_all_sites_filtered.rds")
