# Add Wavelets with Colwell with CSCI

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
df_colwell <- read_rds("output/usgs_gages_colwells_metric.rds")
# ALT 756 REF 219

length(unique(df_colwell$site_no)) # n=967

# CSCI
csci_por <- read_rds("data/02c_selected_final_bmi_csci_dat.rds") %>% 
  distinct(StationCode, SampleID, site_id, .keep_all = TRUE) %>% 
  select(StationCode:HUC_12, site_id, station_nm, lat, lon, comid:sampledate) %>%
  distinct(SampleID, site_id, .keep_all=TRUE)

# this includes replicates, randomly sample 1 from each period
csci_por_n1 <- csci_por %>% ungroup %>% st_drop_geometry() %>% 
  group_by(StationCode, site_id) %>% 
  arrange(desc(sampledate)) %>% 
  slice(n = 1)
csci_por_n1 %>% distinct(StationCode, site_id, .keep_all = TRUE) %>%  
  nrow() # 472

length(unique(csci_por_n1$StationCode)) # n=275 CSCI
length(unique(csci_por_n1$site_id)) # n=226 USGS
table(csci_por_n1$CEFF_type) # n=358 alt, n=114 REF

# Get Wavelet
load("output/wavelet_combined_period_power_outputs.rda")
# df_wav_max: max power no matter the period
# df_wav: raw data, all points
# df_wav_12: max power at 12 months
# df_wav_6: max power at 6 months

# Get GAGE metadata
usgs_daily_alt <- read_csv("data/usgs_metadata_alt_gages.csv")
usgs_daily_ref <- read_csv("data/usgs_metadata_ref_gages.csv") %>% 
  mutate(site_id = as.character(site_id))
gages_meta <- bind_rows(usgs_daily_alt, usgs_daily_ref)

rm(usgs_daily_alt, usgs_daily_ref)

# Get StreamClasses -------------------------------------------------------

st_layers("data/eflows_final_classification_9CLASS/Final_Classification_9CLASS.shp")
ceff_strmclass <- st_read("data/eflows_final_classification_9CLASS/Final_Classification_9CLASS.shp")
st_crs(ceff_strmclass)

# crosswalk
strmclass_xwalk <- tibble("CLASS"=c(1,2,3,4,5,6,7,8,9), "CLASS_NAME"=c("snowmelt","high-volume snowmelt and rain", "low-volume snowmelt and rain", "winter storms","groundwater","perennial groundwater and rain","flashy, ephemeral rain","rain and seasonal groundwater","high elevation low precipitation"))

# join with class names
ceff_strmclass <- left_join(ceff_strmclass, strmclass_xwalk)


# JOIN COLWELL AND CSCI -----------------------------------------------

csci_por_colwell <- left_join(csci_por_n1, df_colwell, by=c("site_id"="site_no")) %>%
  distinct(StationCode, site_id, .keep_all=TRUE)
table(csci_por_colwell$gagetype)

# ALT REF 
# 360 112 

# Join Colwell with StreamClass -------------------------------------------

# JOIN with csci_por_colwell by COMID
df_csci_final <- left_join(csci_por_colwell, ceff_strmclass, by=c("COMID_nhd"="COMID")) %>% select(-station_nm.y) %>% 
  rename(station_nm = station_nm.x) %>% 
  distinct(.keep_all = TRUE)
table(df_csci_final$CLASS_NAME, useNA = "ifany") # lots of NAs
table(df_csci_final$gagetype, useNA = "ifany") # ALT 372, REF 112

# Join Colwell with Wavelet -----------------------------------------------

df_final <- left_join(df_csci_final, df_wav_max, by="site_id") %>%
  ungroup()
#distinct(site_id, StationCode, SampleID, .keep_all = TRUE)
table(df_final$gagetype.x) # which is correct?
table(df_final$gagetype.y) # this version?

# Plot with Stream Class and Update ---------------------------------------

# make sf and plot
library(mapview)
mapviewOptions(fgb = FALSE)

df_final_sf <- df_final %>% 
  st_as_sf(coords=c("dec_long_va", "dec_lat_va"), crs=4269, remove=FALSE)
df_final_sff <- df_final_sf %>% filter(is.na(CLASS_NAME))

mapview(ceff_strmclass, zcol="CLASS_NAME") + mapview(df_final_sff, zcol="gagetype.y")

# Fix NAs
# CLASS 1 = c("10270900", "10271210", "10271200")
# CLASS 3 = c("11113000", "11055350", "11404100", "11063510")
# CLASS 6 = c("11143250"),
# CLASS 7 = c("10251300","10251330")
# CLASS 8 = c("11042900", "11055350", "11044800", "11074000", )
# all remaining sites are on east side around Tahoe and Truckee

table(df_final$CLASS, useNA = "always")
#df_final %>% group_by(CLASS) %>% tally()

df_final <- df_final %>% 
  mutate(CLASS = case_when(
    grepl("10270900|10271210|10271200", site_id) ~ 1,
    grepl("11113000|11055350|11404100|11063510", site_id) ~ 3,
    grepl("11143250", site_id) ~ 6,
    grepl("10251300|10251330", site_id) ~ 7,
    grepl("11042900|11055350|11044800|11074000", site_id) ~ 8,
    TRUE ~ CLASS
  ))

table(df_final$CLASS, useNA = "always")

# get list of all other gages with NA
nagages <- df_final %>% filter(is.na(CLASS_NAME)) %>% distinct(site_id) %>% pull

# assign all to low-vol snowmelt and rain?
df_final <- df_final %>% 
  mutate(CLASS = case_when(
    site_id %in% nagages ~ 3,
    TRUE ~ CLASS
  ))

table(df_final$CLASS, useNA = "always")

## Seasonality vs. Predict by StreamClass for -------

table(df_final$gagetype.y, useNA = "always")
table(df_final$gagetype.x, useNA = "always")

df_final %>%  filter(!is.na(CLASS_NAME)) %>% 
  ggplot() + 
  geom_point(aes(x=MP_metric, y=Power.avg, fill=gagetype.x, 
                 shape=gagetype.x, size=Period), alpha=0.8) + 
  scale_shape_manual("GageType", values=c(21,22)) +
  scale_size_binned("Period (months)",breaks = c(3,6,9,12)) +
  ggthemes::scale_fill_colorblind("GageType") +
  theme_classic(base_family = "Roboto Condensed") +
  facet_wrap(.~CLASS_NAME)

ggsave(filename = "figures/wavelet_vs_colwell_by_streamclass_and_period.png", 
       width = 11, height = 8, dpi=300, units = "in")

# plot 2
df_final %>%  filter(!is.na(CLASS_NAME)) %>% 
  ggplot() + 
  geom_point(aes(x=MP_metric, y=csci, fill=CLASS_NAME, shape=gagetype.x),
             alpha=0.8, size=4, show.legend = FALSE) + 
  scale_shape_manual("GageType", values=c(21,22)) +
  geom_smooth(method = "lm",se = FALSE,
              aes(x=MP_metric, y=csci, group=CLASS_NAME), col="black", alpha=0.5) +
  #ggthemes::scale_fill_colorblind("GageType") +
  scale_fill_viridis_d("GageType") +
  theme_classic(base_family = "Roboto Condensed") +
  facet_wrap(gagetype.x~CLASS_NAME)
ggsave(filename = "figures/wavelet_vs_csci_by_streamclass_and_period_w_trendline.png", 
       width = 11, height = 8, dpi=300, units = "in")


# Save Data Out -----------------------------------------------------------

write_rds(df_final, file = "output/wavelet_csci_colwell_final.rds")


