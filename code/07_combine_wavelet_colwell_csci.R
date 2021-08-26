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

# Get Colwell Data ------------------------------------------------------

# Get Colwell
df_colwell <- read_rds("output/usgs_gages_colwells_metric.rds")
table(df_colwell$gagetype)
# ALT 517 REF 221

length(unique(df_colwell$site_no)) # n=738

# Get CSCI Data ------------------------------------------------------

# CSCI
# csci_trim <- read_rds("data/02c_selected_final_bmi_csci_dat_trim.rds")
# csci_trim %>% st_drop_geometry() %>% 
#   #distinct(StationCode, site_id, .keep_all = TRUE) %>% nrow() # 431
#   #distinct(StationCode, .keep_all = TRUE) %>% nrow() # 246 CSCI sites
#   distinct(site_id, .keep_all = TRUE) %>% nrow() # 209 CSCI sites
# table(csci_trim$CEFF_type, useNA = "ifany") # n=527 alt, n=193 REF

csci_por <- read_rds("data/02c_selected_final_bmi_csci_dat.rds") %>% 
  distinct(StationCode, SampleID, site_id, .keep_all = TRUE) %>% 
  select(StationCode:HUC_12, site_id, station_nm, lat, lon, comid:sampledate) %>%
  distinct(SampleID, site_id, .keep_all=TRUE)

# # this includes replicates, randomly sample 1 from each period
# csci_por_n1 <- csci_por %>% ungroup %>% st_drop_geometry() %>% 
#   group_by(StationCode, site_id) %>% 
#   arrange(desc(sampledate)) %>% 
#   slice(n = 1)
# csci_por_n1 %>% distinct(StationCode, site_id, .keep_all = TRUE) %>%  
#   nrow() # 472

# length(unique(csci_por_n1$StationCode)) # n=275 CSCI
# length(unique(csci_por_n1$site_id)) # n=226 USGS
# table(csci_por_n1$CEFF_type) # n=358 alt, n=114 REF


# Get Wavelet Data --------------------------------------------------------

# Get Wavelet
load("output/wavelet_combined_period_power_outputs.rda")
# df_wav_max: max power no matter the period
# df_wav: raw data, all points
# df_wav_12: max power at 12 months
# df_wav_6: max power at 6 months


# Get Gage Metadata -------------------------------------------------------

# Get GAGE metadata
usgs_daily_alt <- read_csv("data/usgs_metadata_alt_gages.csv")
usgs_daily_ref <- read_csv("data/usgs_metadata_ref_gages.csv") %>% 
  mutate(site_id = as.character(site_id))
gages_meta <- bind_rows(usgs_daily_alt, usgs_daily_ref) %>% 
  filter(site_id %in% df_colwell$site_no)
rm(usgs_daily_alt, usgs_daily_ref)

table(gages_meta$gagetype)

# JOIN COLWELL AND CSCI -----------------------------------------------

csci_por_colwell <- left_join(csci_por, df_colwell, by=c("site_id"="site_no")) %>%
  distinct(StationCode, site_id, .keep_all=TRUE)
table(csci_por_colwell$gagetype)
csci_por_colwell %>% distinct(site_id, .keep_all = TRUE) %>% 
  group_by(gagetype) %>% tally()

# ALT REF 
# 293 114 # but includes replicate CSCI samples
# some NA's
# ALT REF
# 140 55
# Join Colwell with StreamClass -------------------------------------------

# get streamclass
ceff_strmclass <- read_rds("data/eflows_final_classification_9CLASS/ca_stream_class3-9_final.rds")

# tally:
csci_por_colwell %>% st_drop_geometry() %>% 
  distinct(site_id, .keep_all = TRUE) %>% 
  dplyr::select(gagetype) %>% table( useNA = "always")


# JOIN with csci_por_colwell by COMID
df_csci_final <- left_join(st_drop_geometry(csci_por_colwell), 
                           st_drop_geometry(ceff_strmclass), 
                           by=c("comid"="COMID")) %>% 
  select(-station_nm.y) %>% 
  rename(station_nm = station_nm.x) %>% 
  distinct(.keep_all = TRUE) %>% 
  filter(!is.na(gagetype)) %>% 
  #fix some NAs
  mutate(class3_name = case_when(
    is.na(class3_name) ~ "RAIN",
    TRUE ~ class3_name),
    class3_id = case_when(
      is.na(class3_id) ~ 3,
      TRUE ~ class3_id)
  )

# summarize
df_csci_final %>% 
  distinct(site_id, .keep_all = TRUE) %>% 
  select(gagetype) %>% table( useNA = "always")
df_csci_final %>% 
  distinct(site_id, .keep_all = TRUE) %>% 
  select(class3_name) %>% table( useNA = "always")

# make spatial
df_csci_final_sf <- df_csci_final %>% 
  st_as_sf(coords=c("usgs_lon", "usgs_lat"), crs=4269, remove=FALSE) 

# map
mapviewOptions(platform = "leaflet") # change for attributes
mapview(df_csci_final_sf, zcol="class3_name", burst=TRUE)

mapviewOptions(platform = "mapdeck") # change for faster map
mapview(df_csci_final_sf, zcol="class3_name") + 
  mapview(ceff_strmclass, zcol="class3_name")

# double check
table(df_csci_final_sf$class3_name, useNA = "ifany") # 89 mixed, 269 rain, 52 snowmelt
table(df_csci_final_sf$gagetype, useNA = "ifany") # ALT 294, REF 116

# Join Colwell with Wavelet -----------------------------------------------

# join with 12 month value
df_final <- left_join(df_csci_final, df_wav_12, by="site_id") %>%
  ungroup() %>% 
  select(-gagetype.x) %>% 
  rename(gagetype=gagetype.y)
#distinct(site_id, StationCode, SampleID, .keep_all = TRUE)

df_final %>% distinct(site_id, .keep_all=TRUE) %>% 
  select(gagetype) %>% table(useNA = "always")
# matches!!

## Seasonality vs. Predict by StreamClass for -------

df_final %>%
  ggplot() + 
  geom_point(aes(x=MP_metric, y=Power.avg, fill=gagetype, 
                 shape=gagetype), alpha=0.8)+
                 #size=Period), alpha=0.8) + 
  geom_smooth(aes(x=MP_metric, y=Power.avg, color=gagetype), 
              method = "glm", 
              se = FALSE) +
  scale_shape_manual("GageType", values=c(21,22)) +
  #scale_size_binned("Period (months)",breaks = c(3,6,9,12)) +
  ggthemes::scale_fill_colorblind("GageType") +
  ggthemes::scale_color_colorblind("GageType") +
  theme_classic(base_family = "Roboto Condensed") +
  facet_wrap(.~class3_name)

ggsave(filename = "figures/wavelet_vs_colwell_by_streamclass_glm.png", 
       width = 11, height = 8, dpi=300, units = "in")

# plot 2: colwell vs. csci
df_final %>%  
  ggplot() + 
  geom_point(aes(x=MP_metric, y=csci, fill=class3_name, shape=gagetype),
             alpha=0.8, size=4, show.legend = FALSE) + 
  scale_shape_manual("GageType", values=c(21,22)) +
  geom_smooth(method = "glm",se = FALSE,
              aes(x=MP_metric, y=csci, group=class3_name), col="black", alpha=0.5) +
  #ggthemes::scale_fill_colorblind("GageType") +
  scale_fill_viridis_d("GageType") +
  theme_classic(base_family = "Roboto Condensed") +
  facet_wrap(gagetype~class3_name)

ggsave(filename = "figures/colwell_vs_csci_by_streamclass_w_trendline.png", 
       width = 11, height = 8, dpi=300, units = "in")

# plot 3: wavelet vs. csci
df_final %>%  
  ggplot() + 
  geom_point(aes(x=Power.avg, y=csci, fill=class3_name, shape=gagetype),
             alpha=0.8, size=4, show.legend = FALSE) + 
  scale_shape_manual("GageType", values=c(21,22)) +
  geom_smooth(method = "glm",se = FALSE,
              aes(x=Power.avg, y=csci, group=class3_name), col="black", alpha=0.5) +
  #ggthemes::scale_fill_colorblind("GageType") +
  scale_fill_viridis_d("GageType") +
  theme_classic(base_family = "Roboto Condensed") +
  facet_wrap(gagetype~class3_name)

ggsave(filename = "figures/wavelet_vs_csci_by_streamclass_w_trendline.png", 
       width = 11, height = 8, dpi=300, units = "in")

# Save Data Out -----------------------------------------------------------

write_rds(df_final, file = "output/wavelet_csci_colwell_final.rds")


