# evaluate ffc metrics that work for both
 
library(fs)
library(glue)
library(purrr)
library(lubridate)
library(sf)
options(tidyverse.quiet = TRUE)
library(tidyverse) # load quietly
library(conflicted) # deals with conflicting functions
conflict_prefer("filter", "dplyr")
options(scipen = 100) # to print full string instead of sci notation
library(mapview)
mapviewOptions(fgb=FALSE)
options(dplyr.print_max = 1e4)

# Base Data ---------------------------------------------------------------

df_final <- read_rds("output/07_wavelet_asci_csci_colwell_final.rds")
df_final %>% distinct(bioindicator, site_id, .keep_all = TRUE) %>% 
  group_by(bioindicator, gagetype) %>% tally() 

load("data/usgs_Q_daily_ref_gages_trim.rda") # usgs_flows_ref
load("data/usgs_Q_daily_alt_gages_trim.rda") # usgs_flows_alt

# REF: filter to filtered CSCI Paired sites
flow_ref <- usgs_flows_ref_trim %>% 
  filter(site_no %in% unique(df_final$site_id)) %>% 
  # add COMID
  left_join(., df_final %>% 
              select(site_id, comid_gage, SampleID, bioindicator, biovalue, 
                     MP_metric, class3_name, CLASS_NAME, Power.avg), 
            by=c("site_no"="site_id")) %>% 
  rename(flow=Flow)

# ALT
flow_alt <- usgs_flows_alt_trim %>% 
  filter(site_no %in% unique(df_final$site_id)) %>% 
  # add COMID
  left_join(., df_final %>% select(site_id, comid_gage, SampleID, csci, 
                                  MP_metric, class3_name, CLASS_NAME, Power.avg),
            by=c("site_no"="site_id")) %>% 
  rename(flow=Flow)

# make a simple list of gage_id & gageIDName
gages <- df_final %>%
  distinct(site_id, .keep_all = TRUE) %>%
  select(site_id, comid_gage, gagetype, station_nm, usgs_lat, usgs_lon, MP_metric,
         Power.avg, class3_name,  CLASS_NAME)
summary(gages)
table(gages$gagetype)

# Data --------------------------------------------------------------------

datatype <- "predicted_percentiles" # ffc_percentiles
df_ffc_pred <- read_rds(glue("output/ffc_meta_combined_{datatype}.rds"))
datatype <- "ffc_percentiles"
df_ffc_perc <- read_rds(glue("output/ffc_meta_combined_{datatype}.rds"))

# Find out What Metrics work across Both ----------------------------------

# Metrics that Break (give NA)
df_ffc_perc %>% filter(is.na(p50)) %>% select(metric) %>% distinct()
# FA_Dur   
# FA_Mag   
# FA_Tim  
# DS_Dur_WS
# DS_Mag_50
# DS_Mag_90

df_ffc_pred %>% filter(is.na(p50)) %>% select(metric) %>% distinct() # zero

# Gages that didn't work:
df_ffc_perc %>% filter(is.na(p50)) %>% select(gageid, gagetype) %>% distinct()
# ALL are ALT
# 103087889
# 11042631
# 11042900
# 11046360
# 11316605

# predicted?
df_ffc_pred %>% filter(is.na(p50)) %>% select(gageid, gagetype) %>% distinct() # 0
df_ffc_pred %>% distinct(gageid, .keep_all=TRUE) %>% group_by(gagetype) %>% tally()
# matches 160 ALT, 55 REF

# not all metrics have values? (should be 193, some are 170)
df_ffc_pred %>% group_by(metric) %>% tally() %>% View()

df_ffc_pred %>% filter(!is.na(p50)) %>% group_by(metric) %>% tally() %>% View()

# get metrics that didn't work
df_ffc_pred %>% filter(!is.na(p50)) %>% group_by(metric) %>% tally() %>% 
  arrange(n) %>% filter(n<193) %>% pull(metric)

# list of metrics that didn't/couldn't get calculated for everything
missing_from_preds <- c("FA_Dur","Peak_Dur_10","Peak_Dur_2","Peak_Dur_5",
                        "Peak_Fre_10","Peak_Fre_2","Peak_Fre_5","SP_ROC")

# get complete list of metrics by gages:
# fullgage_by_ffclist <- expand(df_ffc_pred, gageid, metric)

# metrics are different: missing from obs compared to pred:
unique(df_ffc_perc$metric) # n=27
unique(df_ffc_pred$metric) # n=24
unique(df_ffc_perc$metric)[!unique(df_ffc_perc$metric) %in% unique(df_ffc_pred$metric)]

# still have Peak Timing Metrics: "Peak_Tim_10" "Peak_Tim_2"  "Peak_Tim_5" in the obs percentiles

# Select and Join ---------------------------------------------------------

# join the observed percentiles to predicted and find gages that are missing (or have missing data)

df_comb <- df_ffc_perc %>% select(p50, metric:usgs_lon, gagetype) %>% 
  filter(!metric %in% c("Peak_Tim_10", "Peak_Tim_2", "Peak_Tim_5")) %>% 
  rename(p50_obs=p50) %>% 
  left_join(., df_ffc_pred %>% select(p50, metric, gageid, gagetype), by=c("metric", "gageid")) %>% 
  rename(p50_pred=p50) %>% 
  relocate(p50_pred, .after=p50_obs) %>% 
  mutate(gagetype = coalesce(gagetype.x, gagetype.y)) %>% select(-gagetype.x, -gagetype.y)
  
# list of gages that are missing predicted metrics
df_comb %>% filter(is.na(p50_pred)) %>% 
  group_by(gageid) %>% tally() %>% 
  arrange(n)# n=25 gages w 8, n=2 with 16, and 32 with all NAs

# map these gages
df_comb %>% filter(is.na(p50_pred)) %>%
  #filter(metric == "DS_Dur_WS") %>%
  filter(metric == "FA_Dur") %>% 
  #group_by(metric) %>% tally() # 2 gages didn't work for anything, 25 didn't work for subset of metrics
  st_as_sf(., coords=c("usgs_lon","usgs_lat"), crs=4326, remove=FALSE) %>%
  mapview(zcol="gagetype")  # gages 11013000 and 11012500 probably should be dropped

# all over! (n=48 ALT, n=10 REF)
df_comb %>% filter(is.na(p50_pred)) %>% 
  filter(metric == "FA_Dur") %>% 
  group_by(gagetype) %>% distinct() %>% tally()

# get list of gages
# drop because near SD/Mexico border, no predictions available
gages_to_drop_in_sd <- c("11013000", "11012500")

# get list of gages to drop:

df_comb %>% filter(is.na(p50_pred)) %>% 
  filter(metric == "FA_Dur") %>% 
  group_by(gagetype) %>% distinct() %>% tally()

# FILTER TO DATA TO USE ---------------------------------------------------

# remove 2 gages above and filter to just metrics that have no NAs across all pred/obs
df_comb_final <- df_comb %>% 
  filter(!gageid %in% gages_to_drop_in_sd) %>% # drop missing gages
  filter(!metric %in% missing_from_preds) # drop metrics with NAs

summary(df_comb_final)
# 4 additional gages have NAs for only a few metrics (all DS and FA metrics)

# how many gages?
df_comb_final %>% distinct(gageid, .keep_all = TRUE) %>% 
  group_by(gagetype) %>% tally()

# ALT        158
# REF         55

# how many metrics?
df_comb_final %>% distinct(metric, gageid, .keep_all = TRUE) %>% 
  group_by(gageid) %>% tally() # 16 metrics, 213 gages

df_comb_final %>% distinct(metric, .keep_all = TRUE) %>% 
  group_by(metric) %>% 
  tally() %>% 
  pull(metric)

# "DS_Dur_WS"      "DS_Mag_50"      "DS_Mag_90"      "DS_Tim"         "FA_Mag"        
# "FA_Tim"         "Peak_10"        "Peak_2"         "Peak_5"         "SP_Dur"        
# "SP_Mag"         "SP_Tim"         "Wet_BFL_Dur"    "Wet_BFL_Mag_10" "Wet_BFL_Mag_50"
# "Wet_Tim"

# get list of gages with NAs
df_comb_final %>% filter(is.na(p50_pred)) %>% group_by(metric, gageid) %>% tally() %>% pull(gageid) %>% unique()

# how many unique site pairs: (N=883)
df_final %>% group_by(bioindicator, site_id, SampleID) %>% tally() %>% 
  group_by(bioindicator) %>% tally()

# JOIN WITH csci/colwell/wavelet
df_ffc_final <- inner_join(df_comb_final, df_final %>% select(-c(usgs_lat, usgs_lon, gagetype, station_nm)), by=c("gageid"="site_id", "comid"="comid_gage")) %>% 
  distinct(gageid, SampleID, metric, .keep_all = TRUE)

summary(df_ffc_final)

# save out
write_rds(df_ffc_final, file="output/ffc_filtered_final_combined.rds")

