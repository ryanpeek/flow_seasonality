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
df_ffc_pred <- read_rds(glue("output/09_ffc_meta_combined_{datatype}.rds"))
datatype <- "ffc_percentiles"
df_ffc_perc <- read_rds(glue("output/09_ffc_meta_combined_{datatype}.rds"))

# Find out What Metrics work across Both ----------------------------------

# Metrics that had NAs
df_ffc_perc %>% filter(is.na(p50)) %>% select(metric) %>% distinct()
# DS_Dur_WS     
# DS_Tim        
# DS_Mag_50     
# DS_Mag_90     
# FA_Dur        
# FA_Mag        
# FA_Tim        
# SP_ROC        
# SP_Dur        
# SP_Mag        
# SP_Tim        
# Wet_BFL_Dur   
# Wet_BFL_Mag_10
# Wet_BFL_Mag_50
# Wet_Tim    

# count missing per metric
df_ffc_perc %>% filter(is.na(p50)) %>% group_by(metric) %>% tally()

# count non-missing per metric
df_ffc_perc %>% filter(!is.na(p50)) %>% group_by(metric) %>% tally()

# no missing in preds (as should be the case)
df_ffc_pred %>% filter(is.na(p50)) %>% select(metric) %>% distinct() # zero

# Gages that didn't work:
df_ffc_perc %>% filter(is.na(p50)) %>% select(gageid, gagetype) %>% distinct()

# 9 gages; should prob drop 7 they are intermittent at best and canals otherwise
# 103087885 ALT  Ck diversion below dam 
# 103087889 ALT  dry 
# 103087891 ALT  dry
# 103087892 ALT  dry
# 11042631  ALT  dry creek
# 11042900  ALT  paved canal
# 11046360  ALT  dry creek

# keep these
# 11316605  ALT  hydropeaking highly regulated Tiger Ck below regulator res
# 10308783  REF  natural flow but many gaps in dataset

# drop all these?
to_drop <- c("103087885", "103087889", "103087891", "103087892", 
             "11042631", "11042900", "11046360")

# count metrics by gage type
df_ffc_perc %>% filter(is.na(p50)) %>% select(gageid, gagetype, metric) %>% 
  group_by(metric, gagetype) %>% tally() %>% arrange(gagetype)

# what about if we drop gages above: # good! only final 2 gages above
df_ffc_perc %>% filter(!gageid %in% to_drop) %>% 
  filter(is.na(p50)) %>% 
  select(gageid, gagetype, metric) %>% 
  group_by(metric, gagetype) %>% tally() %>% arrange(gagetype)

# predicted w na = 0
df_ffc_pred %>% filter(is.na(p50)) %>% group_by(metric) %>% tally()
df_ffc_pred %>% distinct(gageid, .keep_all=TRUE) %>% group_by(gagetype) %>% tally()
# matches 164 ALT, 67 REF

# not all metrics have values? (should be 229, some are 194)
df_ffc_pred %>% group_by(metric) %>% tally() %>% View()

# get 8 metrics that didn't work
df_ffc_pred %>% filter(!is.na(p50)) %>% group_by(metric) %>% tally() %>% 
  arrange(n) %>% filter(n<195) %>% pull(metric)

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

# gages?
unique(df_ffc_perc$gageid)[unique(df_ffc_perc$gageid) %in% unique(df_ffc_pred$gageid)]

# Select and Join ---------------------------------------------------------

# join the observed percentiles to predicted and find gages that are missing (or have missing data)

df_comb <- df_ffc_perc %>% select(p50, metric:usgs_lon, gagetype) %>% 
  filter(!gageid %in% to_drop) %>% 
  filter(!metric %in% c("Peak_Tim_10", "Peak_Tim_2", "Peak_Tim_5")) %>% 
  rename(p50_obs=p50) %>% 
  left_join(., df_ffc_pred %>% select(p50, metric, gageid, gagetype), by=c("metric", "gageid", "gagetype")) %>% 
  rename(p50_pred=p50) %>% 
  relocate(p50_pred, .after=p50_obs) #%>% 
  #mutate(gagetype = coalesce(gagetype.x, gagetype.y)) %>% select(-gagetype.x, -gagetype.y)
  
# list of gages that are missing predicted metrics
df_comb %>% filter(is.na(p50_pred)) %>% 
  group_by(gageid) %>% tally() %>% 
  arrange(n)# n=34 gages, 32 w/ 8 missing, n=2 w 16

# map these gages
df_comb %>% filter(is.na(p50_pred)) %>%
  #filter(metric == "SP_ROC") %>%
  filter(metric == "FA_Dur") %>% 
  #group_by(metric) %>% tally() # 2 gages didn't work for anything, 25 didn't work for subset of metrics
  st_as_sf(., coords=c("usgs_lon","usgs_lat"), crs=4326, remove=FALSE) %>%
  mapview(zcol="gagetype")  # gages 11013000 and 11012500 probably should be dropped

# all over! (n=20 ALT, n=12 REF)
df_comb %>% filter(is.na(p50_pred)) %>% 
  filter(metric == "SP_ROC") %>% 
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

# how many gages?
df_comb_final %>% distinct(gageid, .keep_all = TRUE) %>% 
  group_by(gagetype) %>% tally()

# ALT        155
# REF         67

# how many metrics?
df_comb_final %>% distinct(metric, gageid, .keep_all = TRUE) %>% 
  group_by(gageid) %>% tally() # 16 metrics, 222 gages

# get list of distinct metrics that we are using
df_comb_final %>% distinct(metric, .keep_all = TRUE) %>% 
  group_by(metric) %>% 
  tally() %>% 
  pull(metric)

# "DS_Dur_WS"      "DS_Mag_50"      "DS_Mag_90"      "DS_Tim"         "FA_Mag"        
# "FA_Tim"         "Peak_10"        "Peak_2"         "Peak_5"         "SP_Dur"        
# "SP_Mag"         "SP_Tim"         "Wet_BFL_Dur"    "Wet_BFL_Mag_10" "Wet_BFL_Mag_50"
# "Wet_Tim"

# get list of gages with NAs
df_comb_final %>% filter(is.na(p50_pred)) %>% 
  group_by(metric, gageid) %>% tally() %>% 
  pull(gageid) %>% unique() # should be zero!!

# how many unique site pairs: (N=883, ASCI=365, CSCI=623)
df_final %>% group_by(bioindicator, site_id, SampleID) %>% tally() %>% 
  group_by(bioindicator) %>% tally()

# JOIN WITH csci/colwell/wavelet
df_ffc_final <- inner_join(df_comb_final, df_final %>% select(-c(usgs_lat, usgs_lon, gagetype, station_nm)), by=c("gageid"="site_id", "comid"="comid_gage")) %>% 
  distinct(gageid, SampleID, metric, .keep_all = TRUE)

summary(df_ffc_final)


# Deal with CSCI Duplicates -----------------------------------------------

# calc records by StationCode (how many mult years)
df_ffc_final %>%
  filter(bioindicator=="CSCI") %>% 
  distinct(StationCode, gageid, SampleID, .keep_all=TRUE) %>% 
  group_by(StationCode, gageid) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  filter(n>1) -> csci_sites_to_combine # CSCI n= 147 sites w dups

csci_med <- df_ffc_final %>% 
  filter(bioindicator=="CSCI") %>%
  select(-c(p50_obs:metric, result_type, HUC_12:CLASS_NAME)) %>% 
  distinct(.keep_all=TRUE) %>% 
  inner_join(., csci_sites_to_combine) %>% 
  group_by(StationCode, gageid) %>% 
  summarize(csci_med = median(csci))

csci_ffm <- df_ffc_final %>% 
  filter(bioindicator=="CSCI") %>%
  left_join(., csci_med) %>% 
  relocate(csci_med, .after="csci") %>% 
  #clean up and reset bioindicator value
  mutate(csci2 = case_when(
    is.na(csci_med) ~ csci,
    TRUE ~ csci_med), .after="csci") %>% 
  mutate(biovalue = csci2) %>% 
  select(-csci2)

# double check?
csci_ffm %>% 
  filter(bioindicator=="CSCI") %>% 
  distinct(StationCode, gageid, biovalue, .keep_all=TRUE) %>% 
  group_by(StationCode, gageid) %>% 
  tally() %>% 
  arrange(desc(n)) %>% #View()
  #nrow() # 364 unique!
  filter(n>1) %>% nrow() # 0 dups!

# rebind and save dataset
asci_ffm <- df_ffc_final %>% 
  filter(bioindicator == "ASCI")

bio_ffm_rev <- bind_rows(asci_ffm, csci_ffm) %>% 
  relocate(csci_med, .after="csci")

# save out
write_rds(df_ffc_final, file="output/10_ffc_filtered_final_combined_rev.rds")

