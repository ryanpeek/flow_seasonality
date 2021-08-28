# Run FFC
# see this page: https://ceff-tech.github.io/ffc_api_client/articles/getting_started.html

# Libraries -------------------------------------------------------------

library(fs)
library(glue)
library(purrr)
library(lubridate)
library(sf)
options(tidyverse.quiet = TRUE)
library(tidyverse) # load quietly
library(conflicted) # deals with conflicting functions
conflict_prefer("filter", "dplyr")
library(tictoc) # timing stuff
options(scipen = 100) # to print full string instead of sci notation

# main ffc package
#devtools::install_github('ceff-tech/ffc_api_client/ffcAPIClient')
library(ffcAPIClient)

# set/get the token for using the FFC
ffctoken <- set_token(Sys.getenv("EFLOWS", ""))

# clean up
ffcAPIClient::clean_account(ffctoken)

# Load FFC Bulk Functions ----------------------------------------------------------

# these functions written by R. Peek 2020 to facilitate iteration

# this uses the purrr package to loop through and pull ffc data for each gage
source("code/f_iterate_ffc.R")
source("code/f_ffc_collapse.R")

# Data --------------------------------------------------------------------

df_final <- read_rds("output/07_wavelet_asci_csci_colwell_final.rds")
df_final %>% distinct(bioindicator, site_id, .keep_all = TRUE) %>% 
  group_by(bioindicator, gagetype) %>% tally() 
# CSCI ALT = 125, REF=53
# ASCI ALT = 122

load("data/usgs_Q_daily_ref_gages_trim.rda") # usgs_flows_ref_trim
load("data/usgs_Q_daily_alt_gages_trim.rda") # usgs_flows_alt_trim

# make a simple list of gage_id 
gage_alt_meta <- usgs_flows_alt_trim %>% distinct(site_no, .keep_all=TRUE) %>% 
  select(site_no,station_nm:flowcnt) # n=517
gage_ref_meta <- usgs_flows_ref_trim %>% distinct(site_no, .keep_all=TRUE) %>% 
  select(site_no,station_nm, usgs_lat, usgs_lon, huc8, parm_cd, stat_cd, 
         ref_yr_start, ref_yr_end, ref_por_range, yr_begin, yr_end, yr_total, 
         gagetype, parm_cd:flowcnt) # n=221

# bind rows
gage_metadata <- bind_rows(gage_alt_meta, gage_ref_meta)
gagelist <- unique(df_final$site_id)

# Filter to CSCI Sites Only -----------------------------------------------

# REF: filter to filtered CSCI Paired sites
flow_ref <- usgs_flows_ref_trim %>% 
  filter(site_no %in% gagelist) %>% 
  # add COMID
  left_join(., df_final %>% select(site_id, comid_gage, bioindicator), by=c("site_no"="site_id")) %>% 
  rename(flow=Flow) # n=2276366

# ALT
flow_alt <- usgs_flows_alt_trim %>% 
  filter(site_no %in% gagelist) %>% 
  # add COMID
  left_join(., df_final %>% select(site_id, comid_gage, bioindicator), by=c("site_no"="site_id")) %>% 
  rename(flow=Flow) # n=12293311

# how many gages paired with CSCI?
flow_ref %>% ungroup() %>% distinct(site_no, bioindicator) %>% 
  group_by(bioindicator) %>% tally() # n=47 ASCI, 53 CSCI
flow_alt %>% ungroup() %>% distinct(site_no, bioindicator) %>% 
  group_by(bioindicator) %>% tally() # n=126 ASCI, 125=CSCI

flow_ref %>% ungroup() %>% distinct(site_no, gagetype) %>% 
  group_by(gagetype) %>% tally() # n=67 
flow_alt %>% ungroup() %>% distinct(site_no, gagetype) %>% 
  group_by(gagetype) %>% tally() # n=67 

# Testing FFC Function ------------------------------------------------------

# chunk a set number at a time (and drop sf)
# gagedata_test <- flow_ref %>% 
#   split(.$site_no) %>% 
#   .[1:2]  # extract a few to test

# pull just one to test
# flowseries_df_test <- gagedata_test %>% 
#   .[1] %>% map_df(., ~.x)

# plot each?
# map(gagedata_test, ~(ggplot(data=.x) + geom_line(aes(x=date, y=flow)) +
#                   scale_y_log10()))

# run function
# ffcs_tst <- map(gagedata_test, ~ffc_possible(flowseries_df = .x,
#                                     ffctoken=ffctoken,
#                                     dirToSave="output/ffc_run_tst", 
#                                     save=TRUE))

# REF: Setup Full Flowseries Data ----------------------------------------------

# chunk a set number at a time
gagedata_ref <- flow_ref %>% 
  split(.$site_no)

# REF: RUN FFC --------------------------------------------------------------------

tic() # start time
ffcs_ref <- map(gagedata_ref, ~ffc_possible(flowseries_df = .x,
                                  ffctoken=ffctoken,
                                  dirToSave="output/ffc_ref_bio", 
                                  save=TRUE))
toc() # end time
# 383.114 sec elapsed

# see names
names(ffcs_ref)

# a timestamp: format(Sys.time(), "%Y-%m-%d_%H%M")
(file_ts <- format(Sys.time(), "%Y%m%d_%H%M"))

# identify missing:
ffcs_ref %>% keep(is.na(.)) %>% length()

# make a list of missing gages and save out:
# miss_gages_ref <- ffcs_ref %>% keep(is.na(.)) %>% names()
# write_lines(miss_gages_ref, file = glue("output/usgs_ffm_alt_missing_gages_{file_ts}.txt"))

# SAVE: FFC R6 object (only if save=FALSE)
save(ffcs_ref, file = glue("output/ffc_ref_bio/usgs_ffm_ref_R6_bio_{file_ts}.rda"))

# ALT: Setup Full Flowseries Data ----------------------------------------------

# chunk a set number at a time
gagedata_alt <- flow_alt %>% 
  split(.$site_no)

# ALT: RUN FFC --------------------------------------------------------------------

tic() # start time
ffcs_alt <- map(gagedata_alt, ~ffc_possible(flowseries_df = .x,
                                    ffctoken=ffctoken,
                                    dirToSave="output/ffc_alt_bio", 
                                    save=TRUE))
toc() # end time

# see names
names(ffcs_alt)

# a timestamp: format(Sys.time(), "%Y-%m-%d_%H%M")
(file_ts <- format(Sys.time(), "%Y%m%d_%H%M"))

# identify missing:
ffcs_alt %>% keep(is.na(.)) %>% length()

# make a list of missing gages and save out:
# miss_gages_alt <- ffcs_alt %>% keep(is.na(.)) %>% names()
# write_lines(miss_gages_alt, file = glue("output/usgs_ffm_alt_missing_gages_{file_ts}.txt"))

# Save Out: FFC R6 object (only if save=FALSE)
save(ffcs_alt, file = glue("output/ffc_alt_bio/usgs_ffm_bio_R6_full_{file_ts}.rda"))

# Combine FFC -------------------------------------------------------------

source("code/f_ffc_collapse.R")

## Setup Directory ---------------------------------------------------------

# get type
type <- "ref_bio" # alt_csci

# get dir
ffc_dir <- glue("output/ffc_{type}/")
(ffc_files <- dir_ls(ffc_dir, type = "file", regexp = "*.csv"))
ffc_files
# create output location:
fs::dir_create("output/ffc_combined")

# get run datetime
(runDatetime <- fs::path_file(fs::path_ext_remove(fs::dir_ls(ffc_dir, glob = "*.rda"))) %>% 
  str_extract(., "[0-9]{8}_[0-9]{4}"))
(runDate <- str_extract(runDatetime, "[0-9]{8}"))

# look at modification time:
file_info(fs::dir_ls(ffc_dir, glob = "*.rda"))[[5]] %>% floor_date(unit = "day")

## Combine FFC: predicted_percentiles --------------------------------------

# set the data type:
datatype <- "predicted_percentiles"
df_ffc <- ffc_collapse(datatype, fdir = ffc_dir)

# view how many USGS gages
df_ffc %>% distinct(gageid) %>% count()
# how many records per gage?
df_ffc %>% group_by(gageid) %>% tally() #%>% filter(n>23) %>% View() # view

# save it
write_csv(df_ffc, file = glue("output/ffc_combined/usgs_{type}_{datatype}_run_{runDate}.csv"))
write_rds(df_ffc, file = glue("output/ffc_combined/usgs_{type}_{datatype}_run_{runDate}.rds"), compress = "gz")

## Combine FFC: alteration -------------------------------------------------

# set the data type:
datatype <- "alteration"
df_ffc <- ffc_collapse(datatype, fdir = ffc_dir)

# view how many USGS gages
df_ffc %>% distinct(gageid) %>% count()
# how many records per gage?
df_ffc %>% group_by(gageid) %>% tally() #%>% filter(n>23) %>% View() # view

# save it
write_csv(df_ffc, file = glue("output/ffc_combined/usgs_{type}_{datatype}_run_{runDate}.csv"))
write_rds(df_ffc, file = glue("output/ffc_combined/usgs_{type}_{datatype}_run_{runDate}.rds"), compress = "gz")

## Combine FFC: ffc_percentiles --------------------------------------------

# set the data type:
datatype <- "ffc_percentiles"
df_ffc <- ffc_collapse(datatype, fdir = ffc_dir)

# view how many USGS gages
df_ffc %>% distinct(gageid) %>% count()
# how many records per gage?
df_ffc %>% group_by(gageid) %>% tally()

# save it
write_csv(df_ffc, file = glue("output/ffc_combined/usgs_{type}_{datatype}_run_{runDate}.csv"))
write_rds(df_ffc, file = glue("output/ffc_combined/usgs_{type}_{datatype}_run_{runDate}.rds"), compress = "gz")

## Combine FFC: ffc_results ------------------------------------------------

# set the data type:
datatype <- "ffc_results"
df_ffc <- ffc_collapse(datatype, fdir = ffc_dir)

# view how many USGS gages
df_ffc %>% distinct(gageid) %>% count()
# how many years per gage?
df_ffc %>% group_by(gageid) %>% tally() #%>% View()

# save it
write_csv(df_ffc, file = glue("output/ffc_combined/usgs_{type}_{datatype}_run_{runDate}.csv"))
write_rds(df_ffc, file = glue("output/ffc_combined/usgs_{type}_{datatype}_run_{runDate}.rds"), compress = "gz")

# FOR ffc_results: Pivot Longer
# df_ffc_long <- df_ffc %>%
#   pivot_longer(cols=!c(Year,gageid),
#                names_to="ffm",
#                values_to="value") %>%
#   rename(year=Year) %>%
#   mutate(ffc_version="v1.1_api") %>%
#   filter(!is.na(value))
# 
# # save it
# write_csv(df_ffc_long, file = glue("output/ffc_combined/usgs_{type}_{datatype}_long_run_{runDate}.csv"))
# write_rds(df_ffc_long, file = glue("output/ffc_combined/usgs_{type}_{datatype}_long_run_{runDate}.rds"), compress = "gz")


# Combine Ref and Alt FFC -------------------------------------------------

# get dir
ffc_dir <- glue("output/ffc_combined")
ffc_files <- dir_ls(ffc_dir, type = "file", regexp = "*.csv")

# function to read and combine the data
ffc_combine_alt_ref <- function(datatype, fdir){
  datatype = datatype
  ffc_files = dir_ls(path = fdir, type = "file", regexp = "*.csv")
  csv_list_alt = ffc_files[grepl(glue("(alt)_bio_{datatype}_run*"), ffc_files)]
  csv_list_ref = ffc_files[grepl(glue("(ref)_bio_{datatype}_run*"), ffc_files)]
  # read in all
  df_alt <- purrr::map(csv_list_alt, ~read_csv(.x)) %>%
    # check and fix char vs. num
    map(~mutate_at(.x, 'gageid', as.character)) %>%
    bind_rows()
  df_ref <- purrr::map(csv_list_ref, ~read_csv(.x)) %>%
    # check and fix char vs. num
    map(~mutate_at(.x, 'gageid', as.character)) %>%
    bind_rows()
  df_all <- bind_rows(df_alt, df_ref)
  return(df_all)
}


## RUN FOR PERCENTILES -----------------------------------------------------

# set the data type:
datatype <- "ffc_percentiles" # ffc_percentiles or predicted_percentiles

# combine
df_ffc <- ffc_combine_alt_ref(datatype, fdir = ffc_dir)

# view how many USGS gages
df_ffc %>% distinct(gageid) %>% count()

# how many records per gage?
table(df_ffc$gageid)

# add gagetype & metadata
df_ffc_meta <- left_join(df_ffc, gage_metadata, by=c("gageid"="site_no"))

df_ffc_meta %>% distinct(gageid, .keep_all = TRUE) %>% 
  group_by(gagetype) %>% tally()

# write out
write_csv(df_ffc_meta, file = glue("output/09_ffc_meta_combined_{datatype}.csv"))
write_rds(df_ffc_meta, file = glue("output/09_ffc_meta_combined_{datatype}.rds"), compress = "gz")


## RUN FOR PREDICTED PERCENTILES ---------------------------------------------

# set the data type:
datatype <- "predicted_percentiles" #  predicted_percentiles

# combine
df_ffc <- ffc_combine_alt_ref(datatype, fdir = ffc_dir)

# view how many USGS gages
df_ffc %>% distinct(gageid) %>% count()

# how many records per gage?
table(df_ffc$gageid)

# add gagetype & metadata
df_ffc_meta <- left_join(df_ffc, gage_metadata, by=c("gageid"="site_no"))

df_ffc_meta %>% distinct(gageid, .keep_all = TRUE) %>% 
  group_by(gagetype) %>% tally()

# write out
write_csv(df_ffc_meta, file = glue("output/09_ffc_meta_combined_{datatype}.csv"))
write_rds(df_ffc_meta, file = glue("output/09_ffc_meta_combined_{datatype}.rds"), compress = "gz")
