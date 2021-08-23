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

# Data --------------------------------------------------------------------

df_final <- read_rds("output/wavelet_csci_colwell_final.rds")
df_final %>% distinct(site_id, .keep_all = TRUE) %>% 
  group_by(gagetype) %>% tally() # ALT = 140, REF=55

load("data/usgs_Q_daily_ref_gages_trim.rda") # usgs_flows_ref
load("data/usgs_Q_daily_alt_gages_trim.rda") # usgs_flows_alt

# make a simple list of gage_id & gageIDName
# gages <- df_final %>% 
#   distinct(site_id, .keep_all = TRUE) %>% 
#   mutate(site_id_name = paste0("T",site_id)) %>%
#   select(site_id, site_id_name, comid, gagetype)
# table(gages$gagetype)

# Filter to CSCI Sites Only -----------------------------------------------

# REF: filter to filtered CSCI Paired sites
flow_ref <- usgs_flows_ref_trim %>% 
  filter(site_no %in% unique(df_final$site_id)) %>% 
  # add COMID
  left_join(., df_final %>% select(site_id, comid), by=c("site_no"="site_id")) %>% 
  rename(flow=Flow)

# ALT
flow_alt <- usgs_flows_alt_trim %>% 
  filter(site_no %in% unique(df_final$site_id)) %>% 
  # add COMID
  left_join(., df_final %>% select(site_id, comid), by=c("site_no"="site_id")) %>% 
  rename(flow=Flow)

# how many gages paired with CSCI?
flow_ref %>% ungroup() %>% distinct(site_no) %>% tally() # n=55
flow_alt %>% ungroup() %>% distinct(site_no) %>% tally() # n=140

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
                                  dirToSave="output/ffc_ref_csci", 
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
save(ffcs_ref, file = glue("output/ffc_ref_csci/usgs_ffm_ref_R6_csci_{file_ts}.rda"))

# ALT: Setup Full Flowseries Data ----------------------------------------------

# chunk a set number at a time
gagedata_alt <- flow_alt %>% 
  split(.$site_no)

# ALT: RUN FFC --------------------------------------------------------------------

tic() # start time
ffcs_alt <- map(gagedata_alt, ~ffc_possible(flowseries_df = .x,
                                    ffctoken=ffctoken,
                                    dirToSave="output/ffc_alt_csci", 
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
# save(ffcs_alt, file = glue("output/ffc/usgs_ffm_alt_R6_full_{file_ts}.rda"))

