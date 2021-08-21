# Visualize and EDA of data

# Libraries -------------------------------------------------------------

library(fs)
library(glue)
library(purrr)
library(viridis)
library(lubridate)
library(sf)
library(plotly)
library(patchwork)
library(mapview)
mapviewOptions(fgb = FALSE)

options(tidyverse.quiet = TRUE)
library(tidyverse) # load quietly
library(conflicted) # deals with conflicting functions
conflict_prefer("filter", "dplyr")
library(tictoc) # timing stuff
options(scipen = 100) # to print full string instead of sci notation

# main ffc package
#devtools::install_github('ceff-tech/ffc_api_client/ffcAPIClient')
library(ffcAPIClient)


# Data --------------------------------------------------------------------

df_filt <- read_rds("output/csci_wavelet_col_finalsites_filtered.rds")
df_meta_filt <- read_rds("output/wavelet_colwell_all_sites_filtered.rds")

# Pull Actual Flow Data ---------------------------------------------------

#devtools::install_github("ryanpeek/wateRshedTools")

load("data/usgs_Q_daily_ref_gages_rev.rda") # usgs_flows_ref
load("data/usgs_Q_daily_alt_gages_rev.rda") # usgs_flows_alt

usgs_flows_ref %>% ungroup() %>% distinct(site_no) %>% tally() # n=219
usgs_flows_alt %>% ungroup() %>% distinct(site_no) %>% tally() # n=748

# filter to filtered CSCI Paired sites
flow_ref <- usgs_flows_ref %>% 
  filter(site_no %in% unique(df_filt$site_id)) %>% 
  wateRshedTools::add_WYD("date")
flow_alt <- usgs_flows_alt %>% 
  filter(site_no %in% unique(df_filt$site_id)) %>% 
  wateRshedTools::add_WYD("date")
summary(flow_alt)
# how many gages?
flow_ref %>% ungroup() %>% distinct(site_no) %>% tally() # n=55
flow_alt %>% ungroup() %>% distinct(site_no) %>% tally() # n=137

# Now Run FFC -------------------------------------------------------------

# set/get the token for using the FFC
ffctoken <- set_token(Sys.getenv("EFLOWS", ""))

# clean up
ffcAPIClient::clean_account(ffctoken)

# Load Functions ----------------------------------------------------------

# these functions written by R. Peek 2020 to facilitate iteration

# this uses the purrr package to loop through and pull ffc data for each gage
source("code/f_iterate_ffc.R")

# this takes these data and saves them all into a single file/s
source("code/f_ffc_collapse.R")


# Make Gage List ----------------------------------------------------------

# make a simple list of gage_id & gageIDName
gages <- df_filt %>%
  mutate(site_id_name = paste0("T",site_id)) %>%
  select(site_id, site_id_name, comid)

# check flow data associated with years?
flow_ref %>% group_by(site_no) %>% 
  summarize(minyr = min(WY),
            maxyr = max(WY)) %>% 
  left_join(., df_filt[,c("site_no", "gagetype.y")], by=c("site_id"="site_no"))
  ggplot() + geom_linerange(aes(x=site_no, ymin=minyr, ymax=maxyr, )) +
  coord_flip()
  View()
# Setup Iteration ---------------------------------------------------------

# set start date to start WY 1980
#st_date <- "1979-10-01"

# RUN! --------------------------------------------------------------------

# chunk a set number at a time (and drop sf)
gagelist <- gages %>% st_drop_geometry() %>% slice(1:5)

tic() # start time
ffcs <- gagelist %>%
  select(site_id, comid) %>% # pull just ID column
  pmap(.l = ., .f = ~ffc_possible(.x, startDate = st_date, ffctoken=ffctoken, comid=.y, dirToSave="output/ffc_run_alt", save=TRUE)) %>%
  # add names to list
  set_names(x = ., nm=gagelist$site_id_name)
toc() # end time

# for 935 gages (748 with data!)
## 7047 s (117 min)

# see names
names(ffcs)

# a timestamp: format(Sys.time(), "%Y-%m-%d_%H%M")
(file_ts <- format(Sys.time(), "%Y%m%d_%H%M"))

# identify missing:
ffcs %>% keep(is.na(.)) %>% length() # missing 187

# make a list of gages
miss_gages<-ffcs %>% keep(is.na(.)) %>% names()

# save out missing
write_lines(miss_gages, file = glue("output/usgs_ffm_alt_missing_gages_{file_ts}.txt"))

# save out FFC R6 object (only if save=FALSE)
#save(ffcs, file = glue("output/usgs_ffm_ref_R6_full_{file_ts}.rda"))

# Follow Up for Missing ----------------------------------------------

# just look at missing gages here?

# test
(tst <-miss_gages[100])

# get comid if you don't know it for a gage
gage <- ffcAPIClient::USGSGage$new()
gage$id <- tst
gage$get_data()
gage$get_comid()
(comid <- gage$comid)

# RUN SETUP
fftst <- FFCProcessor$new()  # make a new object we can use to run the commands
fftst$fail_years_data = 9
fftst$gage_start_date = "1979-10-01"

# if you have comid, add via original usgs_alt dataset
fftst$set_up(gage_id=tst, comid = comid, token = ffctoken)
#fftst$set_up(gage_id="09423350", token = ffctoken)

# then run
fftst$run()

