# Colwells

library(hydrostats)
library(tsbox)
library(tidyverse)
library(purrr)
library(lubridate)
library(glue)
#library(tidylog)
library(ggdark)

# GET SITES ---------------------------------------------------------------

# get metadata
usgs_meta <- read_csv("data/usgs_metadata_all_gages.csv")


# Get Data ----------------------------------------------------------------

load("data/usgs_Q_daily_all_gages.rda")

# filter to just ref
usgs_flows_meta <- usgs_flows %>% 
  select(-agency_cd) %>% 
  left_join(., usgs_meta, by=c("site_no"="site_id"))

length(unique(usgs_flows_meta$site_no))

# rm usgs_flows
rm(usgs_flows)


# CALCULATE COLWELL -------------------------------------------------------

# quick colwell analysis of seasonality:
## From Tonkin et al 2017: M (Contingency) as metric of seasonality
## To standardize the role of seasonality in relation to overall predictability,
## we divided (M) by overall predictability (the sum of (M) and constancy (C)



# standardize
df <- usgs_flows_meta %>%   
  rename("Q"=Flow) %>% 
  select(Date, Q, site_no) %>% 
  mutate(Date = ymd(as.character(Date), tz = "US/Pacific")) %>% 
  group_by(site_no) %>% 
  group_split() %>%
  #.[1:30] %>% # extract
  map(., ~as.data.frame(.x)) 

# calc colwells and add ID
df %>% 
  map(., ~hydrostats::Colwells(.x)) %>% 
  map(., ~tibble(MP_metric=c(.x[["MP"]]))) %>%
  bind_rows() %>% 
  mutate(site_no = usgs_meta$site_id[1:30])



