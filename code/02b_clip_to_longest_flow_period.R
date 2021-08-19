# Prep data for colwell/wavelets by trimming to longest continuous period

# LIBRARIES ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(sf)
library(glue)

# Get Flow Data ---------------------------------------------------------------

# flow data
load("data/usgs_Q_daily_alt_gages.rda")
load("data/usgs_Q_daily_ref_gages.rda")

# Prep Date Cols -------------------------------------------------------------

# fix date cols & add continuous count of flow days
usgs_flows_ref <- usgs_flows_ref %>% 
  mutate(date = ymd(as.character(Date), 
                    tz="America/Los_Angeles"), .after="Date") %>% 
  select(-agency_cd, -Date) %>% 
  group_by(site_no) %>% 
  complete(date = seq.Date(min(ymd(date)), max(ymd(date)), by = "day")) %>% 
  # add count by days with flow
  mutate(flowdays = ifelse(!is.na(Flow), 1, 0)) %>% 
  mutate(flowcnt = ave(flowdays, cumsum(flowdays == 0), FUN = cumsum))

# total ref?
usgs_flows_ref %>% distinct(site_no, .keep_all=TRUE) %>% nrow()

# alt flows
usgs_flows_alt <- usgs_flows_alt %>% 
  mutate(date = ymd(as.character(Date), 
                    tz="America/Los_Angeles"), .after="Date") %>% 
  select(-agency_cd, -Date) %>% 
  group_by(site_no) %>% 
  complete(date = seq.Date(min(ymd(date)), max(ymd(date)), by = "day")) %>% 
  # add count by days with flow
  mutate(flowdays = ifelse(!is.na(Flow), 1, 0)) %>% 
  mutate(flowcnt = ave(flowdays, cumsum(flowdays == 0), FUN = cumsum))

usgs_flows_alt %>% distinct(site_no, .keep_all=TRUE) %>% nrow()

save(usgs_flows_ref, file="data/usgs_Q_daily_ref_gages_rev.rda", compress = "xz" )
save(usgs_flows_alt, file="data/usgs_Q_daily_alt_gages_rev.rda", compress = "xz")


