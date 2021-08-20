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

# Get Flow Metadata:

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

# Cross Reference with Original Reference List/Period of Record -----------

# check with FFC Ref list?
ref_223 <- read_csv("https://raw.githubusercontent.com/ceff-tech/bmi_ffm_links/main/data/usgs/gages_ref_223_period_record.csv") %>% 
  separate(stream_class, into = c("class","CLASS"), sep="-", remove=T) %>% 
  select(-c(class, stat, data)) %>% 
  mutate(CLASS = as.numeric(CLASS),
         site_id = as.character(gage))

# join:
ref_223 <- left_join(ref_223, strmclass_xwalk)

# join with gage metadata
ref_223 <- left_join(ref_223, gages_meta) %>% 
  select(CLASS, gage, site_id, minYr, maxYr, YrRange, yr_begin, yr_end, yr_total, gagetype)

# use this to move forward and filter data for reference sites to these periods


# REF: Filter to Reference Periods ---------------------------------------------

# fix date cols & add continuous count of flow days
usgs_flows_ref_por <- usgs_flows_ref %>% 
  mutate(date = ymd(as.character(Date), 
                    tz="America/Los_Angeles"), .after="Date") %>% 
  select(-agency_cd, -Date) %>% 
  left_join(., ref_223, by=c("site_no"="site_id")) %>%
  group_split(site_no) %>% 
  map_df(., ~filter(., waterYear>=minYr & waterYear<=maxYr))
  
## plot??
save(usgs_flows_ref_por, file="data/usgs_Q_daily_ref_gages_por.rda", compress = "xz" )

# REF: Trim to Longest Flow Period -------------------------------------------

# fix date cols & add continuous count of flow days
usgs_flows_ref <- usgs_flows_ref_por %>% 
  group_by(site_no) %>% 
  complete(date = seq.Date(min(ymd(date)), max(ymd(date)), by = "day")) %>% 
  # add count by days with flow
  mutate(flowdays = ifelse(!is.na(Flow), 1, 0)) %>% 
  mutate(flowcnt = ave(flowdays, cumsum(flowdays == 0), FUN = cumsum))

# total ref?
usgs_flows_ref %>% distinct(site_no, .keep_all=TRUE) %>% nrow()

# expects x and y to be date and flow, but specify col name in quotes
source("code/f_plot_gage_facet.R")

# get list of gages and facet plot
usgs_flows_ref %>% filter(site_no %in% ref_223$site_id[1:20]) %>% 
  plot_gage_facet(., x="date", y="Flow", logged = TRUE,
                  facetid = "site_no", plotly = F)

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


# get list of gages and facet plot
usgs_flows_alt %>% filter(site_no %in% gages_meta$site_id[1:20]) %>% 
  plot_gage_facet(., x="date", y="Flow", logged = TRUE,
                  facetid = "site_no", plotly = F)


save(usgs_flows_ref, file="data/usgs_Q_daily_ref_gages_rev.rda", compress = "xz" )
save(usgs_flows_alt, file="data/usgs_Q_daily_alt_gages_rev.rda", compress = "xz")


