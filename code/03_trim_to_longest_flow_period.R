# Prep data for colwell/wavelets by trimming to longest continuous period

# LIBRARIES ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(sf)
library(glue)

# Get Flow Data ---------------------------------------------------------------

# flow data
load("data/usgs_Q_daily_alt_gages_filt.rda")
load("data/usgs_Q_daily_ref_gages_por.rda")

# No of unique stations?
usgs_flows_alt_filt %>% distinct(site_no) %>% nrow() # n=517
usgs_flows_ref_por %>% distinct(site_no) %>% nrow() # n=221

# get gage list:
ref_gagelist <- unique(usgs_flows_ref_por$site_no)
alt_gagelist <- unique(usgs_flows_alt_filt$site_no)

## Get StreamClasses -------------------------------------------------------

# get stream class and combine into 9 class and 3 class Xwalks 
# see "3_color_classes_gages_legend"
ceff_strmclass <- st_read("data/eflows_final_classification_9CLASS/Final_Classification_9CLASS.shp")

# crosswalk
strmclass_xwalk <- tibble(
  "CLASS"=c(1,2,3,4,5,6,7,8,9), 
  "CLASS_NAME"=c("snowmelt", # 3 class: 1=SNOWMELT
                 "high-volume snowmelt and rain", # 3 class: 2=MIXED
                 "low-volume snowmelt and rain", # 3 class: 2=MIXED,
                 "winter storms", # 3 class: 3=RAIN
                 "groundwater", # 3 class: 2=MIXED
                 "perennial groundwater and rain", # 3 class: 3=RAIN
                 "flashy, ephemeral rain", # 3 class: 3=RAIN
                 "rain and seasonal groundwater", # 3 class: 3=RAIN
                 "high elevation low precipitation"), # 3 class: 1=SNOWMELT
  "class3_name" = c("SNOWMELT",
                    "MIXED","MIXED","RAIN","MIXED",
                    "RAIN","RAIN","RAIN",
                    "SNOWMELT"),
  "class3_id" = c(1,
                  2,2,3,2,
                  3,3,3,
                  1))

# join with class names
ceff_strmclass <- left_join(ceff_strmclass, strmclass_xwalk)


# Add Running Count of Continuous Flow Periods -----------------------------

# REF: add continuous count of flow days (takes a minute)
usgs_flows_ref <- usgs_flows_ref_por %>% 
  group_by(site_no) %>% 
  complete(date = seq.Date(min(ymd(date)), max(ymd(date)), by = "day")) %>% 
  # add count by days with flow
  mutate(flowdays = ifelse(!is.na(Flow), 1, 0)) %>% 
  mutate(flowcnt = ave(flowdays, cumsum(flowdays == 0), FUN = cumsum))

# total ref?
usgs_flows_ref %>% distinct(site_no) %>% nrow()

# expects x and y to be date and flow, but specify col name in quotes
source("code/f_plot_gage_facet.R")

# get list of gages and facet plot
usgs_flows_ref %>% filter(site_no %in% ref_gagelist[21:60]) %>% 
  plot_gage_facet(., x="date", y="Flow", logged = TRUE,
                  facetid = "site_no", plotly = F)

# see gages with gaps: 10264000, 11046300, 11058600, 1106300,
# 1111500...etc

# ALT: add continuous count of flow days (takes a minute)
usgs_flows_alt <- usgs_flows_alt_filt %>% 
  group_by(site_no) %>% 
  complete(date = seq.Date(min(ymd(date)), max(ymd(date)), by = "day")) %>% 
  # add count by days with flow
  mutate(flowdays = ifelse(!is.na(Flow), 1, 0)) %>% 
  mutate(flowcnt = ave(flowdays, cumsum(flowdays == 0), FUN = cumsum))

usgs_flows_alt %>% distinct(site_no, .keep_all=TRUE) %>% nrow()

# get list of gages and facet plot
usgs_flows_alt %>% filter(site_no %in% alt_gagelist[1:20]) %>% 
  plot_gage_facet(., x="date", y="Flow", logged = TRUE,
                  facetid = "site_no", plotly = F)


# SAVE OUT ----------------------------------------------------------------

# saving these out because it took awhile to run
save(usgs_flows_ref, file="data/usgs_Q_daily_ref_gages_flwcnts.rda", compress = "xz" )
save(usgs_flows_alt, file="data/usgs_Q_daily_alt_gages_flwcnts.rda", compress = "xz")


# Update and Trim ---------------------------------------------------------


# function to select longest period of record for gage:
get_longest_flowperiod <- function(data){
  data %>% 
    slice( (which.max(flowcnt) - # max value row
              # now 1 minus the max **value** to get first row 
              (data %>% slice_max(flowcnt) %>% 
                 pluck("flowcnt") - 1)):which.max(flowcnt), 
           .preserve = TRUE) %>% 
    as.data.frame()
}

# apply using map
usgs_flows_ref_trim <- usgs_flows_ref %>% 
  group_by(site_no) %>% 
  group_split() %>% 
  map(., ~get_longest_flowperiod(.x)) %>% 
  bind_rows()

# visualize old data vs. updated!
usgs_flows_ref %>% filter(site_no %in% ref_gagelist[1:20]) %>% 
  plot_gage_facet(., x="date", y="Flow", logged = TRUE,
                  facetid = "site_no", plotly = F)

usgs_flows_ref_trim %>% filter(site_no %in% ref_gagelist[1:20]) %>% 
  plot_gage_facet(., x="date", y="Flow", logged = TRUE,
                  facetid = "site_no", plotly = F)

# it works!

# apply for alt
usgs_flows_alt_trim <- usgs_flows_alt %>% 
  group_by(site_no) %>% 
  group_split() %>% 
  map(., ~get_longest_flowperiod(.x)) %>% 
  bind_rows()

# visualize
usgs_flows_alt %>% filter(site_no %in% alt_gagelist[1:20]) %>% 
  plot_gage_facet(., x="date", y="Flow", logged = TRUE,
                  facetid = "site_no", plotly = F)

# updated data
usgs_flows_alt_trim %>% filter(site_no %in% alt_gagelist[1:20]) %>% 
  plot_gage_facet(., x="date", y="Flow", logged = TRUE,
                  facetid = "site_no", plotly = F)


# Save Out ----------------------------------------------------------------


save(usgs_flows_ref_trim, file="data/usgs_Q_daily_ref_gages_trim.rda", compress = "xz" )
save(usgs_flows_alt_trim, file="data/usgs_Q_daily_alt_gages_trim.rda", compress = "xz")
