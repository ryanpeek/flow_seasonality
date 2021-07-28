# Download Daily Gage Data

# LIBRARIES ---------------------------------------------------------------

library(dataRetrieval)
library(sf)
library(tidyverse)
library(mapview)
library(purrr)
library(lubridate)
library(tidylog)
library(ggdark)

# GET SITES ---------------------------------------------------------------

# load data
all_gages <- read_csv("data/list_of_all_gages_w_ffmdat.csv")

# get metadata
usgs_meta <- read_csv("data/usgs_metadata_all_gages.csv")

table(all_gages$refgage)

# unique? (n=748)
length(unique(all_gages$gageid))

ref_only <- all_gages %>% filter(refgage=="Ref")

# Get Data ----------------------------------------------------------------

load("data/usgs_Q_daily_all_gages.rda")

# filter to just ref
usgs_flows_ref <- usgs_flows %>% 
  filter(site_no %in% ref_only$gageid) %>% 
  select(-agency_cd) %>% 
  left_join(., usgs_meta, by=c("site_no"="site_id"))


# Get Date Ranges ---------------------------------------------------------

usgs_flows_ref_span <- usgs_flows_ref %>% 
  select(site_no, station_nm:yr_total) %>% 
  distinct(.keep_all=TRUE)

# VISUALIZE DATE RANGES FOR SITES -----------------------------------------

# visualize the stations to see date ranges
ggplot() + 
  geom_linerange(data=usgs_flows_ref_span, 
                 aes(x=forcats::fct_reorder(site_no, huc8), ymin=yr_begin, ymax=yr_end, color=as.factor(huc8)), size=1.1, show.legend = F) + 
  coord_flip() + 
  labs(x="", y="") + 
  scale_color_viridis_d(option = "A") +
  #theme_bw(base_family = "Roboto Condensed", base_size = 8) +
  ggdark::dark_theme_bw(base_family = "Roboto Condensed", base_size = 8)

# ggsave(filename = "figures/usgs_gage_year_ranges.jpg",
#        width=11, height = 8.5, units = "in", dpi=300)

#ggsave(filename = "figures/usgs_gages_year_ranges.pdf", 
#       width=11, height = 8.5, units = "in", device = cairo_pdf)


# Visualize Flow Data -----------------------------------------------------


usgs_flows_ref %>% 