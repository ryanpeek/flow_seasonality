# Plot the Data

# LIBRARIES ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(glue)
library(ggdark)
library(tidylog)

# GET SITES ---------------------------------------------------------------

# get metadata
usgs_alt <- read_csv("data/usgs_metadata_alt_gages.csv")
usgs_ref <- read_csv("data/usgs_metadata_ref_gages.csv") %>% 
  mutate(site_id = as.character(site_id))

# Get Data ----------------------------------------------------------------

load("data/usgs_Q_daily_alt_gages.rda")
load("data/usgs_Q_daily_ref_gages.rda")
#load("data/usgs_Q_daily_ref_gages_por.rda")

# how many have data?
usgs_flows_alt %>% distinct(site_no) %>% tally() # n=748
usgs_flows_ref %>% distinct(site_no) %>% tally() # n=219


# Join With Metadata ------------------------------------------------------

usgs_flows_alt_meta <- usgs_flows_alt %>% 
  left_join(., usgs_alt, by=c("site_no"="site_id"))

usgs_flows_ref_meta <- usgs_flows_ref %>% 
  left_join(., usgs_ref, by=c("site_no"="site_id"))


# Bind All ----------------------------------------------------------------

# usgs_flows <- bind_rows(usgs_flows_alt_meta, usgs_flows_ref_meta) # 17.2 mill. rows!

# Visualize Flow Data -----------------------------------------------------

# expects x and y to be date and flow, but specify col name in quotes
source("code/f_plot_gage_facet.R")

# get list of gages and facet plot
usgs_flows_ref_meta %>% filter(site_no %in% usgs_ref$site_id[1:20]) %>% 
  plot_gage_facet(., x="Date", y="Flow", logged = TRUE,
                  facetid = "site_no", plotly = F)

# get list of gages and facet plot
# usgs_flows_ref_por %>% filter(site_no %in% ref_223$site_id[1:20]) %>% 
#   plot_gage_facet(., x="date", y="Flow", logged = TRUE,
#                   facetid = "site_no", plotly = T)

# single gage
g1 <- usgs_flows_ref_meta %>% filter(site_no %in% usgs_ref$site_id[2])

# full POR
ggplot(data=g1) +
  geom_line(aes(x=Date, 
                y=Flow,
                #y=log(Flow), 
                group=site_no, color=site_no), show.legend = F) +
  labs(subtitle = glue("USGS {g1$site_no}: {g1$station_nm}")) +
  theme_classic(base_family = "Roboto Condensed", base_size = 9) +
  scale_color_viridis_d() + 
  labs(y="Flow (cfs)", x="") +
  theme(axis.text.x = element_text(angle=90, hjust = 1))

# save
#ggsave(filename = "figs/hydrographs_highly_alt_gageids_1-20.png", width = 11, height = 8.5, dpi=300)

# create a version with a zoom year
zoomYr <- 1988

# full POR
ggplot(data=g1) +
  geom_line(aes(x=Date, 
                y=Flow,
                #y=log(Flow), 
                group=site_no, color=site_no), show.legend = F) +
  labs(subtitle = glue("USGS {g1$site_no}: {g1$station_nm}")) +
  theme_classic(base_family = "Roboto Condensed", base_size = 9) +
  scale_color_viridis_d() + labs(y="Flow (cfs)", x="") +
  theme(axis.text.x = element_text(angle=90, hjust = 1)) +
  ggforce::facet_zoom(x = lubridate::year(g1$Date) == zoomYr, shrink=T)
