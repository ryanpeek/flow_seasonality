# Download Daily Gage Data

# LIBRARIES ---------------------------------------------------------------

library(dataRetrieval)
library(sf)
library(tidyverse)
library(mapview)
library(purrr)
library(lubridate)
library(glue)
#library(tidylog)
library(ggdark)

# GET SITES ---------------------------------------------------------------

# load data
all_gages <- read_csv("data/list_of_all_gages_w_ffmdat.csv")

# get metadata
usgs_meta <- read_csv("data/usgs_metadata_all_gages.csv")

table(all_gages$refgage)

# unique? (n=748)
length(unique(all_gages$gageid))

gages <- all_gages %>% filter(refgage=="Non-Ref")

# Get Data ----------------------------------------------------------------

load("data/usgs_Q_daily_all_gages.rda")

# filter to just ref
usgs_flows_meta <- usgs_flows %>% 
  filter(site_no %in% gages$gageid) %>% 
  select(-agency_cd) %>% 
  left_join(., usgs_meta, by=c("site_no"="site_id"))

length(unique(usgs_flows_meta$site_no))

# rm usgs_flows
rm(usgs_flows)

# VISUALIZE DATE RANGES FOR SITES -----------------------------------------

# visualize the stations to see date ranges
ggplot() + 
  geom_linerange(data=usgs_meta, 
                 aes(x=forcats::fct_reorder(site_id, huc8), ymin=yr_begin, ymax=yr_end, color=as.factor(huc8)), size=1.1, show.legend = F) + 
  coord_flip() + 
  labs(x="", y="") + 
  scale_color_viridis_d(option = "D") +
  ggdark::dark_theme_bw(base_family = "Roboto Condensed", base_size = 8)

# ggsave(filename = "figures/usgs_gage_year_ranges.jpg",
#        width=11, height = 8.5, units = "in", dpi=300)

#ggsave(filename = "figures/usgs_gages_year_ranges.pdf", 
#       width=11, height = 8.5, units = "in", device = cairo_pdf)

# Visualize Flow Data -----------------------------------------------------

# expects x and y to be date and temp, but specify col name in quotes
plot_gage_facet <- function(data, x, y, facetid, plotly=FALSE){
  if(plotly == TRUE){
    p1 <- ggplot() +
      geom_line(data=data,
                aes(x=.data[[x]], y=.data[[y]],
                    group=.data[[facetid]], color=.data[[facetid]]),
                show.legend = F) +
      theme_classic(base_family = "Roboto Condensed", base_size = 9) +
      scale_color_viridis_d() + labs(y="Flow (cfs)", x="") +
      theme(axis.text.x = element_text(angle=90, hjust = 1)) +
      facet_wrap(.~.data[[facetid]], scales= "free_x")
    cat("Plotly it is!")
    return(plotly::ggplotly(p1))
  }
  p2 <- ggplot() +
    geom_line(data=data,
              aes(x=.data[[x]], y=.data[[y]],
                  group=.data[[facetid]], color=.data[[facetid]]),
              show.legend = F) +
    theme_classic(base_family = "Roboto Condensed", base_size = 9) +
    scale_color_viridis_d() + labs(y="Flow (cfs)", x="") +
    theme(axis.text.x = element_text(angle=90, hjust = 1)) +
    facet_wrap(.~.data[[facetid]], scales= "free")
  cat("printing static ggplots...")
  return(print(p2))
}

# get list of gages and facet plot
usgs_flows_meta %>% filter(site_no %in% gages$gageid[30:41]) %>% 
  plot_gage_facet(., x="Date", y="Flow", 
                  facetid = "site_no", plotly = F)

# single gage
g1 <- usgs_flows_meta %>% filter(site_no %in% gages$gageid[20])

ggplot(data=g1) +
  geom_line(aes(x=Date, 
                #y=Flow,
                y=log(Flow), 
                group=site_no, color=site_no), show.legend = F) +
  labs(subtitle = glue("USGS {g1$site_no}: {g1$station_nm}")) +
  theme_classic(base_family = "Roboto Condensed", base_size = 9) +
  scale_color_viridis_d() + labs(y="Flow (cfs)", x="") +
  theme(axis.text.x = element_text(angle=90, hjust = 1))

# save
#ggsave(filename = "figs/hydrographs_highly_alt_gageids_1-20.png", width = 11, height = 8.5, dpi=300)