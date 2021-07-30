# EDA of Colwell and CSCI

library(tidyverse)
library(glue)
library(tidylog)
library(ggdark)
library(sf)
#library(mapview)

# GET DATA ---------------------------------------------------------------

df_colwell <- read_rds("output/usgs_gages_colwells_metric.rds")

# Read CSCI ---------------------------------------------------------------

# only alt dataset with percentiles
csci_alt <- read_rds("data/06_csci_por_trim_final_dataset.rds") %>% 
  select(StationCode:HUC_12, site_id, comid_gage:delta_p50) %>% 
  distinct(StationCode, SampleID, site_id, .keep_all=TRUE)
table(csci_alt$CEFF_type) # n=521

# older dataset w alt and ref gages
csci_por <- read_rds("data/04_selected_csci_ffm_por_trim.rds") %>% 
  select(StationCode:HUC_12, site_id, comid_gage:comid_ffc) %>% 
  distinct(StationCode, SampleID, site_id, .keep_all=TRUE)
table(csci_por$CEFF_type) # n=521 alt, n=192 REF

# JOIN COLWELL AND CSCI ---------------------------------------------------

# join w seasonality:
csci_alt_colwell <- left_join(csci_alt, df_colwell, by=c("site_id"="site_no")) %>%
  distinct(StationCode, SampleID, site_id, .keep_all=TRUE)

csci_por_colwell <- left_join(csci_por, df_colwell, by=c("site_id"="site_no")) %>%
  distinct(StationCode, SampleID, site_id, .keep_all=TRUE)

# PLOTS -------------------------------------------------------------------

csci_por_colwell %>% 
  ggplot() + 
  geom_point(aes(y=MP_metric, x=csci, fill=gagetype), pch=21, alpha=0.5) +
  stat_smooth(aes(y=MP_metric, x=csci, color=gagetype), 
              method = "gam") +
  theme_classic(base_family = "Roboto Condensed") +
  scale_color_viridis_d(option = "B", "Gage Type") +
  scale_fill_viridis_d(option = "A", "Gage Type") +
  labs(y="Seasonality (Colwell's M/P)", x="CSCI",
       caption = "Standardized seasonality in relation to overall predictability \nby dividing seasonality (M) by overall predictability \n(the sum of (M) and constancy (C)), as per Tonkin et al. (2017)")

# this makes sense for the ALT gages, but not for REF??

#ggsave(filename = "figures/csci_colwells.png", width = 10, height = 8, dpi = 300, units = "in")


# Explore the REF Colwells Set --------------------------------------------

df_colwell_ref <- filter(df_colwell, gagetype=="REF")

df_colwell_ref %>% 
  ggplot() + geom_point( aes(x=MP_metric, y=site_no)) +
  geom_vline(xintercept = 0.5, color="maroon", lwd=2) +
  theme_classic() +
  theme(axis.text.y = element_blank())

# how many gages fall below this? 
df_colwell_ref %>% filter(MP_metric<0.5) %>% tally() # n=85

df_colwell_ref %>% filter(MP_metric<0.5) -> low_colw_ref

# grab flow data and plot:
load("data/usgs_Q_daily_ref_gages.rda")

# filter to gages of interest:
usgs_ref_filt <- usgs_flows_ref %>% 
  filter(site_no %in% low_colw_ref$site_no)

# LOG PLOT
(logplot_facet <- ggplot() + geom_line(data=usgs_ref_filt, aes(x=Date, y=log(Flow), group=site_no), 
                     color="steelblue", lwd=0.3) + 
  facet_wrap(~site_no, scales = "free_y"))

# PLOTLY POR PLOT
plotly::ggplotly(ggplot() + geom_line(data=usgs_ref_filt, aes(x=Date, y=Flow, group=site_no, color=site_no), lwd=0.3, 
                     show.legend = FALSE))

# NOTES
# 11529000, truncate after 1967-09-29
# 11526500, start 1957-02-13, end 1980-09-29
# 11525500, truncate after 1960-10-01
# 11472900, truncate after 1975-10-01
# 11467500, truncate after 1971-10-01
# 11467200, start at 2003-10-01
# 11445500, truncate after 1960-10-01 (SFA at lotus)
# 11445500, truncate after 1960-10-01 (SFA at lotus)
# 11341400, truncate after 1968-10-01
# 11208000, truncate after 2002-10-01
# 11169800, truncate after 1982-10-01, then starts again in 2004-10-01 thru current
# 11113000, Sespe Ck, start at 1927-10-01, missing 1985-10-01 to 1990-10-01
# 11095500, Big Tujunga C, dam went in 1931, truncate 1931-10-01, ALT/reg after, truncated spill
# 11082000, WF San Gabriel, below dam that went in 1932, some flattening but largely natural?

# INTERMITTENT?
# 11274500, INTERMITTENT, Orestimba Ck goes dry (zero) most years
# 11208500, MF Kaweah Trib, goes dry many years (zero)
# 11196400, Caliente Ck, goes dry in some years, very flatlined in most years
# 11195500, goes dry in some years, very flatlined in most years (~0.1 cfs)
# 11154700, Clear Ck, goes to zero on occasion
# 11154100, Bodfish Ck, goes to zero on occasion
# 11152900, Cedar Ck goes to zero on occasion
# 11151300, San Lorenzo Ck goes to zero on occasion
# 11143500, Salinas River, goes to zero on occasion
# 11120500, San Jose Ck, goes to zero on occasion
# 11117800, Santa Ana Ck, goes to zero on occasion
# 11111500, Sespe Ck, goes to zero on occasion
# 11094000, Big Tujunga C, goes to zero on occasion
# 11073470, Cucamonga, goes near zero
# 11063000, Cajon flatlined
# 11037700, Pauma Valley
# 11033000, WF San Luis Rey R, flatlined, 1913-1915, then 1956-10-01 to 1986



# DROP/SWITCH?
# 11418000, ENGLEBRIGHT, needs to be moved to ALT data
# 11406999, FEATHER RIVER AT OROVILLE needs to be moved to ALT data
# 11294500, long record, NF Stan at Avery, but ALT??


