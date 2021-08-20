# Plot the Data

# LIBRARIES ---------------------------------------------------

library(tidyverse)
library(lubridate)
library(glue)
library(ggdark)
library(patchwork)
library(tidylog)
library(sf)
library(mapview)
mapviewOptions(fgb = FALSE)

# GET Gage Metadata --------------------------------------------------------

# get metadata
usgs_alt <- read_csv("data/usgs_metadata_alt_gages.csv") %>% 
  select(-agency_cd) %>% 
  rename(site_no=site_id) %>% 
  mutate(huc8 = as.factor(huc8))
usgs_ref <- read_csv("data/usgs_metadata_ref_gages.csv") %>% 
  mutate(site_id = as.character(site_id)) %>% 
  select(-agency_cd) %>% 
  rename(site_no=site_id) %>% 
  mutate(huc8 = as.factor(huc8))

# GET Daily Flow Data ------------------------------------------------------

load("data/usgs_Q_daily_alt_gages.rda")
load("data/usgs_Q_daily_ref_gages.rda")

# how many have data?
usgs_flows_alt %>% distinct(site_no) %>% tally() # n=748
usgs_flows_ref %>% distinct(site_no) %>% tally() # n=221

# REF: Get Period of Record for Ref Gages ---------------------------------

# check with FFC Ref list?
ref_223 <- read_csv("https://raw.githubusercontent.com/ceff-tech/bmi_ffm_links/main/data/usgs/gages_ref_223_period_record.csv") %>% 
  separate(stream_class, into = c("class","CLASS"), sep="-", remove=T) %>% 
  select(-c(class, stat, data)) %>% 
  mutate(CLASS = as.numeric(CLASS),
         site_no = as.character(gage), .before="CLASS") %>% 
  rename(ref_yr_start = minYr,
         ref_yr_end = maxYr,
         ref_por_range = YrRange) %>% 
  # drop two sites that are reservoirs/water temp: 
  # (11299000 New Melones, 11446220 is Amer River Water Temp!)
  filter(!site_no %in% c("11299000", "11446220"))

# join with gage metadata
ref_221 <- left_join(ref_223, usgs_ref) %>% 
  select(CLASS, site_no, ref_yr_start, ref_yr_end, ref_por_range, yr_begin, yr_end, yr_total, gagetype, usgs_lon = dec_long_va, usgs_lat = dec_lat_va, huc8)

# use this to move forward and filter data for reference sites to these periods:
# ref_221 %>% 
#   st_as_sf(coords=c("usgs_lon", "usgs_lat"), crs=4269, remove=F) %>% 
#   mapview(., zcol="huc8", legend=FALSE)

# remove old version
rm(ref_223)


## REF: Visualize Period of Record -----------------------------------------

## plot??
ggref1 <- ggplot() + 
  geom_linerange(data=ref_221, 
                 aes(x=forcats::fct_reorder(site_no, as.integer(huc8)), ymin=ref_yr_start, ymax=ref_yr_end, color=ref_por_range), size=1.1, show.legend = T) + 
  coord_flip() + 
  labs(x="", y="", subtitle="USGS REF Gages: Selected Ref Period") + 
  scale_color_viridis_c(option = "D", "Total Years", breaks=c(0,10,20,40,60)) +
  theme_classic(base_family = "Roboto Condensed", base_size = 8)
ggref1

## now plot full por
ggref2 <- ggplot() + 
  geom_linerange(data=ref_221, 
                 aes(x=forcats::fct_reorder(site_no, as.integer(huc8)), ymin=yr_begin, ymax=yr_end, color=yr_total), size=1.1, show.legend = T) + 
  coord_flip() + 
  labs(x="", y="", subtitle="USGS REF Gages: Full Period of Record (N=221)") + 
  scale_color_viridis_c(option = "D", "Total Years", breaks=c(0,10,20,40,60,80,100,120)) +
  theme_classic(base_family = "Roboto Condensed", base_size = 8)
ggref2

ggref2 / ggref1
ggsave(filename = "figures/reference_gages_periods_of_record_arranged_byhuc.png", 
       width = 11, height = 11, dpi=300)

## plot??
ggref1b <- ggplot() + 
  geom_linerange(data=ref_221, 
                 aes(x=forcats::fct_reorder(site_no, ref_por_range), ymin=ref_yr_start, ymax=ref_yr_end, color=ref_por_range), size=1.1, show.legend = T) + 
  coord_flip() + 
  labs(x="", y="", subtitle="USGS REF Gages: Selected Ref Period") + 
  scale_color_viridis_c(option = "D", "Total Years", breaks=c(0,10,20,40,60)) +
  theme_classic(base_family = "Roboto Condensed", base_size = 8)
ggref1b

## now plot full por
ggref2b <- ggplot() + 
  geom_linerange(data=ref_221, 
                 aes(x=forcats::fct_reorder(site_no, yr_total), ymin=yr_begin, ymax=yr_end, color=yr_total), size=1.1, show.legend = T) + 
  coord_flip() + 
  labs(x="", y="", subtitle="USGS REF Gages: Full Period of Record (N=221)") + 
  scale_color_viridis_c(option = "D", "Total Years",breaks=c(0,10,20,40,60,80,100,120)) +
  theme_classic(base_family = "Roboto Condensed", base_size = 8)
ggref2b

ggref2b / ggref1b
ggsave(filename = "figures/reference_gages_periods_of_record_arranged_by_totyrs.png", 
       width = 11, height = 11, dpi=300)

rm(ggref1, ggref1b, ggref2, ggref2b)

## REF: Filter to Reference Periods ---------------------------------------------

# fix date cols & add continuous count of flow days
usgs_flows_ref_por <- usgs_flows_ref %>% 
  mutate(date = ymd(as.character(Date), 
                    tz="America/Los_Angeles"), .after="Date") %>% 
  select(-agency_cd, -Date) %>% 
  left_join(., ref_221, by=c("site_no")) %>%
  group_split(site_no) %>% 
  map_df(., ~filter(., waterYear>=ref_yr_start & waterYear<=ref_yr_end))

# check number of stations?
length(unique(usgs_flows_ref_por$site_no))

# Add WYD
usgs_flows_ref_por <- usgs_flows_ref_por %>% 
  wateRshedTools::add_WYD("date")

# Calc Mean Ann Flow and Plot:
# use invert_geom_defaults() to switch back
usgs_flows_ref_por %>% 
  group_by(site_no, DOWY) %>% 
  summarize(meanFlow = mean(Flow, na.rm=TRUE)) %>%
  ggplot() + 
  geom_line(aes(x=DOWY, y=meanFlow, group=site_no), alpha=0.8) +
  #scale_y_log10() +
  ggdark::dark_theme_bw(base_family = "Roboto Condensed", base_size = 8) #+
  #facet_wrap(.~site_no, scales = "free")

# add last pieces of metadata:
usgs_flows_ref_por <- usgs_flows_ref_por %>% 
  left_join(., usgs_ref[,c("site_no","station_nm", "parm_cd","stat_cd")])

length(unique(usgs_flows_ref_por$site_no))

## REF: SAVE OUT -------------------------------

write_csv(usgs_flows_ref_por, file = "data/usgs_Q_daily_ref_gages_por.csv.gz")
save(usgs_flows_ref_por, file="data/usgs_Q_daily_ref_gages_por.rda", compress = "xz" )


# ALT: Get Gages to Drop (CANALS WEIRS PH etc) ------------------------------------

# need to filter out a lot of the highly altered / diversion type gages
# first filter to these and view/map them
usgs_alt %>% 
  filter(grepl("CN|TAILRACE|DIV|INTAKE|WEIR|CONDUIT| PP| PH", station_nm)) %>% 
  # need to keep real sites: 
  # TUO early Intake (11276600, 11276900), 11403530 bucks ck, 11230530 bear ck
  # or sites below diversion dams that are on run of river (Slate, NF Feather)
  filter(!site_no %in% c("11276600", "11276900", "11403530", "11230530",
                         "10257501", "11118501","11113001", "11278300",
                         "11278400", "11289651", "11441900", "11413300",
                         "11403200","11355010", "11299996", "10290500")) %>% 
  distinct(site_no, .keep_all = TRUE) %>% 
  st_as_sf(coords=c("dec_long_va", "dec_lat_va"), crs=4269, remove=F) %>% 
  #View() # n=231, 
  #mapview(., zcol="huc8", legend=FALSE) %>% 
  st_drop_geometry() %>% 
  select(site_no, station_nm, yr_begin:gagetype) -> gages_to_drop  

# so 231 gages to drop from 748, ALT TOTAL = 517

## ALT: Filter Daily Flows  -----------------------------------------------

usgs_flows_alt_filt <- usgs_flows_alt %>% 
  ungroup() %>% 
  # drop gages from above (~25% of data)
  filter(!site_no %in% gages_to_drop$site_no) %>% 
  # add WYD info
  wateRshedTools::add_WYD(., datecolumn = "Date") %>% 
  # join with metadata
  left_join(., usgs_alt, by=c("site_no")) %>% 
  mutate(date = ymd(as.character(Date), 
                    tz="America/Los_Angeles"), .after="Date") %>% 
  select(-c(agency_cd, Date, site_tp_cd, coord_acy_cd,
            dec_coord_datum_cd, count_nu, alt_va, alt_acy_va, alt_datum_cd,
            interval, ts_id, loc_web_ds:access_cd)) %>% 
  rename(usgs_lon = dec_long_va, usgs_lat = dec_lat_va)
  
janitor::compare_df_cols(usgs_flows_ref_por, usgs_flows_alt_filt)


## ALT: SAVE OUT -------------------------------

write_csv(usgs_flows_alt_filt, file = "data/usgs_Q_daily_alt_gages_filt.csv.xz")

save(usgs_flows_alt_filt, file="data/usgs_Q_daily_alt_gages_filt.rda", compress = "xz" )



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
