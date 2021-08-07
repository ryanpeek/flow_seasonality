# rerun Colwell with revised flows from REF

library(hydrostats)
library(tidyverse)
library(purrr)
library(lubridate)
library(glue)
library(tidylog)
library(ggdark)
library(sf)

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

# 01: Import Flow/GAGE Data -----------------------------------------------

# flow data
load("data/usgs_Q_daily_alt_gages_rev.rda") # nrow=14724405
load("data/usgs_Q_daily_ref_gages_rev.rda") # nrow=4070777

# revised flow data
alt_revised <- read_rds("output/alt_revised_gages.rds") %>% 
  mutate(date = ymd(as.character(Date), 
                    tz="America/Los_Angeles"), .after="Date") %>% 
  select(-agency_cd, -Date, -gagetype) %>% 
  group_by(site_no) %>% 
  complete(date = seq.Date(min(ymd(date)), max(ymd(date)), by = "day")) %>% 
  # add count by days with flow
  mutate(flowdays = ifelse(!is.na(Flow), 1, 0)) %>% 
  mutate(flowcnt = ave(flowdays, cumsum(flowdays == 0), FUN = cumsum)) %>% 
  ungroup %>% 
  group_by(site_no) %>% 
  group_split() %>% 
  map(., ~get_longest_flowperiod(.x)) %>% 
  bind_rows()

# plot
# ggplot() + 
#   geom_line(data=alt_revised, aes(x=date, y=Flow, color=site_no), show.legend = FALSE) +
#   facet_wrap(.~site_no, scales = "free")

# fix
ref_revised <- read_rds("output/ref_revised_gages.rds") %>% 
  mutate(date = ymd(as.character(Date), 
                    tz="America/Los_Angeles"), .after="Date") %>% 
  select(-agency_cd, -Date) %>% 
  group_by(site_no) %>% 
  complete(date = seq.Date(min(ymd(date)), max(ymd(date)), by = "day")) %>% 
  # add count by days with flow
  mutate(flowdays = ifelse(!is.na(Flow), 1, 0)) %>% 
  mutate(flowcnt = ave(flowdays, cumsum(flowdays == 0), FUN = cumsum)) %>% 
  ungroup %>% 
  group_by(site_no) %>% 
  group_split() %>% 
  map(., ~get_longest_flowperiod(.x)) %>% 
  bind_rows()

# plot
# ggplot() + 
#   geom_line(data=ref_revised, aes(x=date, y=Flow, color=site_no), show.legend = FALSE) +
#   facet_wrap(.~site_no, scales = "free")

# FILTER TO LONGEST PERIOD OF RECORD --------------------------------------


usgs_flows_ref <- usgs_flows_ref %>% 
  ungroup() %>% 
  group_by(site_no) %>% 
  group_split() %>% 
  map(., ~get_longest_flowperiod(.x)) %>% 
  bind_rows()


usgs_flows_alt <- usgs_flows_alt %>% 
  ungroup() %>% 
  group_by(site_no) %>% 
  group_split() %>% 
  map(., ~get_longest_flowperiod(.x)) %>% 
  bind_rows()

# 02: GET SITES -----------------------------------------------------------

# get metadata
alt_meta <- read_csv("data/usgs_metadata_alt_gages.csv") %>% 
  rename(site_no=site_id)
ref_meta <- read_csv("data/usgs_metadata_ref_gages.csv") %>% 
  rename(site_no = site_id) %>% 
  mutate(site_no = as.character(site_no))

# bind
gage_metadat <- bind_rows(alt_meta, ref_meta)

# 03: Filter and Merge with Revised Data --------------------------------------

# ALT REVISED
flows_alt <- usgs_flows_alt %>% 
  bind_rows(., alt_revised)
  
# look at unique, should add sites
usgs_flows_alt %>% distinct(site_no) %>% tally() # n=748
flows_alt %>% distinct(site_no) %>% tally() # n=756

# REF REVISED: here same number of gages but records are diff
flows_ref <- usgs_flows_ref %>% 
  filter(!site_no %in% unique(ref_revised$site_no)) %>% 
  bind_rows(., ref_revised)

# check: should be same number (n=219)
usgs_flows_ref %>% distinct(site_no) %>% tally()
flows_ref %>% distinct(site_no) %>% tally()

# 04: Join with Metadata ------------------------------------------------------

# filter and join
flows_alt_meta <- flows_alt %>% 
  left_join(., gage_metadat, by=c("site_no"))

# ref
flows_ref_meta <- flows_ref %>% 
  left_join(., gage_metadat, by=c("site_no"))

# distinct_gage_lists
gage_alt_distinct <- flows_alt_meta %>% distinct(site_no)
gage_ref_distinct <- flows_ref_meta %>% distinct(site_no)


# 05: CALCULATE COLWELL -------------------------------------------------------

# quick colwell analysis of seasonality:
## From Tonkin et al 2017: M (Contingency) as metric of seasonality
## To standardize the role of seasonality in relation to overall predictability,
## we divided (M) by overall predictability (the sum of (M) and constancy (C)


# standardize
df_ref <- flows_ref_meta %>% 
  ungroup() %>% 
  rename("Q"=Flow, Date=date) %>% 
  select(Date, Q, site_no, flowcnt) %>% 
  group_by(site_no) %>% 
  group_split() %>%
  #.[1:30] %>% # extract
  map(., ~as.data.frame(.x))

# calc colwells and add ID
df_colwell_ref <- df_ref %>% 
  map(., ~hydrostats::Colwells(.x)) %>% 
  map(., ~tibble(MP_metric=c(.x[["MP"]]))) %>%
  bind_rows() %>% 
  mutate(site_no = gage_ref_distinct$site_no,
         gagetype = "REF")

# standardize
df_alt <- flows_alt_meta %>%   
  rename("Q"=Flow, Date=date) %>% 
  select(Date, Q, site_no) %>% 
  group_by(site_no) %>% 
  group_split() %>%
  #.[1:30] %>% # extract
  map(., ~as.data.frame(.x)) 

# calc colwells and add ID
df_colwell_alt <- df_alt %>% 
  map(., ~hydrostats::Colwells(.x)) %>% 
  map(., ~tibble(MP_metric=c(.x[["MP"]]))) %>%
  bind_rows() %>% 
  mutate(site_no = gage_alt_distinct$site_no,
         gagetype = "ALT")

summary(df_colwell_alt)
summary(df_colwell_ref)

# BIND: 
df_colwell_all <- bind_rows(df_colwell_alt, df_colwell_ref)
table(df_colwell_all$gagetype)

# 06: JOIN WITH META ----------------------------------------------------------

df_colwell_all_meta <- df_colwell_all %>% 
  left_join(., gage_metadat %>% select(-gagetype), by=c("site_no"))


table(df_colwell_all$gagetype)

# 07: SAVE ------------------------------------------------------------------

# save
write_rds(df_colwell_all_meta, file = "output/usgs_gages_colwells_metric.rds")

# 08: READ IN DATA ---------------------------------------------------------

df_colwell_all_meta <- read_rds("output/usgs_gages_colwells_metric.rds")

# 09: PLOTS -----------------------------------------------------------------

# Histogram
df_colwell_all %>% ggplot() + geom_histogram(aes(y=MP_metric, fill=gagetype))

# Boxplot of ref/alt
df_colwell_all_meta %>% 
  ggplot() + 
  geom_jitter(aes(y=MP_metric, x=gagetype), color="gray20", alpha=0.5) +
  geom_boxplot(aes(y=MP_metric, x=gagetype, fill=gagetype), lwd=0.75, alpha=0.85, color="black") +
  theme_classic(base_family = "Roboto Condensed")+
  scale_fill_viridis_d(option = "A", "Gage Type") +
  labs(y="Seasonality (Colwell's M/P)", x="",
       caption = "Standardized seasonality in relation to overall predictability \nby dividing seasonality (M) by overall predictability \n(the sum of (M) and constancy (C)), as per Tonkin et al. (2017)")

#ggsave(filename = "figures/boxplot_colwells_ref_alt.png", width = 10, height = 8, dpi = 300, units = "in")

# Notched Boxplot of ref/alt
df_colwell_all_meta %>% 
  ggplot() + 
  geom_jitter(aes(y=MP_metric, x=gagetype), color="gray20", alpha=0.5) +
  geom_boxplot(aes(y=MP_metric, x=gagetype, fill=gagetype), lwd=0.75, alpha=0.85, color="black", notch = TRUE) +
  theme_classic(base_family = "Roboto Condensed")+
  scale_fill_viridis_d(option = "A", "Gage Type") +
  labs(y="Seasonality (Colwell's M/P)", x="",
       caption = "Standardized seasonality in relation to overall predictability \nby dividing seasonality (M) by overall predictability \n(the sum of (M) and constancy (C)), as per Tonkin et al. (2017)")

#ggsave(filename = "figures/boxplot_notched_colwells_ref_alt.png", width = 10, height = 8, dpi = 300, units = "in")

# 10: BRING IN CSCI -------------------------------------------------------

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

# 11: JOIN COLWELL AND CSCI -----------------------------------------------

# join w seasonality:
csci_alt_colwell <- left_join(csci_alt, df_colwell_all_meta, by=c("site_id"="site_no")) %>%
  distinct(StationCode, SampleID, site_id, .keep_all=TRUE)

table(csci_alt_colwell$gagetype)

csci_por_colwell <- left_join(csci_por, df_colwell_all_meta, by=c("site_id"="site_no")) %>%
  distinct(StationCode, SampleID, site_id, .keep_all=TRUE)

table(csci_por_colwell$gagetype)

# 12: PLOTS ---------------------------------------------------------------

# CSCI vs GAGETYPE
csci_por_colwell %>% 
  ggplot() + 
  geom_point(aes(y=MP_metric, x=csci, fill=gagetype), pch=21, size=2.7, alpha=0.9) +
  stat_smooth(aes(y=MP_metric, x=csci, color=gagetype), 
              method = "gam") +
  theme_classic(base_family = "Roboto Condensed") +
  scale_color_viridis_d(option = "B", "Gage Type") +
  scale_fill_viridis_d(option = "A", "Gage Type") +
  labs(y="Seasonality (Colwell's M/P)", x="CSCI",
       caption = "Standardized seasonality in relation to overall predictability \nby dividing seasonality (M) by overall predictability \n(the sum of (M) and constancy (C)), as per Tonkin et al. (2017)")

ggsave(filename = "figures/colwells_ref_alt_vs_csci_gam_trend.png", width = 10, height = 8, dpi = 300, units = "in")

# CSCI BY GAGETYPE FACETED
csci_por_colwell %>% 
  ggplot() + 
  geom_point(aes(y=MP_metric, x=csci, fill=gagetype), pch=21, size=2.7, alpha=0.9) +
  geom_violin(aes(y=MP_metric, x=csci, fill=gagetype), alpha=0.7) +
  theme_classic(base_family = "Roboto Condensed") +
  scale_color_viridis_d(option = "B", "Gage Type") +
  scale_fill_viridis_d(option = "A", "Gage Type") +
  labs(y="Seasonality (Colwell's M/P)", x="CSCI",
       caption = "Standardized seasonality in relation to overall predictability \nby dividing seasonality (M) by overall predictability \n(the sum of (M) and constancy (C)), as per Tonkin et al. (2017)") +
  facet_grid(.~gagetype)

# 13: READ IN STREAM CLASSES --------------------------------------------------

# streamclass data
st_layers("data/eflows_final_classification_9CLASS/Final_Classification_9CLASS.shp")

ceff_strmclass <- st_read("data/eflows_final_classification_9CLASS/Final_Classification_9CLASS.shp")
st_crs(ceff_strmclass)

# crosswalk
strmclass_xwalk <- tibble("CLASS"=c(1,2,3,4,5,6,7,8,9), "CLASS_NAME"=c("snowmelt","high-volume snowmelt and rain", "low-volume snowmelt and rain", "winter storms","groundwater","perennial groundwater and rain","flashy, ephemeral rain","rain and seasonal groundwater","high elevation low precipitation"))

# all ref gages?
ceff_ref_gages <- st_read("data/eflows_final_ref_gages/Final_Reference_Gages.shp")
st_crs(ceff_ref_gages)

# JOIN with csci_por_colwell by COMID
df_final <- left_join(csci_por_colwell, ceff_strmclass, by=c("comid_gage"="COMID"))

# add streamclass name
df_final <- left_join(df_final, strmclass_xwalk)

# replot
# CSCI vs GAGETYPE
df_final %>% 
  ggplot() + 
  geom_point(aes(y=MP_metric, x=csci, fill=CLASS_NAME), pch=21, size=2.7, alpha=0.9) +
  stat_smooth(aes(y=MP_metric, x=csci), color="black",  
              method = "glm", se = FALSE) +
  theme_classic(base_family = "Roboto Condensed") +
  scale_color_viridis_d(option = "B", "Gage Type") +
  scale_fill_viridis_d(option = "A", "StreamClass") +
  labs(y="Seasonality (Colwell's M/P)", x="CSCI",
       caption = "Standardized seasonality in relation to overall predictability \nby dividing seasonality (M) by overall predictability \n(the sum of (M) and constancy (C)), as per Tonkin et al. (2017)") +
  facet_wrap(.~CLASS_NAME)


# Save out ----------------------------------------------------------------

write_rds(df_final, file = "output/usgs_gages_colwells_w_streamclass_metric.rds")
save(ceff_strmclass, file="output/eflows_streamclasses.rda", compress = "xz")
