# Colwells

library(hydrostats)
#library(tsbox)
library(tidyverse)
library(purrr)
library(lubridate)
library(glue)
library(tidylog)
library(ggdark)

# GET SITES ---------------------------------------------------------------

# get metadata
usgs_alt <- read_csv("data/usgs_metadata_alt_gages.csv") %>% 
  rename(site_no=site_id)
usgs_ref <- read_csv("data/usgs_metadata_ref_gages.csv") %>% 
  rename(site_no = site_id) %>% 
  mutate(site_no = as.character(site_no))

# Get Data ----------------------------------------------------------------

load("data/usgs_Q_daily_alt_gages.rda")
load("data/usgs_Q_daily_ref_gages.rda")

# filter and join
usgs_flows_alt <- usgs_flows_alt %>% 
  select(-agency_cd) %>% 
  left_join(., usgs_alt, by=c("site_no"))
usgs_flows_ref <- usgs_flows_ref %>% 
  select(-agency_cd) %>% 
  left_join(., usgs_ref, by=c("site_no"))

# distinct_gage_lists
gage_alt_distinct <- usgs_flows_alt %>% distinct(site_no)
gage_ref_distinct <- usgs_flows_ref %>% distinct(site_no)

# CALCULATE COLWELL -------------------------------------------------------

# quick colwell analysis of seasonality:
## From Tonkin et al 2017: M (Contingency) as metric of seasonality
## To standardize the role of seasonality in relation to overall predictability,
## we divided (M) by overall predictability (the sum of (M) and constancy (C)

# standardize
df_ref <- usgs_flows_ref %>%   
  rename("Q"=Flow) %>% 
  select(Date, Q, site_no) %>% 
  mutate(Date = ymd(as.character(Date), tz = "US/Pacific")) %>% 
  group_by(site_no) %>% 
  group_split() %>%
  #.[1:30] %>% # extract
  map(., ~as.data.frame(.x)) 

# calc colwells and add ID
df_colwell_ref <- df_ref %>% 
  map(., ~hydrostats::Colwells(.x)) %>% 
  map(., ~tibble(MP_metric=c(.x[["MP"]]))) %>%
  bind_rows() %>% 
  mutate(site_no = gage_ref_distinct$site_no)

# standardize
df_alt <- usgs_flows_alt %>%   
  rename("Q"=Flow) %>% 
  select(Date, Q, site_no) %>% 
  mutate(Date = ymd(as.character(Date), tz = "US/Pacific")) %>% 
  group_by(site_no) %>% 
  group_split() %>%
  #.[1:30] %>% # extract
  map(., ~as.data.frame(.x)) 

# calc colwells and add ID
df_colwell_alt <- df_alt %>% 
  map(., ~hydrostats::Colwells(.x)) %>% 
  map(., ~tibble(MP_metric=c(.x[["MP"]]))) %>%
  bind_rows() %>% 
  mutate(site_no = gage_alt_distinct$site_no)

summary(df_colwell_alt)
summary(df_colwell_ref)

# JOIN WITH META ----------------------------------------------------------

df_colwell_alt_meta <- df_colwell_alt %>% left_join(., usgs_alt, by=c("site_no"))
df_colwell_ref_meta <- df_colwell_ref %>% left_join(., usgs_ref, by=c("site_no"))

# bind together
df_colwell_all <- bind_rows(df_colwell_alt_meta, df_colwell_ref_meta)
table(df_colwell_all$gagetype)

# PLOTS --------------------------------------------------------------------

df_colwell_all %>% ggplot() + geom_histogram(aes(y=MP_metric, fill=gagetype))

df_colwell_all %>% 
  ggplot() + 
  geom_point(aes(x=as.factor(huc8), y=MP_metric, fill=gagetype), pch=21, size=4) +
  theme_classic(base_family = "Roboto Condensed")+
  scale_fill_viridis_d()

df_colwell_all %>% 
  ggplot() + 
  geom_jitter(aes(y=MP_metric, x=gagetype), color="gray20", alpha=0.5) +
  geom_boxplot(aes(y=MP_metric, x=gagetype, fill=gagetype), lwd=0.75, alpha=0.85, color="black") +
  theme_classic(base_family = "Roboto Condensed")+
  scale_fill_viridis_d(option = "A", "Gage Type") +
  labs(y="Seasonality (Colwell's M/P)", x="",
       caption = "Standardized seasonality in relation to overall predictability \nby dividing seasonality (M) by overall predictability \n(the sum of (M) and constancy (C)), as per Tonkin et al. (2017)")

ggsave(filename = "figures/boxplot_colwells_ref_alt.png", width = 10, height = 8, dpi = 300, units = "in")

# SAVE --------------------------------------------------------------------

# save
write_rds(df_colwell_all, file = "output/usgs_gages_colwells_metric.rds")

