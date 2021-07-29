# Colwells

library(hydrostats)
library(tsbox)
library(tidyverse)
library(purrr)
library(lubridate)
library(glue)
library(tidylog)
library(ggdark)

# GET SITES ---------------------------------------------------------------

# get metadata
usgs_meta <- read_csv("data/usgs_metadata_all_gages.csv") %>% 
  rename(site_no=site_id)

# ref gages:
gages_ref <- read_csv("https://raw.githubusercontent.com/ryanpeek/ffm_comparison/main/data/usgs_gages_ref_223.txt", col_names = "site_no", col_types = "c") %>% 
  mutate(gagetype="REF")

# any in other list?
table(gages_ref$site_no %in% usgs_meta$site_no)

# NOPE!

# Get Data ----------------------------------------------------------------

load("data/usgs_Q_daily_all_gages.rda")

# filter to just ref
usgs_flows_meta <- usgs_flows %>% 
  select(-agency_cd) %>% 
  left_join(., usgs_meta, by=c("site_no"="site_id"))

gages_list <- usgs_flows_meta %>% 
  select(site_no) %>% distinct()

# rm usgs_flows
rm(usgs_flows)

# CALCULATE COLWELL -------------------------------------------------------

# quick colwell analysis of seasonality:
## From Tonkin et al 2017: M (Contingency) as metric of seasonality
## To standardize the role of seasonality in relation to overall predictability,
## we divided (M) by overall predictability (the sum of (M) and constancy (C)

# standardize
df <- usgs_flows_meta %>%   
  rename("Q"=Flow) %>% 
  select(Date, Q, site_no) %>% 
  mutate(Date = ymd(as.character(Date), tz = "US/Pacific")) %>% 
  group_by(site_no) %>% 
  group_split() %>%
  #.[1:30] %>% # extract
  map(., ~as.data.frame(.x)) 

# calc colwells and add ID
df_colwell <- df %>% 
  map(., ~hydrostats::Colwells(.x)) %>% 
  map(., ~tibble(MP_metric=c(.x[["MP"]]))) %>%
  bind_rows() %>% 
  mutate(site_no = gages_list$site_no)

summary(df_colwell)

# JOIN WITH META ----------------------------------------------------------

df_colwell_meta <- df_colwell %>% left_join(., usgs_meta, by=c("site_no"="site_id"))

# load data w ref?
all_gages <- read_csv("data/list_of_all_gages_w_ffmdat.csv")

df_colwell_meta <- df_colwell_meta %>% left_join(., all_gages, by=c("site_no"="gageid"))

# PLOTS --------------------------------------------------------------------

df_colwell_meta %>% ggplot() + geom_histogram(aes(y=MP_metric))

df_colwell_meta %>% 
  ggplot() + 
  geom_point(aes(x=as.factor(huc8), y=MP_metric, fill=alt_va), pch=21, size=4) +
  theme_classic(base_family = "Roboto Condensed")+
  scale_fill_viridis_c()

df_colwell_meta %>% 
  ggplot() + 
  geom_boxplot(aes(y=MP_metric, x=refgage, fill=refgage)) +
  theme_classic(base_family = "Roboto Condensed")+
  scale_fill_viridis_d()



# SAVE --------------------------------------------------------------------

# save
write_rds(df_colwell, file = "output/usgs_gages_colwell.rds")


