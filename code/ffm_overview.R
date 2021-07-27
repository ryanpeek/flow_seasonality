# test

# R. Peek 2021


# Libraries ---------------------------------------------------------------

library(scales)
library(sf)
library(glue)
options(tidyverse.quiet = TRUE)
library(tidyverse) # load quietly
library(viridis) # colors
library(janitor)

# 01: GET DATA ----------------------------------------------------------------

# REF gages
gages_ref <- read_csv("data/usgs/gages_ref_223_period_record.csv") %>% 
  select(stream_class, gage, maxYr, minYr, YrRange) %>% 
  distinct(gage, .keep_all = TRUE) %>% 
  mutate(refgage="Ref")

# ffc data (alteration status)
ffc_alt <- read_rds(file = "https://github.com/ryanpeek/ffm_comparison/raw/main/output/ffc_combined/usgs_combined_alteration.rds") %>%
  mutate(gage=as.numeric(gageid),
         status=as.factor(status),
         alteration_type=as.factor(alteration_type))

# import observed FFM data for period of record
ffc_obs <- read_rds(file = "https://github.com/ryanpeek/ffm_comparison/raw/main/output/ffc_combined/usgs_combined_ffc_percentiles.rds") %>%
  mutate(gage=as.numeric(gageid))

# import predicted percentiles for period of record
ffc_pred <- read_rds(file="https://github.com/ryanpeek/ffm_comparison/raw/main/output/ffc_combined/usgs_combined_predicted_percentiles.rds") %>% 
  mutate(gage=as.numeric(gageid))

# 02: TIDY AND CLEAN ----------------------------------------------------------

# get distinct gages (n=959)
ffc_gages <- ffc_alt %>% distinct(gageid, .keep_all=TRUE) %>% 
  mutate(gage=as.numeric(gageid)) %>% 
  left_join(., gages_ref, by="gage") %>% 
  mutate(refgage = case_when(
    is.na(refgage) ~ "Non-Ref",
    TRUE ~ refgage)) %>% 
  select(-c(metric:median_in_iqr))

# join FFC_DAT w ref gages (FROM CEFF/FFM) data to get REF/non-ref class
ffc_alt <- left_join(ffc_alt, gages_ref, by="gage") %>% 
  mutate(refgage = case_when(
    is.na(refgage) ~ "Non-Ref",
    TRUE ~ refgage))

# tally (24 metrics x 959 gages = 23016)
ffc_alt %>% group_by(refgage, status) %>%
  tally(name="total_status") %>% # tally by status
  # tally by ref gage class
  add_tally(total_status, name="total_of_refgage_class") # so only 21912, some NA's still

summary(ffc_alt$status)
summary(ffc_alt$status_code)
## so not_enough_data = NA in status_code
summary(ffc_alt$alteration_type)


# 03: CALC PROPORTION GAGES ALTERED PER METRIC ----------------

# count total records by metric & refgage
(ffm_metric_count <- ffc_alt %>% 
   group_by(metric, refgage) %>% 
   count(name = "total_count"))

# calculate proportion of gages/total for each status class
ffm_prcnt_alt <- ffc_alt %>% 
  group_by(metric, status, refgage) %>% # for facet by refgage
  tally() %>% 
  left_join(., ffm_metric_count) %>% 
  mutate(prop_n = n/total_count) %>% 
  # create all possible combos
  complete(metric, status, refgage) %>% 
  # drop duplications here (not sure why getting dups!?)
  distinct(.keep_all = TRUE) %>% 
  # fill NAs with zero
  mutate(across(everything(), ~replace_na(.x, 0)))

# filter to just ref
ffm_prcnt_alt_ref <- ffm_prcnt_alt %>% filter(refgage=="Ref") 

## Plot: ALT STATUS -------------------------------------------------------

# plot faceted by REF/NON-REF type
ggplot() + geom_col(data=ffm_prcnt_alt, aes(x=metric, y=prop_n, fill=status)) +
  theme_classic(base_family = "Roboto Condensed") +
  scale_y_continuous(labels=percent) +
  scale_fill_viridis_d(direction = -1) +
  labs(title="Proportion of Gages by Alteration Status",
       subtitle="Based on 959 gages with sufficient data for FFM R Calculator",
       x="", y="Proportion of Gages Altered") +
  theme(axis.text.x = element_text(angle=270, hjust = 0.1, vjust=0.05)) +
  facet_wrap(vars(refgage))

#ggsave(filename = "figs/prop_gages_by_alt_status_faceted.png", width = 11, height = 8.5, dpi=300)