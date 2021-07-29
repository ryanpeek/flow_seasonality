# Download Daily Gage Data

# LIBRARIES ---------------------------------------------------------------

library(dataRetrieval)
library(sf)
library(tidyverse)
library(mapview)
mapviewOptions(fgb=FALSE)
library(purrr)
library(lubridate)
library(tidylog)
library(ggdark)


# GET SITES ---------------------------------------------------------------

# load data
gages_alt <- read_csv("data/list_of_alt_gages_w_ffmdat.csv") %>% 
  #refgage is false...not sure where it's from, make new col
  mutate(gagetype="ALT") %>% 
  rename(site_no = gageid) %>% 
  select(-refgage)

# unique? (n=748)
length(unique(gages_alt$site_no))

# make list
gages_alt_uniq <- gages_alt %>% distinct(site_no)

# get REF site
gages_ref <- read_csv("https://raw.githubusercontent.com/ryanpeek/ffm_comparison/main/data/usgs_gages_ref_223.txt", col_names = "site_no", col_types = "c") %>% 
  mutate(gagetype="REF")


# GET ALT METADATA ------------------------------------------------------------

# flow: parameterCd = "00060"/ gageHeight="00065", dailymean = statCd "00003"
# https://nwis.waterdata.usgs.gov/usa/nwis/pmcodes/help?codes_help
# check what daily data is available:

# usgs_daily_alt <- whatNWISdata(siteNumber=gages_alt_uniq$site_no, service='dv', parameterCd = '00060', statCd='00003') %>%
#    #select(site_no, station_nm, dec_lat_va, dec_long_va, huc_cd,
#   #        data_type_cd, begin_date:count_nu) %>%
#    rename(interval=data_type_cd, huc8=huc_cd, site_id=site_no,
#           date_begin=begin_date, date_end=end_date) %>%
#    mutate(yr_begin = year(date_begin),
#           yr_end = year(date_end),
#           yr_total = yr_end-yr_begin,
#           gagetype = "ALT")
# 
# # save out:
# write_csv(usgs_daily_alt, file = "data/usgs_metadata_alt_gages.csv")
# 
# # status
# length(unique(usgs_daily_alt$site_id)) # so 748 unique
# table(usgs_daily_alt$gagetype)

# GET REF METADATA --------------------------------------------------------

# usgs_daily_ref <- whatNWISdata(siteNumber=gages_ref$site_no, service='dv', parameterCd = '00060', statCd='00003') %>%
#    rename(interval=data_type_cd, huc8=huc_cd, site_id=site_no,
#           date_begin=begin_date, date_end=end_date) %>%
#    mutate(yr_begin = year(date_begin),
#           yr_end = year(date_end),
#           yr_total = yr_end-yr_begin,
#           site_id = as.character(site_id),
#           gagetype = "REF")
# 
# # save out:
# write_csv(usgs_daily_ref, file = "data/usgs_metadata_ref_gages.csv")
# length(unique(usgs_daily_ref$site_id)) # so dropped 2?


# READ IN METADATA --------------------------------------------------------

# read in:
usgs_daily_alt <- read_csv("data/usgs_metadata_alt_gages.csv")
usgs_daily_ref <- read_csv("data/usgs_metadata_ref_gages.csv") %>% 
  mutate(site_id = as.character(site_id))

# how many in alt list? (NONE)
table(unique(usgs_daily_ref$site_id) %in% unique(usgs_daily_alt$site_id))

# VISUALIZE DATE RANGES FOR SITES -----------------------------------------

# visualize the stations to see date ranges

# ALT
ggplot() + 
  geom_linerange(data=usgs_daily_alt, 
                 aes(x=forcats::fct_reorder(site_id, huc8), ymin=yr_begin, ymax=yr_end, color=as.factor(huc8)), size=1.1, show.legend = F) + 
  coord_flip() + 
  labs(x="", y="") + 
  scale_color_viridis_d(option = "A") +
  #theme_bw(base_family = "Roboto Condensed", base_size = 8) +
  ggdark::dark_theme_bw(base_family = "Roboto Condensed", base_size = 8)

#ggsave(filename = "figures/usgs_gage_year_ranges.jpg",
#       width=11, height = 8.5, units = "in", dpi=300)

#ggsave(filename = "figures/usgs_gages_year_ranges.pdf", 
#       width=11, height = 8.5, units = "in", device = cairo_pdf)

# REF
ggplot() + 
  geom_linerange(data=usgs_daily_ref, 
                 aes(x=forcats::fct_reorder(site_id, huc8), ymin=yr_begin, ymax=yr_end, color=as.factor(huc8)), size=1.1, show.legend = F) + 
  coord_flip() + 
  labs(x="", y="") + 
  scale_color_viridis_d(option = "A") +
  #theme_bw(base_family = "Roboto Condensed", base_size = 8) +
  ggdark::dark_theme_bw(base_family = "Roboto Condensed", base_size = 8)

# ggsave(filename = "figures/usgs_gage_year_ranges_ref.jpg",
#       width=11, height = 8.5, units = "in", dpi=300)

# DOWNLOAD DAILY DATA -----------------------------------------------------

# g700 <- uniq_gages %>% slice(601:748)
# 
# # Get daily
# usgs_day <- dataRetrieval::readNWISdv(siteNumbers=g700$gageid, parameterCd = "00060", statCd='00003') 
# 
# usgs_day100 <- usgs_day %>% dataRetrieval::addWaterYear()
# usgs_day200 <- usgs_day %>% dataRetrieval::addWaterYear()
# usgs_day300 <- usgs_day %>% dataRetrieval::addWaterYear()
# usgs_day400 <- usgs_day %>% dataRetrieval::addWaterYear()
# usgs_day500 <- usgs_day %>% dataRetrieval::addWaterYear()
# usgs_day600 <- usgs_day %>% dataRetrieval::addWaterYear()
# usgs_day700 <- usgs_day %>% dataRetrieval::addWaterYear()
# 
# usgs_flows_alt <- bind_rows(usgs_day100, usgs_day200, usgs_day300, usgs_day400,
#                         usgs_day500, usgs_day600, usgs_day700) %>% 
#   dataRetrieval::renameNWISColumns()

# Save Out ----------------------------------------------------------------

# write out as compressed (.gz, bz2, or xz (lzma)): 
# write_csv(usgs_flows_alt, file = "data/usgs_Q_daily_alt_gages.csv.xz")

# save as rda compressed:
# save(usgs_flows_alt, file = "data/usgs_Q_daily_alt_gages.rda", compress = "xz")

# Download Ref Data -------------------------------------------------------

# break into chunks to avoid timing out on download

g1 <- gages_ref %>% slice(1:25)
g2 <- gages_ref %>% slice(26:50)
g3 <- gages_ref %>% slice(51:75)
g4 <- gages_ref %>% slice(76:100)
g5 <- gages_ref %>% slice(101:125)
g6 <- gages_ref %>% slice(126:150)
g7 <- gages_ref %>% slice(151:175)
g8 <- gages_ref %>% slice(176:221)

# Get daily
usgs_day_ref1 <- dataRetrieval::readNWISdv(siteNumbers=g1$site_no, parameterCd = "00060", statCd='00003') 
beepr::beep(2)
usgs_day_ref2 <- dataRetrieval::readNWISdv(siteNumbers=g2$site_no, parameterCd = "00060", statCd='00003') 
beepr::beep(2)
usgs_day_ref3 <- dataRetrieval::readNWISdv(siteNumbers=g3$site_no, parameterCd = "00060", statCd='00003') 
beepr::beep(2)
usgs_day_ref4 <- dataRetrieval::readNWISdv(siteNumbers=g4$site_no, parameterCd = "00060", statCd='00003') 
beepr::beep(2)
usgs_day_ref5 <- dataRetrieval::readNWISdv(siteNumbers=g5$site_no, parameterCd = "00060", statCd='00003') 
beepr::beep(2)
usgs_day_ref6 <- dataRetrieval::readNWISdv(siteNumbers=g6$site_no, parameterCd = "00060", statCd='00003') 
beepr::beep(2)
usgs_day_ref7 <- dataRetrieval::readNWISdv(siteNumbers=g7$site_no, parameterCd = "00060", statCd='00003') 
beepr::beep(2)
usgs_day_ref8 <- dataRetrieval::readNWISdv(siteNumbers=g8$site_no, parameterCd = "00060", statCd='00003') 
beepr::beep(2)

# bind and add water year
usgs_flows_ref <- bind_rows(usgs_day_ref1, usgs_day_ref2, usgs_day_ref3, usgs_day_ref4,
                            usgs_day_ref5, usgs_day_ref6, usgs_day_ref7, usgs_day_ref8) %>%
  dataRetrieval::addWaterYear() %>% 
  dataRetrieval::renameNWISColumns()

length(unique(usgs_flows_ref$site_no)) # dropped 2 gages...

# write out as compressed (.gz, bz2, or xz (lzma)): 
write_csv(usgs_flows_ref, file = "data/usgs_Q_daily_ref_gages.csv.xz")

# save as rda compressed:
save(usgs_flows_ref, file = "data/usgs_Q_daily_ref_gages.rda", compress = "xz")



