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

table(all_gages$refgage)

# unique? (n=748)
length(unique(all_gages$gageid))

uniq_gages <- all_gages %>% distinct(gageid)

# GET METADATA ------------------------------------------------------------

# flow: parameterCd = "00060"/ gageHeight="00065", dailymean = statCd "00003"
# https://nwis.waterdata.usgs.gov/usa/nwis/pmcodes/help?codes_help
# check what daily data is available:
(usgs_daily <- whatNWISdata(siteNumber=uniq_gages$gageid, service='dv', parameterCd = '00060', statCd='00003') %>% 
   #select(site_no, station_nm, dec_lat_va, dec_long_va, huc_cd, 
  #        data_type_cd, begin_date:count_nu) %>% 
   rename(interval=data_type_cd, huc8=huc_cd, site_id=site_no,
          date_begin=begin_date, date_end=end_date) %>% 
   mutate(yr_begin = year(date_begin),
          yr_end = year(date_end),
          yr_total = yr_end-yr_begin))

# save out:
write_csv(usgs_daily, file = "data/usgs_metadata_all_gages.csv")

length(unique(usgs_daily$huc8))

# VISUALIZE DATE RANGES FOR SITES -----------------------------------------

# visualize the stations to see date ranges
ggplot() + 
  geom_linerange(data=usgs_daily, 
                 aes(x=forcats::fct_reorder(site_id, huc8), ymin=yr_begin, ymax=yr_end, color=as.factor(huc8)), size=1.1, show.legend = F) + 
  coord_flip() + 
  labs(x="", y="") + 
  scale_color_viridis_d(option = "A") +
  #theme_bw(base_family = "Roboto Condensed", base_size = 8) +
  ggdark::dark_theme_bw(base_family = "Roboto Condensed", base_size = 8)

ggsave(filename = "figures/usgs_gage_year_ranges.jpg",
       width=11, height = 8.5, units = "in", dpi=300)

#ggsave(filename = "figures/usgs_gages_year_ranges.pdf", 
#       width=11, height = 8.5, units = "in", device = cairo_pdf)

# DOWNLOAD DAILY DATA -----------------------------------------------------

g700 <- uniq_gages %>% slice(601:748)

# Get daily
usgs_day <- dataRetrieval::readNWISdv(siteNumbers=g700$gageid, parameterCd = "00060", statCd='00003') 

usgs_day100 <- usgs_day %>% dataRetrieval::addWaterYear()
usgs_day200 <- usgs_day %>% dataRetrieval::addWaterYear()
usgs_day300 <- usgs_day %>% dataRetrieval::addWaterYear()
usgs_day400 <- usgs_day %>% dataRetrieval::addWaterYear()
usgs_day500 <- usgs_day %>% dataRetrieval::addWaterYear()
usgs_day600 <- usgs_day %>% dataRetrieval::addWaterYear()
usgs_day700 <- usgs_day %>% dataRetrieval::addWaterYear()

usgs_flows <- bind_rows(usgs_day100, usgs_day200, usgs_day300, usgs_day400,
                        usgs_day500, usgs_day600, usgs_day700) %>% 
  dataRetrieval::renameNWISColumns()


# Save Out ----------------------------------------------------------------

# write out as compressed (.gz, bz2, or xz (lzma)): 
write_csv(usgs_flows, file = "data/usgs_Q_daily_all_gages.csv.xz")

# save as rda compressed:
save(usgs_flows, file = "data/usgs_Q_daily_all_gages.rda", compress = "xz")

