# EDA of Colwell and CSCI

library(tidyverse)
library(glue)
library(lubridate)
library(tidylog)
library(ggdark)
library(sf)
library(plotly)
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


# Get Flow Data -----------------------------------------------------------

# grab flow data and plot:
load("data/usgs_Q_daily_ref_gages.rda")

# filter to gages of interest:
usgs_ref_filt <- usgs_flows_ref %>% 
  filter(site_no %in% low_colw_ref$site_no)

# LOG PLOT
# (logplot_facet <- ggplot() + geom_line(data=usgs_ref_filt, aes(x=Date, y=log(Flow), group=site_no), 
#                      color="steelblue", lwd=0.3) + 
#   facet_wrap(~site_no, scales = "free_y"))

# PLOTLY POR PLOT
plotly::ggplotly(ggplot() + geom_line(data=usgs_ref_filt, aes(x=Date, y=Flow, group=site_no, color=site_no), lwd=0.3, 
                     show.legend = FALSE))


View(low_colw_ref)

# NOTES ON REF GAGES < 0.5 -----------------------------------------------------------

## FIX
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
# 10340500, Prosser Crk, dam went in 1959-1962, truncate 1962-10-01, through current ALT
# 10308783, Leviathan Ck, largely flatlined at nearly zero
# 10291500, Buckeye Ck, truncate (start at) 1953-10-01 to 1979-10-01, starts again 1995

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
# 11031500, Agua Caliente goes to zero in many years
# 10281800, Little Rock Ck, truncates 1977-09-30, goes to zero at times
# 10259200, Deep Ck, goes to zero freq
# 10258500, Palm Cnyn, gaps starting 1941 to 1947, zero freq
# 10258000, TAHQUITZ Ck, zero freq
# 10257600, Mission Ck, zero freq
# 10257500, Falls Ck, zero freq, truncates at 1931, then 1994-10-01 to 2021

# DROP/SWITCH?
# 11418000, ENGLEBRIGHT, needs to be moved to ALT data
# 11406999, FEATHER RIVER AT OROVILLE needs to be moved to ALT data
# 11294500, long record, NF Stan at Avery, but ALT??

# Now Fix: Truncate ----------------------------------------------------------------

### CREATE SPREADSHEET WITH GAGE, ALT/REF, DATE TO TRUNCATE BY, and then plug into purrr::filter?

# pull out ALT sites to relabel
usgs_ref_to_alt <- usgs_ref_filt %>% 
  filter(site_no %in% c("11418000","11406999","11294500")) %>% 
  mutate(gagetype = "ALT")

# now work individually to make updates:

# 11526500, start 1957-02-13, end 1980-09-29
ref_11526500 <- usgs_ref_filt %>% filter(site_no == "11526500") %>% 
  filter(Date > ymd("1957-02-13") & Date < ymd("1980-09-30"))
ggplot(ref_11526500) + geom_line(aes(x=Date, y=Flow), color="steelblue")

# 11529000, truncate after 1967-09-29
ref_11529000 <- usgs_ref_filt %>% filter(site_no == "11529000") %>% 
  filter(Date < ymd("1967-09-30"))
  
ggplot(ref_11529000) + geom_line(aes(x=Date, y=Flow), color="steelblue")

# 11525500, truncate after 1960
ref_11525500 <- usgs_ref_filt %>% filter(site_no == "11525500") %>% 
  filter(Date < ymd("1960-10-01"))

ggplot(ref_11525500) + geom_line(aes(x=Date, y=Flow), color="steelblue")

# 11472900, truncate after 1975-10-01
ref_11472900 <- usgs_ref_filt %>% filter(site_no == "11472900") %>% 
  filter(Date < ymd("1975-10-01"))

ggplot(ref_11472900) + geom_line(aes(x=Date, y=Flow), color="steelblue")

# 11467500, truncate after 1971-10-01
ref_11467500 <- usgs_ref_filt %>% filter(site_no == "11467500") %>% 
  filter(Date < ymd("1971-10-01"))

ggplot(ref_11467500) + geom_line(aes(x=Date, y=Flow), color="steelblue")

# 11467200, start at 2003-10-01
ref_11467200 <- usgs_ref_filt %>% filter(site_no == "11467200") %>% 
  filter(Date < ymd("2003-10-01"))

ggplot(ref_11467200) + geom_line(aes(x=Date, y=Flow), color="steelblue")

# 11445500, truncate after 1960-10-01 (SFA at lotus) (REF)
usgs_ref_filt %>% filter(site_no == "11445500") %>% 
  ggplot(.) + geom_line(aes(x=Date, y=Flow), color="steelblue")

ref_11445500 <- usgs_ref_filt %>% filter(site_no == "11445500") %>% 
  filter(Date < ymd("1960-10-01"))

ggplot(ref_11445500) + geom_line(aes(x=Date, y=Flow), color="steelblue")

alt_11445500 <- usgs_ref_filt %>% filter(site_no == "11445500") %>% 
  filter(Date >= ymd("1960-10-01"))
ggplot(alt_11445500) + geom_line(aes(x=Date, y=Flow), color="steelblue")

# 11341400, truncate after 1968-10-01
usgs_ref_filt %>% filter(site_no == "11341400") %>% 
  ggplot(.) + geom_line(aes(x=Date, y=Flow), color="steelblue")

ref_11341400 <- usgs_ref_filt %>% filter(site_no == "11341400") %>% 
  filter(Date < ymd("1968-10-01"))
ggplot(ref_11341400) + geom_line(aes(x=Date, y=Flow), color="steelblue")

alt_11341400 <- usgs_ref_filt %>% filter(site_no == "11341400") %>% 
  filter(Date >= ymd("1968-10-01"))
ggplot(alt_11341400) + geom_line(aes(x=Date, y=Flow), color="steelblue")

# 11208000, truncate after 2002-10-01
usgs_ref_filt %>% filter(site_no == "11208000") %>% 
  ggplot(.) + geom_line(aes(x=Date, y=Flow), color="steelblue")

ref_11208000 <- usgs_ref_filt %>% filter(site_no == "11208000") %>% 
  filter(Date < ymd("2002-10-01"))
ggplot(ref_11208000) + geom_line(aes(x=Date, y=Flow), color="steelblue")

alt_11208000 <- usgs_ref_filt %>% filter(site_no == "11208000") %>% 
  filter(Date >= ymd("2002-10-01"))
ggplot(alt_11208000) + geom_line(aes(x=Date, y=Flow), color="steelblue")

# 11169800, gap from 1982-10-01 to 2004-10-01, then thru current
# usgs_ref_filt %>% filter(site_no == "11169800") %>% 
#   ggplot(.) + geom_line(aes(x=Date, y=Flow), color="steelblue")
# 
# ref_11169800 <- usgs_ref_filt %>% filter(site_no == "11169800") %>% 
#   filter(Date < ymd("1982-10-01"))
# ggplot(ref_11169800) + geom_line(aes(x=Date, y=Flow), color="steelblue")

# 11113000, Sespe Ck, start at 1927-10-01, missing 1985-10-01 to 1990-10-01
usgs_ref_filt %>% filter(site_no == "11113000") %>% 
  ggplot(.) + geom_line(aes(x=Date, y=Flow), color="steelblue")

ref_11113000 <- usgs_ref_filt %>% filter(site_no == "11113000") %>% 
  filter(Date >= ymd("1927-10-01"))
ggplot(ref_11113000) + geom_line(aes(x=Date, y=Flow), color="steelblue")

# 11095500, Big Tujunga C, dam went in 1931, truncate 1931-10-01, ALT/reg after, truncated spill

usgs_ref_filt %>% filter(site_no == "11095500") %>% 
  ggplot(.) + geom_line(aes(x=Date, y=Flow), color="steelblue")

ref_11095500 <- usgs_ref_filt %>% filter(site_no == "11095500") %>% 
  filter(Date < ymd("1931-10-01"))
ggplot(ref_11095500) + geom_line(aes(x=Date, y=Flow), color="steelblue")

alt_11095500 <- usgs_ref_filt %>% filter(site_no == "11095500") %>% 
  filter(Date >= ymd("1931-10-01"))
ggplot(alt_11095500) + geom_line(aes(x=Date, y=Flow), color="steelblue")

# 10340500, Prosser Crk, dam went in 1959-1962, truncate 1962-10-01, through current ALT
usgs_ref_filt %>% filter(site_no == "10340500") %>%
  ggplot(.) + geom_line(aes(x=Date, y=Flow), color="steelblue")

ref_10340500 <- usgs_ref_filt %>% filter(site_no == "10340500") %>%
  filter(Date < ymd("1962-10-01"))
ggplot(ref_10340500) + geom_line(aes(x=Date, y=Flow), color="steelblue")

alt_10340500 <- usgs_ref_filt %>% filter(site_no == "10340500") %>%
  filter(Date >= ymd("1962-10-01"))
ggplot(alt_10340500) + geom_line(aes(x=Date, y=Flow), color="steelblue")

# 10308783, Leviathan Ck, largely flatlined at nearly zero
usgs_ref_filt %>% filter(site_no == "10308783") %>%
  ggplot(.) + geom_line(aes(x=Date, y=Flow), color="steelblue")

# 10291500, Buckeye Ck, truncate (start at) 1953-10-01 to 1979-10-01, starts again 1995
usgs_ref_filt %>% filter(site_no == "10291500") %>%
  ggplot(.) + geom_line(aes(x=Date, y=Flow), color="steelblue")

ref_10291500 <- usgs_ref_filt %>% filter(site_no == "10291500") %>%
  filter(Date >= ymd("1953-10-01"))
ggplot(ref_10291500) + geom_line(aes(x=Date, y=Flow), color="steelblue")

# 11033000, WF San Luis Rey R, flatlined, 1913-1915, then 1956-10-01 to 1986
usgs_ref_filt %>% filter(site_no == "11033000") %>%
  ggplot(.) + geom_line(aes(x=Date, y=Flow), color="steelblue")

ref_11033000 <- usgs_ref_filt %>% filter(site_no == "11033000") %>%
  filter(Date >= ymd("1956-10-01"))
ggplot(ref_11033000) + geom_line(aes(x=Date, y=Flow), color="steelblue")

# 10257500, Falls Ck, zero freq, truncates at 1931, then 1994-10-01 to 2021
usgs_ref_filt %>% filter(site_no == "10257500") %>%
  ggplot(.) + geom_line(aes(x=Date, y=Flow), color="steelblue")
ref_10257500 <- usgs_ref_filt %>% filter(site_no == "10257500") %>%
  filter(Date >= ymd("1994-10-01"))
ggplot(ref_10257500) + geom_line(aes(x=Date, y=Flow), color="steelblue")


# Now Combine and Rejoin with Full Dataset --------------------------------

# combine these revised layers
# add different categorization based on presence of zero flow days
# rejoin with full datast, make sure ALT/REF labels updated
# rerun Colwells and replot

# COMBINE REF REVISED
ref_revised <- mget(ls(pattern="^ref_(\\d){8}")) %>% bind_rows()
length(ls(pattern="^ref_(\\d){8}"))
length(unique(ref_revised$site_no))
# write out:
write_rds(ref_revised, file = "output/ref_revised_gages.rds")
write_csv(ref_revised, file = "output/ref_revised_gages.csv.zip")

# view plot
ggplot() + 
  geom_line(data=ref_revised, aes(x=Date, y=Flow, group=site_no), color="steelblue", lwd=0.3) +
  facet_wrap(~site_no, scales = "free")

# COMBINE ALT revised
alt_revised <- mget(ls(pattern="^alt_(\\d){8}")) %>% bind_rows() %>% 
  bind_rows(., usgs_ref_to_alt)
length(ls(pattern="^alt_(\\d){8}"))
length(unique(alt_revised$site_no)) # should be 8
# write out
write_rds(alt_revised, file = "output/alt_revised_gages.rds")
write_csv(alt_revised, file = "output/alt_revised_gages.csv.zip")

# view plot
ggplot() + 
  geom_line(data=alt_revised, aes(x=Date, y=Flow, group=site_no), color="maroon", lwd=0.3) +
  facet_wrap(~site_no, scales = "free")

