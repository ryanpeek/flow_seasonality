# join Colwell with ASCI/CSCI

library(hydrostats)
library(tidyverse)
library(purrr)
library(lubridate)
library(glue)
library(tidylog)
library(ggdark)
library(sf)
library(mapview)
mapviewOptions(fgb=FALSE)

# Import Colwell Data -----------------------------------------------------

df_colwell_all_meta <- read_rds("output/04_usgs_gages_colwells_metric.rds")


# 01A: IMPORT CSCI -------------------------------------------------------

# bring in data:
csci_trim <- read_rds("data/02c_selected_final_bmi_csci_dat_trim.rds")
csci_trim %>% st_drop_geometry() %>% 
  #distinct(StationCode, site_id, .keep_all = TRUE) %>% nrow() # 431
  #distinct(StationCode, .keep_all = TRUE) %>% nrow() # 246 CSCI sites
  distinct(site_id, .keep_all = TRUE) %>% nrow() # 209 CSCI sites
table(csci_trim$CEFF_type, useNA = "ifany") # n=527 alt, n=193 REF

# older dataset w alt and ref gages
csci_por <- read_rds("data/04_selected_csci_ffm_por_trim.rds") %>% 
  select(StationCode:HUC_12, site_id, comid_gage:comid_ffc) %>% 
  distinct(StationCode, SampleID, site_id, .keep_all=TRUE)
table(csci_por$CEFF_type, useNA = "ifany") # n=521 alt, n=193 REF


# 07B: IMPORT ASCI ------------------------------------------------------------

asci_trim <- read_rds("https://github.com/ksirving/asci_ffm_2019/blob/master/output_data/02c_selected_final_algae_asci_dat_trim.rds?raw=true") %>% st_drop_geometry() %>% 
select(StationCode:HUC_12, CEFF_type, 
       site_id, comid_gage=comid, Latitude, Longitude, 
       SampleID:YYYY) %>%  
  distinct(StationCode, SampleID, site_id, .keep_all=TRUE)
table(asci_trim$CEFF_type, useNA = "ifany") # n=303 alt, 102 REF

# save out
asci_trim <- write_rds(asci_trim, file = "output/asci_trim.rds")

# 08: JOIN WITH COLWELL AND ASCI/CSCI -----------------------------------------------

# join w seasonality:
csci_trim_colwell <- left_join(csci_trim, df_colwell_all_meta, by=c("site_id"="site_no")) %>%
  distinct(StationCode, SampleID, site_id, .keep_all=TRUE) %>% 
  filter(!is.na(gagetype))

table(csci_trim_colwell$gagetype, useNA = "ifany") # alt 430, ref 193

csci_por_colwell <- left_join(csci_por, df_colwell_all_meta, by=c("site_id"="site_no")) %>%
  distinct(StationCode, SampleID, site_id, .keep_all=TRUE) %>% 
  filter(!is.na(gagetype))

table(csci_por_colwell$gagetype, useNA = "ifany") # alt 430, ref 193

# ASCI
asci_colwell <- left_join(asci_trim, df_colwell_all_meta, by=c("site_id"="site_no")) %>%
  distinct(StationCode, SampleID, site_id, .keep_all=TRUE) %>% 
  # drop crap gages (canals etc)
  filter(!is.na(gagetype))

table(asci_colwell$gagetype, useNA = "ifany") # n=264 alt, 102 ref

# 09: GAM PLOT ---------------------------------------------------------------

# CSCI vs GAGETYPE
(g1 <- csci_por_colwell %>% 
   ggplot() + 
   geom_point(aes(y=MP_metric, x=csci, fill=gagetype), pch=21, size=2.7, alpha=0.9, show.legend = FALSE) +
   stat_smooth(aes(y=MP_metric, x=csci, color=gagetype), 
               method = "gam", show.legend=FALSE) +
   theme_classic(base_family = "Roboto Condensed") +
   scale_color_viridis_d(option = "B", "Gage Type") +
   scale_fill_viridis_d(option = "A", "Gage Type") +
   labs(y="Intra-annual Seasonality \n(Colwell's M/P)", x="CSCI",
        caption = "Standardized seasonality in relation to overall predictability \nby dividing seasonality (M) by overall predictability \n(the sum of (M) and constancy (C)), as per Tonkin et al. (2017)"))

ggsave(filename = "figures/colwells_ref_alt_vs_csci_gam_trend.png", width = 10, height = 8, dpi = 300, units = "in")

# ASCI vs GAGETYPE
(g2 <- asci_colwell %>% 
    ggplot() + 
    geom_point(aes(y=MP_metric, x=H_ASCI, fill=gagetype), pch=21, size=2.7, alpha=0.9) +
    stat_smooth(aes(y=MP_metric, x=H_ASCI, color=gagetype), 
                method = "gam") +
    theme_classic(base_family = "Roboto Condensed") +
    scale_color_viridis_d(option = "B", "Gage Type") +
    scale_fill_viridis_d(option = "A", "Gage Type") +
    labs(y="Intra-annual Seasonality \n(Colwell's M/P)", x="ASCI",
         caption = "Standardized seasonality in relation to overall predictability \nby dividing seasonality (M) by overall predictability \n(the sum of (M) and constancy (C)), as per Tonkin et al. (2017)"))

ggsave(filename = "figures/colwells_ref_alt_vs_asci_gam_trend.png", width = 10, height = 8, dpi = 300, units = "in")

# combined
library(patchwork)
g1 + g2

ggsave(filename = "figures/colwells_ref_alt_vs_asci_&_csci_gam_trend.png", width = 10, height = 8, dpi = 300, units = "in")


# 10: ADD STREAM CLASSES -----------------------------------------------

# get stream classes: 
load("data/eflows_final_classification_9CLASS/ca_stream_class3-9_final.rda") # strm_class_final

# CSCI
csci_final <- left_join(csci_por_colwell, strm_class_final %>% st_drop_geometry(),
                        by=c("comid_gage"="COMID")) %>%
  select(-c(metric:comid_ffc, MM, DD, YYYY, COMID_nhd, COMID)) %>%
  rename(COMID_bio=COMID_bmi) %>% 
  distinct(.keep_all=TRUE)

table(csci_final$class3_name, useNA = "ifany")

# fix missing (NA) values (check with mapview)
csci_final <- csci_final %>% 
  mutate(class3_name = case_when(
    is.na(class3_name) ~ "SNOWMELT",
    TRUE ~ class3_name
  ))

# ASCI
asci_final <- left_join(asci_colwell, 
                        strm_class_final %>% st_drop_geometry(),
                        by=c("comid_gage"="COMID")) %>%
  select(-YYYY, sampledate=SampleDate, longitude=Longitude,
         latitude=Latitude, COMID_bio=COMID_algae) %>% 
  mutate(COMID_bio = as.numeric(COMID_bio)) %>% 
  distinct(.keep_all=TRUE)

table(asci_final$class3_name, useNA = "ifany")

# NAs?
#asci_final %>% st_as_sf(coords=c("usgs_lon", "usgs_lat"), crs=4269, remove=F) %>% mapview(zcol="class3_name", burst=TRUE) #+ 
#mapview(strm_class_final, zcol="class3_name")

# fix based on mapview
asci_final <- asci_final %>% 
  mutate(class3_name = case_when(
    site_id %in% c("11048553") ~ "SNOWMELT",
    is.na(class3_name) ~ "RAIN",
    TRUE ~ class3_name
  ))

# double check!
table(asci_final$gagetype, useNA = "always")
table(asci_final$class3_name, useNA = "always")

# compare names and clean:
library(janitor)
compare_df_cols(asci_final, csci_final)


# 11: PLOT GAGETYPE BY STREAMCLASS ------------------------------------------

# CSCI vs GAGETYPE
csci_final %>% 
  ggplot() + 
  geom_point(aes(y=MP_metric, x=csci, fill=gagetype), pch=21, size=2.7, alpha=0.9) +
  stat_smooth(aes(y=MP_metric, x=csci, color=gagetype),
              method = "gam", se = F) +
  theme_classic(base_family = "Roboto Condensed") +
  ggthemes::scale_color_few(palette = "Medium", "Gage Type") +
  ggthemes::scale_fill_few(palette = "Medium", "Gage Type") +
  #scale_fill_viridis_d(option = "A", "StreamClass") +
  labs(y="Seasonality (Colwell's M/P)", x="CSCI",
       caption = "Standardized seasonality in relation to overall predictability \nby dividing seasonality (M) by overall predictability \n(the sum of (M) and constancy (C)), as per Tonkin et al. (2017)") +
  facet_wrap(.~class3_name)

ggsave(filename = "figures/seasonality_colwell_by_streamclass3_alt_glm.png",
       width=11, height = 8, dpi=300)
ggsave(filename = "figures/seasonality_colwell_by_streamclass3_alt_gam.png",
       width=11, height = 8, dpi=300)


# ASCI vs GAGETYPE
asci_final %>% 
  ggplot() + 
  geom_point(aes(y=MP_metric, x=H_ASCI, fill=gagetype), pch=21, size=2.7, alpha=0.9) +
  stat_smooth(aes(y=MP_metric, x=H_ASCI, color=gagetype),
              method = "gam", se = F) +
  theme_classic(base_family = "Roboto Condensed") +
  ggthemes::scale_color_few(palette = "Medium", "Gage Type") +
  ggthemes::scale_fill_few(palette = "Medium", "Gage Type") +
  #scale_fill_viridis_d(option = "A", "StreamClass") +
  labs(y="Seasonality (Colwell's M/P)", x="ASCI",
       caption = "Standardized seasonality in relation to overall predictability \nby dividing seasonality (M) by overall predictability \n(the sum of (M) and constancy (C)), as per Tonkin et al. (2017)") +
  facet_wrap(.~class3_name)

ggsave(filename = "figures/seasonality_colwell_by_streamclass3_alt_asci_glm.png",
       width=11, height = 8, dpi=300)
ggsave(filename = "figures/seasonality_colwell_by_streamclass3_alt_asci_gam.png",
       width=11, height = 8, dpi=300)

# 12: Save Out ----------------------------------------------------------------

write_rds(csci_final, file = "output/04b_gages_csci_colwells_w_streamclass_metric.rds")

write_rds(asci_final, file = "output/04b_gages_asci_colwells_w_streamclass_metric.rds")


