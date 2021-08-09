# COMBINE WAVELETS

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(fs)
library(purrr)
library(glue)
library(viridis)
library(sf)

# REF: Get CSVs ----------------------------------------------------------

in_dir <- "output/wavelets/ref"
in_csvs <- dir_ls(in_dir, regexp = "\\.csv$")

# import csvs
df <- map_dfr(in_csvs, ~read_csv(.x, id = "filepath"))

# get site col from path
df_ref <- df %>% 
  mutate(filepath = fs::path_ext_remove(path_file(filepath))) %>% 
  separate(col = filepath, into = c(NA, NA, "site_id"), sep = "_", extra = "drop") %>% 
  mutate(gagetype = "REF")

rm(df)

df_ref %>% distinct(site_id) %>% tally

# ALT: Get CSVs ----------------------------------------------------------

in_dir <- "output/wavelets/alt"
in_csvs <- dir_ls(in_dir, regexp = "\\.csv$")

# import csvs
df <- map_dfr(in_csvs, ~read_csv(.x, id = "filepath"))

# get site col from path
df_alt <- df %>% 
  mutate(filepath = fs::path_ext_remove(path_file(filepath))) %>% 
  separate(col = filepath, into = c(NA, NA, "site_id"), sep = "_", extra = "drop") %>% 
  mutate(gagetype = "ALT")

df_alt %>% distinct(site_id) %>% tally
rm(df)

# BIND TOGETHER -----------------------------------------------------------

df_wav <- bind_rows(df_alt, df_ref)
# rm old
rm(df_alt, df_ref)

# single plot of all by ref
ggplot() + 
  geom_line(data=df_wav, aes(x=Period, y=Power.avg, group=site_id), 
            color=ifelse(df_wav$Power.avg.pval<0.05, "black", "orange"), 
            lwd=0.2, alpha=0.5, show.legend = FALSE) +
  #geom_point(data=df_wav, aes(x=Period, y=Power.avg, group=site_id), 
             #fill=ifelse(df_wav$Power.avg.pval<0.05, "transparent", "steelblue"), pch=21, alpha=0.2) +
  #ggthemes::scale_color_colorblind() +
  theme_bw(base_family = "Roboto Condensed") + 
  labs(x="Period (months)", y="Avg Predictability Power") +
  scale_x_continuous(breaks=seq(0,120, 6), limits = c(0,120)) +
  #scale_y_continuous(breaks=seq(0,36,4)) +
  facet_grid(gagetype~.)

# pull max value for period at 12 months
df_wav_12 <- df_wav %>% 
  group_by(site_id, gagetype) %>% 
  mutate(Period_rnd = round(Period, 1)) %>% 
  filter(Period_rnd > 11.8 & Period_rnd < 12.2) %>% 
  slice_max(Power.avg, n = 1)
table(df_wav_12$gagetype)

df_wav_max <- df_wav %>% 
  group_by(site_id, gagetype) %>% 
  slice_max(Power.avg, n = 1)
table(df_wav_max$gagetype)

# plot
ggplot() + 
  geom_jitter(data=df_wav_12, aes(x=gagetype, y=Power.avg, fill=gagetype))

ggplot() + 
  geom_boxplot(data=df_wav_max, aes(x=gagetype, y=Power.avg, fill=gagetype))


# do this by stream class too...
ggplot() + 
  geom_jitter(data=df_wav_12, aes(x=Period, y=Power.avg, fill=gagetype), pch=21) +
  scale_fill_viridis_d()
ggplot() + 
  geom_jitter(data=df_wav_max, aes(x=Period, y=Power.avg, fill=gagetype), pch=21) +
  scale_fill_viridis_d() + facet_wrap(.~gagetype)


# Get StreamClasses -------------------------------------------------------

st_layers("data/eflows_final_classification_9CLASS/Final_Classification_9CLASS.shp")
ceff_strmclass <- st_read("data/eflows_final_classification_9CLASS/Final_Classification_9CLASS.shp")
st_crs(ceff_strmclass)

# crosswalk
strmclass_xwalk <- tibble("CLASS"=c(1,2,3,4,5,6,7,8,9), "CLASS_NAME"=c("snowmelt","high-volume snowmelt and rain", "low-volume snowmelt and rain", "winter storms","groundwater","perennial groundwater and rain","flashy, ephemeral rain","rain and seasonal groundwater","high elevation low precipitation"))

# join with class names
ceff_strmclass <- left_join(ceff_strmclass, strmclass_xwalk)

# Get Colwell's -----------------------------------------------------------

df_colwell <- read_rds("output/usgs_gages_colwells_metric.rds")

# Get CSCI ----------------------------------------------------------------

# older dataset w alt and ref gages
csci_por <- read_rds("data/04_selected_csci_ffm_por_trim.rds") %>% 
  select(StationCode:HUC_12, site_id, comid_gage:comid_ffc) %>% 
  distinct(StationCode, SampleID, site_id, .keep_all=TRUE)
table(csci_por$CEFF_type) # n=521 alt, n=192 REF

# JOIN COLWELL AND CSCI -----------------------------------------------

csci_por_colwell <- left_join(csci_por, df_colwell, by=c("site_id"="site_no")) %>%
  distinct(StationCode, SampleID, site_id, .keep_all=TRUE)
table(csci_por_colwell$gagetype)

# Join Colwell with StreamClass -------------------------------------------

# JOIN with csci_por_colwell by COMID
df_csci_final <- left_join(csci_por_colwell, ceff_strmclass, by=c("comid_gage"="COMID"))


# Join Colwell with Wavelet -----------------------------------------------

df_final <- left_join(df_csci_final, df_wav_max, by="site_id") #%>% 
  #distinct(site_id, StationCode, SampleID, .keep_all = TRUE)
table(df_final$gagetype.x)
table(df_final$gagetype.y)



# PLOT STREAMCLASSES ------------------------------------------------------

# make sf and plot
library(mapview)

df_final_sf <- df_final %>% 
  st_as_sf(coords=c("dec_long_va", "dec_lat_va"), crs=4269, remove=FALSE)
df_final_sff <- df_final_sf %>% filter(is.na(CLASS_NAME))

mapview(ceff_strmclass, zcol="CLASS_NAME") + mapview(df_final_sff, zcol="gagetype.y")

# Fix NAs
# CLASS 1 = c("10270900")
# CLASS 3 = c("11113000", "11055350", "11404100")
# CLASS 8 = c("11042900", "11055350")
# all remaining sites are on east side around Tahoe and Truckee

table(df_final$CLASS, useNA = "always")
#df_final %>% group_by(CLASS) %>% tally()

df_final <- df_final %>% 
  mutate(CLASS = case_when(
    grepl("10270900", site_id) ~ 1,
    grepl("11113000|11055350|11404100", site_id) ~ 3,
    grepl("11042900|11055350", site_id) ~ 8,
    TRUE ~ CLASS
  ))

table(df_final$CLASS, useNA = "always")

# get list of all other gages with NA
nagages <- df_final %>% filter(is.na(CLASS_NAME)) %>% distinct(site_id) %>% pull

# assign all to low-vol snowmelt and rain?
df_final <- df_final %>% 
  mutate(CLASS = case_when(
    site_id %in% nagages ~ 3,
    TRUE ~ CLASS
  ))

table(df_final$CLASS, useNA = "always")

# Make Plot of Seasonality vs. Predictability by StreamClass for Ref -------

df_final %>%  filter(!is.na(CLASS_NAME)) %>% 
  ggplot() + geom_point(aes(x=MP_metric, y=Power.avg, fill=gagetype.x, shape=gagetype.x, size=Period),
                        alpha=0.8) + 
  scale_shape_manual("GageType", values=c(21,22)) +
  scale_size_binned(breaks = c(3,6,9,12)) +
  ggthemes::scale_fill_colorblind("GageType") +
  theme_classic(base_family = "Roboto Condensed") +
  facet_wrap(.~CLASS_NAME)


df_final %>%  filter(!is.na(CLASS_NAME)) %>% 
  ggplot() + geom_point(aes(x=MP_metric, y=csci, fill=CLASS_NAME, shape=gagetype.x),
                        alpha=0.8, size=4) + 
  scale_shape_manual("GageType", values=c(21,22)) +
  geom_smooth(method = "lm",se = FALSE,
              aes(x=MP_metric, y=csci, group=CLASS_NAME), col="black", alpha=0.5) +
  #ggthemes::scale_fill_colorblind("GageType") +
  scale_fill_viridis_d("GageType") +
  theme_classic(base_family = "Roboto Condensed") +
  facet_wrap(gagetype.x~CLASS_NAME)


# Save Data Out -----------------------------------------------------------

write_rds(df_final, file = "output/wavelet_csci_colwell_final.rds")

# save wavelet pieces
save(df_wav, df_wav_12, df_wav_max, file="output/wavelet_combined_period_power_outputs.rda")

