# COMBINE WAVELETS


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(fs)
library(purrr)
library(glue)
library(viridis)
library(sf)

# Get Files ---------------------------------------------------------------

in_dir <- "output/wavelets/ref"
in_csvs <- dir_ls(in_dir, regexp = "\\.csv$")

# Import CSVs -------------------------------------------------------------

df <- map_dfr(in_csvs, ~read_csv(.x, id = "filepath"))

# Get Site_no Column from filepath ----------------------------------------

df_wav <- df %>% 
  mutate(filepath = fs::path_ext_remove(path_file(filepath))) %>% 
  separate(col = filepath, into = c(NA, NA, "site_id"), sep = "_", extra = "drop") %>% 
  mutate(gagetype = "REF")

rm(df)

# pull max value for period at 12 months
df_wav <- df_wav %>% 
  filter(Period > 11 & Period < 13) %>% 
  group_by(site_id) %>% 
  slice_max(Power.avg)

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

df_final <- left_join(df_csci_final, df_wav, by="site_id") #%>% 
  #distinct(site_id, StationCode, SampleID, .keep_all = TRUE)

# Make Plot of Seasonality vs. Predictabilty by StreamClass for Ref -------

df_final %>% filter(gagetype.y=="REF") %>% 
  ggplot() + geom_point(aes(x=MP_metric, y=Power.avg, fill=csci), pch=21, size=4) +
  scale_fill_viridis_c("CSCI") +
  theme_classic(base_family = "Roboto Condensed") +
  facet_grid(.~CLASS_NAME)
  
