

# Libraries ---------------------------------------------------------------

options(tidyverse.quiet = TRUE)
options(scipen = 100) # to print full string instead of sci notation
options(dplyr.print_max = 1e4)

library(fs)
library(glue)
library(sf)
library(tidyverse) # load quietly
library(conflicted) # deals with conflicting functions
conflict_prefer("filter", "dplyr")
library(mapview)
mapviewOptions(fgb=FALSE) # so can save plot with points
mapviewOptions(platform = "mapdeck") # change default from leaflet

# Import Data -------------------------------------------------------------

# get stream class 9 class (this has COMID but not TAHOE/E Sierra) 
ceff_strmclass <- st_read("data/eflows_final_classification_9CLASS/Final_Classification_9CLASS.shp") %>% 
  st_transform(3310) %>% 
  st_zm()

# streamclass for missing tahoe sites (but missing COMID)
tahoe_strmclass <- st_read("data/eflows_final_classification_9CLASS/Final_Classification_9CLASS_tahoe.shp") %>% 
  st_transform(3310) %>% 
  st_zm()


# Make a Crosswalk --------------------------------------------------------

# crosswalk
strmclass_xwalk <- tibble(
  "CLASS"=c(1,2,3,4,5,6,7,8,9), 
  "CLASS_NAME"=c("snowmelt", # 3 class: 1=SNOWMELT
                 "high-volume snowmelt and rain", # 3 class: 2=MIXED
                 "low-volume snowmelt and rain", # 3 class: 2=MIXED,
                 "winter storms", # 3 class: 3=RAIN
                 "groundwater", # 3 class: 2=MIXED
                 "perennial groundwater and rain", # 3 class: 3=RAIN
                 "flashy, ephemeral rain", # 3 class: 3=RAIN
                 "rain and seasonal groundwater", # 3 class: 3=RAIN
                 "high elevation low precipitation"), # 3 class: 1=SNOWMELT
  "class3_name" = c("SNOWMELT",
                    "MIXED","MIXED","RAIN","MIXED",
                    "RAIN","RAIN","RAIN",
                    "SNOWMELT"),
  "class3_id" = c(1,
                  2,2,3,2,
                  3,3,3,
                  1))

# Join --------------------------------------------------------------------

# join with class names
tahoe_strmclass <- left_join(tahoe_strmclass, strmclass_xwalk)
ceff_strmclass <- left_join(ceff_strmclass, strmclass_xwalk)


# Mapdeck -----------------------------------------------------------------

mapview(tahoe_strmclass, zcol="class3_name")
mapview(ceff_strmclass, zcol="class3_name")


# Anti Spatial Join -------------------------------------------------------

ceff_distinct <- ceff_strmclass %>% distinct( .keep_all = TRUE)
tahoe_distinct <- tahoe_strmclass %>% select(-COMID) #%>% distinct( .keep_all = TRUE) %>% 

#tst <- st_join(tahoe_distinct, ceff_distinct[,c("COMID","geometry")], join=st_touches)

# find things that don't match
tst2 <- tahoe_distinct[!lengths(st_intersects(tahoe_distinct, ceff_distinct)), ]

mapviewOptions(platform = "leaflet") # change default from leaflet
mapview(tst2, zcol="class3_name")

# save out:
tahoe_strmclass_only <- tst2

write_rds(tahoe_strmclass_only, file = "data/eflows_final_classification_9CLASS/tahoe_streamclass_only.rds")

# Spatial Join ------------------------------------------------------------

# df_final <- st_join(csci_por_colwell, ceff_strmclass, join= st_is_within_distance, dist=10) %>% 


