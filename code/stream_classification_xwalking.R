

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

write_csv(strmclass_xwalk, file="output/eflows_streamclasses_xwalk_3class.csv")


# Join --------------------------------------------------------------------

# join with class names
tahoe_strmclass <- left_join(tahoe_strmclass, strmclass_xwalk)
ceff_strmclass <- left_join(ceff_strmclass, strmclass_xwalk)

# Mapdeck -----------------------------------------------------------------

mapviewOptions(platform = "mapdeck") # change default from leaflet
mapview(tahoe_strmclass, zcol="class3_name")
mapview(ceff_strmclass, zcol="class3_name")

# Anti Spatial Join -------------------------------------------------------

ceff_distinct <- ceff_strmclass %>% distinct( .keep_all = TRUE)
tahoe_distinct <- tahoe_strmclass %>% select(-COMID) 

# find things that don't match
tahoe_only <- tahoe_distinct[!lengths(st_intersects(tahoe_distinct, ceff_distinct)), ]

mapviewOptions(platform = "leaflet") # change to get attributes
mapview(tahoe_only, zcol="class3_name")

# save out:
write_rds(tahoe_only, file = "data/eflows_final_classification_9CLASS/tahoe_streamclass_only.rds")

# Get COMIDs --------------------------------------------------------------

# read in
tahoe_only <- read_rds("data/eflows_final_classification_9CLASS/tahoe_streamclass_only.rds")

# use nhdtools to get comids
library(nhdplusTools)

# make utm
tahoe_only_3310 <- tahoe_only %>%
  rownames_to_column(var="rowid") %>% 
  st_transform(3310)

# make a lat lon version
tahoe_only <- tahoe_only %>%
  rownames_to_column(var="rowid") %>% 
  st_transform(4269)

# get start point and midpoint of each line:
startpts <- lwgeom::st_startpoint(tahoe_only_3310) %>% st_sf()
midpoints <- st_point_on_surface(tahoe_only_3310) 
mapview(startpts, col.region="orange") + mapview(tahoe_only, zcol="class3_name") + mapview(midpoints, col.region="darkblue", cex=3)

# test
# midpoints %>%
#   st_transform(4269) %>% 
#   slice(1) %>% 
#   discover_nhdplus_id(.$geometry)

# Now get COMID for each 
tahoe_coms <- midpoints %>%
  st_transform(4269) %>% 
  group_split(rowid) %>%
  set_names(midpoints$rowid) %>%
  map(~discover_nhdplus_id(.x$geometry))

# check for NAs?
table(map(tahoe_coms, ~is.na(.x)) == FALSE)

# flatten
tahoe_coms_df <- flatten_dfc(tahoe_coms) %>% t() %>%
  as.data.frame() %>%
  rename("COMID"=V1)

# bind back to midpoints
midpoints_coms <- bind_cols(midpoints, tahoe_coms_df)

mapview(midpoints_coms, col.regions="orange")

# now join back to streamclass data
tahoe_only_final <- left_join(tahoe_only, st_drop_geometry(midpoints_coms[,c("COMID","rowid")]), by="rowid")

# mapview
mapview(tahoe_only_final, zcol="class3_name")


# write it out again
write_rds(tahoe_only_final, file = "data/eflows_final_classification_9CLASS/tahoe_streamclass_w_comids.rds")


# JOIN BACK TO ORIGINAL DATA AND WRITE OUT --------------------------------

mapview(ceff_strmclass, zcol="class3_name")

# combine data
tahoe_only_final <- tahoe_only_final %>% st_transform(3310)

# combine
strm_class_final <- bind_rows(ceff_strmclass, tahoe_only_final)

# switch mapview
mapviewOptions(platform = "mapdeck") # change to get attributes
mapview(strm_class_final, zcol="COMID")

summary(strm_class_final)

# drop rowid
strm_class_final <- strm_class_final %>% select(-rowid)

# write it out!
save(strm_class_final, file="data/eflows_final_classification_9CLASS/ca_stream_class3-9_final.rda")

write_rds(strm_class_final, file = "data/eflows_final_classification_9CLASS/ca_streamclass3-9_final.rds")

st_write(strm_class_final, "data/eflows_final_classification_9CLASS/ca_streamclass3-9_final.shp")
