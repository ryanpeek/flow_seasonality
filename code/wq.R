library(tidyverse)
library(lubridate)

df <- read_csv("~/Downloads/river_water_quality.csv")

df$yday <- yday(df$Date_YMD)

df %>% filter(SampleID %in% c("IowaHill", "Robbers")) %>% 
  ggplot() + 
  geom_point(aes(x=yday, y=EC_uS, fill=watertemp_C), 
                      pch=21, size=4) +
  geom_smooth(aes(x=yday, y=EC_uS), method = "glm") +
  scale_fill_viridis_c() +
  facet_grid(SampleID~.)
