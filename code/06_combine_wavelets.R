# COMBINE WAVELETS

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(fs)
library(purrr)
library(glue)
library(viridis)
library(sf)

# Wav REF: Get CSVs -----------------------------------------------------

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

df_ref %>% distinct(site_id) %>% tally # n= 219 REF gages

# Wav ALT: Get CSVs ----------------------------------------------------------

in_dir <- "output/wavelets/alt"
in_csvs <- dir_ls(in_dir, regexp = "\\.csv$")

# import csvs
df <- map_dfr(in_csvs, ~read_csv(.x, id = "filepath"))

# get site col from path
df_alt <- df %>% 
  mutate(filepath = fs::path_ext_remove(path_file(filepath))) %>% 
  separate(col = filepath, into = c(NA, NA, "site_id"), sep = "_", extra = "drop") %>% 
  mutate(gagetype = "ALT")

df_alt %>% distinct(site_id) %>% tally # n= 748 ALT gages
rm(df)

# Wav: BIND TOGETHER -----------------------------------------------------------

df_wav <- bind_rows(df_alt, df_ref)
# rm old
rm(df_alt, df_ref)


## Period v Predictability Plots  ---------------------------------------------

# single plot of all
ggplot() + 
  geom_line(data=df_wav, aes(x=Period, y=Power.avg, group=site_id), 
            color=ifelse(df_wav$Power.avg.pval<0.05, "black", "orange"), 
            lwd=0.2, alpha=0.5, show.legend = FALSE) +
  theme_bw(base_family = "Roboto Condensed") + 
  labs(x="Period (months)", y="Avg Predictability Power") +
  scale_x_continuous(breaks=seq(0,120, 6), limits = c(0,120)) +
  #scale_y_continuous(breaks=seq(0,36,4)) +
  facet_grid(gagetype~.)

#ggsave(filename = "figures/wavelet_period_plots_all_gages.png", 
#       width = 11, height = 8, dpi=300, units = "in")


## Pull Max, 6mon and 12 mon -----------------------------------------------

# pull max value for period at 12 months
df_wav_12 <- df_wav %>% 
  group_by(site_id, gagetype) %>% 
  mutate(Period_rnd = round(Period, 0)) %>% 
  filter(Period_rnd == 12) %>% 
  slice_max(Power.avg, n = 1)
table(df_wav_12$gagetype) # n=967 total (748 ALT, 219 REF)
length(unique(df_wav_12$site_id))
quantile(df_wav_12$Power.avg)
quantile(df_wav_12$Period)

# pull max value for period at 6 months
df_wav_6 <- df_wav %>% 
  group_by(site_id, gagetype) %>% 
  mutate(Period_rnd = round(Period, 0)) %>% 
  filter(Period_rnd == 6) %>% 
  slice_max(Power.avg, n = 1)
table(df_wav_6$gagetype) # n=967 total (748 ALT, 219 REF)
quantile(df_wav_6$Power.avg)
quantile(df_wav_6$Period)

# pull max value regardless of period
df_wav_max <- df_wav %>% 
  group_by(site_id, gagetype) %>% 
  slice_max(Power.avg, n = 1)
quantile(df_wav_max$Power.avg)
quantile(df_wav_max$Period)


### Exploratory Plots ----------------------------------------------------------

# plot
ggplot() + 
  geom_jitter(data=df_wav_12, aes(x=gagetype, y=Power.avg, fill=gagetype))

ggplot() + 
  geom_boxplot(data=df_wav_max, aes(x=gagetype, y=Power.avg, fill=gagetype)) 

# by period
ggplot() + 
  geom_point(data=df_wav_max, aes(x=Period, y=Power.avg, fill=gagetype), pch=21) +
  facet_wrap(.~gagetype)

# do this by by period...
ggplot() + 
  geom_jitter(data=df_wav_12, aes(x=Period, y=Power.avg, fill=gagetype), pch=21) +
  scale_fill_viridis_d()


# Save Wavelets -----------------------------------------------------------

# save wavelet pieces
save(df_wav, df_wav_6, df_wav_12, df_wav_max, file="output/06_wavelet_combined_period_power_outputs.rda")

#load("output/wavelet_combined_period_power_outputs.rda")

