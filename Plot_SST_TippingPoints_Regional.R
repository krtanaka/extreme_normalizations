library(colorRamps)
library(ggplot2)
library(ggpubr)

rm(list = ls())

p = c(0.975, 0.95, 0.9)[2]

load(paste0("/Users/ktanaka/extreme_normalizations/results/HadI/SST_TippingPoints_", p, "_Global.RData")); hadi_global = yy_anom; hadi_global$region = "Global"
load(paste0("/Users/ktanaka/extreme_normalizations/results/HadI/SST_TippingPoints_", p, "_Arctic Ocean.RData")); hadi_arctic = yy_anom; hadi_arctic$region = "Arctic"
load(paste0("/Users/ktanaka/extreme_normalizations/results/HadI/SST_TippingPoints_", p, "_Indian Ocean.RData")); hadi_indian = yy_anom; hadi_indian$region = "Indian"
load(paste0("/Users/ktanaka/extreme_normalizations/results/HadI/SST_TippingPoints_", p, "_North Atlantic Ocean.RData")); hadi_north_atlantic = yy_anom; hadi_north_atlantic$region = "N.Atlantic"
load(paste0("/Users/ktanaka/extreme_normalizations/results/HadI/SST_TippingPoints_", p, "_North Pacific Ocean.RData")); hadi_north_pacific = yy_anom; hadi_north_pacific$region = "N.Pacific"
load(paste0("/Users/ktanaka/extreme_normalizations/results/HadI/SST_TippingPoints_", p, "_South Atlantic Ocean.RData")); hadi_south_atlantic = yy_anom; hadi_south_atlantic$region = "S.Atlantic"
load(paste0("/Users/ktanaka/extreme_normalizations/results/HadI/SST_TippingPoints_", p, "_South Pacific Ocean.RData")); hadi_south_pacific = yy_anom; hadi_south_pacific$region = "S.Pacific"
load(paste0("/Users/ktanaka/extreme_normalizations/results/HadI/SST_TippingPoints_", p, "_Southern Ocean.RData")); hadi_southern_ocean = yy_anom; hadi_southern_ocean$region = "Southern"

load(paste0("/Users/ktanaka/extreme_normalizations/results/COBE/SST_TippingPoints_", p, "_Global.RData")); cobe_global = yy_anom; cobe_global$region = "Global"
load(paste0("/Users/ktanaka/extreme_normalizations/results/COBE/SST_TippingPoints_", p, "_Arctic Ocean.RData")); cobe_arctic = yy_anom; cobe_arctic$region = "Arctic"
load(paste0("/Users/ktanaka/extreme_normalizations/results/COBE/SST_TippingPoints_", p, "_Indian Ocean.RData")); cobe_indian = yy_anom; cobe_indian$region = "Indian"
load(paste0("/Users/ktanaka/extreme_normalizations/results/COBE/SST_TippingPoints_", p, "_North Atlantic Ocean.RData")); cobe_north_atlantic = yy_anom; cobe_north_atlantic$region = "N.Atlantic"
load(paste0("/Users/ktanaka/extreme_normalizations/results/COBE/SST_TippingPoints_", p, "_North Pacific Ocean.RData")); cobe_north_pacific = yy_anom; cobe_north_pacific$region = "N.Pacific"
load(paste0("/Users/ktanaka/extreme_normalizations/results/COBE/SST_TippingPoints_", p, "_South Atlantic Ocean.RData")); cobe_south_atlantic = yy_anom; cobe_south_atlantic$region = "S.Atlantic"
load(paste0("/Users/ktanaka/extreme_normalizations/results/COBE/SST_TippingPoints_", p, "_South Pacific Ocean.RData")); cobe_south_pacific = yy_anom; cobe_south_pacific$region = "S.Pacific"
load(paste0("/Users/ktanaka/extreme_normalizations/results/COBE/SST_TippingPoints_", p, "_Southern Ocean.RData")); cobe_southern_ocean = yy_anom; cobe_southern_ocean$region = "Southern"

hadi = rbind(hadi_global, 
           hadi_arctic, 
           hadi_indian, 
           hadi_north_atlantic, 
           hadi_north_pacific,
           hadi_south_atlantic, 
           hadi_south_pacific, 
           hadi_southern_ocean)

cobe = rbind(cobe_global, 
           cobe_arctic, 
           cobe_indian, 
           cobe_north_atlantic, 
           cobe_north_pacific,
           cobe_south_atlantic, 
           cobe_south_pacific, 
           cobe_southern_ocean)

hadi$data = "HadISST"
cobe$data = "COBESST"

df = rbind(hadi, cobe)

df = tidyr::separate(df, time, c("Year", "Month"), sep = "-")
df$Month <- sprintf("%02d", as.numeric(df$Month))
df$Day = 01
df$Day <- sprintf("%02d", as.numeric(df$Day))
df$Time = paste(df$Year, df$Month, df$Day, sep = "-")
df$Time = as.Date(df$Time)

df %>% 
  # group_by(Time, region, data) %>% summarise(year_sum = mean(year_sum)) %>%
  ggplot(aes(x = Time, y = year_sum, color = year_sum, group = region)) +
  # geom_point(alpha = 0.8) +
  geom_line(size = 0.5, alpha = 0.75) +
  # geom_smooth(method = "loess", span = 0.1) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") +
  labs(x = "", y = "Area Fraction") +
  scale_color_viridis_c("") + 
  cowplot::theme_cowplot() +
  facet_grid(region~data) +
  scale_x_date(breaks = seq(as.Date("1900-01-01"), as.Date("2019-12-01"), by = "40 years"), 
               labels = scales::date_format("%Y")) 

