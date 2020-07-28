library(colorRamps)
library(ggplot2)
library(ggpubr)
library(dplyr)

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

df1 = df %>% group_by(Year, data, region) %>% summarise(year_sum = mean(year_sum))
df1$Year = as.numeric(df1$Year)

df1 %>% 
  ggplot(aes(x = Year, y = year_sum, color = data)) +
  geom_point(alpha = 0.8) +
  geom_line(alpha = 0.8) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") +
  labs(x = "", y = "Area Fraction") +
  cowplot::theme_cowplot() +
  scale_color_brewer(palette = "Set1", "") + 
  facet_wrap(~region, ncol = 4, scales = "free_y") + 
  scale_x_continuous(breaks = seq(1900, 2020, 40), limits = c(1900, 2020)) + 
  theme(legend.position = "top",
        legend.justification = c(0,1))

# scale_x_date(breaks = seq(as.Date("1900-01-01"), as.Date("2019-12-01"), by = "40 years"), 
#              labels = scales::date_format("%Y")) 

df2 = df %>% group_by(Year, region) %>% summarise(year_sum = mean(year_sum))
df2$Year = as.numeric(df2$Year)

# The palette with black:
cbp <- c("#E69F00", "#000000", "#56B4E9", "#009E73",
         "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

ElNino = subset(df, Year %in% c(1905, 1906, 
                                1911, 1912, 1914, 1915, 
                                1940, 1941, 1942, 
                                1965, 1966, 
                                1972, 1973,
                                1982, 1983, 1987, 1988, 
                                1991, 1992, 1997, 1998, 
                                2015, 2016))

df2$linesize = ifelse(df2$region == "Global", 2, 1)

df2 %>% 
  ggplot(aes(Year, year_sum, group = region, colour = region, size = linesize)) +
  geom_smooth(span = 0.05, se = F, alpha = 0.8)  +
  scale_size(range = c(1, 3), guide = "none") +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") + 
  scale_colour_manual(values = cbp, "") + 
  labs(x = "", y = "Proportion of Area") +
  scale_x_continuous(breaks = seq(1900, 2020, 20), limits = c(1900, 2020)) + 
  scale_y_continuous(breaks = c(seq(0, 1, by = 0.2))) + 
  ggpubr::theme_pubr(I(20)) +
  theme(legend.position = c(0.16, 0.85),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))

df2 %>% 
  ggplot(aes(Year, year_sum, colour = year_sum)) +
  scale_size(range = c(1, 3), guide = "none") +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") + 
  labs(x = "", y = "Proportion of Area") +
  scale_color_viridis_c() + 
  scale_x_continuous(breaks = seq(1900, 2020, 40), limits = c(1900, 2020)) + 
  scale_y_continuous(breaks = c(seq(0, 1, by = 0.2))) + 
  cowplot::theme_cowplot(I(20)) +
  facet_wrap(~region, scales = "free_y", ncol = 4) + 
  theme(legend.position = "none",
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))
# geom_dl(aes(label = region), method = list(dl.trans(x = x - .2), "first.points")) 



df3 = df %>% 
  group_by(Year, region) %>% 
  summarise(year_sum = mean(year_sum)) %>% 
  mutate(y = ifelse(year_sum > 0.5, 1, 0)) %>% 
  select(Year, region, y) %>% 
  subset(y > 0) 

df3$region = factor(df3$region, levels = c(
  "Southern",
  "S.Pacific",
  "Arctic",
  "N.Pacific",
  "Global",
  "N.Atlantic",
  "Indian",
  "S.Atlantic"))

df3  %>% 
  ggplot(aes(Year, y = region, fill = region)) +
  scale_fill_manual(values = cbp, "") + 
  geom_tile(show.legend = F) +
  labs(x = "", y = "") +
  theme_minimal(I(15)) + 
  geom_hline(yintercept = seq(0.5, 8), color = 'white', size = 2) + 
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1))
  
  




