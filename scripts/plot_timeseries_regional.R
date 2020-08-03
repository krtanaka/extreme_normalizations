library(colorRamps)
library(ggplot2)
library(ggpubr)
library(dplyr)

rm(list = ls())

p = c(0.975, 0.95, 0.9)[2]

load(paste0("/Users/ktanaka/extreme_normalizations/results/HadI/timeseries_", p, "_global.RData")); hadi_global = yy_anom; hadi_global$region = "Global"
load(paste0("/Users/ktanaka/extreme_normalizations/results/HadI/timeseries_", p, "_global_no_polar.RData")); hadi_sub_global = yy_anom; hadi_sub_global$region = "Sub_Global"
load(paste0("/Users/ktanaka/extreme_normalizations/results/HadI/timeseries_", p, "_arctic.RData")); hadi_arctic = yy_anom; hadi_arctic$region = "Arctic"
load(paste0("/Users/ktanaka/extreme_normalizations/results/HadI/timeseries_", p, "_indian.RData")); hadi_indian = yy_anom; hadi_indian$region = "Indian"
load(paste0("/Users/ktanaka/extreme_normalizations/results/HadI/timeseries_", p, "_north_atlantic.RData")); hadi_north_atlantic = yy_anom; hadi_north_atlantic$region = "N.Atlantic"
load(paste0("/Users/ktanaka/extreme_normalizations/results/HadI/timeseries_", p, "_north_pacific.RData")); hadi_north_pacific = yy_anom; hadi_north_pacific$region = "N.Pacific"
load(paste0("/Users/ktanaka/extreme_normalizations/results/HadI/timeseries_", p, "_south_atlantic.RData")); hadi_south_atlantic = yy_anom; hadi_south_atlantic$region = "S.Atlantic"
load(paste0("/Users/ktanaka/extreme_normalizations/results/HadI/timeseries_", p, "_south_pacific.RData")); hadi_south_pacific = yy_anom; hadi_south_pacific$region = "S.Pacific"
load(paste0("/Users/ktanaka/extreme_normalizations/results/HadI/timeseries_", p, "_southern.RData")); hadi_southern_ocean = yy_anom; hadi_southern_ocean$region = "Southern"

load(paste0("/Users/ktanaka/extreme_normalizations/results/COBE/timeseries_", p, "_global.RData")); cobe_global = yy_anom; cobe_global$region = "Global"
load(paste0("/Users/ktanaka/extreme_normalizations/results/COBE/timeseries_", p, "_global_no_polar.RData")); cobe_sub_global = yy_anom; cobe_sub_global$region = "Sub_Global"
load(paste0("/Users/ktanaka/extreme_normalizations/results/COBE/timeseries_", p, "_arctic.RData")); cobe_arctic = yy_anom; cobe_arctic$region = "Arctic"
load(paste0("/Users/ktanaka/extreme_normalizations/results/COBE/timeseries_", p, "_indian.RData")); cobe_indian = yy_anom; cobe_indian$region = "Indian"
load(paste0("/Users/ktanaka/extreme_normalizations/results/COBE/timeseries_", p, "_north_atlantic.RData")); cobe_north_atlantic = yy_anom; cobe_north_atlantic$region = "N.Atlantic"
load(paste0("/Users/ktanaka/extreme_normalizations/results/COBE/timeseries_", p, "_north_pacific.RData")); cobe_north_pacific = yy_anom; cobe_north_pacific$region = "N.Pacific"
load(paste0("/Users/ktanaka/extreme_normalizations/results/COBE/timeseries_", p, "_south_atlantic.RData")); cobe_south_atlantic = yy_anom; cobe_south_atlantic$region = "S.Atlantic"
load(paste0("/Users/ktanaka/extreme_normalizations/results/COBE/timeseries_", p, "_south_pacific.RData")); cobe_south_pacific = yy_anom; cobe_south_pacific$region = "S.Pacific"
load(paste0("/Users/ktanaka/extreme_normalizations/results/COBE/timeseries_", p, "_southern.RData")); cobe_southern_ocean = yy_anom; cobe_southern_ocean$region = "Southern"

hadi = rbind(hadi_global, 
             hadi_sub_global,
             hadi_arctic, 
             hadi_indian, 
             hadi_north_atlantic, 
             hadi_north_pacific,
             hadi_south_atlantic, 
             hadi_south_pacific, 
             hadi_southern_ocean)

cobe = rbind(cobe_global, 
             cobe_sub_global,
             cobe_arctic, 
             cobe_indian, 
             cobe_north_atlantic, 
             cobe_north_pacific,
             cobe_south_atlantic, 
             cobe_south_pacific, 
             cobe_southern_ocean)

rm(hadi_global, 
   hadi_sub_global,
   hadi_arctic, 
   hadi_indian, 
   hadi_north_atlantic, 
   hadi_north_pacific,
   hadi_south_atlantic, 
   hadi_south_pacific, 
   hadi_southern_ocean, 
   cobe_global, 
   cobe_sub_global,
   cobe_arctic, 
   cobe_indian, 
   cobe_north_atlantic, 
   cobe_north_pacific,
   cobe_south_atlantic, 
   cobe_south_pacific, 
   cobe_southern_ocean,
   yy_anom)

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

df1$region = factor(df1$region, levels = c("Global", 
                                           "Sub_Global",  
                                           "Arctic",
                                           "N.Atlantic",
                                           "S.Atlantic",
                                           "N.Pacific",
                                           "S.Pacific",
                                           "Indian",
                                           "Southern"))

pdf("~/Desktop/s6.pdf", height = 6, width = 10)

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

dev.off()

pdf("~/Desktop/s6.pdf", height = 5, width = 5)

df1 %>% 
  # subset(region %in% c("Global", "Sub_Global")) %>% 
  group_by(Year, region, data) %>% 
  summarise(year_sum = mean(year_sum)) %>% 
  ggplot(aes(x = Year, y = year_sum, color = data)) +
  geom_point(alpha = 0.8, size = 2) +
  geom_line(alpha = 0.8) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") +
  labs(x = "", y = "Area Fraction") +
  cowplot::theme_cowplot(I(20)) +
  facet_wrap(~region) + 
  scale_color_brewer(palette = "Set1", "") + 
  scale_x_continuous(breaks = seq(1900, 2020, 60), limits = c(1900, 2020)) + 
  theme(legend.position = "right")

dev.off()

# scale_x_date(breaks = seq(as.Date("1900-01-01"), as.Date("2019-12-01"), by = "40 years"), 
#              labels = scales::date_format("%Y")) 

df2 = df %>% group_by(Year, region) %>% summarise(year_sum = mean(year_sum))
df2$Year = as.numeric(df2$Year)

ElNino = subset(df, Year %in% c(1905, 1906, 
                                1911, 1912, 1914, 1915, 
                                1940, 1941, 1942, 
                                1965, 1966, 
                                1972, 1973,
                                1982, 1983, 1987, 1988, 
                                1991, 1992, 1997, 1998, 
                                2015, 2016))

df2$linesize = ifelse(df2$region == "Global", 2, 1)

df2$region[df2$region == "Global"] <- "Global (2009)"
df2$region[df2$region == "Arctic"] <- "Arctic (2016)"
df2$region[df2$region == "N.Atlantic"] <- "N.Atlantic (2003)"
df2$region[df2$region == "N.Pacific"] <- "N.Pacific (2014)"
df2$region[df2$region == "Southern"] <- "Southern (n/a)"
df2$region[df2$region == "S.Atlantic"] <- "S.Atlantic (1993)"
df2$region[df2$region == "S.Pacific"] <- "S.Pacific (n/a)"
df2$region[df2$region == "Indian"] <- "Indian (1995)"

nh = df2 %>% subset(region %in% c("Global (2009)", "Arctic (2016)","N.Atlantic (2003)","N.Pacific (2014)"))
sh = df2 %>% subset(region %in% c("Global (2009)", "Southern (n/a)","S.Atlantic (1993)","S.Pacific (n/a)", "Indian (1995)"))

col1 <- c("gray40", 
          rgb(67, 147, 195, maxColorValue = 255, alpha = 255),
          rgb(33, 102, 172, maxColorValue = 255, alpha = 255),
          rgb(5, 48, 97, maxColorValue = 255, alpha = 255))

col2 <- c("gray40", 
          rgb(103, 0, 31, maxColorValue = 255, alpha = 255),
          rgb(178, 24, 43, maxColorValue = 255, alpha = 255),
          rgb(214, 96, 77, maxColorValue = 255, alpha = 255),
          rgb(244, 165, 130, maxColorValue = 255, alpha = 255))

p1 = nh %>% 
  mutate(region = factor(region, levels = c("Global (2009)", "Arctic (2016)","N.Atlantic (2003)","N.Pacific (2014)"))) %>% 
  ggplot(aes(Year, year_sum, group = region, colour = region, size = linesize)) +
  geom_smooth(span = 0.05, se = F, alpha = 0.8)  +
  scale_size(range = c(1, 3), guide = "none") +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") + 
  scale_colour_manual(values = col1, "") +
  labs(x = "", y = "") +
  scale_x_continuous(breaks = seq(1900, 2020, 20), limits = c(1900, 2020)) + 
  scale_y_continuous(breaks = c(seq(0, 1, by = 0.2)), limits = c(0, 0.8)) + 
  ggthemes::theme_few(I(20)) +
  theme(legend.position = c(0.15, 0.8),
        axis.text.x = element_blank())

p2 = sh %>% 
  mutate(region = factor(region, levels = c("Global (2009)", "Southern (n/a)","S.Atlantic (1993)","S.Pacific (n/a)", "Indian (1995)"))) %>% 
  ggplot(aes(Year, year_sum, group = region, colour = region, size = linesize)) +
  geom_smooth(span = 0.05, se = F, alpha = 0.8)  +
  scale_size(range = c(1, 3), guide = "none") +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") + 
  scale_colour_manual(values = col2, "") +
  labs(x = "", y = "proportion of area extent") +
  scale_x_continuous(breaks = seq(1900, 2020, 20), limits = c(1900, 2020)) + 
  scale_y_continuous(breaks = c(seq(0, 1, by = 0.2))) + 
  ggthemes::theme_few(I(20)) +
  theme(legend.position = c(0.15, 0.8))

pdf(paste0('~/Dropbox (MBA)/PAPER Kisei heat extremes/figures/Fig3_global_timeseries_', Sys.Date(), '.pdf'), height = 12, width = 10)
cowplot::plot_grid(p1, p2, ncol = 1)
dev.off()

df2 %>% 
  ggplot(aes(Year, year_sum, colour = year_sum)) +
  scale_size(range = c(1, 3), guide = "none") +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") + 
  labs(x = "", y = "proportion of area extent") +
  scale_color_viridis_c() + 
  scale_x_continuous(breaks = seq(1900, 2020, 40), limits = c(1900, 2020)) + 
  scale_y_continuous(breaks = c(seq(0, 1, by = 0.2))) + 
  cowplot::theme_cowplot(I(20)) +
  facet_wrap(~region, scales = "free_y", ncol = 3) + 
  theme(legend.position = "none",
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))

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
  "Sub_Global",
  "N.Atlantic",
  "Indian",
  "S.Atlantic"))

df3  %>% 
  ggplot(aes(Year, y = region, fill = region)) +
  # scale_fill_manual(values = cbp, "") + 
  geom_tile(show.legend = F) +
  labs(x = "", y = "") +
  theme_minimal(I(15)) + 
  geom_hline(yintercept = seq(0.5, 8), color = 'white', size = 2) + 
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1))
  
  




