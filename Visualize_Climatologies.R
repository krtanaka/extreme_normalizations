library(raster)
library(colorRamps)
library(ggpubr)
library(rnaturalearth)
library(sf)
library(rgdal)
library(dplyr)
library(maps)
library(rworldmap)
library(ggplot2)
library(geosphere)
library(gpclib)
library(ggalt)

rm(list = ls())

# World map
worldMap <- getMap()
world.points <- fortify(worldMap)
world.points$region <- world.points$id

world.df <- world.points[,c("long","lat","group", "region")]

plot_clim = function(data, mode){
  
  # data = c("HadI", "COBE", "ER")[3]
  # mode = "mean"
  
  setwd("~/Dropbox (MBA)/PAPER Kisei heat extremes")
  
  load(paste0("data/", data, "_SST.RData"))
  
  # e = extent(-140, -100, 30, 40)
  # df = crop(df, e); rm(e)
  
  # set baseline Jan 1870 - Dec 1929, 60 years
  Baseline <- df[[1:720]] 
  names(Baseline)
  
  if (mode == "mean") {
    
    jan = calc(Baseline[[seq(1, 719, 12)]], mean)
    jan = as.data.frame(rasterToPoints(jan))
    jan$month = "jan"

    feb = calc(Baseline[[seq(2, 719, 12)]], mean)
    feb = as.data.frame(rasterToPoints(feb))
    feb$month = "feb"
    
    mar = calc(Baseline[[seq(3, 719, 12)]], mean)
    mar = as.data.frame(rasterToPoints(mar))
    mar$month = "mar"
    
    apr = calc(Baseline[[seq(4, 719, 12)]], mean)
    apr = as.data.frame(rasterToPoints(apr))
    apr$month = "apr"
    
    may = calc(Baseline[[seq(5, 719, 12)]], mean)
    may = as.data.frame(rasterToPoints(may))
    may$month = "may"
    
    jun = calc(Baseline[[seq(6, 719, 12)]], mean)
    jun = as.data.frame(rasterToPoints(jun))
    jun$month = "jun"
    
    jul = calc(Baseline[[seq(7, 719, 12)]], mean)
    jul = as.data.frame(rasterToPoints(jul))
    jul$month = "jul"
    
    aug = calc(Baseline[[seq(8, 719, 12)]], mean)
    aug = as.data.frame(rasterToPoints(aug))
    aug$month = "aug"
    
    sep = calc(Baseline[[seq(9, 719, 12)]], mean)
    sep = as.data.frame(rasterToPoints(sep))
    sep$month = "sep"
    
    oct = calc(Baseline[[seq(10, 719, 12)]], mean)
    oct = as.data.frame(rasterToPoints(oct))
    oct$month = "oct"
    
    nov = calc(Baseline[[seq(11, 719, 12)]], mean)
    nov = as.data.frame(rasterToPoints(nov))
    nov$month = "nov"
    
    dec = calc(Baseline[[seq(12, 719, 12)]], mean)
    dec = as.data.frame(rasterToPoints(dec))
    dec$month = "dec"
    
    stat = rbind(jan, feb, mar, apr, may, jun, jul, aug, sep, oct, nov, dec)
    
  }
  
  if (mode == "sd") {
    
    jan = calc(Baseline[[seq(1, 719, 12)]], sd)
    jan = as.data.frame(rasterToPoints(jan))
    jan$month = "jan"
    
    feb = calc(Baseline[[seq(2, 719, 12)]], sd)
    feb = as.data.frame(rasterToPoints(feb))
    feb$month = "feb"
    
    mar = calc(Baseline[[seq(3, 719, 12)]], sd)
    mar = as.data.frame(rasterToPoints(mar))
    mar$month = "mar"
    
    apr = calc(Baseline[[seq(4, 719, 12)]], sd)
    apr = as.data.frame(rasterToPoints(apr))
    apr$month = "apr"
    
    may = calc(Baseline[[seq(5, 719, 12)]], sd)
    may = as.data.frame(rasterToPoints(may))
    may$month = "may"
    
    jun = calc(Baseline[[seq(6, 719, 12)]], sd)
    jun = as.data.frame(rasterToPoints(jun))
    jun$month = "jun"
    
    jul = calc(Baseline[[seq(7, 719, 12)]], sd)
    jul = as.data.frame(rasterToPoints(jul))
    jul$month = "jul"
    
    aug = calc(Baseline[[seq(8, 719, 12)]], sd)
    aug = as.data.frame(rasterToPoints(aug))
    aug$month = "aug"
    
    sep = calc(Baseline[[seq(9, 719, 12)]], sd)
    sep = as.data.frame(rasterToPoints(sep))
    sep$month = "sep"
    
    oct = calc(Baseline[[seq(10, 719, 12)]], sd)
    oct = as.data.frame(rasterToPoints(oct))
    oct$month = "oct"
    
    nov = calc(Baseline[[seq(11, 719, 12)]], sd)
    nov = as.data.frame(rasterToPoints(nov))
    nov$month = "nov"
    
    dec = calc(Baseline[[seq(12, 719, 12)]], sd)
    dec = as.data.frame(rasterToPoints(dec))
    dec$month = "dec"
    
    stat = rbind(jan, feb, mar, apr, may, jun, jul, aug, sep, oct, nov, dec)
    
  }
  
  # pdf(paste0("~/Dropbox (MBA)/PAPER Kisei heat extremes/figures/Climatologies/", data, "_Climatology_1870-1929.pdf"), height = 10, width = 8.5)
  # par(mfrow = c(2,1))
  # plot(calc(Baseline, mean), col = matlab.like(100), axes = F, main = "Mean", zlim = c(-3, 33))
  # map(add = T, lwd = 0.1, fill = T, col = "gray"); degAxis(1); degAxis(2, las = 1)
  # plot(calc(Baseline, sd), col = matlab.like(100), axes = F, main = "SD", zlim = c(0,10))
  # map(add = T, lwd = 0.1, fill = T, col = "gray"); degAxis(1); degAxis(2, las = 1)
  # dev.off()
  
  return(stat)
  
}

d1 = plot_clim("ER", "mean"); d1$source = "ERSST v4"
d2 = plot_clim("HadI", "mean"); d2$source = "HadISST v1.1"
d3 = plot_clim("COBE", "mean"); d3$source = "COBE v2"

d4 = plot_clim("ER", "sd"); d4$source = "ERSST v4"
d5 = plot_clim("HadI", "sd"); d5$source = "HadISST v1.1"
d6 = plot_clim("COBE", "sd"); d6$source = "COBE v2"

mean = rbind(d1, d2, d3)
sd = rbind(d4, d5, d6)

world <- ne_countries(scale = "small", returnclass = "sf") 

p = mean %>% 
  sample_frac(0.001) %>%
  ggplot() + 
  geom_point(aes(x, y, color = layer)) + 
  geom_polygon(data = world.df, aes(x = long, y = lat, group = group)) +
  scale_color_gradientn(colors = matlab.like(100), "", limits = c(-2, 29.4)) +
  # scale_x_continuous(expand = c(-10, 0), "") +
  # scale_y_continuous(expand = c(0, 0), "") +
  facet_grid(source ~ month) + 
  # coord_sf(xlim = range(d$x), ylim = range(d$y)) +
  coord_map("ortho", orientation = c(10, 250, 0)) +
  # coord_map("moll") +
  theme_pubr()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "right", 
        legend.justification = c(1,0))

print(p)

world <- fortify(getMap())

mean$month = factor(mean$month, levels=c('jan', 'feb', 'mar', 
                                         'apr', 'may', 'jun', 
                                         'jul', 'aug', 'sep', 
                                         'oct', 'nov', 'dec'))

sd$month = factor(sd$month, levels=c('jan', 'feb', 'mar', 
                                     'apr', 'may', 'jun', 
                                     'jul', 'aug', 'sep', 
                                     'oct', 'nov', 'dec'))

p1 = mean %>% 
  sample_frac(1) %>%
  ggplot() + 
  # geom_point(aes(x = x, y = y, color = layer), size = 0.5, alpha = 0.5) +
  geom_raster(aes(x = x, y = y, fill = layer)) +
  geom_map(data = world, map = world, aes(x = long, y = lat, map_id = id),
           color = "black", fill = "gray", size = 0.1) +
  scale_color_gradientn(colors = matlab.like(100), "mean", limits = c(-2, 29.4)) +
  scale_fill_gradientn(colors = matlab.like(100), "mean", limits = c(-2, 29.4)) +
  # coord_proj("+proj=wintri") +
  facet_grid(source ~ month) + 
  theme_pubr(I(9)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "bottom", 
        legend.justification = c(1,0))

p2 = sd %>% 
  sample_frac(1) %>%
  ggplot() + 
  # geom_point(aes(x = x, y = y, color = layer), size = 0.5, alpha = 0.5) +
  geom_raster(aes(x = x, y = y, fill = layer)) +
  geom_map(data = world, map = world, aes(x = long, y = lat, map_id = id),
           color = "black", fill = "gray", size = 0.1) +
  scale_color_gradientn(colors = matlab.like(100), "sd", limits = c(0, 3.7)) +
  scale_fill_gradientn(colors = matlab.like(100), "sd", limits = c(0, 3.7)) +
  # coord_proj("+proj=wintri") +
  facet_grid(source ~ month) + 
  theme_pubr(I(9)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "bottom", 
        legend.justification = c(1,0))


pdf("~/Desktop/Climatologies_1870-1929.pdf", width = 15, height = 7)
cowplot::plot_grid(p1, p2, nrow = 2)
dev.off()
