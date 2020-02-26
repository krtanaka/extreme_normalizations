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
  
  if (mode == "mean") stat = calc(Baseline, mean)
  if (mode == "sd") stat = calc(Baseline, sd)
  
  stat = rasterToPoints(stat)
  colnames(stat) = c("x", "y", mode)
  stat = as.data.frame(stat)
  
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
  sample_frac(1) %>%
  ggplot() + 
  geom_point(aes(x, y, color = mean)) + 
  geom_polygon(data = world.df, aes(x = long, y = lat, group = group)) +
  scale_color_gradientn(colors = matlab.like(100), "", limits = c(-2, 29.4)) +
  # scale_x_continuous(expand = c(-10, 0), "") +
  # scale_y_continuous(expand = c(0, 0), "") +
  facet_wrap(.~source) + 
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

p1 = mean %>% 
  sample_frac(1) %>%
  ggplot() + 
  geom_point(aes(x = x, y = y, color = mean), size = 0.5, alpha = 0.5) +
  geom_map(data = world, map = world, aes(x = long, y = lat, map_id = id),
           color = "black", fill = "gray", size = 0.1) + 
  scale_color_gradientn(colors = matlab.like(100), "Mean", limits = c(-2, 29.4)) +
  coord_proj("+proj=wintri") +
  facet_wrap(.~ source, ncol = 3) + 
  theme_pubr()+
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
  geom_point(aes(x = x, y = y, color = sd), size = 0.5, alpha = 0.5) +
  geom_map(data = world, map = world, aes(x = long, y = lat, map_id = id),
           color = "black", fill = "gray", size = 0.1) + 
  scale_color_gradientn(colors = matlab.like(100), "SD", limits = c(0, 9.1)) +
  coord_proj("+proj=wintri") +
  facet_wrap(.~ source, ncol = 3) + 
  theme_pubr()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "bottom", 
        legend.justification = c(1,0))

pdf("~/Desktop/Climatologies_1870-1929.pdf", width = 10, height = 7)
cowplot::plot_grid(p1, p2, nrow = 2)
dev.off()
