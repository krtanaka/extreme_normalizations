library(raster)
library(colorRamps)
library(ggpubr)
library(rnaturalearth)
library(sf)
library(rgdal)
library(dplyr)
library(maps)
# library(ggdark)
library(ggjoy)
library(rworldmap)
library(ggalt)
library(readr)
library(lwgeom)

rm(list = ls())

cutoff = c(0.95, 0.975)[1]

period = c("1980-1989", "1990-1999", "2000-2009", "2010-2019")

data = c("HadI", "COBE", "ER")

world <- ne_countries(scale = "small", returnclass = "sf") 

worldMap <- getMap()
world.points <- fortify(worldMap)
world.points$region <- world.points$id

world.df <- world.points[,c("long","lat","group", "region")]

world <- fortify(getMap())

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

meow <- readOGR(dsn = paste0("/Users/", Sys.info()[7], "/Downloads/MEOW"), layer = "meow_ecos")
meow <- meow %>% st_as_sf()  

lme <- readOGR("/Users/ktanaka/Google Drive/Research/GIS/LME66/LMEs66.shp")
lme <- rmapshaper::ms_simplify(lme, keep = 0.01, keep_shapes = F)
lme <- lme %>% st_as_sf()  

eez <- readOGR(dsn = "/Users/ktanaka/clim_geo_disp/data/EEZ_land_union", layer = "EEZ_land_v2_201410")
eez <- rmapshaper::ms_simplify(eez, keep = 0.01, keep_shapes = F)
eez <- eez %>% st_as_sf()  

#IPCC - Temperature -
ipcc_temp <- c(rgb(103, 0, 31, maxColorValue = 255, alpha = 255),
               rgb(178, 24, 43, maxColorValue = 255, alpha = 255),
               rgb(214, 96, 77, maxColorValue = 255, alpha = 255),
               rgb(244, 165, 130, maxColorValue = 255, alpha = 255),
               rgb(253, 219, 199, maxColorValue = 255, alpha = 255),
               rgb(247, 247, 247, maxColorValue = 255, alpha = 255),
               rgb(209, 229, 240, maxColorValue = 255, alpha = 255),
               rgb(146, 197, 222, maxColorValue = 255, alpha = 255),
               rgb(67, 147, 195, maxColorValue = 255, alpha = 255),
               rgb(33, 102, 172, maxColorValue = 255, alpha = 255),
               rgb(5, 48, 97, maxColorValue = 255, alpha = 255))

invert_geom_defaults()

map = function(mode){
  
  load(paste0("/Users/ktanaka/extreme_normalizations/results/HadI/SST_Anomalies_1980-1989_", cutoff, ".RData")); hadi1 = anom; hadi1$source = "HadISST v1.1"; hadi1$period = "1980-1989"
  load(paste0("/Users/ktanaka/extreme_normalizations/results/HadI/SST_Anomalies_1990-1999_", cutoff, ".RData")); hadi2 = anom; hadi2$source = "HadISST v1.1"; hadi2$period = "1990-1999"
  load(paste0("/Users/ktanaka/extreme_normalizations/results/HadI/SST_Anomalies_2000-2009_", cutoff, ".RData")); hadi3 = anom; hadi3$source = "HadISST v1.1"; hadi3$period = "2000-2009"
  load(paste0("/Users/ktanaka/extreme_normalizations/results/HadI/SST_Anomalies_2010-2019_", cutoff, ".RData")); hadi4 = anom; hadi4$source = "HadISST v1.1"; hadi4$period = "2010-2019"
  load(paste0("/Users/ktanaka/extreme_normalizations/results/COBE/SST_Anomalies_1980-1989_", cutoff, ".RData")); cobe1 = anom; cobe1$source = "COBE v2"; cobe1$period = "1980-1989"
  load(paste0("/Users/ktanaka/extreme_normalizations/results/COBE/SST_Anomalies_1990-1999_", cutoff, ".RData")); cobe2 = anom; cobe2$source = "COBE v2"; cobe2$period = "1990-1999"
  load(paste0("/Users/ktanaka/extreme_normalizations/results/COBE/SST_Anomalies_2000-2009_", cutoff, ".RData")); cobe3 = anom; cobe3$source = "COBE v2"; cobe3$period = "2000-2009"
  load(paste0("/Users/ktanaka/extreme_normalizations/results/COBE/SST_Anomalies_2010-2019_", cutoff, ".RData")); cobe4 = anom; cobe4$source = "COBE v2"; cobe4$period = "2010-2019"
  load(paste0("/Users/ktanaka/extreme_normalizations/results/ER/SST_Anomalies_1980-1989_", cutoff, ".RData")); er1 = anom; er1$source = "ERSST v5"; er1$period = "1980-1989"
  load(paste0("/Users/ktanaka/extreme_normalizations/results/ER/SST_Anomalies_1990-1999_", cutoff, ".RData")); er2 = anom; er2$source = "ERSST v5"; er2$period = "1990-1999"
  load(paste0("/Users/ktanaka/extreme_normalizations/results/ER/SST_Anomalies_2000-2009_", cutoff, ".RData")); er3 = anom; er3$source = "ERSST v5"; er3$period = "2000-2009"
  load(paste0("/Users/ktanaka/extreme_normalizations/results/ER/SST_Anomalies_2010-2019_", cutoff, ".RData")); er4 = anom; er4$source = "ERSST v5"; er4$period = "2010-2019"
  
  #all periods
  anom = rbind(hadi1, hadi2, hadi3, hadi4, 
               cobe1, cobe2, cobe3, cobe4,
               er1, er2, er3, er4)
  
  anom$source = factor(anom$source, levels = c("HadISST v1.1", "COBE v2",  "ERSST v5"))
  
  if (mode == "annual") {
    
    anom$sum = range01(anom$sum)
    # anom = subset(anom, source %in% c("COBE v2", "HadISST v1.1"))
    
    # p = ggplot(anom) +
    #   geom_point(aes(x, y, color = sum, fill = sum)) +
    #   geom_polygon(data = world.df, aes(x = long, y = lat, group = group)) +
    #   scale_fill_gradientn(colors = matlab.like(100), "", limits = c(0,1)) +
    #   scale_color_gradientn(colors = matlab.like(100), "", limits = c(0,1)) +
    #   scale_x_continuous(expand = c(-0.005, 0), "") +
    #   scale_y_continuous(expand = c(-0.005, 0), "") +
    #   # coord_sf(xlim = range(anom$x), ylim = range(anom$y)) +
    #   facet_wrap(.~source + period, ncol = 3, dir = "v") +
    #   theme_minimal() +
    #   coord_map("ortho", orientation = c(0, 0, 0)) +
    #   theme(axis.title.x = element_blank(),
    #         axis.title.y = element_blank(),
    #         legend.position = "right")
    
    p = anom %>% 
      sample_frac(1) %>% 
      subset(source %in% c("HadISST v1.1", "COBE v2")) %>% 
      group_by(x, y, period) %>% 
      summarise(sum = median(sum)) %>% 
      ggplot(size = 5, alpha = 0.8) + 
      geom_point(aes(x = x, y = y, color = sum), size = 1, alpha = 0.5, shape = 16) +
      geom_map(data = world, map = world, aes(x = long, y = lat, map_id = id),
               color = "gray20", fill = "gray20", size = 0.001) + 
      scale_color_gradientn(colors = rev(ipcc_temp), "", limits = c(0,1), breaks = c(0,0.5,1)) +
      coord_proj("+proj=wintri") +
      # coord_fixed() + 
      # facet_grid(source ~ period) +
      facet_grid(~ period) +
      theme_minimal(I(20)) +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(), 
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(),
            legend.position = "bottom", 
            legend.justification = c(1,0))
    
    p = ggplot(anom) +
      geom_raster(aes(x = x, y = y, fill = sum)) +
      geom_map(data = world, map = world, aes(x = long, y = lat, map_id = id),
               color = "gray20", fill = "gray20", size = 0.001) +
      scale_fill_gradientn(colors = rev(ipcc_temp), "", limits = c(0,1), breaks = c(0,0.5,1)) +
      scale_x_continuous(expand = c(-0.005, 0), "") +
      scale_y_continuous(expand = c(-0.005, 0), "") +
      coord_fixed() +
      facet_grid(period ~ source) +
      theme_minimal(I(20)) +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(),
            legend.position = "bottom",
            legend.justification = c(1,0))
    
    # pdf(paste0("/Users/ktanaka/Desktop/Fig1_", Sys.Date(), "_", cutoff, ".pdf"), height = 12, width = 12)
    png(paste0("/Users/ktanaka/Desktop/Fig1_", Sys.Date(), "_", cutoff, ".png"), height = 12, width = 12, units = "in", res = 100)
    print(p)
    dev.off()
    
  }
  
  if (mode == "seasonal") {
    
    season_1 = anom[,c("x", "y", "jan", "feb", "mar", "source", "period")]; season_1$season = "Jan_Feb_Mar"
    season_2 = anom[,c("x", "y", "jul", "aug", "sep", "source", "period")]; season_2$season = "Jul_Aug_Sep"
    
    season_1$sum = rowSums(season_1[3:5])
    season_2$sum = rowSums(season_2[3:5])
    
    season_1 = season_1[,c(1,2, 6:9)]
    season_2 = season_2[,c(1,2, 6:9)]
    
    anom = rbind(season_1, season_2)
    
    anom$sum = range01(anom$sum)
    
    
    # p = ggplot(anom) + 
    #   geom_point(aes(x, y, color = sum, fill = sum), alpha = 0.5, size = 0.5) + 
    #   geom_polygon(data = world.df, aes(x = long, y = lat, group = group)) +
    #   # geom_sf(data = world, size = 0.15, color = "gray") +
    #   scale_fill_gradientn(colors = matlab.like(100), "", limits = c(0,1)) +
    #   scale_color_gradientn(colors = matlab.like(100), "", limits = c(0,1)) +
    #   scale_x_continuous(expand = c(-0.005, 0), "") +
    #   scale_y_continuous(expand = c(-0.005, 0), "") +
    #   # coord_sf(xlim = range(anom$x), ylim = range(anom$y)) +
    #   # facet_wrap(.~source + period + season, ncol = 3, dir = "v") +
    #   facet_grid(source ~ period + season) +
    # theme_minimal(I(20)) +
      #   coord_map("ortho", orientation = c(0, 0, 0)) + 
    #   theme(axis.title.x = element_blank(),
    #         axis.title.y = element_blank(), 
    #         legend.position = "right")
    
    p = anom %>% 
      # sample_frac(0.01) %>%
      subset(source %in% c("HadISST v1.1", "COBE v2")) %>% 
      group_by(x, y, period, season) %>% 
      summarise(sum = median(sum)) %>% 
      ggplot() + 
      geom_point(aes(x = x, y = y, color = sum), size = 0.5, alpha = 0.5) +
      geom_map(data = world, map = world, aes(x = long, y = lat, map_id = id),
               color = "black", fill = "gray", size = 0.1) + 
      scale_color_gradientn(colors = rev(ipcc_temp), "", limits = c(0,1), breaks = c(0,0.5,1)) +
      coord_proj("+proj=wintri") +
      # facet_grid(source ~ season + period) +
      facet_grid(season ~ period) +
      theme_minimal(I(18)) +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(), 
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(),
            legend.position = "right", 
            legend.justification = c(1,0))
    
    p =  ggplot(anom) + 
      geom_raster(aes(x = x, y = y, fill = sum)) +
      geom_map(data = world, map = world, aes(x = long, y = lat, map_id = id),
               color = "gray20", fill = "gray20", size = 0.001) + 
      scale_fill_gradientn(colors = rev(ipcc_temp), "", limits = c(0,1), breaks = c(0,0.5,1)) +
      scale_x_continuous(expand = c(-0.005, 0), "") +
      scale_y_continuous(expand = c(-0.005, 0), "") +
      coord_fixed() + 
      facet_grid(source ~ season + period) +
      theme_minimal(I(10)) +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(), 
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(),
            legend.position = "bottom", 
            legend.justification = c(1,0))
    
    # pdf(paste0("/Users/ktanaka/Desktop/SST_Anomalies_Season_", cutoff, ".pdf"), height = 5, width = 18)
    png(paste0("/Users/ktanaka/Desktop/Fig2_", Sys.Date(), "_", cutoff, ".png"), height = 12, width = 12, units = "in", res = 100)
    
    print(p)
    dev.off()
    
  }
  
  if (mode == "seasonal_2000-2018") {
    
    #2000-2018
    anom = rbind(hadi3, hadi4, 
                 cobe3, cobe4,
                 er3, er4)
    
    season_1 = anom[,c("x", "y", "jan", "feb", "mar", "source")]; season_1$season = "Jan_Feb_Mar"
    season_2 = anom[,c("x", "y", "jul", "aug", "sep", "source")]; season_2$season = "Jul_Aug_Sep"
    
    season_1$sum = rowSums(season_1[3:5])
    season_2$sum = rowSums(season_2[3:5])
    
    season_1 = season_1[,c(1,2, 6:8)]
    season_2 = season_2[,c(1,2, 6:8)]
    
    anom = rbind(season_1, season_2)
    
    anom$sum = range01(anom$sum)
    
    anom = subset(anom, source %in% c("COBE v2", "HadISST v1.1"))
    
    p = ggplot(anom) +
      geom_point(aes(x, y, color = sum, fill = sum), alpha = 0.5, size = 2) +
      geom_polygon(data = world.df, aes(x = long, y = lat, group = group)) +
      # geom_sf(data = world, size = 0.15, color = "gray") +
      scale_fill_gradientn(colors = matlab.like(100), "", limits = c(0,1)) +
      scale_color_gradientn(colors = matlab.like(100), "", limits = c(0,1)) +
      scale_x_continuous(expand = c(-0.005, 0), "") +
      scale_y_continuous(expand = c(-0.005, 0), "") +
      # coord_sf(xlim = range(anom$x), ylim = range(anom$y)) +
      # facet_wrap(.~source + period + season, ncol = 3, dir = "v") +
      facet_grid(~ season) +
      theme_minimal(I(18)) +
      coord_map("ortho", orientation = c(0, 0, 0)) +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            legend.position = "right")
    
    p = anom %>% 
      sample_frac(1) %>%
      ggplot() + 
      geom_point(aes(x = x, y = y, color = sum), size = 1, alpha = 0.5, shape = 16) +
      geom_map(data = world, map = world, aes(x = long, y = lat, map_id = id),
               color = "gray20", fill = "gray20", size = 0.1) + 
      # scale_color_gradientn(colors = matlab.like(100), "", limits = c(0,1)) +
      scale_color_gradientn(colors = rev(ipcc_temp), "", limits = c(0,1), breaks = c(0, 0.5, 1)) +
      coord_proj("+proj=wintri") +
      facet_grid(source ~ season) +
      theme_pubr() +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(), 
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(),
            legend.position = "bottom", 
            legend.justification = c(1,0))
    
    p = anom %>% 
      # sample_frac(0.01) %>%
      subset(source %in% c("HadISST v1.1", "COBE v2")) %>% 
      group_by(x, y, season) %>% 
      summarise(sum = mean(sum, na.rm = T)) %>% 
      ggplot() + 
      geom_raster(aes(x = x, y = y, fill = sum, interpolate = T)) +
      geom_map(data = world, map = world, aes(x = long, y = lat, map_id = id),
               color = "gray20", fill = "gray20", size = 0.001) + 
      scale_fill_gradientn(colors = rev(ipcc_temp), "", limits = c(0,1), breaks = c(0,0.5,1)) +
      scale_x_continuous(expand = c(-0.005, 0), "") +
      scale_y_continuous(expand = c(-0.005, 0), "") +
      coord_fixed() + 
      facet_grid(~ season) +
      theme_minimal(I(20)) +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "bottom", 
        legend.justification = c(1,0))
    
    pdf(paste0("/Users/ktanaka/Desktop/Fig2_", Sys.Date(), "_", cutoff, ".pdf"), height = 7, width = 10)
    print(p)
    dev.off()
    
  }
  
  if (mode == "combine") {
    
    #all periods
    annual = rbind(hadi1, hadi2, hadi3, hadi4, cobe1, cobe2, cobe3, cobe4)
    annual$sum = range01(annual$sum)
    annual$season = "Annual"
    annual = annual[, c("x", "y", "sum", "source", "period", "season")]
    
    
    #seasonals
    anom = rbind(hadi1, hadi2, hadi3, hadi4, cobe1, cobe2, cobe3, cobe4)
    season_1 = anom[,c("x", "y", "jan", "feb", "mar", "source", "period")]; season_1$season = "Jan-Mar"
    season_2 = anom[,c("x", "y", "jul", "aug", "sep", "source", "period")]; season_2$season = "Jul-Sep"
    season_1$sum = rowSums(season_1[3:5])
    season_2$sum = rowSums(season_2[3:5])
    season_1 = season_1[,c(1,2, 6:9)]
    season_2 = season_2[,c(1,2, 6:9)]
    season = rbind(season_1, season_2)
    season$sum = range01(season$sum)
    season = season[, c("x", "y", "sum", "source", "period", "season")]

    anom = rbind(annual, season) %>% group_by(x, y, period, season) %>% summarise(sum = mean(sum))
    
    anom[anom=="2010-2018"]<-"2010-2019"
    
    p = anom %>% 
      sample_frac(1) %>%
      ggplot(size = 5, alpha = 0.8) + 
      geom_point(aes(x = x, y = y, color = sum), size = 1, alpha = 0.5, shape = 16) +
      geom_map(data = world, map = world, aes(x = long, y = lat, map_id = id),
               color = "gray20", fill = "gray20", size = 0.1) + 
      # scale_color_gradientn(colors = matlab.like(100), "", limits = c(0,1)) +
      scale_color_gradientn(colors = rev(ipcc_temp), "", limits = c(0,1), breaks = c(0, 0.5, 1)) +
      # coord_proj("+proj=wintri") +
      scale_x_continuous(expand = c(-0.005, 0), "") +
      scale_y_continuous(expand = c(-0.005, 0), "") +
      coord_fixed() +
      facet_grid(season ~ period) +
      theme_minimal(I(20)) +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(), 
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(),
            legend.position = "bottom", 
            legend.justification = c(1,0))
    
    p = ggplot(anom) +
      geom_raster(aes(x = x, y = y, fill = sum), interpolate = T) +
      geom_map(data = world, map = world, aes(x = long, y = lat, map_id = id),
               color = "gray20", fill = "gray20", size = 0.001) +
      scale_fill_gradientn(colors = rev(ipcc_temp), "", limits = c(0,1), breaks = c(0,0.5,1)) +
      scale_x_continuous(expand = c(-0.005, 0), "") +
      scale_y_continuous(expand = c(-0.005, 0), "") +
      coord_fixed() +
      facet_grid(season ~ period) +
      theme_minimal(I(20)) +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(),
            legend.position = "bottom",
            legend.justification = c(1,0))
    
    pdf(paste0("/Users/ktanaka/Desktop/Fig1_", Sys.Date(), "_", cutoff, ".pdf"), height = 10, width = 10)
    png(paste0("/Users/ktanaka/Desktop/Fig1_", Sys.Date(), "_", cutoff, ".png"), height = 10, width = 10, units = "in", res = 100)
    print(p)
    dev.off()
    
  }
  
  
}

map("annual")
map("seasonal")
map("seasonal_2000-2018")
map("combine")