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

period = c("1980-1989", "1990-1999", "2000-2009", "2010-2018")

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

rank_mean = function(region){
  
  # region = "lme"
  
  if (region == "meow"){
    shape = meow; shape$UNIT = shape$PROVINCE
  } 
  
  if (region == "lme") {
    shape = lme; shape$UNIT = shape$LME_NAME
  } 
  
  tas_combined = NULL
  
  for (i in 1:length(period)){
    
    # i = 1
    
    load(paste0(paste0("~/extreme_normalizations/results/", cutoff, "/HadI/SST_Anomalies_", period[[i]], ".RData")))
    anom = anom[, c(1:2, 15)]
    tas <- st_as_sf(x = anom, coords = c("x", "y"), crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" )
    summary(tas)
    hadi <- st_intersection(tas, shape)
    hadi$sum = range01(hadi$sum)
    hadi <- hadi %>% group_by(UNIT) %>% 
      summarise(mean = mean(sum, na.rm = T),
                sd = sd(sum, na.rm = T), 
                n = n()) %>%
      mutate(se = sd/sqrt(n),
             lower.ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
             upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se)
    hadi$source = "HadISST v1.1"; hadi$period = period[[i]]
    
    load(paste0(paste0("~/extreme_normalizations/results/", cutoff, "/COBE/SST_Anomalies_", period[[i]], ".RData")))
    anom = anom[, c(1:2, 15)]
    tas <- st_as_sf(x = anom, coords = c("x", "y"), crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" )
    cobe <- st_intersection(tas, shape)
    cobe$sum = range01(cobe$sum)
    cobe <- cobe %>% group_by(UNIT) %>% 
      summarise(mean = mean(sum, na.rm = T),
                sd = sd(sum, na.rm = T), 
                n = n()) %>%
      mutate(se = sd/sqrt(n),
             lower.ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
             upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se)
    cobe$source = "COBE v2"; cobe$period = period[[i]]
    
    load(paste0(paste0("~/extreme_normalizations/results/", cutoff, "/ER/SST_Anomalies_", period[[i]], ".RData")))
    anom = anom[, c(1:2, 15)]
    tas <- st_as_sf(x = anom, coords = c("x", "y"), crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" )
    er <- st_intersection(tas, shape)
    er$sum = range01(er$sum)
    er <- er %>% group_by(UNIT) %>%
      summarise(mean = mean(sum, na.rm = T),
                sd = sd(sum, na.rm = T),
                n = n()) %>%
      mutate(se = sd/sqrt(n),
             lower.ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
             upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se)
    er$source = "ERSST v4"; er$period = period[[i]]
    
    tas = rbind(hadi, cobe, er)
    
    tas_combined = rbind(tas_combined, tas)
    
    
  }
  
  p = tas_combined %>% 
    mutate(unit = forcats::fct_reorder(UNIT, mean)) %>% 
    ggplot() +
    geom_segment(aes(
      color = period, 
      x = unit, 
      xend = unit,
      y = lower.ci, 
      yend = upper.ci),
      size = 1) +
    geom_point(aes(
      color = period,
      x = unit,
      y = mean),
      size = 2) +
    coord_flip() +
    facet_wrap(.~source) +
    scale_colour_manual(values = matlab.like(4)) +
    xlab("") +
    ylab("") +
    # ylim(0,1) +
    dark_theme_bw() +
    theme(legend.position = "right")
  
  pdf(paste0("~/Desktop/", cutoff, "_Mean_", region, ".pdf"), height = 15, width = 10)
  print(p)
  dev.off()
  
}

rank_mean("lme")
rank_mean("eez")



