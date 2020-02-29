library(raster)
library(colorRamps)
library(ggpubr)
library(rnaturalearth)
library(sf)
library(rgdal)
library(dplyr)
library(maps)
library(ggdark)
library(ggjoy)

rm(list = ls())

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

lme <- rgdal::readOGR("/Users/ktanaka/Google Drive/Research/GIS/LME66/LMEs66.shp")
lme <- rmapshaper::ms_simplify(lme, keep = 0.01, keep_shapes = F)
lme <- lme %>% st_as_sf()  

map = function(mode){
  
  load("~/extreme_normalizations/results/HadI/SST_Anomalies_1980-1989.RData"); hadi1 = anom; hadi1$source = "HadISST v1.1"; hadi1$period = "1980-1999"
  load("~/extreme_normalizations/results/HadI/SST_Anomalies_1990-1999.RData"); hadi2 = anom; hadi2$source = "HadISST v1.1"; hadi2$period = "1990-1999"
  load("~/extreme_normalizations/results/HadI/SST_Anomalies_2000-2009.RData"); hadi3 = anom; hadi3$source = "HadISST v1.1"; hadi3$period = "2000-2009"
  load("~/extreme_normalizations/results/HadI/SST_Anomalies_2010-2018.RData"); hadi4 = anom; hadi4$source = "HadISST v1.1"; hadi4$period = "2010-2018"
  load("~/extreme_normalizations/results/COBE/SST_Anomalies_1980-1989.RData"); cobe1 = anom; cobe1$source = "COBE v2"; cobe1$period = "1980-1999"
  load("~/extreme_normalizations/results/COBE/SST_Anomalies_1990-1999.RData"); cobe2 = anom; cobe2$source = "COBE v2"; cobe2$period = "1990-1999"
  load("~/extreme_normalizations/results/COBE/SST_Anomalies_2000-2009.RData"); cobe3 = anom; cobe3$source = "COBE v2"; cobe3$period = "2000-2009"
  load("~/extreme_normalizations/results/COBE/SST_Anomalies_2010-2018.RData"); cobe4 = anom; cobe4$source = "COBE v2"; cobe4$period = "2010-2018"
  load("~/extreme_normalizations/results/ER/SST_Anomalies_1980-1989.RData"); er1 = anom; er1$source = "ERSST v4"; er1$period = "1980-1999"
  load("~/extreme_normalizations/results/ER/SST_Anomalies_1990-1999.RData"); er2 = anom; er2$source = "ERSST v4"; er2$period = "1990-1999"
  load("~/extreme_normalizations/results/ER/SST_Anomalies_2000-2009.RData"); er3 = anom; er3$source = "ERSST v4"; er3$period = "2000-2009"
  load("~/extreme_normalizations/results/ER/SST_Anomalies_2010-2018.RData"); er4 = anom; er4$source = "ERSST v4"; er4$period = "2010-2018"
  
  anom = rbind(hadi1, hadi2, hadi3, hadi4, 
               cobe1, cobe2, cobe3, cobe4,
               er1, er2, er3, er4)
  
  if (mode == "annual") {
    
    anom$sum = range01(anom$sum)
    
    pdf(paste0("~/Desktop/SST_Anomalies_Annual.pdf"), height = 8, width = 8)
    
    # p = ggplot(anom) + 
    #   geom_point(aes(x, y, color = sum, fill = sum)) + 
    #   geom_polygon(data = world.df, aes(x = long, y = lat, group = group)) +
    #   # geom_sf(data = world, size = 0.15, color = "gray") +
    #   scale_fill_gradientn(colors = matlab.like(100), "", limits = c(0,1)) +
    #   scale_color_gradientn(colors = matlab.like(100), "", limits = c(0,1)) +
    #   scale_x_continuous(expand = c(-0.005, 0), "") +
    #   scale_y_continuous(expand = c(-0.005, 0), "") +
    #   # coord_sf(xlim = range(anom$x), ylim = range(anom$y)) +
    #   facet_wrap(.~source + period, ncol = 3, dir = "v") +
    #   theme_pubr() + 
    #   coord_map("ortho", orientation = c(0, 0, 0)) + 
    #   theme(axis.title.x = element_blank(),
    #         axis.title.y = element_blank(), 
    #         legend.position = "right")
    
    p =  anom %>% 
      sample_frac(1) %>%
      ggplot() + 
      geom_point(aes(x = x, y = y, color = sum), size = 0.5, alpha = 0.5) +
      geom_map(data = world, map = world, aes(x = long, y = lat, map_id = id),
               color = "black", fill = "gray", size = 0.1) + 
      scale_color_gradientn(colors = matlab.like(100), "", limits = c(0,1)) +
      coord_proj("+proj=wintri") +
      facet_wrap(.~source + period, ncol = 3, dir = "v") +
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
    
    pdf(paste0("~/Desktop/SST_Anomalies_Season.pdf"), height = 5, width = 18)
    
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
    #   theme_pubr() + 
    #   coord_map("ortho", orientation = c(0, 0, 0)) + 
    #   theme(axis.title.x = element_blank(),
    #         axis.title.y = element_blank(), 
    #         legend.position = "right")
    
    p = anom %>% 
      sample_frac(1) %>%
      ggplot() + 
      geom_point(aes(x = x, y = y, color = sum), size = 0.5, alpha = 0.5) +
      geom_map(data = world, map = world, aes(x = long, y = lat, map_id = id),
               color = "black", fill = "gray", size = 0.1) + 
      scale_color_gradientn(colors = matlab.like(100), "", limits = c(0,1)) +
      coord_proj("+proj=wintri") +
      facet_grid(source ~ season + period) +
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
    dev.off()
    
  }
  
}

map("annual")
map("seasonal")

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
    
    load(paste0("~/extreme_normalizations/results/HadI/SST_Anomalies_", period[[i]], ".RData"))
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
    
    load(paste0("~/extreme_normalizations/results/COBE/SST_Anomalies_", period[[i]], ".RData"))
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
    
    load(paste0("~/extreme_normalizations/results/ER/SST_Anomalies_", period[[i]], ".RData"))
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
  
  pdf(paste0("~/Desktop/Mean_", region, ".pdf"), height = 15, width = 10)
  print(p)
  dev.off()
  
}

rank_mean("lme")
rank_mean("meow")

rank_joy = function(region){
  
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
    
    load(paste0("~/extreme_normalizations/results/HadI/SST_Anomalies_", period[[i]], ".RData"))
    anom = anom[, c(1:2, 15)]
    tas <- st_as_sf(x = anom, coords = c("x", "y"), crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" )
    summary(tas)
    hadi <- st_intersection(tas, shape)
    hadi$sum = range01(hadi$sum)
    prov_levels <- hadi %>% # Reorder levels by mean risk by privince 
      dplyr::select(sum,UNIT) %>%
      group_by(UNIT) %>%
      mutate(mean_of_mean = mean(sum))
    levels <- unique(prov_levels$UNIT[order(prov_levels$mean_of_mean)])
    hadi$UNIT <- factor(hadi$UNIT, levels = levels, ordered=TRUE)
    df = table(hadi$UNIT)
    df = as.data.frame(df)
    df = subset(df, Freq > 2)
    colnames(df)[1] = "UNIT"
    hadi = merge(hadi, df)
    hadi$source = "HadISST v1.1"; hadi$period = period[[i]]
    
    load(paste0("~/extreme_normalizations/results/COBE/SST_Anomalies_", period[[i]], ".RData"))
    anom = anom[, c(1:2, 15)]
    tas <- st_as_sf(x = anom, coords = c("x", "y"), crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" )
    cobe <- st_intersection(tas, shape)
    cobe$sum = range01(cobe$sum)
    prov_levels <- cobe %>% # Reorder levels by mean risk by privince 
      dplyr::select(sum,UNIT) %>%
      group_by(UNIT) %>%
      mutate(mean_of_mean = mean(sum))
    levels <- unique(prov_levels$UNIT[order(prov_levels$mean_of_mean)])
    cobe$UNIT <- factor(cobe$UNIT, levels = levels, ordered=TRUE)
    df = table(cobe$UNIT)
    df = as.data.frame(df)
    df = subset(df, Freq > 2)
    colnames(df)[1] = "UNIT"
    cobe = merge(cobe, df)
    cobe$source = "COBE v2"; cobe$period = period[[i]]
    
    load(paste0("~/extreme_normalizations/results/ER/SST_Anomalies_", period[[i]], ".RData"))
    anom = anom[, c(1:2, 15)]
    tas <- st_as_sf(x = anom, coords = c("x", "y"), crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" )
    er <- st_intersection(tas, shape)
    er$sum = range01(er$sum)
    prov_levels <- er %>% # Reorder levels by mean risk by privince 
      dplyr::select(sum,UNIT) %>%
      group_by(UNIT) %>%
      mutate(mean_of_mean = mean(sum))
    levels <- unique(prov_levels$UNIT[order(prov_levels$mean_of_mean)])
    er$UNIT <- factor(er$UNIT, levels = levels, ordered=TRUE)
    df = table(er$UNIT)
    df = as.data.frame(df)
    df = subset(df, Freq > 2)
    colnames(df)[1] = "UNIT"
    er = merge(er, df)
    er$source = "ERSST v4"; er$period = period[[i]]
    
    tas = rbind(hadi, cobe, er)
    
    tas_combined = rbind(tas_combined, tas)
    
    
  }
  
  p = ggplot(tas_combined, aes(x = sum, y = UNIT, fill = period)) +
    geom_joy(scale = 5, alpha = 0.8, bandwidth = 0.05, size = 0.5) +
    theme_joy(grid = F) +
    scale_y_discrete(expand = c(0.01, 0)) + # will generally have to set the `expand` option
    # scale_x_continuous(limits = c(NA, 1),expand = c(0, 0)) +
    # scale_fill_cyclical(values = matlab.like(length(unique(tas_combined$UNIT))))+
    scale_fill_manual(values = matlab.like(4)) +
    # scale_fill_viridis_d()+
    facet_wrap(.~source) + 
    ylab(NULL) + xlab(NULL) +
    dark_theme_bw() +
    theme(axis.text.y = element_text(size = 10),
          axis.text.x = element_text(size = 10))
  
  tas_combined = subset(tas_combined, source %in% c("HadISST v1.1", "COBE v2")) # remove ERSST
  
  prov_levels <- subset(tas_combined, period %in% c("2010-2018")) %>% # Reorder levels by 2010-2018 
    dplyr::select(sum, UNIT) %>%
    group_by(UNIT) %>%
    mutate(mean_of_mean = mean(sum, na.rm = T))
  
  levels <- unique(prov_levels$UNIT[order(prov_levels$mean_of_mean)])
  tas_combined$UNIT <- factor(tas_combined$UNIT, levels = levels, ordered = TRUE)
  
  p = ggplot(tas_combined, aes(x = sum, y = UNIT, fill = UNIT)) +
    geom_joy(scale = 5, alpha = 0.8, bandwidth = 0.02, size = 0.3) +
    theme_joy(grid = F) +
    scale_y_discrete(expand = c(0.01, 0)) + # will generally have to set the `expand` option
    scale_x_continuous(limits = c(0, 1), expand = c(0.05, 0)) +
    scale_fill_cyclical(values = matlab.like(length(unique(df$UNIT))))+
    facet_wrap(.~period, ncol = 4) +
    ylab(NULL) + xlab(NULL) +
    theme(axis.text.y = element_text(size = 10),
          axis.text.x = element_text(size = 10, angle = 90, hjust = 1),
          legend.position = "none")
  
  pdf(paste0("~/Desktop/Joy_", region, ".pdf"), height = 10, width = 10)
  print(p)
  dev.off()
  
}

rank_joy("lme")
rank_joy("meow")

rank_joy_bgcp = function(){
  
  tas_combined = NULL
  
  for (i in 1:length(period)){
    
    # i = 1
    
    load("~/Dropbox (MBA)/PAPER Kisei heat extremes/data/biogeogr provinces/bgcp_raster_0.25.RData")
    load(paste0("~/extreme_normalizations/results/HadI/SST_Anomalies_", period[[i]], ".RData"))
    anom = anom[, c(1:2, 15)]
    x <- raster(xmn  =-180, xmx = 180, ymn = -90, ymx = 90, res = 1, crs = "+proj=longlat +datum=WGS84")
    anom <- rasterize(anom[, c('x', 'y')], x, anom[, 'sum'], fun = mean)
    bgcp = resample(bgcp, anom, method = "bilinear") 
    anom = as.data.frame(rasterToPoints(anom))
    bgcp = as.data.frame(rasterToPoints(bgcp))
    colnames(anom)[3] = "sum"
    colnames(bgcp)[3] = "bgcp"
    bgcp = merge(anom, bgcp, all = T)
    bgcp$bgcp = round(bgcp$bgcp, 0)
    bgcp$bgcp = as.factor(as.character(bgcp$bgcp))
    bgcp_names <- read_csv("~/Dropbox (MBA)/PAPER Kisei heat extremes/data/biogeogr provinces/NAME_BGCP_2019_REYGONDEAU.csv")
    bgcp_names = bgcp_names[,c("NAME", "BGCP")]
    colnames(bgcp_names) = c("name", "bgcp")
    bgcp = merge(bgcp, bgcp_names)
    bgcp$bgcp = bgcp$name
    bgcp$sum = (bgcp$sum-min(bgcp$sum, na.rm = T))/(max(bgcp$sum, na.rm = T) - min(bgcp$sum, na.rm = T))
    prov_levels <- bgcp %>% # Reorder levels by mean risk by privince 
      dplyr::select(sum, bgcp) %>%
      group_by(bgcp) %>%
      mutate(mean_of_mean = mean(sum, na.rm = T))
    levels <- unique(prov_levels$bgcp[order(prov_levels$mean_of_mean)])
    bgcp$bgcp <- factor(bgcp$bgcp, levels = levels, ordered = TRUE)
    df = table(bgcp$bgcp)
    df = as.data.frame(df)
    df = subset(df, Freq > 2)
    colnames(df)[1] = "bgcp"
    bgcp = merge(bgcp, df)
    df = bgcp[complete.cases(bgcp), ]
    df = df[,c("x", "y", "bgcp", "sum")]
    hadi = df
    hadi$source = "HadISST v1.1"; hadi$period = period[[i]]
    
    load("~/Dropbox (MBA)/PAPER Kisei heat extremes/data/biogeogr provinces/bgcp_raster_0.25.RData")
    load(paste0("~/extreme_normalizations/results/COBE/SST_Anomalies_", period[[i]], ".RData"))
    anom = anom[, c(1:2, 15)]
    x <- raster(xmn  =-180, xmx = 180, ymn = -90, ymx = 90, res = 1, crs = "+proj=longlat +datum=WGS84")
    anom <- rasterize(anom[, c('x', 'y')], x, anom[, 'sum'], fun = mean)
    bgcp = resample(bgcp, anom, method = "bilinear") 
    anom = as.data.frame(rasterToPoints(anom))
    bgcp = as.data.frame(rasterToPoints(bgcp))
    colnames(anom)[3] = "sum"
    colnames(bgcp)[3] = "bgcp"
    bgcp = merge(anom, bgcp, all = T)
    bgcp$bgcp = round(bgcp$bgcp, 0)
    bgcp$bgcp = as.factor(as.character(bgcp$bgcp))
    bgcp_names <- read_csv("~/Dropbox (MBA)/PAPER Kisei heat extremes/data/biogeogr provinces/NAME_BGCP_2019_REYGONDEAU.csv")
    bgcp_names = bgcp_names[,c("NAME", "BGCP")]
    colnames(bgcp_names) = c("name", "bgcp")
    bgcp = merge(bgcp, bgcp_names)
    bgcp$bgcp = bgcp$name
    bgcp$sum = (bgcp$sum-min(bgcp$sum, na.rm = T))/(max(bgcp$sum, na.rm = T) - min(bgcp$sum, na.rm = T))
    prov_levels <- bgcp %>% # Reorder levels by mean risk by privince 
      dplyr::select(sum, bgcp) %>%
      group_by(bgcp) %>%
      mutate(mean_of_mean = mean(sum, na.rm = T))
    levels <- unique(prov_levels$bgcp[order(prov_levels$mean_of_mean)])
    bgcp$bgcp <- factor(bgcp$bgcp, levels = levels, ordered = TRUE)
    df = table(bgcp$bgcp)
    df = as.data.frame(df)
    df = subset(df, Freq > 2)
    colnames(df)[1] = "bgcp"
    bgcp = merge(bgcp, df)
    df = bgcp[complete.cases(bgcp), ]
    df = df[,c("x", "y", "bgcp", "sum")]
    cobe = df
    cobe$source = "COBE v2"; cobe$period = period[[i]]
    
    load("~/Dropbox (MBA)/PAPER Kisei heat extremes/data/biogeogr provinces/bgcp_raster_0.25.RData")
    load(paste0("~/extreme_normalizations/results/ER/SST_Anomalies_", period[[i]], ".RData"))
    anom = anom[, c(1:2, 15)]
    x <- raster(xmn  =-180, xmx = 180, ymn = -90, ymx = 90, res = 2, crs = "+proj=longlat +datum=WGS84")
    anom <- rasterize(anom[, c('x', 'y')], x, anom[, 'sum'], fun = mean)
    bgcp = resample(bgcp, anom, method = "bilinear") 
    anom = as.data.frame(rasterToPoints(anom))
    bgcp = as.data.frame(rasterToPoints(bgcp))
    colnames(anom)[3] = "sum"
    colnames(bgcp)[3] = "bgcp"
    bgcp = merge(anom, bgcp, all = T)
    bgcp$bgcp = round(bgcp$bgcp, 0)
    bgcp$bgcp = as.factor(as.character(bgcp$bgcp))
    bgcp_names <- read_csv("~/Dropbox (MBA)/PAPER Kisei heat extremes/data/biogeogr provinces/NAME_BGCP_2019_REYGONDEAU.csv")
    bgcp_names = bgcp_names[,c("NAME", "BGCP")]
    colnames(bgcp_names) = c("name", "bgcp")
    bgcp = merge(bgcp, bgcp_names)
    bgcp$bgcp = bgcp$name
    bgcp$sum = (bgcp$sum-min(bgcp$sum, na.rm = T))/(max(bgcp$sum, na.rm = T) - min(bgcp$sum, na.rm = T))
    prov_levels <- bgcp %>% # Reorder levels by mean risk by privince 
      dplyr::select(sum, bgcp) %>%
      group_by(bgcp) %>%
      mutate(mean_of_mean = mean(sum, na.rm = T))
    levels <- unique(prov_levels$bgcp[order(prov_levels$mean_of_mean)])
    bgcp$bgcp <- factor(bgcp$bgcp, levels = levels, ordered = TRUE)
    df = table(bgcp$bgcp)
    df = as.data.frame(df)
    df = subset(df, Freq > 2)
    colnames(df)[1] = "bgcp"
    bgcp = merge(bgcp, df)
    df = bgcp[complete.cases(bgcp), ]
    df = df[,c("x", "y", "bgcp", "sum")]
    er = df
    er$source = "ERSST v4"; er$period = period[[i]]
    
    tas = rbind(hadi, cobe, er)
    
    tas_combined = rbind(tas_combined, tas)
    
    
  }
  
  # p = ggplot(tas_combined, aes(x = sum, y = UNIT, fill = period)) +
  #   geom_joy(scale = 5, alpha = 0.8, bandwidth = 0.05, size = 0.5) +
  #   theme_joy(grid = F) +
  #   scale_y_discrete(expand = c(0.01, 0)) + # will generally have to set the `expand` option
  #   # scale_x_continuous(limits = c(NA, 1),expand = c(0, 0)) +
  #   # scale_fill_cyclical(values = matlab.like(length(unique(tas_combined$UNIT))))+
  #   scale_fill_manual(values = matlab.like(4)) +
  #   # scale_fill_viridis_d()+
  #   facet_wrap(.~source) + 
  #   ylab(NULL) + xlab(NULL) +
  #   dark_theme_bw() +
  #   theme(axis.text.y = element_text(size = 10),
  #         axis.text.x = element_text(size = 10))
  
  
  tas_combined = subset(tas_combined, source %in% c("HadISST v1.1", "COBE v2")) # remove ERSST
  
  prov_levels <- subset(tas_combined, period %in% c("2010-2018")) %>% # Reorder levels by 2010-2018 
    dplyr::select(sum, bgcp) %>%
    group_by(bgcp) %>%
    mutate(mean_of_mean = mean(sum, na.rm = T))
  
  levels <- unique(prov_levels$bgcp[order(prov_levels$mean_of_mean)])
  tas_combined$bgcp <- factor(tas_combined$bgcp, levels = levels, ordered = TRUE)
  
  p = ggplot(tas_combined, aes(x = sum, y = bgcp, fill = bgcp)) +
    geom_joy(scale = 5, alpha = 0.8, bandwidth = 0.02, size = 0.3) +
    theme_joy(grid = F) +
    scale_y_discrete(expand = c(0.01, 0)) + # will generally have to set the `expand` option
    scale_x_continuous(limits = c(0, 1), expand = c(0.05, 0)) +
    scale_fill_cyclical(values = matlab.like(length(unique(df$bgcp))))+
    # scale_fill_viridis_c()+
    facet_wrap(.~ period, ncol = 4) +
    ylab(NULL) + xlab(NULL) +
    # dark_theme_bw() +
    theme(axis.text.y = element_text(size = 10),
          axis.text.x = element_text(size = 10, angle = 90, hjust = 1),
          legend.position = "none")

  p
  
  pdf(paste0("~/Desktop/Joy_bgcp.pdf"), height = 10, width = 10)
  print(p)
  dev.off()
  
}

rank_joy_bgcp()