library(raster)
library(colorRamps)
library(ggpubr)
library(rnaturalearth)
library(sf)
library(rgdal)
library(dplyr)
library(maps)
library(ggjoy)
library(rworldmap)
library(ggalt)
library(readr)
library(lwgeom)

rm(list = ls())

cutoff = c(0.95, 0.975)[1]

period = c("1980-1989", "1990-1999", "2000-2009", "2010-2018")

data = c("HadI", "COBE", "ER")

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

# meow <- readOGR(dsn = paste0("/Users/", Sys.info()[7], "/Downloads/MEOW"), layer = "meow_ecos")
# meow <- meow %>% st_as_sf()  
# 
# lme <- readOGR("/Users/ktanaka/Google Drive/Research/GIS/LME66/LMEs66.shp")
# lme <- rmapshaper::ms_simplify(lme, keep = 0.001, keep_shapes = F)
# lme <- lme %>% st_as_sf()  
# 
# eez <- readOGR(dsn = "/Users/ktanaka/clim_geo_disp/data/EEZ_land_union", layer = "EEZ_land_v2_201410")
# eez <- rmapshaper::ms_simplify(eez, keep = 0.001, keep_shapes = F)
# eez <- eez %>% st_as_sf()  
 
load('/Users/ktanaka/extreme_normalizations/eez_sf_dataframe_0.001.RData') 
load('/Users/ktanaka/extreme_normalizations/lme_sf_dataframe_0.001.RData') 
load('/Users/ktanaka/extreme_normalizations/meow_sf_dataframe.RData') 

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

ipcc_temp_4_cols <- c(rgb(153, 0, 2, maxColorValue = 255, alpha = 255),
                      rgb(196, 121, 0, maxColorValue = 255, alpha = 255),
                      rgb(112, 160, 205, maxColorValue = 255, alpha = 255),
                      rgb(0, 52, 102, maxColorValue = 255, alpha = 255))

rank_joy = function(region){
  
  # region = "eez"
  
  if (region == "meow"){
    shape = meow; shape$UNIT = shape$PROVINCE
  } 
  
  if (region == "lme") {
    shape = lme; shape$UNIT = shape$LME_NAME
  } 
  
  if (region == "eez") {
    shape = eez; shape$UNIT = shape$Country
  } 
  
  tas_combined = NULL
  
  for (i in 1:length(period)){
    
    # i = 1
    
    load(paste0(paste0("~/extreme_normalizations/results/HadI/SST_Anomalies_", period[[i]], "_", cutoff, ".RData")))
    anom = anom[, c(1:2, 15)]
    tas <- st_as_sf(x = anom, coords = c("x", "y"), crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" )
    summary(tas)
    # hadi <- st_intersection(tas, shape)
    hadi <- st_intersection(tas, st_make_valid(shape))
    # hadi <- st_intersection(tas, st_buffer(shape, 0))
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
    
    load(paste0(paste0("~/extreme_normalizations/results/COBE/SST_Anomalies_", period[[i]], "_", cutoff, ".RData")))
    anom = anom[, c(1:2, 15)]
    tas <- st_as_sf(x = anom, coords = c("x", "y"), crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" )
    # cobe <- st_intersection(tas, shape)
    cobe <- st_intersection(tas, st_make_valid(shape))
    # cobe <- st_intersection(tas, st_buffer(shape, 0))
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
    
    load(paste0(paste0("~/extreme_normalizations/results/ER/SST_Anomalies_", period[[i]], "_", cutoff, ".RData")))
    anom = anom[, c(1:2, 15)]
    tas <- st_as_sf(x = anom, coords = c("x", "y"), crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" )
    # er <- st_intersection(tas, shape)
    er <- st_intersection(tas, st_make_valid(shape))
    # er <- st_intersection(tas, st_buffer(shape, 0))
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
  
  # tas_combined = subset(tas_combined, source %in% c("HadISST v1.1", "COBE v2")) # remove ERSST
  
  if (region == "lme") {
    tas_combined_sub = subset(tas_combined, UNIT %in% c("Scotian Shelf", 
                                                        "California Current", 
                                                        "East Brazil Shelf"))
  } 
  
  if (region == "meow") {
    tas_combined_sub = subset(tas_combined, UNIT %in% c("Central Indian Ocean Islands", 
                                                        "Cold Temperate Northwest Pacific", 
                                                        "Galapagos")) 
  } 
  
  if (region == "eez") {
    
    country = tas_combined %>% group_by(UNIT) %>% summarise(m = median(sum), freq = n()) %>% filter(freq > 950)
    
    country = as.data.frame(country)
    country = country[, 1, drop=FALSE]
    country = as.character(country)
    
    tas_combined_sub = subset(tas_combined, UNIT %in% country$UNIT)
  } 
  
  pdf(paste0("~/Desktop/Joy_", region, "_selected_", cutoff, ".pdf"), height = 10, width = 12)

      tas_combined_sub = subset(tas_combined, UNIT %in% c("United States", 
                                                        "Greenland", 
                                                        "Japan")) 
  } 
  
  pdf(paste0("~/Desktop/Joy_", region, "_selected_", cutoff, ".pdf"), height = 5, width = 6)

  p = ggplot(tas_combined_sub) +
    geom_density(aes(x = sum, fill = period), alpha = 0.8, size = 0.01) +
    theme_pubr() +
    scale_x_continuous(
      limits = c(0, 1),
      expand = c(0.05, 0.05),
      breaks = c(0, 0.5, 1)) +
    scale_fill_manual(values = rev(ipcc_temp_4_cols), "") +
    facet_grid(source~UNIT, scales = "free") +
    # facet_grid(source~UNIT, scales = "free_y") +
    # facet_wrap(.~source+UNIT, scales = "free_y") +
    ylab(NULL) + xlab(NULL) +
    theme(
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_text(size = 10),
      legend.position = "bottom", 
      legend.justification = c(1,0))
  
  print(p)
  
  dev.off()
  
  if (region == "eez") {
    
    exclude_list = c("Area en controversia (disputed - Peruvian point of view)", 
                     "Area of overlap Australia/Indonesia", 
                     "Conflict zone China/Japan/Taiwan", 
                     "Conflict zone Japan/Russia",
                     "Conflict zone Japan/South Korea",
                     "Disputed Barbados/Trinidad & Tobago",
                     "Disputed Kenya/Somalia",
                     "Disputed Western Sahara/Mauritania",
                     "Joint development area Australia/East Timor",
                     "Joint regime Colombia/Jamaica",
                     "Joint regime Japan/Korea",
                     "Joint regime Nigeria/Sao Tome and Principe",
                     "Protected zone Australia/Papua New Guinea", 
                     "Spratly Islands", 
                     "Antarctica", 
                     "Gaza Strip")
    
    tas_combined = tas_combined[ ! tas_combined$UNIT %in% exclude_list, ]
    
    tas_combined$UNIT = gsub("&", "and", tas_combined$UNIT)
    tas_combined$UNIT = gsub(" Is.", " Islands", tas_combined$UNIT, fixed = T)
    tas_combined$UNIT = gsub(" I.", " Island", tas_combined$UNIT, fixed = T)
    tas_combined$UNIT = gsub("Congo, DRC", "DR Congo", tas_combined$UNIT, fixed = T)
    tas_combined$UNIT = gsub("Bonaire, Sint-Eustasius, Saba", "Netherlands", tas_combined$UNIT, fixed = T)
    tas_combined$UNIT = gsub("United States ", "US ", tas_combined$UNIT, fixed = T)
    tas_combined$UNIT = gsub("US Virgin Islands", "Virgin Islands, US", tas_combined$UNIT, fixed = T)
    tas_combined$UNIT = gsub("St. ", "Saint ", tas_combined$UNIT, fixed = T)
  } 
  
  prov_levels <- subset(tas_combined, period %in% c("2010-2018")) %>% # Reorder levels by 2010-2018 
    dplyr::select(sum, UNIT) %>%
    group_by(UNIT) %>%
    mutate(mean_of_mean = mean(sum, na.rm = T))
  
  levels <- unique(prov_levels$UNIT[order(prov_levels$mean_of_mean)])
  tas_combined$UNIT <- factor(tas_combined$UNIT, levels = levels, ordered = TRUE)
  
  ipcc_temp_expand = colorRampPalette(rev(ipcc_temp))
  ipcc_temp_expand = ipcc_temp_expand(length(unique(tas_combined$UNIT)))
  # invert_geom_defaults()
  
  summary = tas_combined %>% 
    group_by(UNIT, period) %>% 
    summarise_each(funs(mean, sd, se = sd(.)/sqrt(n())), sum)
  
  summary = as.data.frame(summary)
  summary = summary[,c('UNIT', 'period', 'mean', 'sd', 'se')]
  summary$UNIT = as.character(summary$UNIT)
  summary <- summary[order(summary$UNIT),]
  summary[,3:5] = round(summary[,3:5], 2)
  summary$UNIT[duplicated(summary$UNIT)] <- ""
  colnames(summary) = c("Unit", "Period", "Mean", "SD", "SE")

  write_csv(summary, paste0('~/Desktop/', region, "_", cutoff, ".csv"))
  
  p = ggplot(tas_combined, aes(x = sum, y = UNIT, fill = UNIT)) +
    geom_joy(scale = 3, bandwidth = 0.03, alpha = 0.8, size = 0.3) +
    theme_joy(grid = F) +
    scale_y_discrete(expand = c(0.01, 0)) + # will generally have to set the `expand` option
    scale_x_continuous(limits = c(0, 1), expand = c(0.1, 0.1), breaks = c(0, 0.5, 1)) +
    # scale_fill_cyclical(values = matlab.like(length(unique(df$UNIT))))+
    scale_fill_cyclical(values = ipcc_temp_expand)+
    facet_wrap(.~period, ncol = 4) +
    ylab(NULL) + xlab(NULL) +
    theme(axis.text.y = element_text(size = 10),
          legend.position = "none")
  
  pdf(paste0("~/Desktop/Joy_", region, "_", cutoff, ".pdf"), height = 20, width = 10)
  print(p)
  dev.off()
  
  return(tas_combined)
  
}
rank_joy_bgcp = function(){
  
  tas_combined = NULL
  
  for (i in 1:length(period)){
    
    # i = 1
    
    load("~/Dropbox (MBA)/PAPER Kisei heat extremes/data/biogeogr provinces/bgcp_raster_0.25.RData")
    load(paste0(paste0("~/extreme_normalizations/results/HadI/SST_Anomalies_", period[[i]], "_", cutoff, ".RData")))
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
    load(paste0(paste0("~/extreme_normalizations/results/COBE/SST_Anomalies_", period[[i]], "_", cutoff, ".RData")))
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
    load(paste0(paste0("~/extreme_normalizations/results/ER/SST_Anomalies_", period[[i]], "_", cutoff, ".RData")))
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
  
  # tas_combined = subset(tas_combined, source %in% c("HadISST v1.1", "COBE v2")) # remove ERSST
  
  tas_combined_sub = subset(tas_combined, bgcp %in% c("North Atlantic Drift", 
                                                      "Coastal Californian current", 
                                                      "Indian monsoon gyre"))
  
  # p = ggplot(tas_combined_sub, aes(x = sum, y = bgcp, fill = period)) +
  #   geom_joy(scale = 5, alpha = 0.8, size = 0.05) +
  #   theme_pubr() +
  #   scale_x_continuous(
  #     limits = c(0, 1),
  #     expand = c(0.05, 0.05),
  #     breaks = c(0, 0.5, 1)) +
  #   scale_fill_manual(values = rev(ipcc_temp_4_cols), "") +
  #   facet_grid(bgcp ~ source, scales = "free") +
  #   ylab(NULL) + xlab(NULL) +
  #   theme(axis.text.y = element_blank(),
  #         axis.ticks = element_blank(),
  #         axis.text.x = element_text(size = 10),
  #         legend.position = "bottom", 
  #         legend.justification = c(1,0))
  
  p = ggplot(tas_combined_sub) +
    geom_density(aes(x = sum, fill = period), alpha = 0.8, size = 0.01) +
    theme_pubr() +
    scale_x_continuous(
      limits = c(0, 1),
      expand = c(0.05, 0.05),
      breaks = c(0, 0.5, 1)) +
    scale_fill_manual(values = rev(ipcc_temp_4_cols), "") +
    facet_grid(source ~ bgcp, scales = "free") +
    ylab(NULL) + xlab(NULL) +
    theme(
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_text(size = 10),
      legend.position = "bottom", 
      legend.justification = c(1,0))

  pdf(paste0("~/Desktop/Joy_bgcp_selected_", cutoff, ".pdf"), height = 5, width = 6)
  print(p)
  dev.off()
  
  prov_levels <- subset(tas_combined, period %in% c("2010-2018")) %>% # Reorder levels by 2010-2018 
    dplyr::select(sum, bgcp) %>%
    group_by(bgcp) %>%
    mutate(mean_of_mean = mean(sum, na.rm = T))
  
  levels <- unique(prov_levels$bgcp[order(prov_levels$mean_of_mean)])
  tas_combined$bgcp <- factor(tas_combined$bgcp, levels = levels, ordered = TRUE)
  
  ipcc_temp_expand = colorRampPalette(rev(ipcc_temp))
  ipcc_temp_expand = ipcc_temp_expand(length(unique(tas_combined$bgcp)))
  
  summary = tas_combined %>% 
    group_by(bgcp, period) %>% 
    summarise_each(funs(mean, sd, se = sd(.)/sqrt(n())), sum)
  
  summary = as.data.frame(summary)
  summary = summary[,c('bgcp', 'period', 'mean', 'sd', 'se')]
  summary$bgcp = as.character(summary$bgcp)
  summary <- summary[order(summary$bgcp),]
  summary$bgcp[duplicated(summary$bgcp)] <- ""
  summary[,3:5] = round(summary[,3:5], 2)
  colnames(summary) = c("Unit", "Period", "Mean", "SD", "SE")
  write_csv(summary, paste0("~/Desktop/bgcp_", cutoff, ".csv"))
  
  p = tas_combined %>% 
    mutate(bgcp = gsub("\xca", "", bgcp)) %>% 
    ggplot(aes(x = sum, y = bgcp, fill = bgcp)) +
    geom_joy(scale = 3, bandwidth = 0.03, alpha = 0.8, size = 0.3) +
    theme_joy(grid = F) +
    scale_y_discrete(expand = c(0.01, 0)) + # will generally have to set the `expand` option
    scale_x_continuous(limits = c(0, 1), expand = c(0.1, 0.1), breaks = c(0, 0.5, 1)) +
    scale_fill_cyclical(values = ipcc_temp_expand)+
    facet_wrap(.~period, ncol = 4) +
    ylab(NULL) + xlab(NULL) +
    theme(axis.text.y = element_text(size = 10),
          legend.position = "none")
  
  p
  
  pdf(paste0("~/Desktop/Joy_bgcp_", cutoff, ".pdf"), height = 20, width = 10)
  print(p)
  dev.off()
  
  return(tas_combined)
  
}

lme = rank_joy("lme")
# meaw = rank_joy("meow")
eez = rank_joy("eez")
bgcp = rank_joy_bgcp()

df1 = lme %>% group_by(UNIT) %>% summarise(m = median(sum), freq = n())  %>% filter(freq > 100) %>% top_n(15, m)
df2 = lme %>% group_by(UNIT) %>% summarise(m = median(sum), freq = n())  %>% filter(freq > 100) %>% top_n(-15, m)
sub = rbind(df1, df2)
sub = as.vector(sub$UNIT)
lme_sub = subset(lme, UNIT %in% sub & period %in% c("2000-2009", "2010-2018"))
lme_sub = lme_sub %>% group_by(UNIT) %>% mutate(m = median(sum)) %>% arrange(UNIT, m)
lme_sub = lme_sub[,c("UNIT", "sum")]; lme_sub = as.data.frame(lme_sub); lme_sub = lme_sub[1:2]; lme_sub$class = "LME"

df1 = eez %>% group_by(UNIT) %>% summarise(m = median(sum), freq = n()) %>% filter(freq > 100) %>% top_n(15, m)
df2 = eez %>% group_by(UNIT) %>% summarise(m = median(sum), freq = n())  %>% filter(freq > 100) %>% top_n(-15, m)
sub = rbind(df1, df2)
sub = as.vector(sub$UNIT)
eez_sub = subset(eez, UNIT %in% sub & period %in% c("2000-2009", "2010-2018"))
# eez_sub = lme_sub %>% group_by(UNIT) %>% mutate(m = median(sum)) %>% arrange(UNIT, m)
eez_sub = eez_sub[,c("UNIT", "sum")]; eez_sub = as.data.frame(eez_sub); eez_sub = eez_sub[1:2]; eez_sub$class = "EEZ"

df1 = bgcp %>% group_by(bgcp) %>% summarise(m = median(sum), freq = n()) %>% filter(freq > 100) %>% top_n(15, m); df1 = df1 %>% top_n(15)
df2 = bgcp %>% group_by(bgcp) %>% summarise(m = median(sum), freq = n()) %>% filter(freq > 100) %>% top_n(-15, m)
sub = rbind(df1, df2)
sub = as.vector(sub$bgcp)
bgcp_sub = subset(bgcp, bgcp %in% sub & period %in% c("2000-2009", "2010-2018"))
bgcp$bgcp = as.character(bgcp$bgcp)
# bgcp_sub = bgcp_sub %>% group_by(bgcp) %>% mutate(m = median(sum)) %>% arrange(bgcp, m)
bgcp_sub = bgcp_sub[,c("bgcp", "sum")]; bgcp_sub = as.data.frame(bgcp_sub); colnames(bgcp_sub)[1] = "UNIT"; bgcp_sub$class = "BGCP"


ipcc_temp_expand = colorRampPalette(rev(ipcc_temp))
# ipcc_temp_expand = ipcc_temp_expand(60)
# ipcc_temp_expand = paste(ipcc_temp_expand, ipcc_temp_expand)

pdf(paste0("~/Desktop/LME_Joy_", cutoff, ".pdf"), width = 4.5, height = 6)
p = lme_sub %>% 
  mutate(UNIT = forcats::fct_reorder(UNIT, sum)) %>% 
  ggplot(aes(x = sum, y = UNIT, fill = UNIT)) +
  geom_joy(scale = 3, alpha = 0.8, size = 0.5) +
  theme_joy(grid = F) +
  scale_y_discrete(expand = c(0.05, 0)) + # will generally have to set the `expand` option
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0), breaks = c(0,0.5, 1)) +
  scale_fill_cyclical(values = ipcc_temp_expand(length(unique(lme_sub$UNIT))))+
  ylab(NULL) + xlab(NULL) +
  theme(axis.text.y = element_text(size = 10),
        legend.position = "none")
print(p)
dev.off()

pdf(paste0("~/Desktop/EEZ_Joy_", cutoff, ".pdf"), width = 3, height = 6)
p = eez_sub %>% 
  mutate(UNIT = forcats::fct_reorder(UNIT, sum)) %>% 
  ggplot(aes(x = sum, y = UNIT, fill = UNIT)) +
  geom_joy(scale = 3, alpha = 0.8, size = 0.5) +
  theme_joy(grid = F) +
  scale_y_discrete(expand = c(0.05, 0)) + # will generally have to set the `expand` option
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0), breaks = c(0,0.5, 1)) +
  scale_fill_cyclical(values = ipcc_temp_expand(length(unique(eez_sub$UNIT))))+
  ylab(NULL) + xlab(NULL) +
  theme(axis.text.y = element_text(size = 10),
        legend.position = "none")
print(p)
dev.off()

pdf(paste0("~/Desktop/BGCP_Joy_", cutoff, ".pdf"), width = 4.5, height = 6)
p = bgcp_sub %>%  
  mutate(UNIT = gsub("\xca", "", UNIT)) %>% 
  mutate(UNIT = forcats::fct_reorder(UNIT, sum)) %>% 
  ggplot(aes(x = sum, y = UNIT, fill = UNIT)) +
  geom_joy(scale = 3, alpha = 0.8, size = 0.5) +
  theme_joy(grid = F) +
  scale_y_discrete(expand = c(0.05, 0)) + # will generally have to set the `expand` option
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0), breaks = c(0,0.5, 1)) +
  scale_fill_cyclical(values = ipcc_temp_expand(length(unique(bgcp_sub$UNIT))))+
  ylab(NULL) + xlab(NULL) +
  theme(axis.text.y = element_text(size = 10),
        # axis.text.x = element_text(size = 10, angle = 90, hjust = 1),
        legend.position = "none")
print(p)
dev.off()

df = rbind(eez_sub, lme_sub, bgcp_sub)

pdf(paste0("~/Desktop/Joy_", cutoff, ".pdf"), width = 5, height = 5)
p = df %>%  
  mutate(UNIT = gsub("\xca", "", UNIT)) %>% 
  mutate(UNIT = forcats::fct_reorder(UNIT, sum)) %>% 
  ggplot(aes(x = sum, y = UNIT, fill = UNIT)) +
  geom_joy(scale = 3, alpha = 0.8, bandwidth = 0.02, size = 0.1) +
  theme_joy(grid = F) +
  facet_wrap(.~class, scales = "free") +
  scale_y_discrete(expand = c(0.01, 0)) + # will generally have to set the `expand` option
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
  scale_fill_manual(values = ipcc_temp_expand(90))+
  ylab(NULL) + xlab(NULL) +
  theme(axis.text.y = element_text(size = 10),
        legend.position = "none")
print(p)
dev.off()


