library(raster)
library(colorRamps)
library(ggpubr)
library(rnaturalearth)
library(sf)
library(rgdal)
library(dplyr)
library(maps)

rm(list = ls())

period = c("1980-1989", "1990-1999", "2000-2009", "2010-2018")

data = c("Hadl", "COBE")[2]

#Hadley or COBE
if (data == "Hadl") load("~/extreme_normalizations/data/Hadl_SST.RData")
if (data == "COBE") load("~/extreme_normalizations/data/COBE_SST.RData")

# e = extent(-140, -100, 30, 40)
# df = crop(df, e); rm(e)

# set baseline Jan 1870 - Dec 1929, 60 years
Baseline <- df[[1:720]] 
names(Baseline)

# pdf(paste0("~/extreme_normalizations/figs/", data, "_Climatology_1870-1929.pdf"), height = 10, width = 8.5)
# par(mfrow = c(2,1))
# plot(calc(Baseline, mean), col = matlab.like(100), axes = F, main = "Mean", zlim = c(-3, 33))
# map(add = T, lwd = 0.1, fill = T, col = "gray"); degAxis(1); degAxis(2, las = 1)
# plot(calc(Baseline, sd), col = matlab.like(100), axes = F, main = "SD", zlim = c(0,10))
# map(add = T, lwd = 0.1, fill = T, col = "gray"); degAxis(1); degAxis(2, las = 1)
# dev.off()

Baseline <- Baseline %>% rasterToPoints() %>% data.frame()

time_step = data.frame(names(df)) #look at time steps

for(p in length(period)){
  
  # period = period[[1]]
  
  # set target period
  if (period == "1980-1989") Target <- df[[1321:1440]] #Jan 1980 - Dec 1989
  if (period == "1990-1999") Target <- df[[1441:1560]] #Jan 1990 - Dec 1999
  if (period == "2000-2009") Target <- df[[1561:1680]] #Jan 2000 - Dec 2009
  if (period == "2010-2018") Target <- df[[1681:1788]] #Jan 2010 - Dec 2018
  
  Target <- Target %>% rasterToPoints() %>% data.frame()
  
  ll_anom = NULL
  
  # calculate anomalies at every lot/lon grid cell
  for (ll in 1:dim(Baseline)[1]) { 
    
    # ll = 1
    
    print(ll)
    
    monthly_anom = NULL
    
    for (m in 1:12) { # every month
      
      # m = 8
      
      interval = seq(m+2, dim(Baseline)[2], by = 12)
      
      baseline = Baseline[ll, c(interval)]
      baseline = t(baseline)
      baseline = as.data.frame(baseline)
      baseline = baseline[,1]
      
      q = quantile(baseline, prob = 0.975)
      # hist(baseline, breaks = 100, col = matlab.like(100), lty = "blank")
      # abline(v = q)
      
      interval = seq(m+2, dim(Target)[2], by = 12)
      
      present = Target[ll, c(interval)]
      present = t(present)
      present = as.data.frame(present)
      present = present[,1]
      sum = sum(q < present)
      
      monthly_anom = cbind(monthly_anom, sum)
      
    }
    
    ll_anom = rbind(ll_anom, monthly_anom)
    
  }
  
  colnames(ll_anom) = c("jan", "feb", "mar", "apr", "may", "jun",
                        "jul", "aug", "sep", "oct", "nov", "dec")
  
  anom = cbind(Target[1:2], ll_anom)
  
  anom$sum = rowSums(anom[3:14])
  
  if (period == "1980-1989") save(anom, file = "~/extreme_normalizations/results/SST_Anomalies_1980-1989.RData")
  if (period == "1990-1999") save(anom, file = "~/extreme_normalizations/results/SST_Anomalies_1990-1999.RData")
  if (period == "2000-2009") save(anom, file = "~/extreme_normalizations/results/SST_Anomalies_2000-2009.RData")
  if (period == "2010-2018") save(anom, file = "~/extreme_normalizations/results/SST_Anomalies_2010-2018.RData")
  
}

# range01 <- function(x){(x-min(x))/(max(x)-min(x))}
# anom$sum = range01(anom$sum)

world <- ne_countries(scale = "small", returnclass = "sf") #worldwide country polygon

p1 = ggplot(anom) + 
  geom_raster(aes(x, y, color = sum, fill = sum)) + 
  geom_sf(data = world, size = 0.15, color = "gray") +
  scale_fill_gradientn(colors = matlab.like(100), "") +
  scale_x_continuous(expand = c(-0.005, 0), "") +
  scale_y_continuous(expand = c(-0.005, 0), "") +
  # coord_sf(xlim = range(anom$x), ylim = range(anom$y)) +
  theme_pubr() +
  # ggtitle("Total number of months during 2000-2014 above 97th percentile over the 1870-1950 baseline period") + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(), 
        legend.position = "right",
        legend.justification = c(1, 0))

anom = anom[, c(1:2, 15)]

tas <- st_as_sf(x = anom, coords = c("x", "y"), crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" )
summary(tas)

shape <- readOGR(dsn = paste0("/Users/", Sys.info()[7], "/Downloads/MEOW"), layer = "meow_ecos")
shape <- shape %>% st_as_sf()  

# find intersections with disparity
tas <- st_intersection(tas, shape)
rm(shape)

tas_province <- tas %>% 
  group_by(PROVINCE) %>% 
  summarise(mean = mean(sum, na.rm = T),
            sd = sd(sum, na.rm = T), 
            n = n()) %>%
  mutate(se = sd/sqrt(n),
         lower.ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
         upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se)

p2 = tas_province %>% 
  mutate(unit = forcats::fct_reorder(PROVINCE, mean)) %>% 
  ggplot() +
  geom_segment(aes(
    color = mean, 
    x = unit, 
    xend = unit,
    y = lower.ci, 
    yend = upper.ci),
    size = 0.5) +
  geom_point(aes(
    color = mean,
    alpha = 0.5,
    x = unit,
    y = mean),
    size = 1) +
  geom_text(aes(unit, mean, label = unit), hjust = -0.08, vjust = 0, size = 3) +
  scale_colour_gradientn(colours = matlab.like(50)) + 
  xlab("") +
  ylab("") +
  # ylim(0,1) + 
  coord_flip() +
  theme_pubr() +
  theme(legend.position = "none", 
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

pdf(paste0("~/Desktop/SST_Anom_q97.5_", period, ".pdf"), height = 10, width = 8)
p1 + p2 + plot_layout(ncol = 1, heights = c(1,3))
dev.off()

tas = tas[,c(11, 5, 1)]

plot(tas, pch = 20, cex = 0.5)

tas$x = substr(tas$geometry, 3, 7)
xy = stringr::str_split_fixed(tas$geometry, ",", 2)
xy = gsub("^c\\(|\\)$", "", xy)

tas = cbind(xy, tas[,2:3])
tas = as.data.frame(tas[,1:4])

dfr <- rasterFromXYZ(tas[,c(1:2, 4)])  #Convert first two columns as lon-lat and third as value                
plot(dfr)

cumul_imp <-'/Users/ktanaka/Desktop/coral/MBA Risk Calc/TIFFs/New TIFFs/cumulative_impacts_50km_clip.tif' 
cumul_imp <- raster(cumul_imp)
crs(cumul_imp) <- '+proj=cea +lon_0=195 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs' 
cumul_imp <- projectRaster(cumul_imp, crs='+proj=longlat +lon_0=195 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs')
plot(cumul_imp)

dfr = resample(dfr, cumul_imp, method = "bilinear") 

d1 <-data.frame(rasterToPoints(dfr))
d2 <-data.frame(rasterToPoints(cumul_imp))

d = merge(d2, d1, all = T)
d = d[complete.cases(d$cumulative_impacts_50km_clip), ]
d = d[,c(1:2, 4)]

ggplot(d) + 
  geom_raster(aes(x, y, color = sum, fill = sum)) + 
  geom_sf(data = world, size = 0.15, color = "gray") +
  scale_fill_gradientn(colors = matlab.like(100), "") +
  scale_x_continuous(expand = c(-0.005, 0), "") +
  scale_y_continuous(expand = c(-0.005, 0), "") +
  coord_sf(xlim = range(anom$x), ylim = range(anom$y)) +
  theme_pubr() +
  # ggtitle("Total number of months during 2000-2014 above 97th percentile over the 1870-1950 baseline period") + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(), 
        legend.position = "right",
        legend.justification = c(1, 0))

clim_impacts = d

save(clim_impacts, file = paste0("~/cascade_coral/data/clim_impact_raster_", period, ".RData"))

plot(d2[,1:2], pch = ".", col = 2)
points(d[,1:2], pch = ".", col = 4)

clim_impacts <- rasterFromXYZ(d)  #Convert first two columns as lon-lat and third as value                

plot(cumul_imp, col = matlab.like(100)); maps::map(add = T)
plot(clim_impacts, col = matlab.like(100)); maps::map(add = T)

crs(clim_impacts) <- '+proj=cea +lon_0=195 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs' 
clim_impacts <- projectRaster(clim_impacts, crs='+proj=cea +lon_0=195 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs'  )

