rm(list = ls())

period = c("1980-1989", "1990-1999", "2000-2009", "2010-2018")[2]

data = c("Hadl", "COBE")[1]

load(paste0("~/extreme_normalizations/results/", data, "/SST_Anomalies_", period, ".RData"))

world <- ne_countries(scale = "small", returnclass = "sf") 

pdf(paste0("~/extreme_normalizations/results/", data, "_SST_Anomalies_", period, ".pdf"), height = 5, width = 10)

p = ggplot(anom) + 
  geom_raster(aes(x, y, color = sum, fill = sum)) + 
  geom_sf(data = world, size = 0.15, color = "gray") +
  scale_fill_gradientn(colors = matlab.like(100), "", limits = c(0,120)) +
  scale_x_continuous(expand = c(-0.005, 0), "") +
  scale_y_continuous(expand = c(-0.005, 0), "") +
  coord_sf(xlim = range(anom$x), ylim = range(anom$y)) +
  theme_pubr() +
  # ggtitle("Total number of months during 2000-2014 above 97th percentile over the 1870-1950 baseline period") + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(), 
        legend.position = "right",
        legend.justification = c(1, 0))

print(p)
dev.off()

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

p = tas_province %>% 
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
    # alpha = 0.5,
    x = unit,
    y = mean),
    size = 1) +
  # geom_text(aes(unit, mean, label = unit), hjust = -0.08, vjust = 0, size = 3) +
  scale_colour_gradientn(colours = matlab.like(50)) + 
  xlab("") +
  ylab("") +
  # ylim(0,1) + 
  coord_flip() +
  theme_pubr() +
  theme(legend.position = "none", 
        # axis.title.y = element_blank(),
        # axis.text.y = element_blank(),
        # axis.ticks.y = element_blank()
        )

# pdf(paste0("~/Desktop/SST_Anom_q97.5_", period, ".pdf"), height = 10, width = 8)
p
# dev.off()

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

