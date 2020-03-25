rm(list = ls())

lme <- readOGR("/Users/Kisei/Google Drive/Research/GIS/LME66/LMEs66.shp")
lme <- rmapshaper::ms_simplify(lme, keep = 0.01, keep_shapes = F)
lme <- lme %>% st_as_sf()  

png("/Users/Kisei/Desktop/LME.png", units = "in", res = 100, height = 10, width = 10)
lme %>% ggplot() + 
  geom_sf(aes(group = LME_NAME, fill = LME_NAME), color = "NA", show.legend = T) + 
  scale_fill_viridis_d("") + 
  theme_pubr() + 
  geom_map(data = world, map = world, aes(x = long, y = lat, map_id = id),color = "gray60", fill = "gray40", size = 0.001) + 
  guides(fill = guide_legend(nrow = 8), "") + 
  theme(legend.position = "bottom")
dev.off()

eez <- readOGR(dsn = "/Users/Kisei/clim_geo_disp/data/EEZ_land_union", layer = "EEZ_land_v2_201410")
eez <- rmapshaper::ms_simplify(eez, keep = 0.01, keep_shapes = F)
eez <- eez %>% st_as_sf() 

png("/Users/Kisei/Desktop/EEZ.png", units = "in", res = 100, height = 6, width = 10)
eez %>% ggplot() + 
  geom_sf(aes(group = Country, fill = Country), color = "NA", show.legend = T) + 
  scale_fill_viridis_d("") + 
  geom_map(data = world, map = world, aes(x = long, y = lat, map_id = id), color = "gray60", fill = "gray40", size = 0.001) + 
  guides(fill = guide_legend(nrow = 8), "") + 
  theme(legend.position = "none")
dev.off()


load("/Users/Kisei/Dropbox/PAPER Kisei heat extremes/data/biogeogr provinces/bgcp_raster_0.25.RData")
load("/Users/Kisei/extreme_normalizations/results/HadI/SST_Anomalies_1980-1989_0.95.RData")
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

bgcp_names <- read_csv("/Users/Kisei/Dropbox/PAPER Kisei heat extremes/data/biogeogr provinces/NAME_BGCP_2019_REYGONDEAU.csv")
bgcp_names = bgcp_names[,c("NAME", "BGCP")]
colnames(bgcp_names) = c("name", "bgcp")
bgcp = merge(bgcp, bgcp_names)
bgcp$bgcp = bgcp$name

png("/Users/Kisei/Desktop/BGCP.png", units = "in", res = 100, height = 6, width = 10)
p = bgcp %>% 
  ggplot() + 
  geom_raster(aes(x = x, y = y, fill = bgcp)) +
  geom_map(data = world, map = world, aes(x = long, y = lat, map_id = id),
           color = "gray20", fill = "gray20", size = 0.001) + 
  # scale_fill_viridis_d() +
  coord_fixed() + 
  theme_pubr(I(20)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none")
print(p)
dev.off()
