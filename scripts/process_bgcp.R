rm(list = ls())

# bgcp <- read_csv("BGCP_2019_REYGONDEAU.csv")
# qplot(bgcp$Longitude, bgcp$Latitude, color = bgcp$BGCP)
# x <- raster(xmn = -180, xmx = 180, ymn = -90, ymx = 90, res = 0.25, crs = "+proj=longlat +datum=WGS84")
# bgcp <- rasterize(bgcp[, c('Longitude', 'Latitude')], x, bgcp[, 'BGCP'], fun = mean)
# plot(bgcp)
# save(bgcp, file = "bgcp_raster_0.25.RData")

load("~/Dropbox (MBA)/PAPER Kisei heat extremes/data/biogeogr provinces/bgcp_raster_0.25.RData")
load("~/extreme_normalizations/results/HadI/SST_Anomalies_1980-1989.RData")
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

ggplot(df, aes(x = sum, y = bgcp, fill = bgcp)) +
  geom_joy(scale = 5, alpha = 0.8, size = 0.5) +
  theme_joy(grid = F) +
  scale_y_discrete(expand = c(0.01, 0)) + # will generally have to set the `expand` option
  scale_x_continuous(limits = c(NA, 1),expand = c(0, 0)) +
  scale_fill_cyclical(values = matlab.like(length(unique(df$bgcp))))+
  # scale_fill_viridis_c()+
  # facet_wrap(.~source) + 
  ylab(NULL) + xlab(NULL) +
  # dark_theme_bw() +
  theme(axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10), 
        legend.position = "none")
