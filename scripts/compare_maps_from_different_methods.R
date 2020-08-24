rm(list = ls())

library(dplyr)
library(ggthemes)
library(precrec)
library(ggplot2)

world <- fortify(rworldmap::getMap())
range01 <- function(x){(x-min(x))/(max(x)-min(x))}

#IPCC style SST anomalies
load("~/extreme_normalizations/results/COBE/anomalies_2019_ipcc.RData"); cobe_ipcc = anom; cobe_ipcc$data = "COBE"; cobe_ipcc$sum = rowMeans(cobe_ipcc[3:14])
load("~/extreme_normalizations/results/HadI/anomalies_2019_ipcc.RData"); hadi_ipcc = anom; hadi_ipcc$data = "HadI"; hadi_ipcc$sum = rowMeans(hadi_ipcc[3:14])
anom_ipcc = rbind(cobe_ipcc, hadi_ipcc) %>% dplyr::select(x, y, sum) %>% group_by(x, y) %>% summarise(anom = mean(sum))
quantile(anom_ipcc$anom, 0.999)

#Extreme Index for 2019
load("~/extreme_normalizations/results/HadI/anomalies_2019_0.95.rdata"); hadi_extreme = anom; hadi_extreme$data = "HadI"
load("~/extreme_normalizations/results/COBE/anomalies_2019_0.95.rdata"); cobe_extreme = anom; cobe_extreme$data = "COBE"
anom_extreme = rbind(cobe_extreme, hadi_extreme) %>% dplyr::select(x, y, sum) %>% group_by(x, y) %>% summarise(anom = mean(sum))
anom_extreme$anom = range01(anom_extreme$anom)
quantile(anom_extreme$anom, 0.999)

#IPCC colors
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

# ipcc_precip <- c(rgb(84, 48, 5, maxColorValue = 255, alpha = 255),
#                  rgb(140, 81, 10, maxColorValue = 255, alpha = 255),
#                  rgb(191, 129, 45, maxColorValue = 255, alpha = 255),
#                  rgb(223, 194, 125, maxColorValue = 255, alpha = 255),
#                  rgb(246, 232, 195, maxColorValue = 255, alpha = 255),
#                  rgb(245, 245, 245, maxColorValue = 255, alpha = 255),
#                  rgb(199, 234, 229, maxColorValue = 255, alpha = 255),
#                  rgb(128, 205, 193, maxColorValue = 255, alpha = 255),
#                  rgb(53, 151, 143, maxColorValue = 255, alpha = 255),
#                  rgb(1, 102, 94, maxColorValue = 255, alpha = 255),
#                  rgb(0, 60, 48, maxColorValue = 255, alpha = 255))

p1 = anom_ipcc %>% ggplot(aes(x, y, fill = anom)) +  
  geom_tile() + 
  geom_point(data = subset(anom_ipcc, anom >= 1.773027), aes(x, y), color = 5, shape = 4, alpha = 0.5) + 
  geom_map(data = world, map = world, aes(x = long, y = lat, map_id = id),
           color = "gray20", fill = "gray20", size = 0.001) +
  scale_fill_gradientn(colors = rev(ipcc_temp), "") +
  scale_x_continuous(expand = c(-0.005, 0), "") +
  scale_y_continuous(expand = c(-0.005, 0), "") +
  coord_fixed() + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "right",
        legend.justification = c(1,0)) + 
  labs(tag = "a")

p2 = anom_extreme %>% ggplot(aes(x, y, fill = anom)) +  
  geom_tile() + 
  geom_point(data = subset(anom_extreme, anom >= 1), aes(x, y), color = 5, shape = 4, alpha = 0.5) + 
  geom_map(data = world, map = world, aes(x = long, y = lat, map_id = id),
           color = "gray20", fill = "gray20", size = 0.001) +
  scale_fill_gradientn(colors = rev(ipcc_temp), "") +
  scale_x_continuous(expand = c(-0.005, 0), "") +
  scale_y_continuous(expand = c(-0.005, 0), "") +
  coord_fixed() + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "right",
        legend.justification = c(1,0)) + 
  labs(tag = "b")

pdf(paste0("~/Desktop/Fig4_", Sys.Date(), ".pdf"), height = 6, width = 6)

p1/p2

dev.off()