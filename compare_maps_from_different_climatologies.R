#historical extreme from 1870-1919
cutoff = c(0.95, 0.975)[1]
load(paste0("/Users/ktanaka/extreme_normalizations/results/HadI/SST_Anomalies_1980-1989_", cutoff, ".RData")); hadi1 = anom; hadi1$source = "HadISST v1.1"; hadi1$period = "1980-1989"
load(paste0("/Users/ktanaka/extreme_normalizations/results/HadI/SST_Anomalies_1990-1999_", cutoff, ".RData")); hadi2 = anom; hadi2$source = "HadISST v1.1"; hadi2$period = "1990-1999"
load(paste0("/Users/ktanaka/extreme_normalizations/results/HadI/SST_Anomalies_2000-2009_", cutoff, ".RData")); hadi3 = anom; hadi3$source = "HadISST v1.1"; hadi3$period = "2000-2009"
load(paste0("/Users/ktanaka/extreme_normalizations/results/HadI/SST_Anomalies_2010-2019_", cutoff, ".RData")); hadi4 = anom; hadi4$source = "HadISST v1.1"; hadi4$period = "2010-2019"
load(paste0("/Users/ktanaka/extreme_normalizations/results/COBE/SST_Anomalies_1980-1989_", cutoff, ".RData")); cobe1 = anom; cobe1$source = "COBE v2"; cobe1$period = "1980-1989"
load(paste0("/Users/ktanaka/extreme_normalizations/results/COBE/SST_Anomalies_1990-1999_", cutoff, ".RData")); cobe2 = anom; cobe2$source = "COBE v2"; cobe2$period = "1990-1999"
load(paste0("/Users/ktanaka/extreme_normalizations/results/COBE/SST_Anomalies_2000-2009_", cutoff, ".RData")); cobe3 = anom; cobe3$source = "COBE v2"; cobe3$period = "2000-2009"
load(paste0("/Users/ktanaka/extreme_normalizations/results/COBE/SST_Anomalies_2010-2019_", cutoff, ".RData")); cobe4 = anom; cobe4$source = "COBE v2"; cobe4$period = "2010-2019"

a1 = rbind(hadi1, hadi2, hadi3, hadi4, cobe1, cobe2, cobe3, cobe4)

#historical extreme from 1956-2005
cutoff = c(0.95, 0.975)[1]
load(paste0("/Users/ktanaka/extreme_normalizations/results/HadI/1956-2005_baseline/SST_Anomalies_1980-1989_", cutoff, ".RData")); hadi1 = anom; hadi1$source = "HadISST v1.1"; hadi1$period = "1980-1989"
load(paste0("/Users/ktanaka/extreme_normalizations/results/HadI/1956-2005_baseline/SST_Anomalies_1990-1999_", cutoff, ".RData")); hadi2 = anom; hadi2$source = "HadISST v1.1"; hadi2$period = "1990-1999"
load(paste0("/Users/ktanaka/extreme_normalizations/results/HadI/1956-2005_baseline/SST_Anomalies_2000-2009_", cutoff, ".RData")); hadi3 = anom; hadi3$source = "HadISST v1.1"; hadi3$period = "2000-2009"
load(paste0("/Users/ktanaka/extreme_normalizations/results/HadI/1956-2005_baseline/SST_Anomalies_2010-2019_", cutoff, ".RData")); hadi4 = anom; hadi4$source = "HadISST v1.1"; hadi4$period = "2010-2019"
load(paste0("/Users/ktanaka/extreme_normalizations/results/COBE/1956-2005_baseline/SST_Anomalies_1980-1989_", cutoff, ".RData")); cobe1 = anom; cobe1$source = "COBE v2"; cobe1$period = "1980-1989"
load(paste0("/Users/ktanaka/extreme_normalizations/results/COBE/1956-2005_baseline/SST_Anomalies_1990-1999_", cutoff, ".RData")); cobe2 = anom; cobe2$source = "COBE v2"; cobe2$period = "1990-1999"
load(paste0("/Users/ktanaka/extreme_normalizations/results/COBE/1956-2005_baseline/SST_Anomalies_2000-2009_", cutoff, ".RData")); cobe3 = anom; cobe3$source = "COBE v2"; cobe3$period = "2000-2009"
load(paste0("/Users/ktanaka/extreme_normalizations/results/COBE/1956-2005_baseline/SST_Anomalies_2010-2019_", cutoff, ".RData")); cobe4 = anom; cobe4$source = "COBE v2"; cobe4$period = "2010-2019"

a2 = rbind(hadi1, hadi2, hadi3, hadi4, cobe1, cobe2, cobe3, cobe4)

rm(hadi1, hadi2, hadi3, hadi4, cobe1, cobe2, cobe3, cobe4)

a1 = a1 %>% group_by(x, y) %>% summarise(sum = mean(sum))
a1$sum = range01(a1$sum)

a2 = a2 %>% group_by(x, y) %>% summarise(sum = mean(sum))
a2$sum = range01(a2$sum)

a1 %>% ggplot(aes(x, y, fill = sum)) + geom_raster(interpolate = T) + scale_fill_viridis_c() + coord_fixed() + theme_minimal()
a2 %>% ggplot(aes(x, y, fill = sum)) + geom_raster(interpolate = T) + scale_fill_viridis_c() + coord_fixed() + theme_minimal()

a1$time = "based on 1870-1919 climatology"
a2$time = "based on 1956-2005 climatology"

anom = rbind(a1, a2)

anom$time = factor(anom$time, levels = c("based on 1870-1919 climatology", "based on 1956-2005 climatology"))


anom %>% ggplot(aes(x, y, fill = sum)) + 
  geom_raster(interpolate = T) +
  geom_map(data = world, map = world, aes(x = long, y = lat, map_id = id),
           color = "gray20", fill = "gray20", size = 0.001) + 
  scale_fill_gradientn(colors = rev(ipcc_temp), "", limits = c(0,1), breaks = c(0,0.5,1)) +
  coord_fixed() + theme_minimal(I(15)) + 
  ylab("") + xlab("") + 
  facet_wrap(.~time, ncol = 1) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "right")
