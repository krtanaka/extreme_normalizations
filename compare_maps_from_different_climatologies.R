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

anom = anom %>% group_by(x, y) %>% summarise(sum = mean(sum))
anom$sum = range01(anom$sum)

anom %>% ggplot(aes(x, y, fill = sum)) + geom_raster(interpolate = T) + scale_fill_viridis_c() + coord_fixed() + theme_minimal()
