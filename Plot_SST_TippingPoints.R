load("/Users/ktanaka/extreme_normalizations/results/HadI/SST_TippingPoints.RData"); hadi = yy_anom
load("/Users/ktanaka/extreme_normalizations/results/COBE/SST_TippingPoints.RData"); cobe = yy_anom
load("/Users/ktanaka/extreme_normalizations/results/ER/SST_TippingPoints.RData"); er = yy_anom

hadi$source = "HadISSTv1.1"
cobe$source = "COBEv2"
er$source = "ERSSTv4"


library(colorRamps)
pdf("~/Desktop/Tipping_Posint.pdf", height = 5, width = 6)
plot(cobe$year_sum, type = "o", axes = F, pch = 20, xlab = "", ylab = "", col = matlab.like(10)[8])
points(hadi$year_sum, type = "o", pch = 20, xlab = "", ylab = "", ylab = "", col = matlab.like(10)[9])
points(er$year_sum, type = "o", pch = 20, xlab = "", ylab = "", ylab = "", col = matlab.like(10)[10])
axis(1)
axis(2, las = 2, at = seq(0, 0.8, 0.1))
abline(h = 0.5, lty = 2)
legend("topleft", c("COBEv2", "HadISSTv1.1", "ERSSTv4"), lty = 1, col = c(matlab.like(10)[8], 
                                                                          matlab.like(10)[9], 
                                                                          matlab.like(10)[10]), 
       bty = "n")
dev.off()

df = rbind(hadi, cobe, er)


library(ggplot2)
ggplot(df, aes(year_sum, time, color = source)) + 
         geom_line()
