load("/Users/ktanaka/extreme_normalizations/results/HadI/SST_TippingPoints.RData"); hadi = yy_anom
load("/Users/ktanaka/extreme_normalizations/results/COBE/SST_TippingPoints.RData"); cobe = yy_anom
load("/Users/ktanaka/extreme_normalizations/results/ER/SST_TippingPoints.RData"); er = yy_anom

hadi$source = "HadISSTv1.1"
cobe$source = "COBEv2"
er$source = "ERSSTv4"

library(colorRamps)
pdf("~/Desktop/Tipping_Posint.pdf", height = 5, width = 6)
par(mar = c(2,3,1,2))
plot(cobe$year_sum, type = "o", axes = F, pch = 20, xlab = "", ylab = "", col = matlab.like(10)[8])
points(hadi$year_sum, type = "o", pch = 20, xlab = "", ylab = "", ylab = "", col = matlab.like(10)[9])
points(er$year_sum, type = "o", pch = 20, xlab = "", ylab = "", ylab = "", col = matlab.like(10)[10])
axis(1)
axis(2, las = 2, at = seq(0, 0.8, 0.1))
abline(h = 0.5, lty = 2)
legend("topleft", c("COBEv2", "HadISSTv1.1", "ERSSTv4"), 
       lty = 1, col = c(matlab.like(10)[8], 
                        matlab.like(10)[9], 
                        matlab.like(10)[10]), lwd = 2,
       bty = "n")
dev.off()

df = rbind(hadi, cobe, er)
rownames(df) <- c()

df = tidyr::separate(df, time, c("Year", "Month"), sep = "-")

df$Month <- sprintf("%02d", as.numeric(df$Month))
df$Day = 01
df$Day <- sprintf("%02d", as.numeric(df$Day))
df$Time = paste(df$Year, df$Month, df$Day, sep = "-")
df$Time = as.Date(df$Time)

cobe_date = paste(subset(df, source == "COBEv2" & year_sum > 0.5)[1,2:3], collapse = "")
hadi_date = paste(subset(df, source == "HadISSTv1.1" & year_sum > 0.5)[1,2:3], collapse = "")
er_date = paste(subset(df, source == "ERSSTv4" & year_sum > 0.5)[1,2:3], collapse = "")

library(ggplot2)
library(ggdark)
library(ggpubr)

pdf("~/Desktop/Tipping_Points_Year_Month.pdf", height = 8, width = 16)
p1 = ggplot(data = df, aes(x = Time, y = year_sum, color = source, group = source)) +
  # geom_point(alpha = 0.8) +
  geom_line() +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") + 
  labs(x = "", y = "") +
  scale_color_manual(values = heat.colors(3), "") +
  # scale_color_viridis_d() + 
  scale_x_date(breaks = seq(as.Date("1900-01-01"), as.Date("2018-12-01"), by="10 years"), 
               labels = scales::date_format("%Y")) +
  # dark_theme_classic(I(20)) +
  theme_pubr() +
  # facet_wrap(.~Month) +
  theme(legend.position = c(0.15, 0.95), 
        axis.text.x = element_text(angle = 90, hjust = 1))

p2 = ggplot(data = df, aes(x = Time, y = year_sum, color = source, group = source)) +
  # geom_point(alpha = 0.8) +
  geom_line() +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") + 
  labs(x = "", y = "") +
  scale_color_manual(values = heat.colors(3), "") +
  scale_x_date(breaks = seq(as.Date("1900-01-01"), as.Date("2018-12-01"), by = "25 years"), 
               labels = scales::date_format("%Y")) +
  dark_theme_classic(I(20)) +
  # theme_pubr() +
  facet_wrap(.~Month) +
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 90, hjust = 1))

cowplot::plot_grid(p1, p2)

dev.off()

