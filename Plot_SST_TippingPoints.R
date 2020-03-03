rm(list = ls())

load("/Users/ktanaka/extreme_normalizations/results/HadI/SST_TippingPoints.RData"); hadi = yy_anom
load("/Users/ktanaka/extreme_normalizations/results/COBE/SST_TippingPoints.RData"); cobe = yy_anom
load("/Users/ktanaka/extreme_normalizations/results/ER/SST_TippingPoints.RData"); er = yy_anom

hadi$source = "HadISSTv1.1"
cobe$source = "COBEv2"
er$source = "ERSSTv4"

library(colorRamps)

# pdf("~/Desktop/Tipping_Posint.pdf", height = 5, width = 6)
# par(mar = c(2,3,1,2))
# plot(cobe$year_sum, type = "o", axes = F, pch = 20, xlab = "", ylab = "", col = matlab.like(10)[8])
# points(hadi$year_sum, type = "o", pch = 20, xlab = "", ylab = "", ylab = "", col = matlab.like(10)[9])
# points(er$year_sum, type = "o", pch = 20, xlab = "", ylab = "", ylab = "", col = matlab.like(10)[10])
# axis(1)
# axis(2, las = 2, at = seq(0, 0.8, 0.1))
# abline(h = 0.5, lty = 2)
# legend("topleft", c("COBEv2", "HadISSTv1.1", "ERSSTv4"), 
#        lty = 1, col = c(matlab.like(10)[8], 
#                         matlab.like(10)[9], 
#                         matlab.like(10)[10]), lwd = 2,
#        bty = "n")
# dev.off()

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

# cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# cbPalette <- c(rgb(153, 0, 2, maxColorValue = 255, alpha = 255),
#                rgb(196, 121, 43, maxColorValue = 255, alpha = 255),
#                rgb(0, 52, 102, maxColorValue = 255, alpha = 255))

invert_geom_defaults()

pdf("~/Desktop/Tipping_Points_Year.pdf", height = 6, width = 8)

p1 = ggplot(data = df, aes(x = Time, y = year_sum, color = source, group = source)) +
  # geom_point(alpha = 0.8) +
  geom_line(size = 1.25, alpha = 0.75) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") + 
  labs(x = "", y = "") +
  scale_colour_manual(values = cbPalette, "") + 
  scale_x_date(breaks = seq(as.Date("1900-01-01"), as.Date("2018-12-01"), by = "10 years"), 
               labels = scales::date_format("%Y")) +
  theme_pubr(I(15)) +
  # facet_wrap(.~Month) +
  theme(legend.position = c(0.12, 0.95), 
        axis.text.x = element_text(angle = 90, hjust = 1))

print(p1)

dev.off()

pdf("~/Desktop/Tipping_Points_Month.pdf", height = 5, width = 25)

p2 = ggplot(data = df, aes(x = Time, y = year_sum, color = source, group = source)) +
  # geom_point(alpha = 0.8) +
  geom_line(size = 1, alpha = 0.75) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") + 
  labs(x = "", y = "") +
  scale_colour_manual(values = cbPalette, "") + 
  scale_x_date(breaks = seq(as.Date("1900-01-01"), as.Date("2018-12-01"), by = "20 years"), 
               labels = scales::date_format("%Y")) +
  # dark_theme_classic(I(20)) +
  theme_bw(I(20)) +
  facet_wrap(.~Month, ncol = 12) +
  # facet_grid(source ~ Month) +
  theme(legend.position = "bottom",
        legend.justification = c(1,0),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))

# cowplot::plot_grid(p1, p2, ncol = 1)
print(p2)

dev.off()

# df$threshold = ifelse(df$year_sum < 0.5, "Yes", "No")

# cbPalette <- c(rgb(202, 0, 32, maxColorValue = 255, alpha = 255), 
#                "white")

df$source = factor(df$source, levels = c("COBEv2","HadISSTv1.1","ERSSTv4"))

tipped_hadi = subset(df, source == "HadISSTv1.1" & year_sum > 0.5)
tipped_cobe = subset(df, source == "COBEv2" & year_sum > 0.5)

# cbPalette <- c(rgb(0, 0, 0, maxColorValue = 255, alpha = 255),
#                rgb(112, 160, 205, maxColorValue = 255, alpha = 255),
#                rgb(196, 121, 0, maxColorValue = 255, alpha = 255))

p1 = ggplot(data = df, aes(x = Time, y = year_sum, color = source, group = source, label = as.character(Time))) +
  geom_vline(data = tipped_cobe, aes(xintercept = Time), color = cbPalette[1], alpha = 0.1) +
  geom_vline(data = tipped_hadi, aes(xintercept = Time), color = cbPalette[2], alpha = 0.1) +
  # annotate("segment", x = tipped_cobe[,6], xend = tipped_cobe[,6], y = 0, yend = 0.02, color = cbPalette[1]) +
  # annotate("segment", x = tipped_hadi[,6], xend = tipped_hadi[,6], y = 0.03, yend = 0.05, color = cbPalette[2]) +
  annotate("segment", x = tipped_cobe[1,6], xend = tipped_cobe[1,6], y = 0.55, yend = 0.73, color = cbPalette[1]) +
  annotate("segment", x = tipped_hadi[1,6], xend = tipped_hadi[1,6], y = 0.62, yend = 0.77, color = cbPalette[2]) +
  annotate("segment", x = as.Date(paste(2014, 04, 01, sep = "-")), xend = as.Date(paste(2014, 04, 01, sep = "-")), y = 0.75, yend = 0.84, 
           color = cbPalette[1]) +
  annotate("text", x = as.Date(paste(2014, 04, 01, sep = "-")), y = 0.86, hjust = 1, label = "2014-01, point of no return", color = cbPalette[1]) + 
  geom_text(data = tipped_cobe[1,], 
            aes(x = Time, label = paste0(Year, "-", Month), y = 0.75), 
            colour = cbPalette[1], angle = 0, hjust = 1, text = element_text(size = 11)) +
  geom_text(data = tipped_hadi[1,], 
            aes(x = Time, label = paste0(Year, "-", Month), y = 0.8), 
            colour = cbPalette[2], angle = 0, hjust = 1,text = element_text(size = 11)) +
  geom_line(size = 1, alpha = 0.75) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") + 
  annotate("text", x = as.Date(paste(1910, 01, 01, sep = "-")), y = 0.5, vjust = -1, label = "50% threshold", color = "black") + 
  labs(x = "", y = "Area Fraction") +
  scale_colour_manual(values = cbPalette, "") + 
  scale_x_date(breaks = seq(as.Date("1900-01-01"), as.Date("2018-12-01"), by = "20 years"), 
               labels = scales::date_format("%Y")) +
  theme_pubr(I(15)) +
  theme(legend.position = c(0.15, 0.95), 
        axis.text.x = element_text(angle = 90, hjust = 1))

pdf("~/Desktop/Normalized_Points.pdf", height = 5, width = 7)
print(p1)
dev.off()
