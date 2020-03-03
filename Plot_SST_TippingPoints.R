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

df$threshold = ifelse(df$year_sum < 0.5, "Yes", "No")

cbPalette <- c(rgb(202, 0, 32, maxColorValue = 255, alpha = 255), 
               "white")

df$source = factor(df$source, levels = c("COBEv2","HadISSTv1.1","ERSSTv4"))

p1 = ggplot(data = df, aes(x = Time, y = year_sum)) +
  geom_rect(aes(xmin = Time, xmax = Time, ymin = -Inf, ymax = Inf, color = year_sum < 0.5),
            show.legend = FALSE) +
  geom_line() +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") + 
  scale_x_date(breaks = seq(as.Date("1900-01-01"), as.Date("2018-12-01"), by = "10 years"), 
               labels = scales::date_format("%Y"), expand = c(0.05, -9)) +
  scale_y_continuous(expand = c(0,0)) + 
  labs(x = "", y = "") +
  theme_pubr(I(10)) +
  facet_wrap(.~source, ncol = 3, scales = "fixed") +
  scale_colour_manual(values = cbPalette, "") + 
  geom_text(aes(label = source), x = -Inf, y = Inf, hjust = -0.1, vjust = 1.5, data = df %>% dplyr::distinct(source)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
        panel.border = element_blank(),
        strip.text = element_blank(),
        strip.background = element_blank())

pdf("~/Desktop/Normalized_Points.pdf", height = 3, width = 8)
print(p1)
dev.off()
