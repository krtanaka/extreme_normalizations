library(colorRamps)
library(ggplot2)
library(ggpubr)

rm(list = ls())

load("/Users/ktanaka/extreme_normalizations/results/HadI/SST_TippingPoints.RData"); hadi = yy_anom
load("/Users/ktanaka/extreme_normalizations/results/COBE/SST_TippingPoints.RData"); cobe = yy_anom
load("/Users/ktanaka/extreme_normalizations/results/ER/SST_TippingPoints.RData"); er = yy_anom

hadi$source = "HadISSTv1.1"
cobe$source = "COBEv2"
er$source = "ERSSTv4"


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

cbPalette <- c("#000000", "#56B4E9", "#E69F00")

invert_geom_defaults()

df$source = factor(df$source, levels = c("COBEv2","HadISSTv1.1","ERSSTv4"))

ggplot(data = df, aes(x = Time, y = year_sum, color = source, group = source)) +
  # geom_point(alpha = 0.8) +
  geom_line(size = 1.25, alpha = 0.75) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") + 
  labs(x = "", y = "Area Fraction") +
  scale_colour_manual(values = cbPalette, "") + 
  scale_x_date(breaks = seq(as.Date("1900-01-01"), as.Date("2018-12-01"), by = "10 years"), 
               labels = scales::date_format("%Y")) +
  theme_pubr(I(15)) +
  # facet_wrap(.~Month) +
  theme(legend.position = c(0.12, 0.95), 
        axis.text.x = element_text(angle = 90, hjust = 1))

pdf("~/Desktop/Time_Series__Month.pdf", height = 10, width = 10)
p1 = ggplot(data = df, aes(x = Time, y = year_sum, color = source, group = source)) +
  # geom_point(alpha = 0.8) +
  geom_line(size = 1, alpha = 0.8) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") + 
  labs(x = "", y = "prop of global area") +
  scale_colour_manual(values = cbPalette, "") + 
  scale_x_date(breaks = seq(as.Date("1900-01-01"), as.Date("2018-12-01"), by = "20 years"), 
               labels = scales::date_format("%Y")) +
  theme_pubr(I(20)) +
  facet_wrap(.~Month, ncol = 4) +
  # facet_grid(source ~ Month) +
  theme(legend.position = "top",
        legend.justification = c(1,0),
        # strip.background = element_blank(),
        axis.text.x = element_text(angle = 0, hjust = 1))
print(p1)
dev.off()

tipped_hadi = subset(df, source == "HadISSTv1.1" & year_sum > 0.5)
tipped_cobe = subset(df, source == "COBEv2" & year_sum > 0.5)

p1 = ggplot(data = df, aes(x = Time, y = year_sum, color = source, group = source)) +
  # geom_vline(data = tipped_cobe, aes(xintercept = Time), color = cbPalette[1], alpha = 0.1) +
  # geom_vline(data = tipped_hadi, aes(xintercept = Time), color = cbPalette[2], alpha = 0.1) +
  annotate("segment", x = tipped_cobe[,6], xend = tipped_cobe[,6], y = 0, yend = 0.02, color = cbPalette[1]) +
  annotate("segment", x = tipped_hadi[,6], xend = tipped_hadi[,6], y = 0.03, yend = 0.05, color = cbPalette[2]) +
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
  annotate("text", x = as.Date(paste(1915, 01, 01, sep = "-")), y = 0.5, vjust = -1, label = "50% threshold", color = "black") + 
  labs(x = "", y = "Proportion of Global Ocean") +
  scale_colour_manual(values = cbPalette, "") + 
  scale_x_date(breaks = seq(as.Date("1900-01-01"), as.Date("2018-12-01"), by = "20 years"), 
               labels = scales::date_format("%Y")) +
  theme_pubr(I(15)) +
  theme(legend.position = c(0.1, 0.95))

pdf("~/Desktop/Time_Series_V3.pdf", height = 6, width = 12)
print(p1)
dev.off()

ipcc_temp_4_cols <- c(rgb(153, 0, 2, maxColorValue = 255, alpha = 255),
                      rgb(196, 121, 0, maxColorValue = 255, alpha = 255),
                      # rgb(112, 160, 205, maxColorValue = 255, alpha = 255),
                      rgb(0, 52, 102, maxColorValue = 255, alpha = 255))

p1 = ggplot(data = df, aes(x = Time, y = year_sum, group = source, color = source)) +
  geom_vline(data = tipped_cobe, aes(xintercept = Time), color = ipcc_temp_4_cols[1], alpha = 0.2) +
  geom_vline(data = tipped_hadi, aes(xintercept = Time), color = ipcc_temp_4_cols[2], alpha = 0.2) +
  geom_line() +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") + 
  scale_x_date(breaks = seq(as.Date("1900-01-01"), as.Date("2018-12-01"), by = "10 years"), 
               labels = scales::date_format("%Y"), expand = c(0.01, -100)) +
  scale_y_continuous(expand = c(0,0)) + 
  labs(x = "", y = "Area Fraction") +
  theme_pubr(I(10)) +
  facet_wrap(.~source, ncol = 1) +
  scale_colour_manual(values = ipcc_temp_4_cols, "") + 
  geom_text(aes(label = source), x = -Inf, y = Inf, hjust = -0.1, vjust = 1.5, data = df %>% dplyr::distinct(source)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
        strip.text = element_blank(),
        strip.background = element_blank(),
        legend.position = "none")

pdf("~/Desktop/Time_Series_V4.pdf", height = 6, width = 5)
print(p1)
dev.off()
