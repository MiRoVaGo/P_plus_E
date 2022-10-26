#Required Libraries
library(ggplot2)
library(dplyr)
library(data.table)
library(readr)
library(tablesgg)
library(tables)
#Load Data
global_20cr <- read_csv('./../data//20cr.csv') %>% as.data.table()
global_era20 <- read_csv('./../data/era20.csv') %>% as.data.table()
global_era5 <- read_csv('./../data/era5.csv') %>% as.data.table()
global_ncep <- read_csv('./../data/ncep.csv') %>% as.data.table()
#Plot All
p00 <- ggplot(global_20cr, aes(x = mean(E), y = mean(P))) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#377eb8", size = 2) +
  geom_abline(slope = -1, intercept = 2270, linetype = "dashed", color = "#4daf4a", size = 2) +
  geom_abline(slope = -1, intercept = 2100, linetype = "dashed", color = "#4daf4a", size = 2) +
  geom_text(label = "20CRv3", size = 10, vjust = -1.5) +
  geom_point(shape = 8, size = 5) +
  geom_text(data = global_era20, label = "ERA20C", vjust = -0.5, hjust = 1, size = 10) +
  geom_point(data = global_era20, shape = 8, size = 5) +
  geom_text(data = global_era5, label = "ERA5", vjust = -0.5, hjust = 1, size = 10) +
  geom_point(data = global_era5, shape = 8, size = 5) +
  geom_text(data = global_ncep, label = "NCEP1", vjust = 1.5, hjust = -0.01, size = 10) +
  geom_point(data = global_ncep, shape = 8, size = 5) +
  annotate("text", x = 1075, y = 1075, label = "P = E", angle = 45, vjust = -0.5, color = "#377eb8", size = 10) +
  annotate("text", x = 1080, y = 1080, label = "Atmosphere stable", angle = 45, vjust = 1.5, color = "#377eb8", size = 10) +
  geom_label(aes(x = 1175, y = 975, label = "P < E\nAtmosphere\ngains water"), fill = "white", size = 9, label.size = NA) +
  geom_label(aes(x = 975, y = 1175, label = "P > E\nAtmosphere\nloses water"), fill = "white", size = 9, label.size = NA) +
  theme_bw() + 
  theme(panel.grid.minor = element_blank(), aspect.ratio = 1, plot.tag = element_text(size = 24), plot.title = element_text(size=30), axis.text = element_text(size = 28), axis.title = element_text(size = 30), panel.border = element_rect(colour = "black", size = 4)) +
  scale_x_continuous(limits = c(950, 1200), expand = c(0,0), breaks = seq(1000, 1150, 50)) +
  scale_y_continuous(limits = c(950, 1200), expand = c(0,0), breaks = seq(1000, 1150, 50)) +
  labs(x = "Evaporation [mm/yr]", y = "Precipitation [mm/yr]", title = NULL, tag = "A")

#Plots 20CRv3
p01 <- ggplot(global_20cr, aes(x = mean(E), y = mean(P), label = as.factor(paste(range(Year), collapse = "-")))) + 
  geom_segment(aes(x = 1162.56, y = 1107.28, xend = 1165.66, yend =1102.83), arrow = arrow(type="closed", length = unit(0.2, "cm")), color = "gray50", size = 1) +
  geom_segment(aes(x = 1165.66, y = 1102.83, xend = 1174.03, yend =1102.08), arrow = arrow(type="closed", length = unit(0.2, "cm")), color = "gray50", size = 1) +
  geom_segment(aes(x = 1174.03, y = 1102.08, xend = 1183.4, yend =1109.74), arrow = arrow(type="closed", length = unit(0.2, "cm")), color = "gray50", size = 1) +
  geom_segment(aes(x = 1183.4, y = 1109.74, xend = 1190.89, yend =1125.94), arrow = arrow(type="closed", length = unit(0.2, "cm")), color = "gray50", size = 1) +
  geom_point(aes(x = frollmean(E, 30), y = frollmean(P, 30), alpha = Year/max(Year)), color = "gray", size = 2, show.legend = FALSE) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#377eb8", size = 1.5) +
  geom_point(data = global_20cr[Year > 1860 & Year <= 1890], color = "black", size = 5) +
  geom_point(data = global_20cr[Year > 1890 & Year <= 1920], color = "black", size = 5) +
  geom_point(data = global_20cr[Year > 1920 & Year <= 1950], color = "black", size = 5) +
  geom_point(data = global_20cr[Year > 1950 & Year <= 1980], color = "black", size = 5) +
  geom_point(data = global_20cr[Year > 1980 & Year <= 2010], color = "black", size = 5) +
  geom_segment(aes(x = 1183.4, y = 1109.74, xend = 1179.04, yend = 1114.1), arrow = arrow(type="closed", length = unit(0.3, "cm")), color = "#377eb8", size = 2) +
  geom_segment(aes(x = 1179.04, y = 1114.1, xend = 1190.89, yend = 1125.94), arrow = arrow(type="closed", length = unit(0.3, "cm")), color = "#4daf4a", size = 2) +
  geom_text(data = global_20cr[Year > 1860 & Year <= 1890], vjust = -0.5, size = 8) +
  geom_text(data = global_20cr[Year > 1890 & Year <= 1920], vjust = 1.5, size = 8, hjust = 1) +
  geom_text(data = global_20cr[Year > 1920 & Year <= 1950], vjust = 1.5, size = 8) +
  geom_text(data = global_20cr[Year > 1950 & Year <= 1980], vjust = 1.5, hjust = -0.1 ,size = 8) +
  geom_text(data = global_20cr[Year > 1980 & Year <= 2010], vjust = -0.5, size = 8, hjust = 1) +
  geom_point(shape = 8, size = 8) +
  theme_bw() + 
  theme(panel.grid.minor = element_blank(), aspect.ratio = 1, plot.tag = element_text(size = 24), plot.title = element_text(size=28), axis.text = element_text(size = 24), axis.title = element_text(size = 28), panel.border = element_rect(colour = "black", size=2)) +
  scale_x_continuous(limits = c(1155, 1195), expand = c(0,0), breaks = seq(1150, 1190, 20)) +
  scale_y_continuous(limits = c(1095, 1135), expand = c(0,0), breaks = seq(1110, 1150, 20)) +
  labs(x = NULL, y = NULL, title = "20CRv3", tag = "B")

#Plots ERA20C
p02 <- ggplot(global_era20, aes(x = mean(E), y = mean(P), label = as.factor(paste(range(Year), collapse = "-")))) + 
  geom_segment(aes(x = 1022.8, y = 1016.62, xend = 1027.77, yend =1022.5), arrow = arrow(type="closed", length = unit(0.2, "cm")), color = "gray50", size = 1) +
  geom_segment(aes(x = 1027.77, y = 1022.5, xend = 1040.35, yend =1035.23), arrow = arrow(type="closed", length = unit(0.2, "cm")), color = "gray50", size = 1) +
  geom_point(aes(x = frollmean(E, 30), y = frollmean(P, 30), alpha = Year/max(Year)), color = "gray", size = 2, show.legend = FALSE) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#377eb8", size = 1.5) +
  geom_point(data = global_era20[Year > 1920 & Year <= 1950], color = "black", size = 5) +
  geom_point(data = global_era20[Year > 1950 & Year <= 1980], color = "black", size = 5) +
  geom_point(data = global_era20[Year > 1980 & Year <= 2010], color = "black", size = 5) +
  geom_segment(aes(x = 1027.78, y = 1022.5, xend = 1027.7, yend = 1022.58), arrow = arrow(type="closed", length = unit(0.1, "cm")), color = "#377eb8", size = 2) +
  geom_segment(aes(x = 1027.7, y = 1022.58, xend = 1040.35, yend = 1035.23), arrow = arrow(type="closed", length = unit(0.3, "cm")), color = "#4daf4a", size = 2) +
  geom_text(data = global_era20[Year > 1920 & Year <= 1950], vjust = 1.5, size = 8) +
  geom_text(data = global_era20[Year > 1950 & Year <= 1980], vjust = 1.5, hjust = -0.1, size = 8) +
  geom_text(data = global_era20[Year > 1980 & Year <= 2010], vjust = -0.5, size = 8) +
  geom_point(shape = 8, size = 8) +
  theme_bw() + 
  theme(panel.grid.minor = element_blank(), aspect.ratio = 1, plot.tag = element_text(size = 24), plot.title = element_text(size=28), axis.text = element_text(size = 24), axis.title = element_text(size = 28), panel.border = element_rect(colour = "black", size=2)) +
  scale_x_continuous(limits = c(1005, 1045), expand = c(0,0), breaks = seq(1020, 1040, 20)) +
  scale_y_continuous(limits = c(1005, 1045), expand = c(0,0), breaks = seq(1020, 1040, 20)) +
  labs(x = NULL, y = NULL, title = "ERA20C", tag = "C")

#Plots ERA5
p03 <- ggplot(global_era5, aes(x = mean(E), y = mean(P), label = as.factor(paste(range(Year), collapse = "-")))) + 
  geom_segment(aes(x = 1014.92, y = 1044.22, xend = 1042.4, yend =1058.24), arrow = arrow(type="closed", length = unit(0.2, "cm")), color = "gray50", size = 1) +
  geom_point(aes(x = frollmean(E, 30), y = frollmean(P, 30), alpha = Year/max(Year)), color = "gray", size = 2, show.legend = FALSE) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#377eb8", size = 1.5) +
  geom_point(data = global_era5[Year > 1950 & Year <= 1980], color = "black", size = 5) +
  geom_point(data = global_era5[Year > 1980 & Year <= 2010], color = "black", size = 5) +
  geom_segment(aes(x = 1014.92, y = 1044.22, xend = 1021.65, yend = 1037.49), arrow = arrow(type="closed", length = unit(0.3, "cm")), color = "#377eb8", size = 2) +
  geom_segment(aes(x = 1021.65, y = 1037.49, xend = 1042.4, yend = 1058.24), arrow = arrow(type="closed", length = unit(0.3, "cm")), color = "#4daf4a", size = 2) +
  geom_text(data = global_era5[Year > 1950 & Year <= 1980], vjust = -0.5, hjust = 0.9, size = 8) +
  geom_text(data = global_era5[Year > 1980 & Year <= 2010], vjust = -0.7, hjust = 0.8, size = 8) +
  geom_point(shape = 8, size = 8) +
  theme_bw() + 
  theme(panel.grid.minor = element_blank(), aspect.ratio = 1, plot.tag = element_text(size = 24), plot.title = element_text(size=28), axis.text = element_text(size = 24), axis.title = element_text(size = 28), panel.border = element_rect(colour = "black", size=2)) +
  scale_x_continuous(limits = c(1005, 1055), expand = c(0,0), breaks = seq(1020, 1040, 20)) +
  scale_y_continuous(limits = c(1025, 1065), expand = c(0,0), breaks = seq(1040, 1060, 20)) +
  labs(x = NULL, y = NULL, title = "ERA5", tag = "D")

#Plots NCEP
p04 <- ggplot(global_ncep, aes(x = mean(E), y = mean(P), label = as.factor(paste(range(Year), collapse = "-")))) + 
  geom_segment(aes(x = 1041.73, y = 1023.13, xend = 1030.93, yend =1013.35), arrow = arrow(type="closed", length = unit(0.2, "cm")), color = "gray50", size = 1) +
  geom_point(aes(x = frollmean(E, 30), y = frollmean(P, 30), alpha = Year/max(Year)), color = "gray", size = 2, show.legend = FALSE) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#377eb8", size = 1.5) +
  geom_point(data = global_ncep[Year > 1950 & Year <= 1980], color = "black", size = 5) +
  geom_point(data = global_ncep[Year > 1980 & Year <= 2010], color = "black", size = 5) +
  geom_segment(aes(x = 1041.73, y = 1023.13, xend = 1041.22, yend = 1023.64), arrow = arrow(type="closed", length = unit(0.1, "cm")), color = "#377eb8", size = 2) +
  geom_segment(aes(x = 1041.22, y = 1023.64, xend = 1030.93, yend = 1013.35), arrow = arrow(type="closed", length = unit(0.3, "cm")), color = "#4daf4a", size = 2) +
  geom_text(data = global_ncep[Year > 1950 & Year <= 1980], vjust = -1, hjust = 0.7, size = 8) +
  geom_text(data = global_ncep[Year > 1980 & Year <= 2010], vjust = 1.5, size = 8) +
  geom_point(shape = 8, size = 8) +
  theme_bw() + 
  theme(panel.grid.minor = element_blank(), aspect.ratio = 1, plot.tag = element_text(size = 24), plot.title = element_text(size=28), axis.text = element_text(size = 24), axis.title = element_text(size = 28), panel.border = element_rect(colour = "black", size=2)) +
  scale_x_continuous(limits = c(1005, 1045), expand = c(0,0), breaks = seq(1020, 1040, 20)) +
  scale_y_continuous(limits = c(995, 1035), expand = c(0,0), breaks = seq(1010, 1030, 20)) +
  labs(x = NULL, y = NULL, title = "NCEP1", tag = "E")

yleft = grid::textGrob("P [mm/yr]", rot=90, gp = grid::gpar(fontsize = 24))
xdown = grid::textGrob("E [mm/yr]", gp = grid::gpar(fontsize = 24))


p05 <- gridExtra::grid.arrange(p01, p02, p03, p04, ncol = 2) #, left = yleft, bottom = xdown
p06 <- gridExtra::grid.arrange(p00, p05, widths = c(1.5,2))

ggsave("./../plots/PvE.pdf", p06, dpi = 600, width = 8.15*3.5, height = 8.15*2)






