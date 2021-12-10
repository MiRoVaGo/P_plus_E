#Required Libraries
library(ggplot2)
library(dplyr)
library(data.table)
library(tidyverse)
#Load chronological data
gwc_data <- read_csv('gwc_chronology.csv') %>% as.data.table()
global_20cr <- read_csv('20CRv3.csv') %>% as.data.table()
global_era20 <- read_csv('ERA20C.csv') %>% as.data.table()
global_era5 <- read_csv('ERA_5.csv') %>% as.data.table()
global_ncep <- read_csv('NCEP_NCAR.csv') %>% as.data.table()
#Land Data
land_data <- gwc_data[, c(1, 2, 3, 8)][, PpE := P_L + ET][, PmE := P_L - ET][, mean_PpE := mean(PpE, na.rm = TRUE), by = period][, sd_PpE := sd(PpE, na.rm = TRUE), by = period][, mean_PmE := mean(PmE, na.rm = TRUE), by = period][, sd_PmE := sd(PmE, na.rm = TRUE), by = period]
land_20cr <- global_20cr[, c(1:3, 16)][, PpE := P_L + ET][, PmE := P_L - ET][, mean_PpE := mean(PpE, na.rm = TRUE), by = period][, mean_PmE := mean(PmE, na.rm = TRUE), by = period]
land_era20 <- global_era20[, c(1, 5, 7, 16)][, ET := P_L - PmE_L][, PpE := P_L + ET][, mean_PpE := mean(PpE, na.rm = TRUE), by = period][, mean_PmE := mean(PmE_L, na.rm = TRUE), by = period]
#Ocean Data
ocean_data <- gwc_data[,c(1, 4, 5, 8)][, PpE := P_O + E][, PmE := P_O - E][, mean_PpE := mean(PpE, na.rm = TRUE), by = period][, sd_PpE := sd(PpE, na.rm = TRUE), by = period][, mean_PmE := mean(PmE, na.rm = TRUE), by = period][, sd_PmE := sd(PmE, na.rm = TRUE), by = period]
ocean_20cr <- global_20cr[, c(1, 4, 5, 16)][, PpE := P_O + E][, PmE := P_O - E][, mean_PpE := mean(PpE, na.rm = TRUE), by = period][, mean_PmE := mean(PmE, na.rm = TRUE), by = period]
ocean_era20 <- global_era20[, c(1, 6, 8, 16)][, E := P_O - PmE_O][, PpE := P_O + E][, mean_PpE := mean(PpE, na.rm = TRUE), by = period][, mean_PmE := mean(PmE_O, na.rm = TRUE), by = period]
#Global
p17 <- ggplot(data = gwc_data, aes(x = period, y = mean_PpE, group = 1)) +
  stat_summary(data = global_20cr[Year >= 1900], fun = mean, geom = "line", color = '#e6ab02', size = 1.5) +
  geom_point(data = global_20cr[Year >= 1900], fill = '#e6ab02', shape = 23, size = 3) +
  stat_summary(data = global_era20, fun = mean, geom = "line", color = '#7570b3', size = 1.5) +
  geom_point(data = global_era20, fill = '#7570b3', shape = 23, size = 3) +
  stat_summary(data = global_era5, fun = mean, geom = "line", color = '#a6761d', size = 1.5) +
  geom_point(data = global_era5, fill = '#a6761d', shape = 23, size = 3) +
  stat_summary(data = global_ncep, fun = mean, geom = "line", color = '#666666', size = 1.5) +
  geom_point(data = global_ncep, fill = '#666666', shape = 23, size = 3) +
  geom_errorbar(aes(ymin = mean_PpE - sd_PpE, ymax = mean_PpE + sd_PpE), width = 0, size = 1, color = '#d95f02') +
  stat_summary(fun = mean, geom = "line", color = '#d95f02', size = 1.5) +
  geom_point(fill = '#d95f02', shape = 23, size = 3) +
  labs(x = "Period", y = "Global Mean P + E [mm/day]") +
  theme_bw() +
  scale_y_continuous(limits = c(3.5, 6.5), expand = c(0, 0)) +
  theme(panel.grid.major.x = element_blank(), axis.text.y = element_text(size = 20), axis.title.y = element_text(size = 24), axis.text.x = element_text(size = 20), axis.title.x = element_text(size = 24))
ggsave("global_plus_30.png", p17, dpi = 600, limitsize = FALSE)

p18 <- ggplot(data = gwc_data, aes(x = period, y = mean_PmE, group = 1)) +
  stat_summary(data = global_20cr[Year >= 1900], fun = mean, geom = "line", color = '#e6ab02', size = 1.5) +
  geom_point(data = global_20cr[Year >= 1900], fill = '#e6ab02', shape = 23, size = 3) +
  stat_summary(data = global_era20, fun = mean, geom = "line", color = '#7570b3', size = 1.5) +
  geom_point(data = global_era20, fill = '#7570b3', shape = 23, size = 3) +
  stat_summary(data = global_era5, fun = mean, geom = "line", color = '#a6761d', size = 1.5) +
  geom_point(data = global_era5, fill = '#a6761d', shape = 23, size = 3) +
  stat_summary(data = global_ncep, fun = mean, geom = "line", color = '#666666', size = 1.5) +
  geom_point(data = global_ncep, fill = '#666666', shape = 23, size = 3) +
  geom_errorbar(aes(ymin = mean_PmE - sd_PmE, ymax = mean_PmE + sd_PmE), width = 0, size = 1, color = "#d95f02") +
  stat_summary(fun = mean, geom = "line", color = '#d95f02', size = 1.5) +
  geom_point(fill = '#d95f02', shape = 23, size = 3) +
  labs(fill = NULL, x = "Period", y = "Global Mean P - E [mm/day]") +
  theme_bw() +
  scale_y_continuous(limits = c(-0.25, 0.15), expand = c(0, 0)) +
  theme(panel.grid.major.x = element_blank(), axis.text.y = element_text(size = 20), axis.title.y = element_text(size = 24), axis.text.x = element_text(size = 20), axis.title.x = element_text(size = 24))
ggsave("global_minus_30.png", p18, dpi = 600, limitsize = FALSE)
#Land
p19 <- ggplot(data = land_data, aes(x = period, y = mean_PpE, group = 1)) +
  stat_summary(data = land_20cr[Year >= 1900], fun = mean, geom = "line", color = '#e6ab02', size = 1.5) +
  geom_point(data = land_20cr[Year >= 1900], fill = '#e6ab02', shape = 23, size = 3) +
  stat_summary(data = land_era20, fun = mean, geom = "line", color = '#7570b3', size = 1.5) +
  geom_point(data = land_era20, fill = '#7570b3', shape = 23, size = 3) +
  geom_errorbar(aes(ymin = mean_PpE - sd_PpE, ymax = mean_PpE + sd_PpE), width = 0, size = 1, color = "#d95f02") +
  stat_summary(fun = mean, geom = "line", color = '#d95f02', size = 1.5) +
  geom_point(fill = '#d95f02', shape = 23, size = 3) +
  labs(x = "Period", y = "Land Mean P + E [mm/day]") +
  theme_bw() +
  scale_y_continuous(limits = c(2.5, 4), expand = c(0, 0)) +
  theme(panel.grid.major.x = element_blank(), axis.text.y = element_text(size = 20), axis.title.y = element_text(size = 24), axis.text.x = element_text(size = 20), axis.title.x = element_text(size = 24))
ggsave("land_plus_30.png", p19, dpi = 600, limitsize = FALSE)

p20 <- ggplot(data = land_data, aes(x = period, y = mean_PmE, group = 1)) +
  stat_summary(data = land_20cr[Year >= 1900], fun = mean, geom = "line", color = '#e6ab02', size = 1.5) +
  geom_point(data = land_20cr[Year >= 1900], fill = '#e6ab02', shape = 23, size = 3) +
  stat_summary(data = land_era20, fun = mean, geom = "line", color = '#7570b3', size = 1.5) +
  geom_point(data = land_era20, fill = '#7570b3', shape = 23, size = 3) +
  geom_errorbar(aes(ymin = mean_PmE - sd_PmE, ymax = mean_PmE + sd_PmE), width = 0, size = 1, color = "#d95f02") +
  stat_summary(fun = mean, geom = "line", color = '#d95f02', size = 1.5) +
  geom_point(fill = '#d95f02', shape = 23, size = 3) +
  labs(x = "Period", y = "Land Mean P - E [mm/day]") +
  theme_bw() +
  scale_y_continuous(limits = c(0.45, 0.9), expand = c(0, 0)) +
  theme(panel.grid.major.x = element_blank(), axis.text.y = element_text(size = 20), axis.title.y = element_text(size = 24), axis.text.x = element_text(size = 20), axis.title.x = element_text(size = 24))
ggsave("land_minus_30.png", p20, dpi = 600, limitsize = FALSE)
#Ocean
p21 <- ggplot(data = ocean_data, aes(x = period, y = mean_PpE, group = 1)) +
  stat_summary(data = ocean_20cr[Year >= 1900], fun = mean, geom = "line", color = '#e6ab02', size = 1.5) +
  geom_point(data = ocean_20cr[Year >= 1900], fill = '#e6ab02', shape = 23, size = 3) +
  stat_summary(data = ocean_era20, fun = mean, geom = "line", color = '#7570b3', size = 1.5) +
  geom_point(data = ocean_era20, fill = '#7570b3', shape = 23, size = 3) +
  geom_errorbar(aes(ymin = mean_PpE - sd_PpE, ymax = mean_PpE + sd_PpE), width = 0, size = 1, color ="#d95f02") +
  stat_summary(fun = mean, geom = "line", color = '#d95f02', size = 1.5) +
  geom_point(fill = '#d95f02', shape = 23, size = 3) +
  labs(x = "Period", y = "Ocean MEan P + E [mm/day]") +
  theme_bw() +
  scale_y_continuous(limits = c(3.5, 8), expand = c(0, 0)) +
  theme(panel.grid.major.x = element_blank(), axis.text.y = element_text(size = 20), axis.title.y = element_text(size = 24), axis.text.x = element_text(size = 20), axis.title.x = element_text(size = 24))
ggsave("ocean_plus_30.png", p21, dpi = 600, limitsize = FALSE)

p22 <- ggplot(data = ocean_data, aes(x = period, y = mean_PmE, group = 1)) +
  stat_summary(data = ocean_20cr[Year >= 1900], fun = mean, geom = "line", color = '#e6ab02', size = 1.5) +
  geom_point(data = ocean_20cr[Year >= 1900], fill = '#e6ab02', shape = 23, size = 3) +
  stat_summary(data = ocean_era20, fun = mean, geom = "line", color = '#7570b3', size = 1.5) +
  geom_point(data = ocean_era20, fill = '#7570b3', shape = 23, size = 3) +
  geom_errorbar(aes(ymin = mean_PmE - sd_PmE, ymax = mean_PmE + sd_PmE), width = 0, size = 1, color = "#d95f02") +
  stat_summary(fun = mean, geom = "line", color = '#d95f02', size = 1.5) +
  geom_point(fill = '#d95f02', shape = 23, size = 3) +
  labs("Period", y = "Ocean Mean P - E [mm/day]") +
  theme_bw() +
  scale_y_continuous(limits = c(-0.65, -0.15), expand = c(0, 0)) +
  theme(panel.grid.major.x = element_blank(), axis.text.y = element_text(size = 20), axis.title.y = element_text(size = 24), axis.text.x = element_text(size = 20), axis.title.x = element_text(size = 24))
ggsave("ocean_minus_30.png", p22, dpi = 600, limitsize = FALSE)
