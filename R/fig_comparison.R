#Required Libraries
library(ggplot2)
library(dplyr)
library(data.table)
library(tidyverse)
#Load Data
global_20cr <- read_csv('./../data/20cr.csv') %>% as.data.table() %>% 
  .[, PpE := P + E] %>% .[, PmE := P - E]
global_era20 <- read_csv('./../data/era20.csv') %>% as.data.table() %>% 
  .[, PpE := P + E] %>% .[, PmE := P - E]
global_era5 <- read_csv('./../data/era5.csv') %>% as.data.table() %>% 
  .[, PpE := P + E] %>% .[, PmE := P - E]
global_ncep <- read_csv('./../data/ncep.csv') %>% as.data.table() %>%
  .[, PpE := P + E] %>% .[, PmE := P - E]
#Plots 20CRv3
range_PpE_20cr <- max(global_20cr$PpE) - min(global_20cr$PpE)
min_PpE_20cr <- min(global_20cr$PpE)
range_T_20cr <- max(global_20cr$T) - min(global_20cr$T)
min_T_20cr <- min(global_20cr$T)

p01 <- ggplot(data = global_20cr, aes(x = Year, y = PmE)) +
  geom_line(show.legend = FALSE, color = "gray") +
  labs(x = NULL, y = NULL, title = "20CR v3", tag = "A") +
  geom_line(aes(x = frollmean(Year, 30), y = frollmean(PmE, 30)),
            color = "#377eb8", size = 2, align = "center") +
  theme_bw() +
  scale_x_continuous(limits = c(1831, 2026), expand = c(0, 0), 
                     breaks = seq(1810, 2050, 30)) +
  theme(panel.grid.minor = element_blank(), plot.tag = element_text(size = 24),
        plot.title = element_text(size=28), axis.text = element_text(size = 20),
        axis.title = element_text(size = 24))

p02 <- ggplot(data = global_20cr, aes(x = Year, y = PpE)) +
  geom_line(show.legend = FALSE, color = "#4daf4a", size = 1) +
  geom_line(aes(y = 2*P), show.legend = FALSE, color = "#7570b3", size = 1) +
  geom_line(aes(y = 2*E), show.legend = FALSE, color = "#c1e5a1", size = 1) +
  geom_line(aes(y = (T - min_T_20cr)*range_PpE_20cr/range_T_20cr + min_PpE_20cr), color = "red") +
  labs(x = NULL, y = NULL, title = "20CR v3", tag = "A") +
  theme_bw() +
  scale_y_continuous(sec.axis = sec_axis(~(. - min_PpE_20cr)*range_T_20cr/range_PpE_20cr + min_T_20cr, name = NULL)) +
  scale_x_continuous(limits = c(1831, 2026), expand = c(0, 0), breaks = seq(1810, 2050, 30)) +
  theme(panel.grid.minor = element_blank(), plot.tag = element_text(size = 24),
        plot.title = element_text(size=28), axis.text = element_text(size = 20),
        axis.title = element_text(size = 24))

#Plots ERA20C
range_PpE_era20 <- max(global_era20$PpE) - min(global_era20$PpE)
min_PpE_era20 <- min(global_era20$PpE)
range_T_era20 <- max(global_era20$T) - min(global_era20$T)
min_T_era20 <- min(global_era20$T)

p03 <- ggplot(data = global_era20, aes(x = Year, y = PmE)) +
  geom_line(show.legend = FALSE, color = "gray") +
  geom_line(aes(x = frollmean(Year, 30), y = frollmean(PmE, 30)),
            color = "#377eb8", size = 2, align = "center") +
  labs(x = NULL, y = NULL, title = "ERA-20C", tag = "B") +
  theme_bw() +
  scale_x_continuous(limits = c(1831, 2026), expand = c(0, 0),
                     breaks = seq(1810, 2050, 30)) +
  theme(panel.grid.minor = element_blank(), plot.tag = element_text(size = 24),
        plot.title = element_text(size=28), axis.text = element_text(size = 20),
        axis.title = element_text(size = 24))

p04 <- ggplot(data = global_era20, aes(x = Year, y = PpE)) +
  geom_line(aes(y = 2*P), show.legend = FALSE, color = "#7570b3", size = 1) +
  geom_line(aes(y = 2*E), show.legend = FALSE, color = "#c1e5a1", size = 1) +
  geom_line(show.legend = FALSE, color = "#4daf4a", size = 1) +
  geom_line(aes(y = (T - min_T_era20)*range_PpE_era20/range_T_era20 + min_PpE_era20), color = "red") +
  labs(x = NULL, y = NULL, title = "ERA-20C", tag = "B") +
  theme_bw() +
  scale_y_continuous(sec.axis = sec_axis(~(. - min_PpE_era20)*range_T_era20/range_PpE_era20 + min_T_era20, name = NULL)) +
  scale_x_continuous(limits = c(1831, 2026), expand = c(0, 0), breaks = seq(1810, 2050, 30)) +
  theme(panel.grid.minor = element_blank(), plot.tag = element_text(size = 24),
        plot.title = element_text(size=28), axis.text = element_text(size = 20),
        axis.title = element_text(size = 24))

#Plots ERA5
range_PpE_era5 <- max(global_era5$PpE) - min(global_era5$PpE)
min_PpE_era5 <- min(global_era5$PpE)
range_T_era5 <- max(global_era5$T) - min(global_era5$T)
min_T_era5 <- min(global_era5$T)

p05 <- ggplot(data = global_era5, aes(x = Year, y = PmE)) +
  geom_line(show.legend = FALSE, color = "gray") +
  geom_line(aes(x = frollmean(Year, 30), y = frollmean(PmE, 30)),
            color = "#377eb8", size = 2, align = "center") +
  labs(x = NULL, y = NULL, title = "ERA5", tag = "C") +
  theme_bw() +
  scale_x_continuous(limits = c(1831, 2026), expand = c(0, 0),
                     breaks = seq(1810, 2050, 30)) +
  theme(panel.grid.minor = element_blank(), plot.tag = element_text(size = 24),
        plot.title = element_text(size=28), axis.text = element_text(size = 20),
        axis.title = element_text(size = 24))

p06 <- ggplot(data = global_era5, aes(x = Year, y = PpE)) +
  geom_line(aes(y = 2*P), show.legend = FALSE, color = "#7570b3", size = 1) +
  geom_line(aes(y = 2*E), show.legend = FALSE, color = "#c1e5a1", size = 1) +
  geom_line(show.legend = FALSE, color = "#4daf4a", size = 1) +
  geom_line(aes(y = (T - min_T_era5)*range_PpE_era5/range_T_era5 + min_PpE_era5), color = "red") +
  labs(x = NULL, y = NULL, title = "ERA5", tag = "C") +
  theme_bw() +
  scale_y_continuous(sec.axis = sec_axis(~(. - min_PpE_era5)*range_T_era5/range_PpE_era5 + min_T_era5, name = NULL)) +
  scale_x_continuous(limits = c(1831, 2026), expand = c(0, 0),
                     breaks = seq(1810, 2050, 30)) +
  theme(panel.grid.minor = element_blank(), plot.tag = element_text(size = 24),
        plot.title = element_text(size=28), axis.text = element_text(size = 20),
        axis.title = element_text(size = 24))

#Plots NCEP
range_PpE_ncep <- max(global_ncep$PpE) - min(global_ncep$PpE)
min_PpE_ncep <- min(global_ncep$PpE)
range_T_ncep <- max(global_ncep$T) - min(global_ncep$T)
min_T_ncep <- min(global_ncep$T)

p07 <- ggplot(data = global_ncep, aes(x = Year, y = PmE)) +
  geom_line(show.legend = FALSE, color = "gray") +
  geom_line(aes(x = frollmean(Year, 30), y = frollmean(PmE, 30)),
            color = "#377eb8", size = 2, align = "center") +
  labs(x = NULL, y = NULL, title = "NCEP1", tag = "D") +
  theme_bw() +
  scale_x_continuous(limits = c(1831, 2026), expand = c(0, 0),
                     breaks = seq(1810, 2050, 30)) +
  theme(panel.grid.minor = element_blank(), plot.tag = element_text(size = 24),
        plot.title = element_text(size=28), axis.text = element_text(size = 20),
        axis.title = element_text(size = 24))

p08 <- ggplot(data = global_ncep, aes(x = Year, y = PpE)) +
  geom_line(aes(y = 2*P), show.legend = FALSE, color = "#7570b3", size = 1) +
  geom_line(aes(y = 2*E), show.legend = FALSE, color = "#c1e5a1", size = 1) +
  geom_line(show.legend = FALSE, color = "#4daf4a", size = 1) +
  geom_line(aes(y = (T - min_T_ncep)*range_PpE_ncep/range_T_ncep + min_PpE_ncep),
            color = "red") +
  labs(x = NULL, y = NULL, title = "NCEP1", tag = "D") +
  theme_bw() +
  scale_y_continuous(sec.axis = sec_axis(~(. - min_PpE_ncep)*range_T_ncep/range_PpE_ncep + min_T_ncep, name = NULL)) +
  scale_x_continuous(limits = c(1831, 2026), expand = c(0, 0),
                     breaks = seq(1810, 2050, 30)) +
  theme(panel.grid.minor = element_blank(), plot.tag = element_text(size = 24),
        plot.title = element_text(size=28), axis.text = element_text(size = 20),
        axis.title = element_text(size = 24))

yleft1 = grid::textGrob("Precipitation minus Evaporation [mm]", rot=90, 
                        gp = grid::gpar(fontsize = 28))
yleft2 = grid::textGrob("Annual Precipitation plus Evaporation [mm]", rot=90, 
                        gp = grid::gpar(fontsize = 28))
yright = grid::textGrob("Mean Annual Temperature [°C]", rot=-90, 
                        gp = grid::gpar(fontsize = 28))
p09 <- gridExtra::grid.arrange(p01, p03, p05, p07, ncol = 2, left = yleft1, 
                               padding = unit(1, "line"))
p10 <- gridExtra::grid.arrange(p02, p04, p06, p08, ncol = 2, right = yright,
                               left = yleft2, padding = unit(1, "line"))

ggsave("./../plots/budget.pdf", p09, dpi = 600, width = 8.15*2,
       height = 5.01*2)
ggsave("./../plots/comparison.pdf", p10, dpi = 600, width = 8.15*2,
       height = 5.01*2)
