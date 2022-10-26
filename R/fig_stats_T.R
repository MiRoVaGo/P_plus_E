#Required Libraries
library(ggplot2)
library(dplyr)
library(data.table)
library(tidyverse)
library(ggpubr)
library(Metrics)
library(gridExtra)
library(patchwork)
library(gtable)
library(grid)
#Load Data
global_20cr <- read_csv('./../data/20cr.csv') %>% as.data.table() %>%
  .[, .(Year, T)]
global_era20 <- read_csv('./../data/era20.csv') %>% as.data.table() %>%
  .[, .(Year, T)]
global_era5 <- read_csv('./../data/era5.csv') %>% as.data.table() %>%
  .[, .(Year, T)]
global_ncep <- read_csv('./../data/ncep.csv') %>% as.data.table() %>%
  .[, .(Year, T)]
global_hadcrut <- read_csv('./../data/hadcrut.csv') %>% as.data.table()
#Plots 20CRv3
mean_20cr <- mean(global_20cr[Year > 1960 & Year < 1991, T])
mean_era20 <- mean(global_era20[Year > 1960 & Year < 1991, T])
mean_era5 <- mean(global_era5[Year > 1960 & Year < 1991, T])
mean_ncep <- mean(global_ncep[Year > 1960 & Year < 1991, T])
sd_20cr <- sd(global_20cr[Year > 1960 & Year < 1991, T])
sd_era20 <- sd(global_era20[Year > 1960 & Year < 1991, T])
sd_era5 <- sd(global_era5[Year > 1960 & Year < 1991, T])
sd_ncep <- sd(global_ncep[Year > 1960 & Year < 1991, T])

global_cli <- data.frame(name = c("20CRv3", "ERA20C", "ERA5", 
                                  "NCEP1"),
                         cli_mean = c(mean_20cr, mean_era20, mean_era5, 
                                      mean_ncep),
                         cli_sd = c(sd_20cr, sd_era20, sd_era5, sd_ncep))
global_cli$name <- factor(global_cli$name, levels = global_cli$name)

sim <- merge(global_20cr[, tp1 := T - mean_20cr][, .(Year, tp1)],
             global_era20[, tp2 := T - mean_era20][, .(Year, tp2)],
             by = "Year", all = TRUE) %>%
  merge(global_ncep[, tp3 := T - mean_ncep][, .(Year, tp3)],
        by = "Year", all = TRUE) %>%
  merge(global_era5[, tp4 := T - mean_era5][, .(Year, tp4)], 
        by = "Year", all = TRUE)
sim <- sim[, T := mean(c(tp1, tp2, tp3, tp4)), by = Year
           ][, spread := sd(c(tp1, tp2, tp3, tp4)), by = Year
             ][!is.na(T), .(Year, T, spread)]

global_20cr[, sims := "20CRv3"]
global_era20[, sims := "ERA20C"]
global_era5[, sims := "ERA5"]
global_ncep[, sims := "NCEP1"]
global_hadcrut[, obs := "HadCRUT5"]

global_cor <- rbind(global_20cr[, .(Year, T, sims)], 
                    global_era20[, .(Year, T, sims)], 
                    global_era5[, .(Year, T, sims)], 
                    global_ncep[, .(Year, T, sims)]) %>% 
  merge(global_hadcrut[, .(Year, T, obs)], by = "Year", all = TRUE,
        allow.cartesian = TRUE)
global_cor <- global_cor[Year >= 1950 & Year <= 2010]

global <- rbind(global_20cr[, name := sims][, .(Year, T, name)], 
                global_era20[, name := sims][, .(Year, T, name)], 
                global_era5[, name := sims][, .(Year, T, name)], 
                global_ncep[, name := sims][, .(Year, T, name)], 
                global_hadcrut[, name := obs][, .(Year, T, name)])

p00 <- copy(global_cor)
p00 <- p00[, `p-value` := formatC(cor.test(T.y, T.x)$p.value, format = "e",
                                  digits = 0), by = .(sims, obs)
           ][, RMSE := round(rmse(T.y, T.x), 3), by = .(sims, obs)
             ][, R2 := round((cor(T.y, T.x)^2), 3), by = .(sims, obs)
               ][, .(obs, sims, R2, `p-value`, RMSE)] %>%
  unique() %>% setnames("obs", "Observations") %>% 
  setnames("sims", "Reanalyses") %>%
  tableGrob(rows = NULL, theme = ttheme_minimal(base_size = 16))

p00 <- gtable_add_grob(p00, grobs = segmentsGrob(x0 = unit(0,"npc"),
                                                 y0 = unit(0,"npc"),
                                                 x1 = unit(1,"npc"),
                                                 y1 = unit(0,"npc"),
                                                 gp = gpar(lwd = 4)),
                       t = 1, b = 1, l = 1, r = 5)


p01 <- ggplot(data = sim[Year <= 2010], aes(x = Year, y = T)) +
  geom_ribbon(aes(ymax = T + spread, ymin = T - spread), 
              fill = "gray") +
  geom_line(color = "white") +
  geom_line(data = global_hadcrut[Year <= 2010], aes(color = "HadCRUT5"), 
            size = 1) +
  scale_x_continuous(limits = c(1950, 2010), expand = c(0, 0), 
                     breaks = seq(1920, 2040, 10)) +
  scale_color_manual(name = "Data set", values = c("HadCRUT5" = "#e41a1c",
                                                   "Reanalyses" = "gray")) +
  scale_y_continuous(limits = c(-0.8, 0.8), expand = c(0, 0), 
                     breaks = c(-0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6)) +
  geom_hline(yintercept = 0) +
  labs(x = NULL, y = "Temperature \n Anomaly [°C]", title = NULL) +
  theme_bw() + 
  theme(panel.border = element_rect(colour = "black", size = 2),
        panel.grid = element_blank(), 
        plot.title = element_text(size = 20), 
        axis.text = element_text(size = 16),
        axis.text.x = element_text(face = "bold"),
        axis.title = element_text(size = 20), 
        legend.text = element_text(size = 16), 
        legend.title = element_text(size = 20), 
        axis.ticks.length = unit(-.25, "cm"))

p02 <- ggplot(data = global_cli, aes(x = cli_mean, y = name)) +
  geom_point(color = "#7570b3") +
  geom_pointrange(aes(xmin = cli_mean - cli_sd, xmax = cli_mean + cli_sd),
                  color = "#7570b3") +
  theme_bw() +
  scale_y_discrete(limits = rev) +
  labs(y = NULL, x = "Temperature [°C]", title = "1961-1990 Average") +
  theme(panel.border = element_rect(colour = "black", size = 2),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.y = element_text(face = "bold"),
        plot.tag = element_text(size = 20), 
        plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
        plot.title.position = "plot",
        axis.text = element_text(size = 16), 
        axis.title = element_text(size = 20), 
        legend.text = element_text(size = 16), 
        legend.title = element_text(size = 20), 
        axis.ticks.length.y = unit(0, "cm"))

p03 <- ggarrange(p01, ggarrange(p02, p00, ncol = 2, widths = c(1, 1.5), 
                                labels = c("B", "C")), nrow = 2, labels = "A")

ggsave("./../plots/stats_T.pdf", p03, width = 8.15*1.5, height = 5.01, dpi = 600)
