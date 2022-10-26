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
  .[, .(Year, P)]
global_era20 <- read_csv('./../data/era20.csv') %>% as.data.table() %>%
  .[, .(Year, P)]
global_era5 <- read_csv('./../data/era5.csv') %>% as.data.table() %>%
  .[, .(Year, P)]
global_ncep <- read_csv('./../data/ncep.csv') %>% as.data.table() %>%
  .[, .(Year, P)]
global_gpcp <- read_csv('./../data/gpcp.csv') %>% as.data.table()
#Plots 20CRv3
mean_20cr <- mean(global_20cr[Year > 1980 & Year < 2011, P])
mean_era20 <- mean(global_era20[Year > 1980 & Year < 2011, P])
mean_era5 <- mean(global_era5[Year > 1980 & Year < 2011, P])
mean_ncep <- mean(global_ncep[Year > 1980 & Year < 2011, P])
mean_gpcp <- mean(global_gpcp[Year > 1980 & Year < 2011, P])
sd_20cr <- sd(global_20cr[Year > 1980 & Year < 2011, P])
sd_era20 <- sd(global_era20[Year > 1980 & Year < 2011, P])
sd_era5 <- sd(global_era5[Year > 1980 & Year < 2011, P])
sd_ncep <- sd(global_ncep[Year > 1980 & Year < 2011, P])
sd_gpcp <- sd(global_gpcp[Year > 1980 & Year < 2011, P])

global_cli <- data.frame(name = c("20CRv3", "ERA20C", "ERA5", 
                                  "NCEP1", "GPCPv2.3"),
                         cli_mean = c(mean_20cr, mean_era20, mean_era5, 
                                      mean_ncep, 
                                      mean_gpcp),
                         cli_sd = c(sd_20cr, sd_era20, sd_era5,
                                    sd_ncep, sd_gpcp))
global_cli$name <- factor(global_cli$name, levels = global_cli$name)

sim <- merge(global_20cr[, tp1 := 100*(P - mean_20cr)/mean_20cr
                         ][, .(Year, tp1)],
             global_era20[, tp2 := 100*(P - mean_era20)/mean_era20
                          ][, .(Year, tp2)], 
             by = "Year", all = TRUE) %>% 
  merge(global_ncep[, tp3 := 100*(P - mean_ncep)/mean_ncep][, .(Year, tp3)], 
        by = "Year", all = TRUE) %>%
  merge(global_era5[, tp4 := 100*(P - mean_era5)/mean_era5][, .(Year, tp4)], 
        by = "Year", all = TRUE)
sim <- sim[, P := mean(c(tp1, tp2, tp3, tp4)), by = Year
           ][, spread := sd(c(tp1, tp2, tp3, tp4)), by = Year
             ][!is.na(P), .(Year, P, spread)]

global_20cr[, sims := "20CRv3"]
global_era20[, sims := "ERA20C"]
global_era5[, sims := "ERA5"]
global_ncep[, sims := "NCEP1"]
global_gpcp[, obs := "GPCPv2.3"]

global_cor <- rbind(global_20cr[, .(Year, P, sims)], 
                global_era20[, .(Year, P, sims)], 
                global_era5[, .(Year, P, sims)], 
                global_ncep[, .(Year, P, sims)]) %>% 
  merge(global_gpcp[, .(Year, P, obs)], by = "Year", all = TRUE, 
        allow.cartesian = TRUE)
global_cor <- global_cor[Year >= 1981 & Year <= 2010]

global <- rbind(global_20cr[, name := sims][, .(Year, P, name)], 
                global_era20[, name := sims][, .(Year, P, name)], 
                global_era5[, name := sims][, .(Year, P, name)], 
                global_ncep[, name := sims][, .(Year, P, name)], 
                global_gpcp[, name := obs][, .(Year, P, name)])

p00 <- copy(global_cor)
p00 <- p00[, `p-value` := formatC(cor.test(P.y, P.x)$p.value, format = "e",
                                  digits = 0), by = .(sims, obs)
           ][, RMSE := round(rmse(P.y, P.x), 3), by = .(sims, obs)
             ][, R2 := round((cor(P.y, P.x)^2), 3), by = .(sims, obs)
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

p01 <- ggplot(data = global_cli, aes(x = cli_mean, y = name)) +
  geom_point(color = "#7570b3") +
  geom_pointrange(aes(xmin = cli_mean - cli_sd, xmax = cli_mean + cli_sd),
                  color = "#7570b3") +
  geom_point(data = global_cli[global_cli$name == "GPCPv2.3",], 
             color = "#1b9e77") +
  geom_pointrange(data = global_cli[global_cli$name == "GPCPv2.3",], 
                  aes(xmin = cli_mean - cli_sd, xmax = cli_mean + cli_sd),
                  color = "#1b9e77") +
  theme_bw() +
  scale_y_discrete(limits = rev) +
  labs(y = NULL, x = "Precipitation [mm]", title = "1981-2010 Average") +
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

p02 <- ggplot(data = sim[Year <= 2010], aes(x = Year, y = P)) +
  geom_ribbon(aes(ymax = P + spread, ymin = P - spread), 
              fill = "gray") +
  geom_line(color = "white") +
  geom_line(data = global_gpcp[Year <= 2010], aes(y = 100*(P - mean_gpcp)/mean_gpcp, 
                                    color = "GPCPv2.3"), size = 1) +
  scale_x_continuous(limits = c(1950, 2010), expand = c(0, 0), 
                     breaks = seq(1920, 2040, 10)) +
  scale_color_manual(name = "Data set", values = c("GPCPv2.3" = "#1b9e77", 
                                                   "Reanalyses" = "gray")) +
  scale_y_continuous(limits = c(-5, 5), expand = c(0, 0), 
                     breaks = seq(-4, 4, 1)) +
  geom_hline(yintercept = 0) +
  labs(x = NULL, y = "Precipitation \n Anomaly [%]", title = NULL) +
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

p03 <- ggarrange(p02, ggarrange(p01, p00, ncol = 2, widths = c(1, 1.5), 
                                labels = c("B", "C")), nrow = 2, labels = "A")

ggsave("./../plots/stats.pdf", p03, width = 8.15*1.5, height = 5.01, dpi = 600)
