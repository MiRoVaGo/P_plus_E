#Required Libraries
library(ggplot2)
library(dplyr)
library(data.table)
library(tidyverse)
#Load Data
global_20cr <- read_csv('20CRv3.csv') %>% as.data.table()
global_era20 <- read_csv('ERA20C.csv') %>% as.data.table()
global_era5 <- read_csv('ERA_5.csv') %>% as.data.table()
global_ncep <- read_csv('NCEP_NCAR.csv') %>% as.data.table()
#Plots
p17 <- ggplot(data = global_20cr, aes(x = 2*(P_L - lag(P_L)), y = P_L + ET - lag(P_L + ET))) +
  geom_point(color = "#4daf4a", alpha = 0.4) +
  geom_smooth(color = "#4daf4a", se = FALSE, method = "lm") +
  geom_point(aes(x = 2*(P_O - lag(P_O)), y = P_O + E - lag(P_O + E)), color = "#377eb8", alpha = 0.4) +
  geom_smooth(aes(x = 2*(P_O - lag(P_O)), y = P_O + E - lag(P_O + E)), color = "#377eb8", se = FALSE, method = "lm") +
  geom_point(aes(x = 2*(P_TOT - lag(P_TOT)), y = PpE - lag(PpE)), color = "#984ea3", alpha = 0.4) +
  geom_smooth(aes(x = 2*(P_TOT - lag(P_TOT)), y = PpE - lag(PpE)), color = "#984ea3", se = FALSE, method = "lm") +
  geom_abline(intercept = 0, size = 1, linetype = "dashed") +
  labs(x = "\u03B4 (2P) [mm/day]", y = "\u03B4 (P + E) [mm/day]", title = "20CR v3 Mean", parse = TRUE, tag = "a)") +
  theme_bw() +
  scale_y_continuous(limits = c(-0.25, 0.25), expand = c(0, 0)) +
  scale_x_continuous(limits = c(-0.25, 0.25), expand = c(0, 0)) +
  theme(plot.tag = element_text(size = 20), plot.title = element_text(size=28), axis.text = element_text(size = 20), axis.title = element_text(size = 24))
ggsave("20crv3_global_deltas.png", p17, dpi = 600, limitsize = FALSE)

p18 <- ggplot(data = global_era20, aes(x = 2*(P_L - lag(P_L)), y = 2*P_L - PmE_L - lag(2*P_L - PmE_L))) +
  geom_point(color = "#4daf4a", alpha = 0.4) +
  geom_smooth(color = "#4daf4a", se = FALSE, method = "lm") +
  geom_point(aes(x = 2*(P_O - lag(P_O)), y = 2*P_O - PmE_O - lag(2*P_O - PmE_O)), color = "#377eb8", alpha = 0.4) +
  geom_smooth(aes(x = 2*(P_O - lag(P_O)), y = 2*P_O - PmE_O - lag(2*P_O - PmE_O)), color = "#377eb8", se = FALSE, method = "lm") +
  geom_point(aes(x = 2*(P - lag(P)), y = 2*P - PmE - lag(2*P - PmE)), color = "#984ea3", alpha = 0.4) +
  geom_smooth(aes(x = 2*(P - lag(P)), y = 2*P - PmE - lag(2*P - PmE)), color = "#984ea3", se = FALSE, method = "lm") +
  geom_abline(intercept = 0, size = 1, linetype = "dashed") +
  labs(x = "\u03B4 (2P) [mm/day]", y = "\u03B4 (P + E) [mm/day]", title = "ERA-20C Mean", parse = TRUE, tag = "b)") +
  theme_bw() +
  scale_y_continuous(limits = c(-0.25, 0.25), expand = c(0, 0)) +
  scale_x_continuous(limits = c(-0.25, 0.25), expand = c(0, 0)) +
  theme(plot.tag = element_text(size = 20), plot.title = element_text(size=28), axis.text = element_text(size = 20), axis.title = element_text(size = 24))
ggsave("era20_global_deltas.png", p18, dpi = 600, limitsize = FALSE)

p19 <- ggplot(data = global_era5, aes(x = 2*(P - lag(P)), y = PpE - lag(PpE))) +
  geom_point(color = "#984ea3", alpha = 0.4) +
  geom_smooth(color = "#984ea3", se = FALSE, method = "lm") +
  geom_point(aes(x = 2*(P_O - lag(P_O)), y = P_O + E_O - lag(P_O + E_O)), color = "#377eb8", alpha = 0.4) +
  geom_smooth(aes(x = 2*(P_O - lag(P_O)), y = P_O + E_O - lag(P_O + E_O)), color = "#377eb8", se = FALSE, method = "lm") +
  geom_point(aes(x = 2*(P_L - lag(P_L)), y = P_L + E_L - lag(P_L + E_L)), color = "#4daf4a", alpha = 0.4) +
  geom_smooth(aes(x = 2*(P_L - lag(P_L)), y = P_L + E_L - lag(P_L + E_L)), color = "#4daf4a", se = FALSE, method = "lm") +
  geom_abline(intercept = 0, size = 1, linetype = "dashed") +
  geom_point(color = "#984ea3", alpha = 0.4) +
  geom_smooth(color = "#984ea3", se = FALSE, method = "lm") +
  labs(x = "\u03B4 (2P) [mm/day]", y = "\u03B4 (P + E) [mm/day]", title = "ERA-5 Mean", parse = TRUE, tag = "c)") +
  theme_bw() +
  scale_y_continuous(limits = c(-0.3, 0.3), expand = c(0, 0)) +
  scale_x_continuous(limits = c(-0.3, 0.3), expand = c(0, 0)) +
  theme(plot.tag = element_text(size = 20), plot.title = element_text(size=28), axis.text = element_text(size = 20), axis.title = element_text(size = 24))
ggsave("era5_global_deltas.png", p19, dpi = 600, limitsize = FALSE)

p08 <- ggplot(data = global_ncep, aes(x = 2*(P - lag(P)), y = PpE - lag(PpE))) +
  geom_point(color = "#984ea3", alpha = 0.4) +
  geom_smooth(color = "#984ea3", se = FALSE, method = "lm") +
  geom_abline(intercept = 0, size = 1, linetype = "dashed") +
  labs(x = "\u03B4 (2P) [mm/day]", y = "\u03B4 (P + E) [mm/day]", title = "NCEP/NCAR R1 Mean", parse = TRUE, tag = "d)") +
  theme_bw() +
  scale_y_continuous(limits = c(-0.30, 0.30), expand = c(0, 0)) +
  scale_x_continuous(limits = c(-0.30, 0.30), expand = c(0, 0)) +
  theme(plot.tag = element_text(size = 20), plot.title = element_text(size=28), axis.text = element_text(size = 20), axis.title = element_text(size = 24))
ggsave("ncep_global_deltas.png", p08, dpi = 600, limitsize = FALSE)
