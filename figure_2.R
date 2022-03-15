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
#20 CR
p09 <- ggplot(data = global_20cr, aes(x = dT, y = dPpE)) +
  geom_point(color = "#4daf4a") + 
  geom_point(aes(y = dPmE), color = "#377eb8") +
  geom_smooth(method = "lm", color = "#4daf4a", se = FALSE) +
  geom_smooth(aes(y = dPmE), method = "lm", color = "#377eb8", se = FALSE) +
  #annotate("label", x = -0.7, y = 0.29, label = "m[P + E] == 0.204", parse = TRUE, size = 6) +
  #annotate("label", x = -0.7, y = 0.17, label = "m[P - E] == -0.012", parse = TRUE, size = 6) +
  labs(x = "T Anomalies [°C]", y = "Flux Anomalies [mm/day]", title = "20CR v3 Global Mean 1836-2015", tag = "a)") +
  theme_bw() +
  scale_y_continuous(limits = c(-0.35, 0.35), expand = c(0, 0), breaks = seq(-0.4, 0.3, 0.1)) +
  scale_x_continuous(limits = c(-1, 0.75), expand = c(0, 0)) +
  theme(plot.tag = element_text(size = 20), plot.title = element_text(size=28), axis.text = element_text(size = 20), axis.title = element_text(size = 24))
ggsave("20crv3_global_anomalies.png", p09, dpi = 600, limitsize = FALSE)

p10 <- ggplot(data = global_20cr[Year >= 1990], aes(x = dT, y = dPpE)) +
  geom_point(color = "#4daf4a") + 
  geom_point(aes(y = dPmE), color = "#377eb8") +
  geom_smooth(method = "lm", color = "#4daf4a", se = FALSE) +
  geom_smooth(aes(y = dPmE), method = "lm", color = "#377eb8", se = FALSE) +
  #annotate("label", x = -0.125, y = 0.205, label = "m[P + E] == 0.142", parse = TRUE, size = 6) +
  #annotate("label", x = -0.125, y = 0.125, label = "m[P - E] == 0.021", parse = TRUE, size = 6) +
  labs(x = "T Anomalies [°C]", y = "Flux Anomalies [mm/day]", title = "20CR v3 Global Mean 1990-2015", tag = "b)") +
  theme_bw() +
  scale_y_continuous(limits = c(-0.25, 0.25), expand = c(0, 0), breaks = seq(-0.4, 0.3, 0.1)) +
  scale_x_continuous(limits = c(-0.3, 0.7), expand = c(0, 0)) +
  theme(plot.tag = element_text(size = 20), plot.title = element_text(size=28), axis.text = element_text(size = 20), axis.title = element_text(size = 24))
ggsave("20crv3_global_anomalies_now.png", p10, dpi = 600, limitsize = FALSE)
#ERA20
p11 <- ggplot(data = global_era20, aes(x = dT, y = dPpE)) +
  geom_point(color = "#4daf4a") + 
  geom_point(aes(y = dPmE), color = "#377eb8") +
  geom_smooth(method = "lm", color = "#4daf4a", se = FALSE) +
  geom_smooth(aes(y = dPmE), method = "lm", color = "#377eb8", se = FALSE) +
  #annotate("label", x = -0.7, y = 0.29, label = "m[P + E] == 0.184", parse = TRUE, size = 6) +
  #annotate("label", x = -0.7, y = 0.17, label = "m[P - E] == 0.003", parse = TRUE, size = 6) +
  labs(x = "T Anomalies [°C]", y = "Flux Anomalies [mm/day]", title = "ERA-20C Global Mean 1900-2010", tag = "c)") +
  theme_bw() +
  scale_y_continuous(limits = c(-0.35, 0.35), expand = c(0, 0), breaks = seq(-0.4, 0.3, 0.1)) +
  scale_x_continuous(limits = c(-1, 0.75), expand = c(0, 0)) +
  theme(plot.tag = element_text(size = 20), plot.title = element_text(size=28), axis.text = element_text(size = 20), axis.title = element_text(size = 24))
ggsave("era20_global_anomalies.png", p11, dpi = 600, limitsize = FALSE)

p12 <- ggplot(data = global_era20[Year >= 1990], aes(x = dT, y = dPpE)) +
  geom_point(color = "#4daf4a") + 
  geom_point(aes(y = dPmE), color = "#377eb8") +
  geom_smooth(method = "lm", color = "#4daf4a", se = FALSE) +
  geom_smooth(aes(y = dPmE), method = "lm", color = "#377eb8", se = FALSE) +
  #annotate("label", x = -0.125, y = 0.205, label = "m[P + E] == 0.185", parse = TRUE, size = 6) +
  #annotate("label", x = -0.125, y = 0.125, label = "m[P - E] == 0.009", parse = TRUE, size = 6) +
  labs(x = "T Anomalies [°C]", y = "Flux Anomalies [mm/day]", title = "ERA-20C Global Mean 1990-2010", tag = "d)") +
  theme_bw() +
  scale_y_continuous(limits = c(-0.25, 0.25), expand = c(0, 0), breaks = seq(-0.4, 0.3, 0.1)) +
  scale_x_continuous(limits = c(-0.3, 0.7), expand = c(0, 0)) +
  theme(plot.tag = element_text(size = 20), plot.title = element_text(size=28), axis.text = element_text(size = 20), axis.title = element_text(size = 24))
ggsave("era20_global_anomalies_now.png", p12, dpi = 600, limitsize = FALSE)
#Plots ERA5
p13 <- ggplot(data = global_era5, aes(x = dT, y = dPpE)) +
  geom_point(color = "#4daf4a") + 
  geom_point(aes(y = dPmE), color = "#377eb8") +
  geom_smooth(method = "lm", color = "#4daf4a", se = FALSE) +
  geom_smooth(aes(y = dPmE), method = "lm", color = "#377eb8", se = FALSE) +
  #annotate("label", x = -0.7, y = 0.29, label = "m[P + E] == 0.307", parse = TRUE, size = 6) +
  #annotate("label", x = -0.7, y = 0.17, label = "m[P - E] == -0.066", parse = TRUE, size = 6) +
  labs(x = "T Anomalies [°C]", y = "Flux Anomalies [mm/day]", title = "ERA-5 Global Mean 1979-now", tag = "e)") +
  theme_bw() +
  scale_y_continuous(limits = c(-0.35, 0.35), expand = c(0, 0), breaks = seq(-0.4, 0.3, 0.1)) +
  scale_x_continuous(limits = c(-1, 0.75), expand = c(0, 0)) +
  theme(plot.tag = element_text(size = 20), plot.title = element_text(size=28), axis.text = element_text(size = 20), axis.title = element_text(size = 24))
ggsave("era5_global_anomalies.png", p13, dpi = 600, limitsize = FALSE)

p14 <- ggplot(data = global_era5[Year >= 1990], aes(x = dT, y = dPpE)) +
  geom_point(color = "#4daf4a") + 
  geom_point(aes(y = dPmE), color = "#377eb8") +
  geom_smooth(method = "lm", color = "#4daf4a", se = FALSE) +
  geom_smooth(aes(y = dPmE), method = "lm", color = "#377eb8", se = FALSE) +
  #annotate("label", x = -0.125, y = 0.205, label = "m[P + E] == 0.352", parse = TRUE, size = 6) +
  #annotate("label", x = -0.125, y = 0.125, label = "m[P - E] == 0.009", parse = TRUE, size = 6) +
  labs(x = "T Anomalies [°C]", y = "Flux Anomalies [mm/day]", title = "ERA-5 Global Mean 1990-now", tag = "f)") +
  theme_bw() +
  scale_y_continuous(limits = c(-0.25, 0.25), expand = c(0, 0), breaks = seq(-0.4, 0.3, 0.1)) +
  scale_x_continuous(limits = c(-0.3, 0.7), expand = c(0, 0)) +
  theme(plot.tag = element_text(size = 20), plot.title = element_text(size=28), axis.text = element_text(size = 20), axis.title = element_text(size = 24))
ggsave("era5_global_anomalies_now.png", p14, dpi = 600, limitsize = FALSE)
#Plots NCEP
p15 <- ggplot(data = global_ncep, aes(x = dT, y = dPpE)) +
  geom_point(color = "#4daf4a") + 
  geom_point(aes(y = dPmE), color = "#377eb8") +
  geom_smooth(method = "lm", color = "#4daf4a", se = FALSE) +
  geom_smooth(aes(y = dPmE), method = "lm", color = "#377eb8", se = FALSE) +
  #annotate("label", x = -0.7, y = 0.29, label = "m[P + E] == 0.160", parse = TRUE, size = 6) +
  #annotate("label", x = -0.7, y = 0.17, label = "m[P - E] == 0.048", parse = TRUE, size = 6) +
  labs(x = "T Anomalies [°C]", y = "Flux Anomalies [mm/day]", title = "NCEP/NCAR R1 Global Mean 1948-now", tag = "g)") +
  theme_bw() +
  scale_y_continuous(limits = c(-0.35, 0.35), expand = c(0, 0), breaks = seq(-0.4, 0.3, 0.1)) +
  scale_x_continuous(limits = c(-1, 0.75), expand = c(0, 0)) +
  theme(plot.tag = element_text(size = 20), plot.title = element_text(size=28), axis.text = element_text(size = 20), axis.title = element_text(size = 24))
ggsave("ncep_global_anomalies.png", p15, dpi = 600, limitsize = FALSE)

p16 <- ggplot(data = global_ncep[Year >= 1990], aes(x = dT, y = dPpE)) +
  geom_point(color = "#4daf4a") + 
  geom_point(aes(y = dPmE), color = "#377eb8") +
  geom_smooth(method = "lm", color = "#4daf4a", se = FALSE) +
  geom_smooth(aes(y = dPmE), method = "lm", color = "#377eb8", se = FALSE) +
  #annotate("label", x = -0.125, y = 0.205, label = "m[P + E] == 0.205", parse = TRUE, size = 6) +
  #annotate("label", x = -0.125, y = 0.125, label = "m[P - E] == 0.071", parse = TRUE, size = 6) +
  labs(x = "T Anomalies [°C]", y = "Flux Anomalies [mm/day]", title = "NCEP/NCAR R1 Global Mean 1990-now", tag = "h)") +
  theme_bw() +
  scale_y_continuous(limits = c(-0.25, 0.25), expand = c(0, 0), breaks = seq(-0.4, 0.3, 0.1)) +
  scale_x_continuous(limits = c(-0.3, 0.7), expand = c(0, 0)) +
  theme(plot.tag = element_text(size = 20), plot.title = element_text(size=28), axis.text = element_text(size = 20), axis.title = element_text(size = 24))
ggsave("ncep_global_anomalies_now.png", p16, dpi = 600, limitsize = FALSE)
