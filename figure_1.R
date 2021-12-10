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
#Plots 20CRv3
p01 <- ggplot(data = global_20cr, aes(x = Year, y = PpE)) +
  geom_line(show.legend = FALSE, color = "#4daf4a") + geom_point(color = "#4daf4a") + 
  geom_line(aes(y = (T - 14)*(6.6 - 6)/(16 - 14) + 6), color = "red") +
  annotate("label", x = 1990, y = 6.1, label = "italic(R) == 0.92", parse = TRUE, size = 6) +
  labs(x = "Year", y = "Global Mean P + E [mm/day]") +
  theme_bw() +
  scale_y_continuous(limits = c(6, 6.6), expand = c(0, 0), sec.axis = sec_axis(~(. - 6)*(16 - 14)/(6.6 - 6) + 14, name = "Global Mean T [°C]")) +
  scale_x_continuous(limits = c(1830, 2025), expand = c(0, 0), breaks = seq(1810, 2020, 30)) +
  theme(axis.text.y = element_text(size = 20), axis.title.y = element_text(size = 24), axis.text.x = element_text(size = 20), axis.title.x = element_text(size = 24))
ggsave("20crv3_global_plus.png", p01, dpi = 600, limitsize = FALSE)

p02 <- ggplot(data = global_20cr, aes(x = Year, y = PmE)) +
  geom_line(show.legend = FALSE, color = "#377eb8") + geom_point(color = "#377eb8") + 
  geom_line(aes(y = (T - 14)*(-0.1 + 0.3)/(16 - 14) - 0.3), color = "red") +
  annotate("label", x = 1990, y = -0.275, label = "italic(R) == -0.11", parse = TRUE, size = 6) +
  labs(x = "Year", y = "Global Mean P - E [mm/day]") +
  theme_bw() +
  scale_y_continuous(limits = c(-0.3, -0.1), expand = c(0, 0), sec.axis = sec_axis(~(. + 0.3)*(16 - 14)/(-0.1 + 0.3) + 14, name = "Global Mean T [°C]")) +
  scale_x_continuous(limits = c(1830, 2025), expand = c(0, 0), breaks = seq(1810, 2020, 30)) +
  theme(axis.text.y = element_text(size = 20), axis.title.y = element_text(size = 24), axis.text.x = element_text(size = 20), axis.title.x = element_text(size = 24))
ggsave("20crv3_global_minus.png", p02, dpi = 600, limitsize = FALSE)
#Plots ERA20C
p03 <- ggplot(data = global_era20, aes(x = Year, y = PpE)) +
  geom_line(show.legend = FALSE, color = "#4daf4a") + geom_point(color = "#4daf4a") + 
  geom_line(aes(y = (T - 12.5)*(5.9 - 5.5)/(14 - 12.5) + 5.5), color = "red") +
  annotate("label", x = 1990, y = 5.5, label = "italic(R) == 0.90", parse = TRUE, size = 6) +
  labs(x = "Year", y = "Global Mean P + E [mm/day]") +
  theme_bw() +
  scale_y_continuous(limits = c(5.4, 5.9), expand = c(0, 0), sec.axis = sec_axis(~(. - 5.5)*(14 - 12.5)/(5.9 - 5.5) + 12.5, name = "Global Mean T [°C]")) +
  scale_x_continuous(limits = c(1895, 2025), expand = c(0, 0), breaks = seq(1810, 2020, 30)) +
  theme(axis.text.y = element_text(size = 20), axis.title.y = element_text(size = 24), axis.text.x = element_text(size = 20), axis.title.x = element_text(size = 24))
ggsave("era20_global_plus.png", p03, dpi = 600, limitsize = FALSE)

p04 <- ggplot(data = global_era20, aes(x = Year, y = PmE)) +
  geom_line(show.legend = FALSE, color = "#377eb8") + geom_point(color = "#377eb8") + 
  geom_line(aes(y = (T - 12.5)*(-0.007 + 0.035)/(14 - 12.5) - 0.035), color = "red") +
  annotate("label", x = 2005, y = -0.035, label = "italic(R) == 0.25", parse = TRUE, size = 6) +
  labs(x = "Year", y = "Global Mean P - E [mm/day]") +
  theme_bw() +
  scale_y_continuous(limits = c(-0.04, -0.005), expand = c(0, 0), sec.axis = sec_axis(~(. + 0.035)*(14 - 12.5)/(-0.007 + 0.035) + 12.5, name = "Global Mean T [°C]")) +
  scale_x_continuous(limits = c(1895, 2025), expand = c(0, 0), breaks = seq(1810, 2020, 30)) +
  theme(axis.text.y = element_text(size = 20), axis.title.y = element_text(size = 24), axis.text.x = element_text(size = 20), axis.title.x = element_text(size = 24))
ggsave("era20_global_minus.png", p04, dpi = 600, limitsize = FALSE)
#Plots ERA5
p05 <- ggplot(data = global_era5, aes(x = Year, y = PpE)) +
  geom_line(show.legend = FALSE, color = "#4daf4a") + geom_point(color = "#4daf4a") + 
  geom_line(aes(y = (T - 13.2)*(6 - 5.5)/(14.5 - 13.2) + 5.5), color = "red") +
  annotate("label", x = 2005, y = 5.5, label = "italic(R) == 0.87", parse = TRUE, size = 6) +
  labs(x = "Year", y = "Global Mean P + E [mm/day]") +
  theme_bw() +
  scale_y_continuous(limits = c(5.45, 6), expand = c(0, 0), sec.axis = sec_axis(~(. - 5.5)*(14.5 - 13.2)/(6 - 5.5) + 13.2, name = "Global Mean T [°C]")) +
  scale_x_continuous(limits = c(1945, 2025), expand = c(0, 0), breaks = seq(1810, 2020, 30)) +
  theme(axis.text.y = element_text(size = 20), axis.title.y = element_text(size = 24), axis.text.x = element_text(size = 20), axis.title.x = element_text(size = 24))
ggsave("era5_global_plus.png", p05, dpi = 600, limitsize = FALSE)

p06 <- ggplot(data = global_era5, aes(x = Year, y = PmE)) +
  geom_line(show.legend = FALSE, color = "#377eb8") + geom_point(color = "#377eb8") + 
  geom_line(aes(y = (T - 13.2)*(0.13 + 0.04)/(14.5 - 13.2) - 0.04), color = "red") +
  annotate("label", x = 2005, y = -0.025, label = "italic(R) == -0.60", parse = TRUE, size = 6) +
  labs(x = "Year", y = "Global Mean P - E [mm/day]") +
  theme_bw() +
  scale_y_continuous(limits = c(-0.05, 0.15), expand = c(0, 0), sec.axis = sec_axis(~(. + 0.04)*(14.5 - 13.2)/(0.13 + 0.04) + 13.2, name = "Global Mean T [°C]")) +
  scale_x_continuous(limits = c(1945, 2025), expand = c(0, 0), breaks = seq(1810, 2020, 30)) +
  theme(axis.text.y = element_text(size = 20), axis.title.y = element_text(size = 24), axis.text.x = element_text(size = 20), axis.title.x = element_text(size = 24))
ggsave("era5_global_minus.png", p06, dpi = 600, limitsize = FALSE)
#Plots NCEP
p07 <- ggplot(data = global_ncep, aes(x = Year, y = PpE)) +
  geom_line(show.legend = FALSE, color = "#4daf4a") + geom_point(color = "#4daf4a") + 
  geom_line(aes(y = (T - 14)*(5.9 - 5.4)/(15.4 - 14) + 5.4), color = "red") +
  annotate("label", x = 2005, y = 5.4, label = "italic(R) == 0.35", parse = TRUE, size = 6) +
  labs(x = "Year", y = "Global Mean P + E [mm/day]") +
  theme_bw() +
  scale_y_continuous(limits = c(5.3, 6), expand = c(0, 0), sec.axis = sec_axis(~(. - 5.4)*(15.4 - 14)/(5.9 - 5.4) + 14, name = "Global Mean T [°C]")) +
  scale_x_continuous(limits = c(1945, 2025), expand = c(0, 0), breaks = seq(1810, 2020, 30)) +
  theme(axis.text.y = element_text(size = 20), axis.title.y = element_text(size = 24), axis.text.x = element_text(size = 20), axis.title.x = element_text(size = 24))
ggsave("ncep_global_plus.png", p07, dpi = 600, limitsize = FALSE)

p08 <- ggplot(data = global_ncep, aes(x = Year, y = PmE)) +
  geom_line(show.legend = FALSE, color = "#377eb8") + geom_point(color = "#377eb8") + 
  geom_line(aes(y = (T - 14)*(0.01 + 0.13)/(15.4 - 14) - 0.13), color = "red") +
  annotate("label", x = 2005, y = -0.125, label = "italic(R) == 0.41", parse = TRUE, size = 6) +
  labs(x = "Year", y = "Global Mean P - E [mm/day]") +
  theme_bw() +
  scale_y_continuous(limits = c(-0.14, 0.02), expand = c(0, 0), sec.axis = sec_axis(~(. + 0.3)*(15.4 - 14)/(0.01 + 0.13) + 14, name = "Global Mean T [°C]")) +
  scale_x_continuous(limits = c(1945, 2025), expand = c(0, 0), breaks = seq(1810, 2020, 30)) +
  theme(axis.text.y = element_text(size = 20), axis.title.y = element_text(size = 24), axis.text.x = element_text(size = 20), axis.title.x = element_text(size = 24))
ggsave("ncep_global_minus.png", p08, dpi = 600, limitsize = FALSE)

