#Required Libraries
library(ggplot2)
library(dplyr)
library(data.table)
library(readr)
#Load Data
global_20cr <- read_csv('./data/raw/20cr.csv') %>% as.data.table()
#Plot
p00 <- ggplot(global_20cr, aes(x = E, y = P)) + 
  geom_abline(slope = 1, intercept = seq(-0.5, 0.5, 0.5), linetype = "dashed", color = "#377eb8", size = 1.5) +
  annotate("text", x = 0.5, y = 0.5, label = "P = E", angle = 45, vjust = -0.5, color = "#377eb8", size = 7) +
  geom_abline(slope = -1, intercept = seq(0.5, 1.5, 1), linetype = "dashed", color = "#4daf4a", size = 1.5) +
  geom_segment(aes(x = 0.375, y = -0.04, xend = 0.187, yend = 0.148), arrow = arrow(type="closed", length = unit(0.3, "cm")), size = 2, color = "#377eb8") +
  annotate("text", x = 0.295, y = 0.05, label = "Increasing P - E", angle = -45, vjust = -0.7, size = 6) +
  geom_segment(aes(x = 0.64, y = -0.04, xend = 1, yend = 0.32), arrow = arrow(type="closed", length = unit(0.3, "cm")), size = 2, color = "#4daf4a") +
  annotate("text", x = 0.82, y = 0.14, label = "Increasing P + E", angle = 45, vjust = -0.7, size = 7) +
  geom_label(aes(x = 1, y = 1, label = "Warmer,\ndrier"), fill = "white", size = 5, label.size = NA) +
  geom_label(aes(x = 1, y = 0, label = "Warmer,\nwetter"), fill = "white", size = 5, label.size = NA) +
  geom_label(aes(x = 0, y = 0, label = "Cooler,\nwetter"), fill = "white", size = 5, label.size = NA) +
  geom_label(aes(x = 0, y = 1, label = "Cooler,\ndrier"), fill = "white", size = 5, label.size = NA) +
  theme_bw() + 
  theme(panel.grid = element_blank(), axis.text = element_blank(), aspect.ratio = 1, plot.tag = element_text(size = 20), plot.title = element_text(size=28),  axis.title = element_text(size = 28), panel.border = element_rect(colour = "black", size = 4), axis.ticks = element_blank()) +
  scale_x_continuous(limits = c(-0.08, 1.08), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-0.08, 1.08), expand = c(0 ,0)) +
  labs(y = expression(Precipitation %->% ""), x = expression(Evaporation %->% ""), title = NULL) 
  
ggsave("fig00.pdf", p00, dpi = 600, width = 7, height = 7, units = "in", device = cairo_pdf)




