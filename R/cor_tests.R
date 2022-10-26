#Required Libraries
library(dplyr)
library(data.table)
library(readr)
#Load Data
global_20cr <- read_csv('./../data/20cr.csv') %>% as.data.table() %>%
  .[, PpE := P + E]
global_era20 <- read_csv('./../data/era20.csv') %>% as.data.table() %>%
  .[, PpE := P + E]
global_era5 <- read_csv('./../data/era5.csv') %>% as.data.table() %>%
  .[, PpE := P + E]
global_ncep <- read_csv('./../data/ncep.csv') %>% as.data.table() %>%
  .[, PpE := P + E]

mean_20cr_T <- mean(global_20cr[Year > 1980 & Year < 2011, T])
mean_era20_T <- mean(global_era20[Year > 1980 & Year < 2011, T])
mean_era5_T <- mean(global_era5[Year > 1980 & Year < 2011, T])
mean_ncep_T <- mean(global_ncep[Year > 1980 & Year < 2011, T])

mean_20cr_P <- mean(global_20cr[Year > 1980 & Year < 2011, P])
mean_era20_P <- mean(global_era20[Year > 1980 & Year < 2011, P])
mean_era5_P <- mean(global_era5[Year > 1980 & Year < 2011, P])
mean_ncep_P <- mean(global_ncep[Year > 1980 & Year < 2011, P])

mean_20cr_PpE <- mean(global_20cr[Year > 1980 & Year < 2011, PpE])
mean_era20_PpE <- mean(global_era20[Year > 1980 & Year < 2011, PpE])
mean_era5_PpE <- mean(global_era5[Year > 1980 & Year < 2011, PpE])
mean_ncep_PpE <- mean(global_ncep[Year > 1980 & Year < 2011, PpE])

global_20cr <- global_20cr[, PpE := 100*(PpE - mean_20cr_PpE)/mean_20cr_PpE
                           ][, P := 100*(P - mean_20cr_P)/mean_20cr_P
                             ][, T := T - mean_20cr_T]
global_era20 <- global_era20[, PpE := 100*(PpE - mean_era20_PpE)/mean_era20_PpE
                             ][, P := 100*(P - mean_era20_P)/mean_era20_P
                               ][, T := T - mean_era20_T]
global_era5 <- global_era5[, PpE := 100*(PpE - mean_era5_PpE)/mean_era5_PpE
                           ][, P := 100*(P - mean_era5_P)/mean_era5_P
                             ][, T := T - mean_era5_T]
global_ncep <- global_ncep[, PpE := 100*(PpE - mean_ncep_PpE)/mean_ncep_PpE
                           ][, P := 100*(P - mean_ncep_P)/mean_ncep_P
                             ][, T := T - mean_ncep_T]
global_gpcp <- global_gpcp[, P := 100*(P - mean_gpcp_P)/mean_gpcp_P]

summary(lm(PpE ~ T, global_20cr))
summary(lm(PpE ~ T, global_era20))
summary(lm(PpE ~ T, global_era5))
summary(lm(PpE ~ T, global_ncep))

summary(lm(P ~ T, global_20cr))
summary(lm(P ~ T, global_era20))
summary(lm(P ~ T, global_era5))
summary(lm(P ~ T, global_ncep))
