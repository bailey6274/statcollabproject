library(tidyverse)
library(mosaic)
library(knitr)
library(ggplot2)
library(arm)
library(broom)
library(boot)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(fields)
library(psych)
library(pROC)
library(klaR)


minnesota_stats <- read_csv('/Users/baileyh6274/Library/Mobile Documents/com~apple~CloudDocs/STAT 4680/Project_Code/player-stats-minnesota.csv')
player_info <- read_csv('/Users/baileyh6274/Library/Mobile Documents/com~apple~CloudDocs/STAT 4680/Project_Code/player-info-minnesota.csv')
minnesota_roster <- read_csv('/Users/baileyh6274/Library/Mobile Documents/com~apple~CloudDocs/STAT 4680/Project_Code/total_roster_min.csv')
minnesota_roster <- minnesota_roster[,2:ncol(minnesota_roster)]
minnesota_stats <- minnesota_stats[,2:ncol(minnesota_stats)]

minnesota_roster <- minnesota_roster %>%
  separate(HEIGHT,c('feet', 'inches'), sep = '-', convert = TRUE, remove = FALSE) %>%
  mutate(HEIGHT = 12*feet + inches) %>%
  dplyr::select(-c(inches))

minnesota_roster <- minnesota_roster %>%
  mutate(position_group = POSITION) %>%
  mutate(position_group = ifelse(substr(POSITION, 1, 1) == "C", "center", position_group)) %>%
  mutate(position_group = ifelse(substr(POSITION, 1, 1) == "F", "forward", position_group)) %>%
  mutate(position_group = ifelse(substr(POSITION, 1, 1) == "G", "guard", position_group))


minnesota <- minnesota_stats %>%
  left_join(minnesota_roster, by = c("PLAYER_ID" = "PLAYER_ID")) 


minnesota <- minnesota[minnesota$SEASON >= 2016,]








