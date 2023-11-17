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
library(psych)
library(pROC)
library(klaR)


minnesota_stats <- read_csv('player-stats-minnesota.csv')
minnesota_roster <- read_csv('total_roster_min.csv')

# converting the height into inches

minnesota_roster <- minnesota_roster %>%
  separate(HEIGHT,c('feet', 'inches'), sep = '-', convert = TRUE, remove = FALSE) %>%
  mutate(HEIGHT = 12*feet + inches) %>%
  dplyr::select(-c(inches))

# adding position types
minnesota_roster <- minnesota_roster %>%
  mutate(position_group = POSITION) %>%
  mutate(position_group = ifelse(substr(POSITION, 1, 1) == "C", "center", position_group)) %>%
  mutate(position_group = ifelse(substr(POSITION, 1, 1) == "F", "forward", position_group)) %>%
  mutate(position_group = ifelse(substr(POSITION, 1, 1) == "G", "guard", position_group))

# making seasonid match season 
minnesota_stats$SEASON <- rep(0, nrow(minnesota_stats))
for(i in 1:nrow(minnesota_stats)){
  temp <- as.double(unlist(strsplit(minnesota_stats$SEASON_ID[i], split = "-")))
  minnesota_stats$SEASON[i] <- temp[1]
}


# join datasets
minnesota <- minnesota_stats %>%
  left_join(minnesota_roster, by = c("PLAYER_ID" = "PLAYER_ID", "SEASON" = "SEASON")) 


write_csv(minnesota, "minnesota_data.csv")




