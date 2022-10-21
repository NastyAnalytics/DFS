library(zoo)
library(cfbfastR)
library(tidyverse)
library(caret)
library(MLmetrics)
library(ggplot2)
library(stringi)
library(data.table)
setwd('~/Documents/CFB')


setwd("~/Documents/CFB/receivers_targets")

receiving_summary<- read.csv('receiving_summary_22_7.csv')
receiving_summary$week <- 7
receiving_summary$year <- 2022


team_targets <- aggregate(targets ~ team_name + week + year,data = receiving_summary,FUN = sum)
colnames(team_targets) <- c('team_name','week','year','team_targets')
receiving_summary <- left_join(receiving_summary,team_targets)
receiving_summary$rec_usage <- receiving_summary$targets/receiving_summary$team_targets

team_total_targets <- aggregate(targets ~ team_name + year,data = receiving_summary,FUN = sum)
total_targets <- aggregate(targets ~ player + team_name + year,data = receiving_summary,FUN = sum)
colnames(total_targets) <- c('player','team_name','year','season_targets')
colnames(team_total_targets) <- c('team_name','year','season_team_targets')
receiving_summary <- left_join(receiving_summary,team_total_targets)
receiving_summary <- left_join(receiving_summary,total_targets)
receiving_summary$season_usage <- receiving_summary$season_targets/receiving_summary$season_team_targets



setwd('~/Documents/CFB')



receiving_summary$dk_pts <- ifelse(receiving_summary$yards >= 100,3 + (.1*receiving_summary$yards) + (6*receiving_summary$touchdowns) + (receiving_summary$receptions) - (receiving_summary$fumbles),(.1*receiving_summary$yards) + (6*receiving_summary$touchdowns) + (receiving_summary$receptions) - (receiving_summary$fumbles))
receiving_summary$fd_pts <- (.1*receiving_summary$yards) + (6*receiving_summary$touchdowns) + (0.5*receiving_summary$receptions) - (2*receiving_summary$fumbles)


logos <- read.csv("https://raw.githubusercontent.com/sportsdataverse/cfbfastR-data/main/themes/logos.csv")
logos$school <- ifelse(logos$team_id == 23, 'San Jose State',logos$school)
logos <- logos %>% select(-.data$conference)
logos <- rename(logos, team = school)

team_names <- read.csv('coach and pace names.csv')
team_names <- team_names[,c(1,3)]
colnames(team_names) <- c('team','team_name')
team_names <- data.table(team_names)
team_names[, team := stri_trans_general(str = team, 
                                        id = "Latin-ASCII")]

receiving_summary <- left_join(receiving_summary,team_names)

receiving_summary <- left_join(receiving_summary,logos)
receiving_summary <- receiving_summary[,c(35,1,4,6,9:15,19,23:24)] %>% 
  arrange(desc(dk_pts))

receiving_summary$rec_usage <- round(receiving_summary$rec_usage, digits = 3)

receiving_summary <- receiving_summary %>% top_n(20)

receiving_summary %>% gt() %>% 
  tab_header(title = "Week 7 Top Receiving Performances") %>%
  cols_label(logo = '',
             player = "Player", 
             caught_percent = "Catch %",
             yards = "Rec Yds",
             touchdowns = "TDs",
             fumbles = "Fumbles",
             yards_per_reception = "Yds Per Rec",
             team_name = 'Team',
             dk_pts = 'Draftkings Pts',
             fd_pts = 'FanDuel Pts',
             longest = 'Longest Rec',
             receptions = 'Rec',
             targets = 'Targets',
             rec_usage = 'Target Share') %>%
  cols_align(
    .,
    align = c("center"),
    columns = everything()
  ) %>%
  text_transform(
    locations = cells_body(c(logo)),
    fn = function(logo){
      web_image(url= logo)
    }) 

receiving_summary <- receiving_summary[,-c(1)]

write.csv(receiving_summary,'receiving_summary_week_7.csv')











