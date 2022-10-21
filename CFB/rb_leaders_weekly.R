library(zoo)
library(cfbfastR)
library(tidyverse)
library(caret)
library(MLmetrics)
library(ggplot2)
library(stringi)
library(data.table)
setwd('~/Documents/CFB')


setwd("~/Documents/CFB/rushing_summaries")

rushing_summary<- read.csv('rushing_summary_22_7.csv')
rushing_summary$week <- 7
rushing_summary$year <- 2022


team_rushes <- aggregate(attempts ~ team_name + week + year,data = rushing_summary,FUN = sum)
colnames(team_rushes) <- c('team_name','week', 'year','team_rushes')
rushing_summary <- left_join(rushing_summary,team_rushes)
rushing_summary$runshare <- rushing_summary$attempts/rushing_summary$team_rushes

team_rushes <- aggregate(attempts ~ team_name + year,data = rushing_summary,FUN = sum)
player_rushes <- aggregate(attempts ~ player + year,data = rushing_summary,FUN = sum)
colnames(player_rushes) <- c('player','year','season_rushes')
colnames(team_rushes) <- c('team_name','year','season_team_rushes')
rushing_summary <- left_join(rushing_summary,team_rushes)
rushing_summary <- left_join(rushing_summary,player_rushes)
rushing_summary$total_runshare <- rushing_summary$season_rushes/rushing_summary$season_team_rushes
avg_runshare <- aggregate(runshare ~ player + year, data = rushing_summary, FUN = mean)
colnames(avg_runshare) <- c('player','year','avg_runshare')
rushing_summary <- left_join(rushing_summary,avg_runshare)
rushing_summary <- rushing_summary %>% filter(position != 'QB')


setwd('~/Documents/CFB')



rushing_summary$dk_pts <- ifelse(rushing_summary$yards >= 100,3 + (6*rushing_summary$touchdowns) + (.1*rushing_summary$yards) - (rushing_summary$fumbles) + (.1*rushing_summary$rec_yards) + (rushing_summary$receptions),(6*rushing_summary$touchdowns) + (.1*rushing_summary$yards) - (rushing_summary$fumbles) + (.1*rushing_summary$rec_yards) + (rushing_summary$receptions))
rushing_summary$fd_pts <- (6*rushing_summary$touchdowns) + (.1*rushing_summary$yards) - (2*rushing_summary$fumbles) + (.1*rushing_summary$rec_yards) + (.5*rushing_summary$receptions)


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

rushing_summary <- left_join(rushing_summary,team_names)

rushing_summary <- left_join(rushing_summary,logos)
rushing_summary <- rushing_summary[,c(38,1,4,6,9:17,20,21,26,27)] %>% 
  arrange(desc(dk_pts))
 
rushing_summary$runshare <- round(rushing_summary$runshare, digits = 3)

rushing_summary <- rushing_summary %>% top_n(20)

rushing_summary %>% gt() %>% 
  tab_header(title = "Week 7 Top RB Performances") %>%
  cols_label(logo = '',
             player = "Player", 
             attempts = "Carries",
             yards = "Rushing Yds",
             touchdowns = "TDs",
             fumbles = "Fumbles",
             ypa = "Rushing YPA",
             team_name = 'Team',
             dk_pts = 'Draftkings Pts',
             fd_pts = 'FanDuel Pts',
             team_rushes = "Team Attempts",
             longest = 'Longest Run',
             rec_yards = 'Rec Yds',
             receptions = 'Rec',
             targets = 'Targets',
             total_touches = 'Opportunities',
             runshare = 'Runshare') %>%
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

rushing_summary <- rushing_summary[,-c(1)]

write.csv(rushing_summary,'rushing_summary_week_7.csv')










