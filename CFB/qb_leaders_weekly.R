library('RSelenium')
library('rvest')
library('tidyverse')
library('httr')
library('data.table')
library('stringi')
library('zoo')
library('cfbfastR')
library("gt")

setwd('~/Documents/CFB/passing_summaries')
passing_summary<- read.csv('passing_summary_22_6.csv')
passing_summary$week <- 6
passing_summary$year <- 2022

setwd('~/Documents/CFB/rushing_summaries')
rushing_summary <- read.csv('rushing_summary_22_6.csv')
rushing_summary$week <- 6
rushing_summary$year <- 2022

setwd('~/Documents/CFB')


passing_summary$pa_dk_pts <- ifelse(passing_summary$yards >= 300,3 + (4*passing_summary$touchdowns) + (.04*passing_summary$yards) - (passing_summary$interceptions), (4*passing_summary$touchdowns) + (.04*passing_summary$yards) - (passing_summary$interceptions))
passing_summary$pa_fd_pts <- (4*passing_summary$touchdowns) + (.04*passing_summary$yards) - (passing_summary$interceptions)

rushing_summary <- rushing_summary[,-c(7,10:14)]

rushing_summary$ru_dk_pts <- ifelse(rushing_summary$yards >= 100,3 + (6*rushing_summary$touchdowns) + (.1*rushing_summary$yards) - (rushing_summary$fumbles),(6*rushing_summary$touchdowns) + (.1*rushing_summary$yards) - (rushing_summary$fumbles))
rushing_summary$ru_fd_pts <- (6*rushing_summary$touchdowns) + (.1*rushing_summary$yards) - (2*rushing_summary$fumbles)

rushing_summary <- rename(rushing_summary, rush_att = attempts)
rushing_summary <- rename(rushing_summary, rush_touchdowns = touchdowns)
rushing_summary <- rename(rushing_summary, rush_yds = yards)
rushing_summary <- rename(rushing_summary, rush_ypa = ypa)

passing_summary <- left_join(passing_summary,rushing_summary) %>% 
  mutate_all(~replace(., is.na(.), 0))

passing_summary$dk_pts <- passing_summary$pa_dk_pts + passing_summary$ru_dk_pts
passing_summary$fd_pts <- passing_summary$pa_fd_pts + passing_summary$ru_fd_pts


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

passing_summary <- left_join(passing_summary,team_names)
passing_summary <- left_join(passing_summary,logos)
passing_summary <- passing_summary[,c(40,1,4,6,8,7,11:16,21:25,28,29)] %>% 
  arrange(desc(dk_pts))

passing_summary <- passing_summary %>% top_n(20)

passing_summary %>% gt() %>% 
  tab_header(title = "Week 6 Top QB's") %>%
  cols_label(logo = '',
             player = "Player", 
             completions = "C", 
             attempts = "Att",
             yards = "Passing Yds",
             touchdowns = "Passing TDs",
             interceptions = "INTs",
             ypa = "Passing YPA",
             team_name = 'Team',
             completion_percent ='Completion %',
             dk_pts = 'Draftkings Pts',
             fd_pts = 'FanDuel Pts',
             sacks = 'Sacks Allowed',
             qb_rating = 'QBR',
             rush_att = "Carries",
             fumbles = 'Fumbles',
             rush_touchdowns = 'Rushing TDs',
             rush_yds = 'Rushing Yds',
             rush_ypa = 'Rushing YPA'
             ) %>%
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

