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
passing_summary<- read.csv('passing_summary_22_4.csv')
passing_summary$week <- 4
passing_summary$year <- 2022

setwd('~/Documents/CFB')


passing_summary$dk_pts <- ifelse(passing_summary$yards >= 300,3 + (4*passing_summary$touchdowns) + (.04*passing_summary$yards) - (passing_summary$interceptions), (4*passing_summary$touchdowns) + (.04*passing_summary$yards) - (passing_summary$interceptions))
passing_summary$fd_pts <- (4*passing_summary$touchdowns) + (.04*passing_summary$yards) - (passing_summary$interceptions)

logos <- read.csv("https://raw.githubusercontent.com/sportsdataverse/cfbfastR-data/main/themes/logos.csv")
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
passing_summary <- passing_summary[,c(31,1,4,6,8,7,11:16,19,20)] %>% 
  arrange(desc(dk_pts))

passing_summary <- passing_summary %>% top_n(10)

passing_summary %>% gt() %>% 
  tab_header(title = "Passing Performances Through the Air") %>%
  cols_label(logo = '',
             player = "Player", 
             completions = "C", 
             attempts = "Att",
             yards = "Yds",
             touchdowns = "TDs",
             interceptions = "INTs",
             ypa = "YPA",
             team_name = 'Team',
             completion_percent ='Completion %',
             dk_pts = 'Draftkings Pts',
             fd_pts = 'FanDuel Pts',
             sacks = 'Sacks Allowed',
             qb_rating = 'QBR') %>%
  text_transform(
    locations = cells_body(c(logo)),
    fn = function(logo){
      web_image(url= logo)
    }) 
