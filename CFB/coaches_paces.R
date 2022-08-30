library(cfbfastR)
library(RSelenium)
library(rvest)
library(tidyverse)
library(data.table)
library(stringi)
library(ggplot2)
library(dplyr)


setwd("~/Documents/CFB")

coaches <- cfbd_coaches(
  min_year = 2000,
  max_year = 2022
)

coaches$coach <- paste0(coaches$first_name," ",coaches$last_name)

coaches <- coaches[,c(16,4,5)]
colnames(coaches) <- c('head_coach','team','season')

team_stats_2004 <- cfbd_stats_season_team(2004)
team_stats_2005 <- cfbd_stats_season_team(2005)
team_stats_2006 <- cfbd_stats_season_team(2006)
team_stats_2007 <- cfbd_stats_season_team(2007)
team_stats_2008 <- cfbd_stats_season_team(2008)
team_stats_2009 <- cfbd_stats_season_team(2009)
team_stats_2010 <- cfbd_stats_season_team(2010)
team_stats_2011 <- cfbd_stats_season_team(2011)
team_stats_2012 <- cfbd_stats_season_team(2012)
team_stats_2013 <- cfbd_stats_season_team(2013)
team_stats_2014 <- cfbd_stats_season_team(2014)
team_stats_2015 <- cfbd_stats_season_team(2015)
team_stats_2016 <- cfbd_stats_season_team(2016)
team_stats_2017 <- cfbd_stats_season_team(2017)
team_stats_2018 <- cfbd_stats_season_team(2018)
team_stats_2019 <- cfbd_stats_season_team(2019)
team_stats_2020 <- cfbd_stats_season_team(2020)
team_stats_2021 <- cfbd_stats_season_team(2021)

team_stats_total <- rbind(team_stats_2004,
                          team_stats_2005,
                          team_stats_2006,
                          team_stats_2007,
                          team_stats_2008,
                          team_stats_2009,
                          team_stats_2010,
                          team_stats_2011,
                          team_stats_2012,
                          team_stats_2013,
                          team_stats_2014,
                          team_stats_2015,
                          team_stats_2016,
                          team_stats_2017,
                          team_stats_2018,
                          team_stats_2019,
                          team_stats_2020,
                          team_stats_2021)

team_stats_total <- left_join(team_stats_total,coaches)
team_stats_total$pace <- team_stats_total$time_of_poss_total / (team_stats_total$pass_atts + team_stats_total$rush_atts)
team_stats_total$toppg <- (team_stats_total$time_of_poss_total/team_stats_total$games)/60


year = 2003
plays_per_game_total <- data.frame()

rD <- rsDriver(browser = c("firefox"),check=F)

driver <- rD[["client"]]

driver$open()

while (year <= 2022) {
    tryCatch({
  #set URL
  url <- paste0("https://www.teamrankings.com/college-football/stat/plays-per-game?date=",year,"-02-01")
  
  # navigate to an URL
  driver$navigate(url)
  Sys.sleep(1)
  
  #Getting Nodes
  html <- driver$getPageSource()[[1]]
  team <- read_html(html) %>%
    html_nodes("td.text-left.nowrap") %>% 
    html_text()
  team <- as.data.frame(team)
  
  ppg <- read_html(html) %>%
    html_nodes("td.text-left.nowrap+ .text-right") %>% 
    html_text()
  ppg <- as.data.frame(ppg)
  
  #Combining Nodes
  plays_per_game <- cbind(team,ppg)
  plays_per_game$year <- year-1
  
  assign(paste0("plays_per_game",year),plays_per_game)
  plays_per_game_total <- rbind(plays_per_game,plays_per_game_total)
  
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  year <- year+1
}

#close the driver
driver$close()

#close the server
rD[["server"]]$stop()


year = 2004
top_total <- data.frame()

rD <- rsDriver(browser = c("firefox"),check=F)

driver <- rD[["client"]]

driver$open()

while (year <= 2022) {
  tryCatch({
    #set URL
    url <- paste0("https://www.teamrankings.com/college-football/stat/average-time-of-possession-net-of-ot?date=",year,"-02-01")
    
    # navigate to an URL
    driver$navigate(url)
    Sys.sleep(1)
    
    #Getting Nodes
    html <- driver$getPageSource()[[1]]
    team <- read_html(html) %>%
      html_nodes("td.text-left.nowrap") %>% 
      html_text()
    team <- as.data.frame(team)
    
    top <- read_html(html) %>%
      html_nodes("td.text-left.nowrap+ .text-right") %>% 
      html_text()
    top <- as.data.frame(top)
    
    #Combining Nodes
    top <- cbind(team,top)
    top$year <- year-1
    
    assign(paste0("plays_per_game",year),plays_per_game)
    top_total <- rbind(top,top_total)
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  year <- year+1
}

#close the driver
driver$close()

#close the server
rD[["server"]]$stop()

pace_total <- left_join(plays_per_game_total,top_total)
pace_total <- separate(data = pace_total, col = top, into = c("minutes", "seconds"), sep = "\\:")
pace_total <- pace_total[complete.cases(pace_total),]

pace_total$minutes <- as.numeric(pace_total$minutes)
pace_total$seconds <- as.numeric(pace_total$seconds)
pace_total$ppg <- as.numeric(pace_total$ppg)

pace_total$tpp <- ((pace_total$minutes*60) + pace_total$seconds) / pace_total$ppg

team_names <- read.csv('coach and pace names.csv')
team_names <- team_names[,c(1,2)]
team_names <- team_names[c(1:130),]


colnames(coaches) <- c('head_coach','coach.teams','year')
colnames(pace_total) <- c('pace.teams','ppg','year','minutes','seconds','tpp')

coaches <- data.table(coaches)
coaches[, "coach.teams" := stri_trans_general(str = coach.teams, 
                                                            id = "Latin-ASCII")]
pace_total <- data.table(pace_total)
pace_total[, "pace.teams" := stri_trans_general(str = pace.teams, 
                                                            id = "Latin-ASCII")]
team_names <- data.table(team_names)
team_names[, "pace.teams" := stri_trans_general(str = pace.teams, 
                                              id = "Latin-ASCII")]
team_names[, "coach.teams" := stri_trans_general(str = coach.teams, 
                                                id = "Latin-ASCII")]


pace_total <- left_join(pace_total,team_names)

pace_total <- left_join(pace_total,coaches)

pace_total$posession <- ((pace_total$minutes*60) + pace_total$seconds) / 2880

pace_total$prior_year <- pace_total$year - 1
prior_year_pace_total <- pace_total[,c(1,3,6,8,9)]
colnames(prior_year_pace_total) <- c('prior_team','prior_year','prior_tpp','head_coach','prior_possession')
pace_total <- left_join(pace_total,prior_year_pace_total)
pace_total <- pace_total[complete.cases(pace_total),]
pace_total <- pace_total[,c(3,2,6:13)]



