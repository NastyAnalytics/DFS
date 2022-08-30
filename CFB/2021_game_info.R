library(cfbfastR)
library(RSelenium)
library(rvest)
library(tidyverse)
library(data.table)
library(stringi)
library(ggplot2)
library(dplyr)
library(zoo)

setwd("~/Documents/CFB")


i <- 2009
adv_stats_total <- data.frame()
while (i <= 2021) {
  for (j in 1:16) {
    tryCatch({
      adv_stats <- cfbd_stats_season_advanced(
        year = i,
        start_week = j,
        end_week = j,
      )
      adv_stats$week <- j
      assign(paste0('adv_stats_',j,'_',i),adv_stats)
      adv_stats_total <- rbind(adv_stats_total,adv_stats)
      
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
  i <- i + 1
}

adv_stats_total <- adv_stats_total[,c(1,2,6:18,20,21,25:42,45:60,64:82)]
adv_stats_total <- adv_stats_total[complete.cases(adv_stats_total),]









cfbd_metrics_ppa_predicted <- cfbd_metrics_ppa_predicted(1, 10)

cfbd_metrics_ppa_predicted1 <- cfbd_metrics_ppa_predicted(1, 9)
cfbd_metrics_ppa_predicted1 <- filter(cfbd_metrics_ppa_predicted1,yard_line == 91)

cfbd_metrics_ppa_predicted2 <- cfbd_metrics_ppa_predicted(1, 8)
cfbd_metrics_ppa_predicted2 <- filter(cfbd_metrics_ppa_predicted2,yard_line == 92)

cfbd_metrics_ppa_predicted3 <- cfbd_metrics_ppa_predicted(1, 7)
cfbd_metrics_ppa_predicted3 <- filter(cfbd_metrics_ppa_predicted3,yard_line == 93)

cfbd_metrics_ppa_predicted4 <- cfbd_metrics_ppa_predicted(1, 6)
cfbd_metrics_ppa_predicted4 <- filter(cfbd_metrics_ppa_predicted4,yard_line == 94)

cfbd_metrics_ppa_predicted5 <- cfbd_metrics_ppa_predicted(1, 5)
cfbd_metrics_ppa_predicted5 <- filter(cfbd_metrics_ppa_predicted5,yard_line == 95)

cfbd_metrics_ppa_predicted6 <- cfbd_metrics_ppa_predicted(1, 4)
cfbd_metrics_ppa_predicted6 <- filter(cfbd_metrics_ppa_predicted6,yard_line == 96)

cfbd_metrics_ppa_predicted7 <- cfbd_metrics_ppa_predicted(1, 3)
cfbd_metrics_ppa_predicted7 <- filter(cfbd_metrics_ppa_predicted7,yard_line == 97)

cfbd_metrics_ppa_predicted8 <- cfbd_metrics_ppa_predicted(1, 2)
cfbd_metrics_ppa_predicted8 <- filter(cfbd_metrics_ppa_predicted8,yard_line == 98)

cfbd_metrics_ppa_predicted9 <- cfbd_metrics_ppa_predicted(1, 1)
cfbd_metrics_ppa_predicted9 <- filter(cfbd_metrics_ppa_predicted9,yard_line == 99)

cfbd_metrics_ppa_predicted <- rbind(cfbd_metrics_ppa_predicted,
                                    cfbd_metrics_ppa_predicted1,
                                    cfbd_metrics_ppa_predicted2,
                                    cfbd_metrics_ppa_predicted3,
                                    cfbd_metrics_ppa_predicted4,
                                    cfbd_metrics_ppa_predicted5,
                                    cfbd_metrics_ppa_predicted6,
                                    cfbd_metrics_ppa_predicted7,
                                    cfbd_metrics_ppa_predicted8,
                                    cfbd_metrics_ppa_predicted9)
colnames(cfbd_metrics_ppa_predicted) <- c('start_yardline','drive_predicted_pts')


j = 2010
i = 1 
cfbd_plays_total <- data.frame()

while(j <= 2021){
  for(i in 1:16)
    tryCatch({
      cfbd_plays <- cfbd_plays(year = j, week = i)
      cfbd_plays$week <- i
      cfbd_plays$season <- j
      
      assign(paste0('cfbd_plays',i,"_",j),cfbd_plays)
      cfbd_plays_total <- rbind(cfbd_plays_total,cfbd_plays)
      
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
  j <- j + 1
  
}
avg_down <- aggregate(down ~ offense + week + season, data = cfbd_plays_total, FUN = mean)
colnames(avg_down) <- c('team','week','season','avg_down')
avg_distance <- aggregate(distance ~ offense + week + season, data = cfbd_plays_total, FUN = mean)
colnames(avg_distance) <- c('team','week','season','avg_distance')

avg_def_down <- aggregate(down ~ defense + week + season, data = cfbd_plays_total, FUN = mean)
colnames(avg_def_down) <- c('team','week','season','avg_def_down')
avg_def_distance <- aggregate(distance ~ defense + week + season, data = cfbd_plays_total, FUN = mean)
colnames(avg_def_distance) <- c('team','week','season','avg_def_distance')




j = 2010
i = 1
cfbd_drives_total <- data.frame()

while(j <= 2021){
  for(i in 1:16)
    tryCatch({
      cfbd_drives <- cfbd_drives(j, week = i)
      
      cfbd_drives <- filter(cfbd_drives, drive_result != 'END OF HALF')
      cfbd_drives <- filter(cfbd_drives, drive_result != 'END OF GAME')
      cfbd_drives <- filter(cfbd_drives, drive_result != 'END OF 4TH QUARTER')
      cfbd_drives <- filter(cfbd_drives, drive_result != 'Uncategorized')
      
      cfbd_drives$drive_pts <- cfbd_drives$end_offense_score - cfbd_drives$start_offense_score
      cfbd_drives$drive_pts <- ifelse(between(cfbd_drives$drive_pts,6,8),7,cfbd_drives$drive_pts)
      cfbd_drives <- filter(cfbd_drives,drive_pts <= 7)
      
      cfbd_drives <- left_join(cfbd_drives,cfbd_metrics_ppa_predicted)
      cfbd_drives$epa <- cfbd_drives$drive_pts - as.numeric(cfbd_drives$drive_predicted_pts)
      cfbd_drives$week <- i 
      cfbd_drives$season <- j 
      assign(paste0('cfbd_drives',i,"_",j),cfbd_drives)
      cfbd_drives_total <- rbind(cfbd_drives_total,cfbd_drives)
      
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
  j <- j + 1
  
}


drive_efficiency <- aggregate(epa ~ offense + season + week, data = cfbd_drives_total, FUN = mean)
def_drive_efficiency <- aggregate(epa ~ defense + season + week, data = cfbd_drives_total, FUN = mean)


colnames(drive_efficiency) <- c('team','season','week','avg_drive_efficiency')
colnames(def_drive_efficiency) <- c('team','season','week','avg_def_drive_efficiency')













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
pace_total <- pace_total[,c(3,2,6:13)]



library(RSelenium)
library(tidyverse)
library(cfbfastR)
library(data.table)

cfbd_game_info2004 <- cfbd_game_info(2004)
cfbd_game_info2005 <- cfbd_game_info(2005)
cfbd_game_info2006 <- cfbd_game_info(2006)
cfbd_game_info2007 <- cfbd_game_info(2007)
cfbd_game_info2008 <- cfbd_game_info(2008)
cfbd_game_info2009 <- cfbd_game_info(2009)
cfbd_game_info2010 <- cfbd_game_info(2010)
cfbd_game_info2011 <- cfbd_game_info(2011)
cfbd_game_info2012 <- cfbd_game_info(2012)
cfbd_game_info2013 <- cfbd_game_info(2013)
cfbd_game_info2014 <- cfbd_game_info(2014)
cfbd_game_info2015 <- cfbd_game_info(2015)
cfbd_game_info2016 <- cfbd_game_info(2016)
cfbd_game_info2017 <- cfbd_game_info(2017)
cfbd_game_info2018 <- cfbd_game_info(2018)
cfbd_game_info2019 <- cfbd_game_info(2019)
cfbd_game_info2020 <- cfbd_game_info(2020)
cfbd_game_info2021 <- cfbd_game_info(2021)

cfbd_game_info_total <- rbind(cfbd_game_info2004,
                              cfbd_game_info2005,
                              cfbd_game_info2006,
                              cfbd_game_info2007,
                              cfbd_game_info2008,
                              cfbd_game_info2009,
                              cfbd_game_info2010,
                              cfbd_game_info2011,
                              cfbd_game_info2012,
                              cfbd_game_info2013,
                              cfbd_game_info2014,
                              cfbd_game_info2015,
                              cfbd_game_info2016,
                              cfbd_game_info2017,
                              cfbd_game_info2018,
                              cfbd_game_info2019,
                              cfbd_game_info2020,
                              cfbd_game_info2021)



cfbd_game_info_total <- cfbd_game_info_total[,c(1,2,3,13,16,21,24)]

cfbd_game_team_stats_total <- data.frame()
i = 1
j = 2004
while(j <= 2021){
  for(i in 1:16)
  tryCatch({
    cfbd_game_team_stats <- cfbd_game_team_stats(year = j,week = i)
    assign(paste0('cfbd_game_team_stats',i,"_",j),cfbd_game_team_stats)
    cfbd_game_team_stats_total <- rbind(cfbd_game_team_stats_total,cfbd_game_team_stats)
  
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

  j <- j + 1
  
}
cfbd_game_team_stats_total <- cfbd_game_team_stats_total[,c(1,2,4,10,16)]
cfbd_game_team_stats_total <- separate(data = cfbd_game_team_stats_total, col = completion_attempts,c('completions','attempts'),sep = '-')
cfbd_game_team_stats_total <- cfbd_game_team_stats_total %>% drop_na()
ra <- as.data.frame(unlist(cfbd_game_team_stats_total$rushing_attempts))
colnames(ra) <- c('ra')
cfbd_game_team_stats_total <- cbind(cfbd_game_team_stats_total[,c(1:5)],ra)
cfbd_game_team_stats_total$ra <- as.numeric(cfbd_game_team_stats_total$ra)
cfbd_game_team_stats_total$attempts <- as.numeric(cfbd_game_team_stats_total$attempts)
cfbd_game_team_stats_total$total_plays <- cfbd_game_team_stats_total$attempts + cfbd_game_team_stats_total$ra
cfbd_game_team_stats_total$run_perc <- cfbd_game_team_stats_total$ra / cfbd_game_team_stats_total$total_plays
cfbd_game_team_stats_total$pass_perc <- cfbd_game_team_stats_total$attempts / cfbd_game_team_stats_total$total_plays

cfbd_game_team_stats_total_away <- filter(cfbd_game_team_stats_total, home_away == 'away')
cfbd_game_team_stats_total_home <- filter(cfbd_game_team_stats_total, home_away == 'home')
colnames(cfbd_game_team_stats_total_away) <- c('game_id','away_team','home_away','away_completions','away_attempts','away_ra','away_total_plays','away_run_perc','away_pass_perc')
colnames(cfbd_game_team_stats_total_home) <- c('game_id','home_team','home_away','home_completions','home_attempts','home_ra','home_total_plays','home_run_perc','home_pass_perc')
cfbd_game_team_stats_total_away <- cfbd_game_team_stats_total_away[,c(1,2,4:9)]
cfbd_game_team_stats_total_home <- cfbd_game_team_stats_total_home[,c(1,2,4:9)]

cfbd_game_info_total <- left_join(cfbd_game_team_stats_total_home, cfbd_game_info_total)
cfbd_game_info_total <- left_join(cfbd_game_team_stats_total_away, cfbd_game_info_total)
away_pace <- pace_total
home_pace <- pace_total
colnames(away_pace) <- c('season','a_ppg','a_tpp','away_team','a_coach','a_pos','a_prior_year','a_prior_team','a_prior_tpp','a_prior_pos')
colnames(home_pace) <- c('season','h_ppg','h_tpp','home_team','h_coach','h_pos','h_prior_year','h_prior_team','h_prior_tpp','h_prior_pos')

games_w_coaches <- left_join(cfbd_game_info_total, home_pace)
games_w_coaches <- left_join(games_w_coaches,away_pace)
games_w_coaches$total_plays <- games_w_coaches$away_total_plays + games_w_coaches$home_total_plays
games_w_coaches$total_score <- games_w_coaches$away_points + games_w_coaches$home_points
colnames(games_w_coaches) <- c('game_id','team','completions','attempts','ra','team_total_plays','run_perc','pass_perc','opp_team','opp_completions','opp_attempts','opp_ra','opp_total_plays','opp_run_perc','opp_pass_perc','season','week','opp_pts','pts',
                               'opp_ppg','opp_tpp','opp_coach','opp_pos','opp_prior_year','opp_prior_team','opp_prior_tpp','opp_prior_pos','ppg','tpp','coach','pos','prior_year','prior_team','prior_tpp','prior_pos','total_plays','total_score')

games_w_coaches2 <- games_w_coaches[,c(1,9:15,2:8,16:17,19,18,28:35,20:27,36,37)]
colnames(games_w_coaches2) <- c('game_id','team','completions','attempts','ra','team_total_plays','run_perc','pass_perc','opp_team','opp_completions','opp_attempts','opp_ra','opp_total_plays','opp_run_perc','opp_pass_perc','season','week','opp_pts','pts',
                               'opp_ppg','opp_tpp','opp_coach','opp_pos','opp_prior_year','opp_prior_team','opp_prior_tpp','opp_prior_pos','ppg','tpp','coach','pos','prior_year','prior_team','prior_tpp','prior_pos','total_plays','total_score')
games_w_coaches <- rbind(games_w_coaches,games_w_coaches2)

avg_run_perc <- aggregate(run_perc ~ coach + season, data = games_w_coaches, FUN = mean)
colnames(avg_run_perc) <- c('coach','prior_year','avg_run_perc')

avg_pass_perc <- aggregate(pass_perc ~ coach + season, data = games_w_coaches, FUN = mean)
colnames(avg_pass_perc) <- c('coach','prior_year','avg_pass_perc')


games_w_coaches <- left_join(games_w_coaches,avg_pass_perc)
games_w_coaches <- left_join(games_w_coaches,avg_run_perc)

i <- 2013
lines_total <- data.frame()
while (i <= 2021) {
  for (j in 1:16) {
    lines <- cfbd_betting_lines(
      year = i,
      week = j,
      line_provider = 'teamrankings'
    )
    assign(paste0('lines_',j,'_',i),lines)
    lines_total <- rbind(lines_total,lines)
    
  }
  i <- i + 1
}

lines_total <- lines_total %>% drop_na(over_under)
lines_total <- lines_total %>% drop_na(spread)

lines_total$implied_total <- (as.numeric(lines_total$over_under)/2) - (as.numeric(lines_total$spread)/2)
lines_total$opp_implied_total <- (as.numeric(lines_total$over_under)/2) + (as.numeric(lines_total$spread)/2)

lines_total$error <- abs(lines_total$implied_total - lines_total$home_score)

lines_total <- lines_total[,c(1,2,4,6,8,9,11,20,21)]
colnames(lines_total) <- c('game_id','season','week','team','pts','opp_team','opp_pts','implied_total','opp_implied_total')
lines_total1 <- lines_total[,c(1:3,6,7,4,5,9,8)]
colnames(lines_total1) <- c('game_id','season','week','team','pts','opp_team','opp_pts','implied_total','opp_implied_total')

lines_total <- rbind(lines_total,lines_total1)

games_w_coaches <-left_join(games_w_coaches,lines_total)

games_w_coaches <- games_w_coaches %>% drop_na(implied_total)

games_w_coaches$gm_pos <- games_w_coaches$team_total_plays / games_w_coaches$total_plays
games_w_coaches <- games_w_coaches[!duplicated(games_w_coaches),]
write.csv(games_w_coaches, 'games_w_coaches.csv')

games_w_coaches <- read.csv('games_w_coaches.csv')
games_w_coaches <- games_w_coaches[,-c(1)]
games_w_coaches1 <- left_join(games_w_coaches, adv_stats_total)
games_w_coaches1 <- left_join(games_w_coaches1, avg_down)
games_w_coaches1 <- left_join(games_w_coaches1, avg_distance)

games_w_coaches1 <- left_join(games_w_coaches1, avg_def_down)
games_w_coaches1 <- left_join(games_w_coaches1, avg_def_distance)

games_w_coaches1 <- left_join(games_w_coaches1, drive_efficiency)
games_w_coaches1 <- left_join(games_w_coaches1, def_drive_efficiency)
games_w_coaches1 <- games_w_coaches1[!duplicated(games_w_coaches1),]

games_w_coaches1 <- games_w_coaches1 %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_attempts = rollapply(attempts, width = list(-1:-3), align = 'right', fill = NA, FUN = mean))

games_w_coaches1 <- games_w_coaches1 %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_ra = rollapply(ra, width = list(-1:-3), align = 'right', fill = NA, FUN = mean))

games_w_coaches1 <- games_w_coaches1 %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_gm_pos = rollapply(gm_pos, width = list(-1:-3), align = 'right', fill = NA, FUN = mean))


games_w_coaches1 <- games_w_coaches1 %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_run_perc = rollapply(run_perc, width = list(-1:-3), align = 'right', fill = NA, FUN = mean))

games_w_coaches1 <- games_w_coaches1 %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_pass_perc = rollapply(pass_perc, width = list(-1:-3), align = 'right', fill = NA, FUN = mean))

games_w_coaches1 <- games_w_coaches1 %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_opp_run_perc = rollapply(opp_run_perc, width = list(-1:-3), align = 'right', fill = NA, FUN = mean))

games_w_coaches1 <- games_w_coaches1 %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_opp_pass_perc = rollapply(opp_pass_perc, width = list(-1:-3), align = 'right', fill = NA, FUN = mean))



games_w_coaches1 <- games_w_coaches1 %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_opp_pts = rollapply(opp_pts, width = list(-1:-3), align = 'right', fill = NA, FUN = mean))

games_w_coaches1 <- games_w_coaches1 %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_pts = rollapply(pts, width = list(-1:-3), align = 'right', fill = NA, FUN = mean))


games_w_coaches1 <- games_w_coaches1 %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_opp_ppg = rollapply(opp_ppg, width = list(-1:-3), align = 'right', fill = NA, FUN = mean))

games_w_coaches1 <- games_w_coaches1 %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_opp_tpp = rollapply(opp_tpp, width = list(-1:-3), align = 'right', fill = NA, FUN = mean))


games_w_coaches1 <- games_w_coaches1 %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_opp_pos = rollapply(opp_pos, width = list(-1:-3), align = 'right', fill = NA, FUN = mean))

games_w_coaches1 <- games_w_coaches1 %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_ppg = rollapply(ppg, width = list(-1:-3), align = 'right', fill = NA, FUN = mean))


games_w_coaches1 <- games_w_coaches1 %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_tpp = rollapply(tpp, width = list(-1:-3), align = 'right', fill = NA, FUN = mean))

games_w_coaches1 <- games_w_coaches1 %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_off_ppa = rollapply(off_ppa, width = list(-1:-3), align = 'right', fill = NA, FUN = mean))


games_w_coaches1 <- games_w_coaches1 %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_off_success_rate = rollapply(off_success_rate, width = list(-1:-3), align = 'right', fill = NA, FUN = mean))

games_w_coaches1 <- games_w_coaches1 %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_off_explosiveness = rollapply(off_explosiveness, width = list(-1:-3), align = 'right', fill = NA, FUN = mean))


games_w_coaches1 <- games_w_coaches1 %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_off_power_success = rollapply(off_power_success, width = list(-1:-3), align = 'right', fill = NA, FUN = mean))

games_w_coaches1 <- games_w_coaches1 %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_off_stuff_rate = rollapply(off_stuff_rate, width = list(-1:-3), align = 'right', fill = NA, FUN = mean))


games_w_coaches1 <- games_w_coaches1 %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_off_line_yds = rollapply(off_line_yds, width = list(-1:-3), align = 'right', fill = NA, FUN = mean))

games_w_coaches1 <- games_w_coaches1 %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_off_second_lvl_yds = rollapply(off_second_lvl_yds, width = list(-1:-3), align = 'right', fill = NA, FUN = mean))


games_w_coaches1 <- games_w_coaches1 %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_off_open_field_yds = rollapply(off_open_field_yds, width = list(-1:-3), align = 'right', fill = NA, FUN = mean))

games_w_coaches1 <- games_w_coaches1 %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_off_field_pos_avg_predicted_points = rollapply(off_field_pos_avg_predicted_points, width = list(-1:-3), align = 'right', fill = NA, FUN = mean))


games_w_coaches1 <- games_w_coaches1 %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_off_standard_downs_rate = rollapply(off_standard_downs_rate, width = list(-1:-3), align = 'right', fill = NA, FUN = mean))

games_w_coaches1 <- games_w_coaches1 %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_off_standard_downs_ppa = rollapply(off_standard_downs_ppa, width = list(-1:-3), align = 'right', fill = NA, FUN = mean))


games_w_coaches1 <- games_w_coaches1 %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_off_standard_downs_success_rate = rollapply(off_standard_downs_success_rate, width = list(-1:-3), align = 'right', fill = NA, FUN = mean))

games_w_coaches1 <- games_w_coaches1 %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_off_standard_downs_explosiveness = rollapply(off_standard_downs_explosiveness, width = list(-1:-3), align = 'right', fill = NA, FUN = mean))


games_w_coaches1 <- games_w_coaches1 %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_off_passing_downs_rate = rollapply(off_passing_downs_rate, width = list(-1:-3), align = 'right', fill = NA, FUN = mean))

games_w_coaches1 <- games_w_coaches1 %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_off_passing_downs_ppa = rollapply(off_passing_downs_ppa, width = list(-1:-3), align = 'right', fill = NA, FUN = mean))


games_w_coaches1 <- games_w_coaches1 %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_off_passing_downs_success_rate = rollapply(off_passing_downs_success_rate, width = list(-1:-3), align = 'right', fill = NA, FUN = mean))

games_w_coaches1 <- games_w_coaches1 %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_off_passing_downs_explosiveness = rollapply(off_passing_downs_explosiveness, width = list(-1:-3), align = 'right', fill = NA, FUN = mean))


games_w_coaches1 <- games_w_coaches1 %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_off_rushing_plays_rate = rollapply(off_rushing_plays_rate, width = list(-1:-3), align = 'right', fill = NA, FUN = mean))

games_w_coaches1 <- games_w_coaches1 %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_off_rushing_plays_ppa = rollapply(off_rushing_plays_ppa, width = list(-1:-3), align = 'right', fill = NA, FUN = mean))


games_w_coaches1 <- games_w_coaches1 %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_off_rushing_plays_success_rate = rollapply(off_rushing_plays_success_rate, width = list(-1:-3), align = 'right', fill = NA, FUN = mean))

games_w_coaches1 <- games_w_coaches1 %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_off_rushing_plays_explosiveness = rollapply(off_rushing_plays_explosiveness, width = list(-1:-3), align = 'right', fill = NA, FUN = mean))


games_w_coaches1 <- games_w_coaches1 %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_off_passing_plays_rate = rollapply(off_passing_plays_rate, width = list(-1:-3), align = 'right', fill = NA, FUN = mean))

games_w_coaches1 <- games_w_coaches1 %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_off_passing_plays_ppa = rollapply(off_passing_plays_ppa, width = list(-1:-3), align = 'right', fill = NA, FUN = mean))


games_w_coaches1 <- games_w_coaches1 %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_off_passing_plays_success_rate = rollapply(off_passing_plays_success_rate, width = list(-1:-3), align = 'right', fill = NA, FUN = mean))

games_w_coaches1 <- games_w_coaches1 %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_off_passing_plays_explosiveness = rollapply(off_passing_plays_explosiveness, width = list(-1:-3), align = 'right', fill = NA, FUN = mean))



games_w_coaches1 <- games_w_coaches1 %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_def_ppa = rollapply(def_ppa, width = list(-1:-3), align = 'right', fill = NA, FUN = mean))

games_w_coaches1 <- games_w_coaches1 %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_def_success_rate = rollapply(def_success_rate, width = list(-1:-3), align = 'right', fill = NA, FUN = mean))



games_w_coaches1 <- games_w_coaches1 %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_def_explosiveness = rollapply(def_explosiveness, width = list(-1:-3), align = 'right', fill = NA, FUN = mean))

games_w_coaches1 <- games_w_coaches1 %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_def_power_success = rollapply(def_power_success, width = list(-1:-3), align = 'right', fill = NA, FUN = mean))



games_w_coaches1 <- games_w_coaches1 %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_def_stuff_rate = rollapply(def_stuff_rate, width = list(-1:-3), align = 'right', fill = NA, FUN = mean))

games_w_coaches1 <- games_w_coaches1 %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_def_line_yds = rollapply(def_line_yds, width = list(-1:-3), align = 'right', fill = NA, FUN = mean))



games_w_coaches1 <- games_w_coaches1 %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_def_second_lvl_yds = rollapply(def_second_lvl_yds, width = list(-1:-3), align = 'right', fill = NA, FUN = mean))

games_w_coaches1 <- games_w_coaches1 %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_def_open_field_yds = rollapply(def_open_field_yds, width = list(-1:-3), align = 'right', fill = NA, FUN = mean))



games_w_coaches1 <- games_w_coaches1 %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_def_pts_per_opp = rollapply(def_pts_per_opp, width = list(-1:-3), align = 'right', fill = NA, FUN = mean))

games_w_coaches1 <- games_w_coaches1 %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3def_field_pos_avg_predicted_points = rollapply(def_field_pos_avg_predicted_points, width = list(-1:-3), align = 'right', fill = NA, FUN = mean))



games_w_coaches1 <- games_w_coaches1 %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_def_standard_downs_rate = rollapply(def_standard_downs_rate, width = list(-1:-3), align = 'right', fill = NA, FUN = mean))

games_w_coaches1 <- games_w_coaches1 %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_def_standard_downs_ppa = rollapply(def_standard_downs_ppa, width = list(-1:-3), align = 'right', fill = NA, FUN = mean))


games_w_coaches1 <- games_w_coaches1 %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_def_standard_downs_success_rate = rollapply(def_standard_downs_success_rate, width = list(-1:-3), align = 'right', fill = NA, FUN = mean))

games_w_coaches1 <- games_w_coaches1 %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_def_standard_downs_explosiveness = rollapply(def_standard_downs_explosiveness, width = list(-1:-3), align = 'right', fill = NA, FUN = mean))



games_w_coaches1 <- games_w_coaches1 %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_def_passing_downs_rate = rollapply(def_passing_downs_rate, width = list(-1:-3), align = 'right', fill = NA, FUN = mean))

games_w_coaches1 <- games_w_coaches1 %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_def_passing_downs_ppa = rollapply(def_passing_downs_ppa, width = list(-1:-3), align = 'right', fill = NA, FUN = mean))



games_w_coaches1 <- games_w_coaches1 %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_def_passing_downs_success_rate = rollapply(def_passing_downs_success_rate, width = list(-1:-3), align = 'right', fill = NA, FUN = mean))

games_w_coaches1 <- games_w_coaches1 %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_def_passing_downs_explosiveness = rollapply(def_passing_downs_explosiveness, width = list(-1:-3), align = 'right', fill = NA, FUN = mean))



games_w_coaches1 <- games_w_coaches1 %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_def_rushing_plays_rate = rollapply(def_rushing_plays_rate, width = list(-1:-3), align = 'right', fill = NA, FUN = mean))

games_w_coaches1 <- games_w_coaches1 %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_def_rushing_plays_ppa = rollapply(def_rushing_plays_ppa, width = list(-1:-3), align = 'right', fill = NA, FUN = mean))


games_w_coaches1 <- games_w_coaches1 %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_def_rushing_plays_success_rate = rollapply(def_rushing_plays_success_rate, width = list(-1:-3), align = 'right', fill = NA, FUN = mean))

games_w_coaches1 <- games_w_coaches1 %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_def_rushing_plays_explosiveness = rollapply(def_rushing_plays_explosiveness, width = list(-1:-3), align = 'right', fill = NA, FUN = mean))



games_w_coaches1 <- games_w_coaches1 %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_def_passing_plays_rate = rollapply(def_passing_plays_rate, width = list(-1:-3), align = 'right', fill = NA, FUN = mean))

games_w_coaches1 <- games_w_coaches1 %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_def_passing_plays_ppa = rollapply(def_passing_plays_ppa, width = list(-1:-3), align = 'right', fill = NA, FUN = mean))



games_w_coaches1 <- games_w_coaches1 %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_def_passing_plays_success_rate = rollapply(def_passing_plays_success_rate, width = list(-1:-3), align = 'right', fill = NA, FUN = mean))

games_w_coaches1 <- games_w_coaches1 %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_def_passing_plays_explosiveness = rollapply(def_passing_plays_explosiveness, width = list(-1:-3), align = 'right', fill = NA, FUN = mean))

games_w_coaches1 <- games_w_coaches1 %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_avg_down = rollapply(avg_down, width = list(-1:-3), align = 'right', fill = NA, FUN = mean))

games_w_coaches1 <- games_w_coaches1 %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_avg_distance = rollapply(avg_distance, width = list(-1:-3), align = 'right', fill = NA, FUN = mean))

games_w_coaches1 <- games_w_coaches1 %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_avg_def_drive_efficiency = rollapply(avg_def_drive_efficiency, width = list(-1:-3), align = 'right', fill = NA, FUN = mean))

games_w_coaches1 <- games_w_coaches1 %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_avg_drive_efficiency = rollapply(avg_drive_efficiency, width = list(-1:-3), align = 'right', fill = NA, FUN = mean))

games_w_coaches1 <- games_w_coaches1 %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_avg_def_distance = rollapply(avg_def_distance, width = list(-1:-3), align = 'right', fill = NA, FUN = mean))

games_w_coaches1 <- games_w_coaches1 %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_avg_def_down = rollapply(avg_def_down, width = list(-1:-3), align = 'right', fill = NA, FUN = mean))


games_w_coaches1$gm_pos <- games_w_coaches1$team_total_plays / games_w_coaches1$total_plays
games_w_coaches1$favorite <- ifelse(games_w_coaches1$implied_total > games_w_coaches1$opp_implied_total,1,0)
sos <- read.csv('strength_of_schedules.csv')
games_w_coaches1 <- left_join(games_w_coaches1,sos)


games_w_coaches2 <- games_w_coaches1[,c("team", "season", "week", "off_ppa", "off_total_ppa", "off_success_rate", "off_explosiveness", "off_power_success", "off_stuff_rate", "off_line_yds", "off_line_yds_total", "off_second_lvl_yds", "off_second_lvl_yds_total", "off_open_field_yds",
                                          "off_open_field_yds_total", "off_total_opportunities", "off_field_pos_avg_start", "off_field_pos_avg_predicted_points", "off_standard_downs_rate", "off_standard_downs_ppa", "off_standard_downs_success_rate", "off_standard_downs_explosiveness",
                                          "off_passing_downs_rate", "off_passing_downs_ppa", "off_passing_downs_success_rate", "off_passing_downs_explosiveness", "off_rushing_plays_rate", "off_rushing_plays_ppa", "off_rushing_plays_total_ppa", "off_rushing_plays_success_rate",
                                          "off_rushing_plays_explosiveness", "off_passing_plays_rate", "off_passing_plays_ppa", "off_passing_plays_total_ppa", "off_passing_plays_success_rate", "off_passing_plays_explosiveness", "def_ppa", "def_total_ppa", "def_success_rate",
                                          "def_explosiveness", "def_power_success", "def_stuff_rate", "def_line_yds", "def_line_yds_total", "def_second_lvl_yds", "def_second_lvl_yds_total", "def_open_field_yds", "def_open_field_yds_total", "def_total_opportunities", "def_pts_per_opp",
                                          "def_field_pos_avg_start", "def_field_pos_avg_predicted_points", "def_standard_downs_rate", "def_standard_downs_ppa", "def_standard_downs_success_rate", "def_standard_downs_explosiveness", "def_passing_downs_rate", "def_passing_downs_ppa",
                                          "def_passing_downs_total_ppa", "def_passing_downs_success_rate", "def_passing_downs_explosiveness", "def_rushing_plays_rate", "def_rushing_plays_ppa", "def_rushing_plays_total_ppa", "def_rushing_plays_success_rate",
                                          "def_rushing_plays_explosiveness", "def_passing_plays_rate", "def_passing_plays_ppa", "def_passing_plays_success_rate", "def_passing_plays_explosiveness", "L3_attempts", "L3_ra", "L3_run_perc", "L3_pass_perc", "L3_opp_run_perc", "L3_opp_pass_perc",
                                          "L3_opp_pts", "L3_pts", "L3_opp_ppg", "L3_opp_tpp", "L3_opp_pos", "L3_ppg", "L3_tpp", "L3_off_ppa", "L3_off_success_rate", "L3_off_explosiveness", "L3_off_power_success", "L3_off_stuff_rate", "L3_off_line_yds", "L3_off_second_lvl_yds",
                                          "L3_off_open_field_yds", "L3_off_field_pos_avg_predicted_points", "L3_off_standard_downs_rate", "L3_off_standard_downs_ppa", "L3_off_standard_downs_success_rate", "L3_off_standard_downs_explosiveness", "L3_off_passing_downs_rate",
                                          "L3_off_passing_downs_ppa", "L3_off_passing_downs_success_rate", "L3_off_passing_downs_explosiveness", "L3_off_rushing_plays_rate", "L3_off_rushing_plays_ppa", "L3_off_rushing_plays_success_rate", "L3_off_rushing_plays_explosiveness",
                                          "L3_off_passing_plays_rate", "L3_off_passing_plays_ppa", "L3_off_passing_plays_success_rate", "L3_off_passing_plays_explosiveness", "L3_def_ppa", "L3_def_success_rate", "L3_def_explosiveness", "L3_def_power_success", "L3_def_stuff_rate",
                                          "L3_def_line_yds", "L3_def_second_lvl_yds", "L3_def_open_field_yds", "L3_def_pts_per_opp", "L3def_field_pos_avg_predicted_points", "L3_def_standard_downs_rate", "L3_def_standard_downs_ppa", "L3_def_standard_downs_success_rate",
                                          "L3_def_standard_downs_explosiveness", "L3_def_passing_downs_rate", "L3_def_passing_downs_ppa", "L3_def_passing_downs_success_rate", "L3_def_passing_downs_explosiveness", "L3_def_rushing_plays_rate", "L3_def_rushing_plays_ppa",
                                          "L3_def_rushing_plays_success_rate", "L3_def_rushing_plays_explosiveness", "L3_def_passing_plays_rate", "L3_def_passing_plays_ppa", "L3_def_passing_plays_success_rate", "L3_def_passing_plays_explosiveness", "L3_avg_drive_efficiency","L3_avg_def_drive_efficiency",
                                        "L3_avg_down","L3_avg_def_down",
                                        "L3_avg_distance","L3_avg_def_distance")]

colnames(games_w_coaches2) <- c("opp_team", "season", "week", "opp_off_ppa", "opp_off_total_ppa", "opp_off_success_rate", "opp_off_explosiveness", "opp_off_power_success", "opp_off_stuff_rate", "opp_off_line_yds", "opp_off_line_yds_total", "opp_off_second_lvl_yds", "opp_off_second_lvl_yds_total", "opp_off_open_field_yds",
                                "opp_off_open_field_yds_total", "opp_off_total_opportunities", "opp_off_field_pos_avg_start", "opp_off_field_pos_avg_predicted_points", "opp_off_standard_downs_rate", "opp_off_standard_downs_ppa", "opp_off_standard_downs_success_rate", "opp_off_standard_downs_explosiveness",
                                "opp_off_passing_downs_rate", "opp_off_passing_downs_ppa", "opp_off_passing_downs_success_rate", "opp_off_passing_downs_explosiveness", "opp_off_rushing_plays_rate", "opp_off_rushing_plays_ppa", "opp_off_rushing_plays_total_ppa", "opp_off_rushing_plays_success_rate",
                                "opp_off_rushing_plays_explosiveness", "opp_off_passing_plays_rate", "opp_off_passing_plays_ppa", "opp_off_passing_plays_total_ppa", "opp_off_passing_plays_success_rate", "opp_off_passing_plays_explosiveness", "opp_def_ppa", "opp_def_total_ppa", "opp_def_success_rate",
                                "opp_def_explosiveness", "opp_def_power_success", "opp_def_stuff_rate", "opp_def_line_yds", "opp_def_line_yds_total", "opp_def_second_lvl_yds", "opp_def_second_lvl_yds_total", "opp_def_open_field_yds", "opp_def_open_field_yds_total", "opp_def_total_opportunities", "opp_def_pts_per_opp",
                                "opp_def_field_pos_avg_start", "opp_def_field_pos_avg_predicted_points", "opp_def_standard_downs_rate", "opp_def_standard_downs_ppa", "opp_def_standard_downs_success_rate", "opp_def_standard_downs_explosiveness", "opp_def_passing_downs_rate", "opp_def_passing_downs_ppa",
                                "opp_def_passing_downs_total_ppa", "opp_def_passing_downs_success_rate", "opp_def_passing_downs_explosiveness", "opp_def_rushing_plays_rate", "opp_def_rushing_plays_ppa", "opp_def_rushing_plays_total_ppa", "opp_def_rushing_plays_success_rate",
                                "opp_def_rushing_plays_explosiveness", "opp_def_passing_plays_rate", "opp_def_passing_plays_ppa", "opp_def_passing_plays_success_rate", "opp_def_passing_plays_explosiveness", "opp_L3_attempts", "opp_L3_ra", "opp_L3_run_perc", "opp_L3_pass_perc", "opp_L3_opp_run_perc", "opp_L3_opp_pass_perc",
                                "opp_L3_opp_pts", "opp_L3_pts", "opp_L3_opp_ppg", "opp_L3_opp_tpp", "opp_L3_opp_pos", "opp_L3_ppg", "opp_L3_tpp", "opp_L3_off_ppa", "opp_L3_off_success_rate", "opp_L3_off_explosiveness", "opp_L3_off_power_success", "opp_L3_off_stuff_rate", "opp_L3_off_line_yds", "opp_L3_off_second_lvl_yds",
                                "opp_L3_off_open_field_yds", "opp_L3_off_field_pos_avg_predicted_points", "opp_L3_off_standard_downs_rate", "opp_L3_off_standard_downs_ppa", "opp_L3_off_standard_downs_success_rate", "opp_L3_off_standard_downs_explosiveness", "opp_L3_off_passing_downs_rate",
                                "opp_L3_off_passing_downs_ppa", "opp_L3_off_passing_downs_success_rate", "opp_L3_off_passing_downs_explosiveness", "opp_L3_off_rushing_plays_rate", "opp_L3_off_rushing_plays_ppa", "opp_L3_off_rushing_plays_success_rate", "opp_L3_off_rushing_plays_explosiveness",
                                "opp_L3_off_passing_plays_rate", "opp_L3_off_passing_plays_ppa", "opp_L3_off_passing_plays_success_rate", "opp_L3_off_passing_plays_explosiveness", "opp_L3_def_ppa", "opp_L3_def_success_rate", "opp_L3_def_explosiveness", "opp_L3_def_power_success", "opp_L3_def_stuff_rate",
                                "opp_L3_def_line_yds", "opp_L3_def_second_lvl_yds", "opp_L3_def_open_field_yds", "opp_L3_def_pts_per_opp", "opp_L3def_field_pos_avg_predicted_points", "opp_L3_def_standard_downs_rate", "opp_L3_def_standard_downs_ppa", "opp_L3_def_standard_downs_success_rate",
                                "opp_L3_def_standard_downs_explosiveness", "opp_L3_def_passing_downs_rate", "opp_L3_def_passing_downs_ppa", "opp_L3_def_passing_downs_success_rate", "opp_L3_def_passing_downs_explosiveness", "opp_L3_def_rushing_plays_rate", "opp_L3_def_rushing_plays_ppa",
                                "opp_L3_def_rushing_plays_success_rate", "opp_L3_def_rushing_plays_explosiveness", "opp_L3_def_passing_plays_rate", "opp_L3_def_passing_plays_ppa", "opp_L3_def_passing_plays_success_rate", "opp_L3_def_passing_plays_explosiveness", "opp_L3_avg_drive_efficiency","opp_L3_avg_def_drive_efficiency",
                                "opp_L3_avg_down","opp_L3_avg_def_down",
                                "opp_L3_avg_distance","opp_L3_avg_def_distance")

games_w_coaches1 <- left_join(games_w_coaches1,games_w_coaches2)
games_w_coaches1 <- games_w_coaches1[complete.cases(games_w_coaches1),]


corr <- games_w_coaches1[,c("attempts", "ra","gm_pos", "team_total_plays", "run_perc", "pass_perc", "opp_attempts", "opp_ra",
                            "opp_run_perc", "opp_pass_perc", "opp_pts", "pts", "opp_ppg", "opp_tpp", "opp_pos", "opp_prior_tpp", "opp_prior_pos",
                            "ppg", "tpp", "prior_tpp", "prior_pos", "total_plays", "total_score", "avg_pass_perc", "avg_run_perc", "implied_total", "opp_implied_total",
                            "off_ppa", "off_success_rate", "off_explosiveness", "off_power_success", "off_stuff_rate", "off_line_yds", "off_second_lvl_yds",
                            "off_open_field_yds", "off_field_pos_avg_predicted_points",
                            "off_standard_downs_rate", "off_standard_downs_ppa", "off_standard_downs_success_rate", "off_standard_downs_explosiveness", "off_passing_downs_rate", "off_passing_downs_ppa",
                            "off_passing_downs_success_rate", "off_passing_downs_explosiveness", "off_rushing_plays_rate", "off_rushing_plays_ppa",  "off_rushing_plays_success_rate",
                            "off_rushing_plays_explosiveness", "off_passing_plays_rate", "off_passing_plays_ppa", "off_passing_plays_success_rate", "off_passing_plays_explosiveness",
                            "def_ppa", "def_success_rate", "def_explosiveness", "def_power_success", "def_stuff_rate", "def_line_yds", "def_second_lvl_yds",
                            "def_open_field_yds", "def_pts_per_opp", "def_field_pos_avg_predicted_points",
                            "def_standard_downs_rate", "def_standard_downs_ppa", "def_standard_downs_success_rate", "def_standard_downs_explosiveness", "def_passing_downs_rate", "def_passing_downs_ppa",
                            "def_passing_downs_success_rate", "def_passing_downs_explosiveness", "def_rushing_plays_rate", "def_rushing_plays_ppa",
                            "def_rushing_plays_success_rate", "def_rushing_plays_explosiveness", "def_passing_plays_rate", "def_passing_plays_ppa", "def_passing_plays_success_rate", "def_passing_plays_explosiveness",
                            
                            "L3_attempts", "L3_ra", "L3_gm_pos", "L3_run_perc", "L3_pass_perc",
                            "L3_opp_run_perc", "L3_opp_pass_perc", "L3_opp_pts", "L3_pts", "L3_opp_ppg", "L3_opp_tpp", "L3_opp_pos",
                            "L3_ppg", "L3_tpp",
                            "L3_off_ppa", "L3_off_success_rate", "L3_off_explosiveness", "L3_off_power_success", "L3_off_stuff_rate", "L3_off_line_yds", "L3_off_second_lvl_yds",
                            "L3_off_open_field_yds",
                            "L3_off_standard_downs_rate", "L3_off_standard_downs_ppa", "L3_off_standard_downs_success_rate", "L3_off_standard_downs_explosiveness", "L3_off_passing_downs_rate", "L3_off_passing_downs_ppa",
                            "L3_off_passing_downs_success_rate", "L3_off_passing_downs_explosiveness", "L3_off_rushing_plays_rate", "L3_off_rushing_plays_ppa",  "L3_off_rushing_plays_success_rate",
                            "L3_off_rushing_plays_explosiveness", "L3_off_passing_plays_rate", "L3_off_passing_plays_ppa", "L3_off_passing_plays_success_rate", "L3_off_passing_plays_explosiveness",
                            "L3_def_ppa", "L3_def_success_rate", "L3_def_explosiveness", "L3_def_power_success", "L3_def_stuff_rate", "L3_def_line_yds", "L3_def_second_lvl_yds",
                            "L3_def_open_field_yds", "L3_def_pts_per_opp",
                            "L3_def_standard_downs_rate", "L3_def_standard_downs_ppa", "L3_def_standard_downs_success_rate", "L3_def_standard_downs_explosiveness", "L3_def_passing_downs_rate", "L3_def_passing_downs_ppa",
                            "L3_def_passing_downs_success_rate", "L3_def_passing_downs_explosiveness", "L3_def_rushing_plays_rate", "L3_def_rushing_plays_ppa",
                            "L3_def_rushing_plays_success_rate", "L3_def_rushing_plays_explosiveness", "L3_def_passing_plays_rate", "L3_def_passing_plays_ppa", "L3_def_passing_plays_success_rate", "L3_def_passing_plays_explosiveness",
                            "L3_avg_drive_efficiency","L3_avg_down","L3_avg_distance","L3_avg_def_drive_efficiency","L3_avg_def_down","L3_avg_def_distance", "L4_sos",
                            
                            "opp_off_ppa", "opp_off_total_ppa", "opp_off_success_rate", "opp_off_explosiveness", "opp_off_power_success", "opp_off_stuff_rate", "opp_off_line_yds", "opp_off_line_yds_total", "opp_off_second_lvl_yds", "opp_off_second_lvl_yds_total", "opp_off_open_field_yds",
                            "opp_off_open_field_yds_total", "opp_off_total_opportunities", "opp_off_field_pos_avg_start", "opp_off_field_pos_avg_predicted_points", "opp_off_standard_downs_rate", "opp_off_standard_downs_ppa", "opp_off_standard_downs_success_rate", "opp_off_standard_downs_explosiveness",
                            "opp_off_passing_downs_rate", "opp_off_passing_downs_ppa", "opp_off_passing_downs_success_rate", "opp_off_passing_downs_explosiveness", "opp_off_rushing_plays_rate", "opp_off_rushing_plays_ppa", "opp_off_rushing_plays_total_ppa", "opp_off_rushing_plays_success_rate",
                            "opp_off_rushing_plays_explosiveness", "opp_off_passing_plays_rate", "opp_off_passing_plays_ppa", "opp_off_passing_plays_total_ppa", "opp_off_passing_plays_success_rate", "opp_off_passing_plays_explosiveness", "opp_def_ppa", "opp_def_total_ppa", "opp_def_success_rate",
                            "opp_def_explosiveness", "opp_def_power_success", "opp_def_stuff_rate", "opp_def_line_yds", "opp_def_line_yds_total", "opp_def_second_lvl_yds", "opp_def_second_lvl_yds_total", "opp_def_open_field_yds", "opp_def_open_field_yds_total", "opp_def_total_opportunities", "opp_def_pts_per_opp",
                            "opp_def_field_pos_avg_start", "opp_def_field_pos_avg_predicted_points", "opp_def_standard_downs_rate", "opp_def_standard_downs_ppa", "opp_def_standard_downs_success_rate", "opp_def_standard_downs_explosiveness", "opp_def_passing_downs_rate", "opp_def_passing_downs_ppa",
                            "opp_def_passing_downs_total_ppa", "opp_def_passing_downs_success_rate", "opp_def_passing_downs_explosiveness", "opp_def_rushing_plays_rate", "opp_def_rushing_plays_ppa", "opp_def_rushing_plays_total_ppa", "opp_def_rushing_plays_success_rate",
                            "opp_def_rushing_plays_explosiveness", "opp_def_passing_plays_rate", "opp_def_passing_plays_ppa", "opp_def_passing_plays_success_rate", "opp_def_passing_plays_explosiveness", 
                            
                            "opp_L3_attempts", "opp_L3_ra", "opp_L3_run_perc", "opp_L3_pass_perc", "opp_L3_opp_run_perc", "opp_L3_opp_pass_perc",
                            "opp_L3_opp_pts", "opp_L3_pts", "opp_L3_opp_ppg", "opp_L3_opp_tpp", "opp_L3_opp_pos", "opp_L3_ppg", "opp_L3_tpp", "opp_L3_off_ppa", "opp_L3_off_success_rate", "opp_L3_off_explosiveness", "opp_L3_off_power_success", "opp_L3_off_stuff_rate", "opp_L3_off_line_yds", "opp_L3_off_second_lvl_yds",
                            "opp_L3_off_open_field_yds", "opp_L3_off_field_pos_avg_predicted_points", "opp_L3_off_standard_downs_rate", "opp_L3_off_standard_downs_ppa", "opp_L3_off_standard_downs_success_rate", "opp_L3_off_standard_downs_explosiveness", "opp_L3_off_passing_downs_rate",
                            "opp_L3_off_passing_downs_ppa", "opp_L3_off_passing_downs_success_rate", "opp_L3_off_passing_downs_explosiveness", "opp_L3_off_rushing_plays_rate", "opp_L3_off_rushing_plays_ppa", "opp_L3_off_rushing_plays_success_rate", "opp_L3_off_rushing_plays_explosiveness",
                            "opp_L3_off_passing_plays_rate", "opp_L3_off_passing_plays_ppa", "opp_L3_off_passing_plays_success_rate", "opp_L3_off_passing_plays_explosiveness", "opp_L3_def_ppa", "opp_L3_def_success_rate", "opp_L3_def_explosiveness", "opp_L3_def_power_success", "opp_L3_def_stuff_rate",
                            "opp_L3_def_line_yds", "opp_L3_def_second_lvl_yds", "opp_L3_def_open_field_yds", "opp_L3_def_pts_per_opp", "opp_L3def_field_pos_avg_predicted_points", "opp_L3_def_standard_downs_rate", "opp_L3_def_standard_downs_ppa", "opp_L3_def_standard_downs_success_rate",
                            "opp_L3_def_standard_downs_explosiveness", "opp_L3_def_passing_downs_rate", "opp_L3_def_passing_downs_ppa", "opp_L3_def_passing_downs_success_rate", "opp_L3_def_passing_downs_explosiveness", "opp_L3_def_rushing_plays_rate", "opp_L3_def_rushing_plays_ppa",
                            "opp_L3_def_rushing_plays_success_rate", "opp_L3_def_rushing_plays_explosiveness", "opp_L3_def_passing_plays_rate", "opp_L3_def_passing_plays_ppa", "opp_L3_def_passing_plays_success_rate", "opp_L3_def_passing_plays_explosiveness",
                            "opp_L3_avg_drive_efficiency","opp_L3_avg_down","opp_L3_avg_distance","opp_L3_avg_def_drive_efficiency","opp_L3_avg_def_down","opp_L3_avg_def_distance","L4_opp_sos")]
write.csv(games_w_coaches1,'games_w_coaches1.csv')
games_w_coaches1 <- read.csv('games_w_coaches1.csv')

p_att <- games_w_coaches1[,c("attempts", "L3_attempts", "L3_ra", "L3_run_perc", "L3_pass_perc", "L3_opp_pts", "L3_pts", "L3_tpp",
                             "L3_off_line_yds", "L3_off_second_lvl_yds", "L3_off_open_field_yds", "L3_off_passing_downs_ppa",
                             "L3_off_passing_downs_success_rate", "L3_off_rushing_plays_rate", "L3_off_rushing_plays_ppa",
                             "L3_off_rushing_plays_explosiveness", "L3_off_passing_plays_rate", "L3_off_passing_plays_success_rate",
                             "L3_off_passing_plays_explosiveness", "L3_def_open_field_yds", "L3_avg_distance", "opp_L3_opp_run_perc",
                             "opp_L3_opp_pass_perc", "opp_L3_pts", "opp_L3_tpp", "opp_L3_off_ppa", "opp_L3_off_success_rate",
                             "opp_L3_off_explosiveness", "opp_L3_off_standard_downs_ppa", "opp_L3_off_standard_downs_success_rate",
                             "opp_L3_off_standard_downs_explosiveness", "opp_L3_off_passing_plays_ppa", "opp_L3_off_passing_plays_success_rate",
                             "opp_L3_def_line_yds", "opp_L3_def_rushing_plays_rate", "opp_L3_def_passing_plays_rate", "opp_L3_avg_def_distance",
                             "favorite")]

p_att <- p_att[!duplicated(p_att),]

library(MLmetrics)
library(caret) 



sum(is.na(p_att))

set.seed(100)

TrainingIndex <- createDataPartition(p_att$attempts, p=0.8, list = FALSE)
TrainingSet <- p_att[TrainingIndex,] 
TestingSet <- p_att[-TrainingIndex,] 

TrainControl <- trainControl( method = "repeatedcv", number = 10, repeats = 4)


xgboost_p_att_model <- train(attempts ~ ., data = TrainingSet,
                             method = "xgbTree",
                             na.action = na.omit,
                             preProcess=c("scale","center"),
                             trControl= TrainControl,
                             verbosity = 0
)

xgboost_p_att_model.training <-predict(xgboost_p_att_model, TrainingSet) 
xgboost_p_att_model.testing <-predict(xgboost_p_att_model, TestingSet) 


plot(TrainingSet$attempts,xgboost_p_att_model.training, col = "blue" )
plot(TestingSet$attempts,xgboost_p_att_model.testing, col = "blue" )

summary(xgboost_p_att_model)

xgboost_p_att_r.training <- cor(TrainingSet$attempts,xgboost_p_att_model.training)
xgboost_p_att_r.testing <- cor(TestingSet$attempts,xgboost_p_att_model.testing)

xgboost_p_att_r2.training <- xgboost_p_att_r.training^2
xgboost_p_att_r2.testing <- xgboost_p_att_r.testing^2

xgboost_p_att_actuals_preds <- data.frame(cbind(actuals=(TestingSet$attempts), predicteds=(xgboost_p_att_model.testing)))
xgboost_p_att_actuals_preds$diff <- (xgboost_p_att_actuals_preds$actuals - xgboost_p_att_actuals_preds$predicteds)

TestingSet <- cbind(TestingSet,xgboost_p_att_actuals_preds)

plot(TestingSet$actuals,TestingSet$predicteds, col = "blue" )

xgboost_p_att_min_max_accuracy <- mean(apply(xgboost_p_att_actuals_preds, 1, min) / apply(xgboost_p_att_actuals_preds, 1, max))  
xgboost_p_att_mape <- MAPE(xgboost_p_att_actuals_preds$predicteds, xgboost_p_att_actuals_preds$actuals)
xgboost_p_att_RMSE <- sqrt(mean((TestingSet$actuals - TestingSet$predicteds)^2))
xgboost_p_att_MAE <- mean(abs(TestingSet$actuals - TestingSet$predicteds))

TestingSet <- left_join(TestingSet, p_att)
saveRDS(xgboost_p_att_model, "xgboost_p_att_model.rds")

r_att <- games_w_coaches1[,c( "ra",                                    "L3_attempts",                          
                              "L3_ra",                        
                              "L3_run_perc",                           "L3_pass_perc",    
                              "L3_tpp",                               
                              "L3_off_success_rate",                  
                              "L3_off_power_success",                  "L3_off_stuff_rate",                    
                              "L3_off_line_yds",                       "L3_off_second_lvl_yds",                
                              "L3_off_open_field_yds",                 "L3_off_standard_downs_rate",           
                              "L3_off_standard_downs_ppa",             "L3_off_standard_downs_success_rate",   
                              "L3_off_passing_downs_rate",             "L3_off_rushing_plays_rate",            
                              "L3_off_rushing_plays_ppa",              "L3_off_rushing_plays_success_rate",    
                              "L3_off_rushing_plays_explosiveness",    "L3_off_passing_plays_rate",            
                              "L3_off_passing_plays_explosiveness",
                              "L3_avg_distance",                       "opp_L3_attempts",                      
                              "opp_L3_ra",                             "opp_L3_run_perc",                      
                              "opp_L3_pass_perc",                      "opp_L3_opp_run_perc",                  
                              "opp_L3_opp_pass_perc",                  "opp_L3_opp_pts",                       
                              "opp_L3_pts",                            "opp_L3_tpp",                           
                              "opp_L3_off_ppa",                        "opp_L3_off_success_rate",              
                              "opp_L3_off_standard_downs_rate",        "opp_L3_off_standard_downs_ppa",        
                              "opp_L3_off_standard_downs_success_rate","opp_L3_off_passing_downs_rate",        
                              "opp_L3_off_passing_downs_ppa",          "opp_L3_off_rushing_plays_rate",        
                              "opp_L3_off_passing_plays_rate",        
                              "opp_L3_off_passing_plays_ppa",          "opp_L3_off_passing_plays_success_rate",
                              "opp_L3_off_passing_plays_explosiveness","opp_L3_def_ppa",                       
                              "opp_L3_def_success_rate",               "opp_L3_def_line_yds",                  
                              "opp_L3_def_second_lvl_yds",             "opp_L3_def_standard_downs_rate",       
                              "opp_L3_def_standard_downs_ppa",         "opp_L3_def_standard_downs_success_rate",
                              "opp_L3_def_passing_downs_rate",         "opp_L3_def_passing_downs_success_rate",
                              "opp_L3_def_rushing_plays_rate",         "opp_L3_def_rushing_plays_ppa",         
                              "opp_L3_def_passing_plays_rate",       
                              "opp_L3_avg_def_drive_efficiency",       "opp_L3_avg_def_down",                  
                              "opp_L3_avg_def_distance"   )]
r_att <- r_att[!duplicated(r_att),]


library(MLmetrics)
library(caret) 



sum(is.na(r_att))

set.seed(100)

TrainingIndex <- createDataPartition(r_att$ra, p=0.8, list = FALSE)
TrainingSet <- r_att[TrainingIndex,] 
TestingSet <- r_att[-TrainingIndex,] 

TrainControl <- trainControl( method = "repeatedcv", number = 10, repeats = 4)


xgboost_r_att_model <- train(ra ~ ., data = TrainingSet,
                             method = "xgbTree",
                             na.action = na.omit,
                             preProcess=c("scale","center"),
                             trControl= TrainControl
)

xgboost_r_att_model.training <-predict(xgboost_r_att_model, TrainingSet) 
xgboost_r_att_model.testing <-predict(xgboost_r_att_model, TestingSet) 


plot(TrainingSet$ra,xgboost_r_att_model.training, col = "blue" )
plot(TestingSet$ra,xgboost_r_att_model.testing, col = "blue" )

summary(xgboost_r_att_model)

xgboost_r_att_r.training <- cor(TrainingSet$ra,xgboost_r_att_model.training)
xgboost_r_att_r.testing <- cor(TestingSet$ra,xgboost_r_att_model.testing)

xgboost_r_att_r2.training <- xgboost_r_att_r.training^2
xgboost_r_att_r2.testing <- xgboost_r_att_r.testing^2

xgboost_r_att_actuals_preds <- data.frame(cbind(actuals=(TestingSet$ra), predicteds=(xgboost_r_att_model.testing)))
xgboost_r_att_actuals_preds$diff <- (xgboost_r_att_actuals_preds$actuals - xgboost_r_att_actuals_preds$predicteds)

TestingSet <- cbind(TestingSet,xgboost_r_att_actuals_preds)

plot(TestingSet$actuals,TestingSet$predicteds, col = "blue" )

xgboost_r_att_min_max_accuracy <- mean(apply(xgboost_r_att_actuals_preds, 1, min) / apply(xgboost_r_att_actuals_preds, 1, max))  
xgboost_r_att_mape <- MAPE(xgboost_r_att_actuals_preds$predicteds, xgboost_r_att_actuals_preds$actuals)
xgboost_r_att_RMSE <- sqrt(mean((TestingSet$actuals - TestingSet$predicteds)^2))
xgboost_r_att_MAE <- mean(abs(TestingSet$actuals - TestingSet$predicteds))

TestingSet <- left_join(TestingSet, r_att)
saveRDS(xgboost_r_att_model, "xgboost_r_att_model.rds")

comp <- left_join(TestingSet,games_w_coaches1)

games_w_coaches1 <- write.csv(games_w_coaches1,'games_w_coaches1.csv')

