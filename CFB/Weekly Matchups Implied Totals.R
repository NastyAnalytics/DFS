library('RSelenium')
library('rvest')
library('tidyverse')
library('httr')
library('data.table')
library('stringi')
library('zoo')
library('cfbfastR')
library('ggplot2')
library('ggimage')




year = 2022
currentweek = 8
odds <- read.csv('imp_totals.csv')
odds <- odds[,-c(1)]
imp_totals <- odds
imp_totals$spread <- as.numeric(imp_totals$spread)
imp_totals$spread <- ifelse(is.na(imp_totals$spread),0,imp_totals$spread)
imp_totals$ou <- as.numeric(imp_totals$ou)
imp_totals$imp_totals <- (imp_totals$ou/2) - (imp_totals$spread/2)
imp_totals <- imp_totals[,c(1,4)]
team_names <- read.csv('coach and pace names.csv')
team_names <- team_names[,c(1,7)]
colnames(team_names) <- c('team','imp_team')
team_names <- data.table(team_names)
team_names[, imp_team := stri_trans_general(str = imp_team, 
                                            id = "Latin-ASCII")]
team_names[, team := stri_trans_general(str = team, 
                                        id = "Latin-ASCII")]
imp_totals <- left_join(imp_totals,team_names)

cfbd_game_info_prev <- cfbd_game_info(year - 1)
cfbd_game_info <- cfbd_game_info(year)
cfbd_game_info_total <- rbind(cfbd_game_info,cfbd_game_info_prev)
cfbd_game_info_total <- cfbd_game_info_total[,c(1,2,3,13,16,21,24)]
week_games <- cfbd_game_info_total
week_games <- week_games %>% filter(season == year)
week_games <- week_games %>% filter(week == currentweek)
week_games <- week_games[,c(4,6)]
cfbd_game_info_total <- cfbd_game_info_total[complete.cases(cfbd_game_info_total),]
cfbd_game_info_total1 <- cfbd_game_info_total[,c(1,2,3,6,7,4,5)]
colnames(cfbd_game_info_total) <- c('game_id','year','week','team','pts','opp_team','opp_pts')
colnames(cfbd_game_info_total1) <- c('game_id','year','week','team','pts','opp_team','opp_pts')
cfbd_game_info_total <- rbind(cfbd_game_info_total,cfbd_game_info_total1)


colnames(week_games) <- c('team','opp_team')
week_games1 <- week_games[,c(2,1)]
colnames(week_games1) <- c('team','opp_team')
week_games <- rbind(week_games,week_games1)

imp_totals <- data.table(imp_totals)
imp_totals[, team := stri_trans_general(str = team, 
                                        id = "Latin-ASCII")]
week_games <- data.table(week_games)
week_games[, team := stri_trans_general(str = team, 
                                        id = "Latin-ASCII")]
week_games[, opp_team := stri_trans_general(str = opp_team, 
                                        id = "Latin-ASCII")]
week_games <- left_join(week_games,imp_totals)
colnames(imp_totals) <- c('opp_imp_team','opp_imp_totals','opp_team')
week_games <- left_join(week_games,imp_totals)
week_games <- week_games[complete.cases(week_games),]



logos <- read.csv("https://raw.githubusercontent.com/sportsdataverse/cfbfastR-data/main/themes/logos.csv")
logos$school <- ifelse(logos$team_id == 23, 'San Jose State',logos$school)
logos <- logos %>% select(-.data$conference)
logos <- rename(logos, team = school)
logos <- logos[,c(2,11)]
logos <- data.table(logos)

logos[, team := stri_trans_general(str = team, 
                                        id = "Latin-ASCII")]

week_games <- left_join(week_games,logos)

annotations <- data.frame(
  X = c(-Inf,-Inf,Inf,Inf),
  Y =  c(-Inf, Inf,-Inf,Inf),
  text = c("Low Scoring","Blowout",
           "Blowout","High Scoring"),
  x_adjust = c(0,0,1,1),
  y_adjust = c(-2,1,-2,1))

ggplot(week_games, aes(imp_totals, opp_imp_totals)) + 
  geom_image(aes(image = week_games$logo), size = 0.05) +
  xlab("Implied Totals") + 
  ylab("Opponent Implied Totals") + 
  theme_minimal() + 
  labs(title = "Implied Totals for Week 8", subtitle = "Data courtesy: DraftKings Sportsbook") + 
  geom_hline(yintercept=mean(week_games$opp_imp_totals), linetype="dashed", color = "red") + 
  geom_vline(xintercept=mean(week_games$imp_totals), linetype="dashed", color = "red") + 
  geom_text(data=annotations, aes(x=X,y=Y,hjust=x_adjust,vjust=y_adjust,label=text),color = 'red')
