library(cfbfastR)


year = 2022
currentweek = 8
total_team_stats <- data.frame()

cfbd_stats_season_team <- cfbd_stats_season_team(
  year,
  season_type = "regular",
  start_week = 1,
  end_week = currentweek
)

team_stats <- cfbd_stats_season_team
team_stats$tot_plays <- team_stats$pass_atts + team_stats$rush_atts
team_stats$tpp <- team_stats$time_of_poss_total / team_stats$tot_plays
time_per_play <- team_stats[,c(2,34)]


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


time_per_play <- data.table(time_per_play)
time_per_play[, team := stri_trans_general(str = team, 
                                        id = "Latin-ASCII")]
week_games <- data.table(week_games)
week_games[, team := stri_trans_general(str = team, 
                                        id = "Latin-ASCII")]
week_games[, opp_team := stri_trans_general(str = opp_team, 
                                            id = "Latin-ASCII")]
week_games <- left_join(week_games,time_per_play)
colnames(time_per_play) <- c('opp_team','opp_tpp')
week_games <- left_join(week_games,time_per_play)
week_games <- week_games[complete.cases(week_games),]

annotations <- data.frame(
  X = c(-Inf,-Inf,Inf,Inf),
  Y =  c(-Inf, Inf,-Inf,Inf),
  text = c("Fast Game","Slow Opponent vs Fast Team",
           "Fast Opponent vs Slow Team","Slow Game"),
  x_adjust = c(0,0,1,1),
  y_adjust = c(-2,1,-2,1))

logos <- read.csv("https://raw.githubusercontent.com/sportsdataverse/cfbfastR-data/main/themes/logos.csv")
logos$school <- ifelse(logos$team_id == 23, 'San Jose State',logos$school)
logos <- logos %>% select(-.data$conference)
logos <- rename(logos, team = school)
logos <- logos[,c(2,11)]
logos <- data.table(logos)

logos[, team := stri_trans_general(str = team, 
                                   id = "Latin-ASCII")]

week_games <- left_join(week_games,logos)
ggplot(week_games, aes(tpp, opp_tpp)) + 
  geom_image(aes(image = week_games$logo), size = 0.03) +
  xlab("Time/Play") + 
  ylab("Opponent Time/Play") + 
  theme_minimal() + 
  labs(title = "Pace of Play Matchups for Week 8", subtitle = "Data courtesy: cfbfastR") + 
  geom_hline(yintercept=mean(week_games$opp_tpp), linetype="dashed", color = "red") + 
  geom_vline(xintercept=mean(week_games$tpp), linetype="dashed", color = "red") + 
  geom_text(data=annotations, aes(x=X,y=Y,hjust=x_adjust,vjust=y_adjust,label=text),color = 'red')
  

