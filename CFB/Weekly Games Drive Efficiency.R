
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
cfbd_game_info_total <- cfbd_game_info_total %>% filter(home_division == 'fbs')
cfbd_game_info_total <- cfbd_game_info_total %>% filter(away_division == 'fbs')
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

i = 0
cfbd_drives_total <- data.frame()

while(i <= currentweek - 1 ){
    tryCatch({
      cfbd_drives <- cfbd_drives(year, week = i)
      
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
      assign(paste0('cfbd_drives',i),cfbd_drives)
      cfbd_drives_total <- rbind(cfbd_drives_total,cfbd_drives)
      
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
  i <- i + 1
  
}


drive_efficiency <- aggregate(epa ~ offense, data = cfbd_drives_total, FUN = mean)
def_drive_efficiency <- aggregate(epa ~ defense, data = cfbd_drives_total, FUN = mean)


colnames(drive_efficiency) <- c('team','avg_drive_efficiency')
colnames(def_drive_efficiency) <- c('team','avg_def_drive_efficiency')

efficiency <- left_join(drive_efficiency,def_drive_efficiency)
week_games <- left_join(week_games, efficiency)
colnames(efficiency) <- c('opp_team','opp_avg_drive_efficiency','opp_avg_def_drive_efficiency')
week_games <- left_join(week_games, efficiency)
week_games <- week_games[complete.cases(week_games),]


annotations <- data.frame(
  X = c(-Inf,-Inf,Inf,Inf),
  Y =  c(-Inf, Inf,-Inf,Inf),
  text = c("Inefficient Offense vs Efficient Defense","Inefficient Offense vs Inefficient Defense",
           "Efficient Offense vs Efficient Defense","Efficient Offense vs Inefficient Defense"),
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
week_games <- week_games[complete.cases(week_games),]

ggplot(week_games, aes(avg_drive_efficiency, opp_avg_def_drive_efficiency)) + 
  geom_image(aes(image = week_games$logo), size = 0.03) +
  xlab("Team Offensive Drive Efficiency") + 
  ylab("Opponent Defensive Drive Efficiency") + 
  theme_minimal() + 
  labs(title = "Drive Efficiency Matchups for Week 8", subtitle = "Data courtesy: cfbfastR") + 
  geom_hline(yintercept=mean(week_games$opp_avg_def_drive_efficiency), linetype="dashed", color = "red") + 
  geom_vline(xintercept=mean(week_games$avg_drive_efficiency), linetype="dashed", color = "red") + 
  geom_text(data=annotations, aes(x=X,y=Y,hjust=x_adjust,vjust=y_adjust,label=text),color = 'red')
