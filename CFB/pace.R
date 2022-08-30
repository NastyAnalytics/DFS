library(cfbfastR)


year = 2017
total_team_stats <- data.frame()
while (year <= 2022) {
  for (i in 1:16) {
    tryCatch({
    cfbd_stats_season_team <- cfbd_stats_season_team(
      year,
      season_type = "regular",
      start_week = i,
      end_week = i
    )
    cfbd_stats_season_team$week <- i
    assign(paste0('total_stats_',i,'_',year),cfbd_stats_season_team)
    total_team_stats <- rbind(cfbd_stats_season_team,total_team_stats)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
  
  year <- year + 1
}

team_stats <- total_team_stats
team_stats <- rename(team_stats, year = season)
team_stats$tot_plays <- team_stats$pass_atts + team_stats$rush_atts
team_stats$tpp <- team_stats$time_of_poss_total / team_stats$tot_plays
time_per_play <- team_stats[,c(1,2,33,35)]


time_per_play <- time_per_play %>% 
  group_by(team) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_tpp = rollapplyr(tpp, width = list(-1:-4), align = 'right', fill = NA, FUN = mean, partial = TRUE))

time_per_play <- time_per_play %>% filter(L3_tpp != Inf)

time_per_play <- time_per_play[complete.cases(time_per_play),]
write.csv(time_per_play,'time_per_play.csv')
