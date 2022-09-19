cfbd_game_team_stats_total <- data.frame()
i = 2
j = 2022

cfbd_game_team_stats <- cfbd_game_team_stats(year = j,week = i)
cfbd_game_team_stats <- cfbd_game_team_stats[,c(1,2,4,10,16)]


cfbd_game_team_stats_total <- rbind(cfbd_game_team_stats_total,cfbd_game_team_stats)

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
cfbd_game_team_stats_total <- left_join(cfbd_game_team_stats_total_home, cfbd_game_team_stats_total_away)

colnames(cfbd_game_team_stats_total) <- c('game_id','team','completions','attempts','ra','team_total_plays','run_perc','pass_perc','opp_team','opp_completions','opp_attempts','opp_ra','opp_total_plays','opp_run_perc','opp_pass_perc')

cfbd_game_team_stats_total1 <- cfbd_game_team_stats_total[,c(1,9:15,2:8)]
colnames(cfbd_game_team_stats_total1) <- c('game_id','team','completions','attempts','ra','team_total_plays','run_perc','pass_perc','opp_team','opp_completions','opp_attempts','opp_ra','opp_total_plays','opp_run_perc','opp_pass_perc')
cfbd_game_team_stats_total <- rbind(cfbd_game_team_stats_total,cfbd_game_team_stats_total1)

setwd('~/Documents/CFB')
week_games <- read.csv('est_p_att_and_r_att.csv')
week_games <- week_games[,-c(1)]
cfbd_game_team_stats_total <- left_join(cfbd_game_team_stats_total,week_games)
cfbd_game_team_stats_total$pa_diff <- cfbd_game_team_stats_total$est_pa - cfbd_game_team_stats_total$attempts
cfbd_game_team_stats_total$ra_diff <- cfbd_game_team_stats_total$est_ra - cfbd_game_team_stats_total$ra
cfbd_game_team_stats_total <- cfbd_game_team_stats_total[,c(2,4,18,20,5,19,21)]
cfbd_game_team_stats_total <- cfbd_game_team_stats_total[complete.cases(cfbd_game_team_stats_total),]

plot(cfbd_game_team_stats_total$attempts,cfbd_game_team_stats_total$est_pa)
plot(cfbd_game_team_stats_total$ra,cfbd_game_team_stats_total$est_ra)
