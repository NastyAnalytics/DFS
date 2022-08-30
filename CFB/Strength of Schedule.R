fbs_teams <- cfbd_team_info()

adv_game_stats_2020 <- cfbd_stats_game_advanced(2020)
adv_game_stats_2020 <- adv_game_stats_2020[c("game_id","team","opponent")]

team_stats_2020 <- cfbd_stats_season_team(2020)

adv_team_stats_2020 <- cfbd_stats_season_advanced(2020, excl_garbage_time = TRUE)

team_records_2020 <- cfbd_game_records(2020)
team_records_2020 <- team_records_2020[c(2,7,8)]
team_records_2020$win_percentage <- team_records_2020$total_wins / (team_records_2020$total_wins + team_records_2020$total_losses) 

game_info_2020 <- cfbd_game_info(2020)
game_info_2020 <- game_info_2020[c("game_id","season","week", "home_team", "home_points", "away_team", "away_points")]
game_info_2020 <- merge(game_info_2020, adv_game_stats_2020, by = "game_id")
game_info_2020 <- game_info_2020[c(1:3,8,9,4:7)]
game_info_2020$points <- ifelse(game_info_2020$team == game_info_2020$home_team, game_info_2020$home_points,game_info_2020$away_points)
game_info_2020$opp_points <- ifelse(game_info_2020$team == game_info_2020$home_team, game_info_2020$away_points,game_info_2020$home_points)
game_info_2020$Home_Away <- ifelse(game_info_2020$team == game_info_2020$home_team, "H","A")

game_info_2020$Win <- ifelse(game_info_2020$points >= game_info_2020$opp_points, 1, 0)
game_info_2020$Loss <- ifelse(game_info_2020$opp_points >= game_info_2020$points, 1, 0)
game_info_2020 <- game_info_2020[-c(6:9)]

game_info_2020 <- merge(game_info_2020, team_records_2020, by = "team")
team_records_2020 <- rename(team_records_2020, opponent = team)
team_records_2020 <- rename(team_records_2020, opp_win_percentage = win_percentage)
team_records_2020 <- team_records_2020[c(1,4)]
game_info_2020 <- merge(game_info_2020, team_records_2020, by = "opponent")
game_info_2020 <- game_info_2020[c(3,2,1,4:14)]

avg_opp_wp <- aggregate(opp_win_percentage ~ team, data = game_info_2020, mean)
avg_opp_wp <- rename(avg_opp_wp, avg_opp_wp = opp_win_percentage)
game_info_2020 <- merge(game_info_2020, avg_opp_wp, by = "team")

opp_opp_win_percentage <- aggregate(opp_win_percentage ~ team, data = game_info_2020, mean)
opp_opp_win_percentage <- rename(opp_opp_win_percentage, opponent = team)
opp_opp_win_percentage <- rename(opp_opp_win_percentage, opp_opp_wp = opp_win_percentage)

game_info_2020 <- merge(game_info_2020, opp_opp_win_percentage, by = "opponent")
avg_opp_opp_wp <- aggregate(opp_opp_wp ~ team, data = game_info_2020, mean)
avg_opp_opp_wp <- rename(avg_opp_opp_wp, avg_opp_opp_wp = opp_opp_wp)
game_info_2020 <- merge(game_info_2020, avg_opp_opp_wp, by = "team")

game_info_2020$SOS <- ((2*game_info_2020$avg_opp_wp)+game_info_2020$avg_opp_opp_wp) / 3

SOS_2020 <- aggregate(SOS ~ team, data = game_info_2020, mean)
