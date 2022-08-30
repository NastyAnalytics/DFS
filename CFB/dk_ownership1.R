
setwd('~/Documents/CFB')

#dk_own
dk_ownership <- read.csv('dk_ownership.csv')
dk_ownership_b <- read.csv('dk_ownership_b.csv')
dk_ownership <- rbind(dk_ownership, dk_ownership_b)
dk_ownership <- dk_ownership[,-c(1)]
dk_ownership$week <- gsub('Week ','',as.character(dk_ownership$week))
dk_ownership <- dk_ownership %>% extract(week, c("week", "year"), "([^,]+), ([^)]+)")
dk_ownership$dk_own <- gsub('%','',as.character(dk_ownership$dk_own))
dk_ownership$salary <- gsub('[$]*','',as.character(dk_ownership$salary))
dk_ownership$salary <- gsub(',','',as.character(dk_ownership$salary))

dk_ownership$dk_own <- as.numeric(dk_ownership$dk_own)/100
dk_ownership$week <- as.numeric(dk_ownership$week)
dk_ownership$year <- as.numeric(dk_ownership$year)
dk_ownership$salary <- as.numeric(dk_ownership$salary)

dk_ownership <- dk_ownership[complete.cases(dk_ownership),]
dk_ownership <- dk_ownership %>% filter(slate_size != 1)
team_names <- read.csv('coach and pace names.csv')
team_names <- team_names[,c(3,4)]
colnames(team_names) <- c('team_name','team')
team_names <- data.table(team_names)
team_names[, team := stri_trans_general(str = team, 
                                     id = "Latin-ASCII")]
team_names[, team_name := stri_trans_general(str = team_name, 
                                        id = "Latin-ASCII")]
dk_ownership <- rename(dk_ownership,team = team_name)
dk_ownership <- left_join(dk_ownership,team_names)

#QB's
qb_ownership <- dk_ownership %>% filter(dk_pos == 'QB')
qb_stats <- read.csv('qb_stats.csv')
qb_stats <- qb_stats[,-c(1,3,4,6,23)]
qb_stats <- rename(qb_stats, player_name = player)
qb_rushing_stats <- read.csv('qb_rushing_stats.csv')
qb_rushing_stats <- qb_rushing_stats[,c(2,5,7,16:18,19,20,27,28,31,33:39)]
qb_rushing_stats <- rename(qb_rushing_stats, player_name = player)
qb_rushing_stats <-  rename(qb_rushing_stats, ra = attempts)
qb_rushing_stats <-  rename(qb_rushing_stats, ry = yards)
qb_rushing_stats <-  rename(qb_rushing_stats, rtd = touchdowns)
qb_rushing_stats <-  rename(qb_rushing_stats, rypa = ypa)
qb_rushing_stats <-  rename(qb_rushing_stats, ru_dkpts = dk_pts)
qb_rushing_stats <-  rename(qb_rushing_stats, ru_fdpts = fd_pts)

qb_rushing_stats <-  rename(qb_rushing_stats, L3_ra = L3_attempts)
qb_rushing_stats <-  rename(qb_rushing_stats, L3_ry = L3_yards)
qb_rushing_stats <-  rename(qb_rushing_stats, L3_rtd = L3_touchdowns)
qb_rushing_stats <-  rename(qb_rushing_stats, L3_rypa = L3_ypa)
qb_rushing_stats <-  rename(qb_rushing_stats, L3_ru_dkpts = L3_dk_pts)
qb_rushing_stats <-  rename(qb_rushing_stats, L3_ru_fdpts = L3_fd_pts)

qb_stats <-  rename(qb_stats, pa_dkpts = dk_pts)
qb_stats <-  rename(qb_stats, pa_fdpts = fd_pts)

qb_stats <-  rename(qb_stats, L3_pa_dkpts = L3_dk_pts)
qb_stats <-  rename(qb_stats, L3_pa_fdpts = L3_fd_pts)

qb_ownership <- left_join(qb_ownership,qb_stats)

qb_ownership <- left_join(qb_ownership,qb_rushing_stats)

qb_ownership$opportunities <- qb_ownership$ra + qb_ownership$attempts
qb_ownership$dk_pts <- qb_ownership$pa_dkpts + qb_ownership$ru_dkpts 
qb_ownership$fd_pts <- qb_ownership$pa_fdpts + qb_ownership$ru_fdpts 
qb_ownership$L3_dk_pts <- qb_ownership$L3_pa_dkpts + qb_ownership$L3_ru_dkpts 
qb_ownership$L3_fd_pts <- qb_ownership$L3_pa_fdpts + qb_ownership$L3_ru_fdpts 

qb_ownership <- qb_ownership[,-c(2)]
team_names <- read.csv('coach and pace names.csv')
team_names <-team_names[,c(1,3)]
colnames(team_names) <- c('team','team_name')
sos <- read.csv('strength_of_schedules.csv')
sos <- sos[,-c(1)]
sos <- rename(sos, year = season)
games_w_coaches <- read.csv('games_w_coaches1.csv')
games_w_coaches <- games_w_coaches[,c(3,10,17,18,41,42,177:188,259:328)]

games_w_coaches <- rename(games_w_coaches, year = season)

qb_ownership <- left_join(qb_ownership,team_names)

qb_ownership <- left_join(qb_ownership,sos)

qb_ownership <- qb_ownership[complete.cases(qb_ownership),]
qb_ownership <- left_join(qb_ownership,games_w_coaches)
qb_ownership <- left_join(qb_ownership,qb_ownership)
qb_ownership <- qb_ownership[,c("salary", "dk_own", "L3_attempts", "L3_completion_percent", "L3_completions", "L3_first_downs",
                                "L3_interceptions", "L3_qb_rating", "L3_touchdowns", "L3_yards", "L3_ypa", "L3_pa_dkpts", "L3_pa_fdpts", "L3_ra",
                                "L3_longest", "L3_rtd", "L3_ry", "L3_rypa", "L3_ru_dkpts", "L3_ru_fdpts", "dk_pts", "fd_pts", "L3_dk_pts",
                                "L3_fd_pts", "implied_total", "L3_def_rushing_plays_explosiveness", "L3_def_passing_plays_rate",
                                "L3_def_passing_plays_ppa", "L3_def_passing_plays_success_rate", "L3_def_passing_plays_explosiveness", "L3_avg_down",
                                "L3_avg_distance", "L3_avg_def_drive_efficiency", "L3_avg_drive_efficiency", "favorite", "opp_L3_attempts",
                                "opp_L3_opp_run_perc", "opp_L3_opp_pass_perc", "opp_L3_opp_pts", "opp_L3_pts", "opp_L3_ppg", "opp_L3_tpp",
                                "opp_L3_off_success_rate", "opp_L3_off_explosiveness", "opp_L3_off_stuff_rate", "opp_L3_off_open_field_yds",
                                "opp_L3_off_field_pos_avg_predicted_points", "opp_L3_off_standard_downs_rate",
                                "opp_L3_off_standard_downs_success_rate", "opp_L3_off_standard_downs_explosiveness", "opp_L3_off_passing_downs_rate",
                                "opp_L3_off_passing_downs_explosiveness", "opp_L3_off_rushing_plays_success_rate",
                                "opp_L3_off_rushing_plays_explosiveness", "opp_L3_off_passing_plays_success_rate",
                                "opp_L3_off_passing_plays_explosiveness", "opp_L3_def_ppa", "opp_L3_def_success_rate", "opp_L3_def_explosiveness",
                                "opp_L3_def_stuff_rate", "opp_L3_def_line_yds", "opp_L3_def_second_lvl_yds", "opp_L3_def_open_field_yds",
                                "opp_L3_def_pts_per_opp", "opp_L3def_field_pos_avg_predicted_points", "opp_L3_def_standard_downs_rate",
                                "opp_L3_def_standard_downs_ppa", "opp_L3_def_standard_downs_success_rate", "opp_L3_def_standard_downs_explosiveness",
                                "opp_L3_def_passing_downs_rate", "opp_L3_def_passing_downs_ppa", "opp_L3_def_passing_downs_success_rate",
                                "opp_L3_def_passing_downs_explosiveness", "opp_L3_def_rushing_plays_rate", "opp_L3_def_rushing_plays_ppa",
                                "opp_L3_def_rushing_plays_success_rate", "opp_L3_def_rushing_plays_explosiveness", "opp_L3_def_passing_plays_rate",
                                "opp_L3_def_passing_plays_ppa", "opp_L3_def_passing_plays_success_rate", "opp_L3_def_passing_plays_explosiveness",
                                "opp_L3_avg_def_drive_efficiency", "opp_L3_avg_def_down", "opp_L3_avg_distance", "opp_L3_avg_def_distance")]
qb_ownership <- qb_ownership[complete.cases(qb_ownership),]
qb_ownership <- qb_ownership[!duplicated(qb_ownership),]

corr <- cor(qb_ownership)
corr <- as.data.frame(corr)
corr <- corr %>% filter(!between(dk_own,-.05,.05))
corr <- as.data.frame(t(corr))
corr <- left_join(corr,corr)
view(corr)


write.csv(qb_ownership,'qb_ownership.csv')

view(corr)

#RB's 
rb_ownership <- dk_ownership %>% filter(dk_pos == 'RB')
cfbd_stats_season_player_total <- read.csv('rb_stats.csv')
cfbd_stats_season_player_total <- cfbd_stats_season_player_total[,-c(1)]
cfbd_stats_season_player_total <- cfbd_stats_season_player_total[,c(1,4,6:10,14:28,30:31)]
cfbd_stats_season_player_total <- rename(cfbd_stats_season_player_total, player_name = player)
rb_rec_stats <- read.csv('wr_stats.csv')
rb_rec_stats <- rb_rec_stats[,-c(1,3,4,6,8:11)]
rb_rec_stats <- rename(rb_rec_stats, player_name = player)
rb_rec_stats <-  rename(rb_rec_stats, re_touchdowns = touchdowns)
rb_rec_stats <-  rename(rb_rec_stats, re_yds = yards)
rb_rec_stats <-  rename(rb_rec_stats, re_string = string)
rb_rec_stats <-  rename(rb_rec_stats, re_dkpts = dk_pts)
rb_rec_stats <-  rename(rb_rec_stats, re_fdpts = fd_pts)
cfbd_stats_season_player_total <-  rename(cfbd_stats_season_player_total, ru_dkpts = dk_pts)
cfbd_stats_season_player_total <-  rename(cfbd_stats_season_player_total, ru_fdpts = fd_pts)
rb_ownership <- left_join(rb_ownership,cfbd_stats_season_player_total)
rb_ownership <- left_join(rb_ownership,rb_rec_stats)
rb_ownership$opportunities <- rb_ownership$receptions + rb_ownership$attempts
rb_ownership$dk_pts <- rb_ownership$ru_dkpts + rb_ownership$re_dkpts 
rb_ownership$fd_pts <- rb_ownership$ru_fdpts + rb_ownership$re_fdpts 

rb_ownership <- rb_ownership[,-c(2)]
team_names <- read.csv('coach and pace names.csv')
team_names <-team_names[,c(1,3)]
colnames(team_names) <- c('team','team_name')

sos <- read.csv('strength_of_schedules.csv')
sos <- sos[,-c(1)]
sos <- rename(sos, year = season)


games_w_coaches <- read.csv('games_w_coaches1.csv')
games_w_coaches <- games_w_coaches[,c(3,10,17,18,41,42,177:188,259:328)]
games_w_coaches <- rename(games_w_coaches, year = season)

rb_ownership <- left_join(rb_ownership,team_names)
rb_ownership <- left_join(rb_ownership,sos)

rb_ownership <- rb_ownership[complete.cases(rb_ownership),]
rb_ownership <- rb_ownership[!duplicated(rb_ownership),]

rb_ownership <- left_join(rb_ownership,games_w_coaches)
rb_ownership <- left_join(rb_ownership,rb_ownership)

rb_ownership <- rb_ownership[, c("salary", "dk_own", "string", "L3_first_downs", "L3_fumbles", "L3_rec_usage", "L3_targets", "dk_pts",
                                 "L4_opp_sos", "L3_def_rushing_plays_explosiveness", "L3_def_passing_plays_rate", "L3_def_passing_plays_success_rate",
                                 "L3_avg_down", "L3_avg_distance", "L3_avg_def_drive_efficiency", "L3_avg_drive_efficiency", "L3_avg_def_distance",
                                 "favorite", "opp_L3_opp_run_perc", "opp_L3_opp_pass_perc", "opp_L3_opp_pts", "opp_L3_pts", "opp_L3_opp_tpp",
                                 "opp_L3_opp_pos", "opp_L3_tpp", "opp_L3_off_ppa", "opp_L3_off_success_rate", "opp_L3_off_power_success",
                                 "opp_L3_off_stuff_rate", "opp_L3_off_line_yds", "opp_L3_off_second_lvl_yds", "opp_L3_off_standard_downs_rate",
                                 "opp_L3_off_standard_downs_ppa", "opp_L3_off_standard_downs_success_rate", "opp_L3_off_passing_downs_rate",
                                 "opp_L3_off_passing_downs_ppa", "opp_L3_off_passing_downs_success_rate", "opp_L3_off_rushing_plays_ppa",
                                 "opp_L3_off_rushing_plays_success_rate", "opp_L3_off_passing_plays_success_rate", "opp_L3_def_ppa",
                                 "opp_L3_def_success_rate", "opp_L3_def_power_success", "opp_L3_def_stuff_rate", "opp_L3_def_line_yds",
                                 "opp_L3_def_second_lvl_yds", "opp_L3_def_open_field_yds", "opp_L3_def_pts_per_opp",
                                 "opp_L3def_field_pos_avg_predicted_points", "opp_L3_def_standard_downs_rate", "opp_L3_def_standard_downs_ppa",
                                 "opp_L3_def_standard_downs_success_rate", "opp_L3_def_standard_downs_explosiveness", "opp_L3_def_passing_downs_rate",
                                 "opp_L3_def_passing_downs_ppa", "opp_L3_def_passing_downs_success_rate", "opp_L3_def_rushing_plays_rate",
                                 "opp_L3_def_rushing_plays_ppa", "opp_L3_def_rushing_plays_success_rate", "opp_L3_def_rushing_plays_explosiveness",
                                 "opp_L3_def_passing_plays_rate", "opp_L3_def_passing_plays_ppa", "opp_L3_def_passing_plays_success_rate",
                                 "opp_L3_avg_drive_efficiency", "opp_L3_avg_def_drive_efficiency", "opp_L3_avg_down", "opp_L3_avg_def_down",
                                 "opp_L3_avg_def_distance","implied_total","opp_implied_total")]
rb_ownership <- rb_ownership[complete.cases(rb_ownership),]
rb_ownership <- rb_ownership[!duplicated(rb_ownership),]

corr <- cor(rb_ownership)
corr <- as.data.frame(corr)
corr <- corr %>% filter(!between(dk_own,-.05,.05))
corr <- as.data.frame(t(corr))
corr <- left_join(corr,corr)
view(corr)



write.csv(rb_ownership,'rb_ownership.csv')


#WR's
wr_ownership <- dk_ownership %>% filter(dk_pos == 'WR')
wr_stats <- read.csv('wr_stats.csv')
wr_stats <- wr_stats[,-c(1,3,4,6)]
wr_stats <- rename(wr_stats, player_name = player)
wr_ownership <- left_join(wr_ownership,wr_stats)
wr_ownership <- wr_ownership[complete.cases(wr_ownership),]

wr_ownership <- wr_ownership[,-c(2)]
team_names <- read.csv('coach and pace names.csv')
team_names <-team_names[,c(1,3)]
colnames(team_names) <- c('team','team_name')

sos <- read.csv('strength_of_schedules.csv')
sos <- sos[,-c(1)]
sos <- rename(sos, year = season)


games_w_coaches <- read.csv('games_w_coaches1.csv')
games_w_coaches <- games_w_coaches[,c(3,10,17,18,41,42,177:188,259:328)]
games_w_coaches <- rename(games_w_coaches, year = season)

wr_ownership <- left_join(wr_ownership,team_names)
wr_ownership <- left_join(wr_ownership,sos)

wr_ownership <- left_join(wr_ownership,games_w_coaches)
wr_ownership <- left_join(wr_ownership,wr_ownership)

wr_ownership <- wr_ownership[,c("salary", "dk_own", "slate_size", "dk_pts", "string", "L3_rec_usage", "L3_targets", "implied_total",
                                "L3_def_rushing_plays_explosiveness", "L3_def_passing_plays_ppa", "L3_def_passing_plays_success_rate", "L3_avg_down",
                                "L3_avg_def_drive_efficiency", "L3_avg_drive_efficiency", "L3_avg_def_distance", "favorite", "opp_L3_ra",
                                "opp_L3_opp_run_perc", "opp_L3_opp_pass_perc", "opp_L3_opp_pts", "opp_L3_ppg", "opp_L3_tpp",
                                "opp_L3_off_explosiveness", "opp_L3_off_field_pos_avg_predicted_points", "opp_L3_off_standard_downs_rate",
                                "opp_L3_off_passing_downs_rate", "opp_L3_off_rushing_plays_explosiveness", "opp_L3_def_ppa",
                                "opp_L3_def_success_rate", "opp_L3_def_stuff_rate", "opp_L3_def_line_yds", "opp_L3_def_second_lvl_yds",
                                "opp_L3_def_open_field_yds", "opp_L3_def_pts_per_opp", "opp_L3def_field_pos_avg_predicted_points",
                                "opp_L3_def_standard_downs_rate", "opp_L3_def_standard_downs_ppa", "opp_L3_def_standard_downs_success_rate",
                                "opp_L3_def_passing_downs_rate", "opp_L3_def_passing_downs_ppa", "opp_L3_def_passing_downs_success_rate",
                                "opp_L3_def_rushing_plays_rate", "opp_L3_def_rushing_plays_ppa", "opp_L3_def_rushing_plays_success_rate",
                                "opp_L3_def_rushing_plays_explosiveness", "opp_L3_def_passing_plays_rate", "opp_L3_def_passing_plays_ppa",
                                "opp_L3_def_passing_plays_success_rate", "opp_L3_avg_def_drive_efficiency", "opp_L3_avg_def_down",
                                "opp_L3_avg_distance", "opp_L3_avg_def_distance")]
wr_ownership <- wr_ownership[complete.cases(wr_ownership),]
wr_ownership <- wr_ownership[!duplicated(wr_ownership),]

corr <- cor(wr_ownership)
corr <- as.data.frame(corr)
corr <- corr %>% filter(!between(dk_own,-.05,.05))
corr <- as.data.frame(t(corr))
corr <- left_join(corr,corr)
view(corr)


write.csv(wr_ownership,'wr_ownership.csv')

