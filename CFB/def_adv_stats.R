library(cfbfastR)
library(RSelenium)
library(rvest)
library(tidyverse)
library(data.table)
library(stringi)
library(ggplot2)
library(dplyr)
library(zoo)


year = 2017
total_team_adv_stats <- data.frame()
while (year <= 2022) {
  for (i in 1:16) {
    tryCatch({
      cfbd_stats_season_advanced <- cfbd_stats_season_advanced(
      year,
      excl_garbage_time = FALSE,
      start_week = i,
      end_week = i
    )
      cfbd_stats_season_advanced$week <- i
    assign(paste0('total_stats_',i,'_',year),cfbd_stats_season_advanced)
    total_team_adv_stats <- rbind(cfbd_stats_season_advanced,total_team_adv_stats)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
  
  year <- year + 1
}

team_adv_stats <- total_team_adv_stats
team_adv_stats <- rename(team_adv_stats, year = season)
team_adv_stats <- rename(team_adv_stats, opp_team = team)
team_adv_stats <- team_adv_stats[,c(1,2,43:82)]

team_adv_stats <- team_adv_stats %>% 
  group_by(opp_team) %>%  arrange(week) %>% arrange(year) %>%
  mutate(L3_def_ppa = rollapply(def_ppa, width = list(-1:-4), align = 'right', fill = NA, FUN = mean, partial = TRUE))

team_adv_stats <- team_adv_stats %>% 
  group_by(opp_team) %>%  arrange(week) %>% arrange(year) %>%
  mutate(L3_def_success_rate = rollapply(def_success_rate, width = list(-1:-4), align = 'right', fill = NA, FUN = mean, partial = TRUE))



team_adv_stats <- team_adv_stats %>% 
  group_by(opp_team) %>%  arrange(week) %>% arrange(year) %>%
  mutate(L3_def_explosiveness = rollapply(def_explosiveness, width = list(-1:-4), align = 'right', fill = NA, FUN = mean, partial = TRUE))

team_adv_stats <- team_adv_stats %>% 
  group_by(opp_team) %>%  arrange(week) %>% arrange(year) %>%
  mutate(L3_def_power_success = rollapply(def_power_success, width = list(-1:-4), align = 'right', fill = NA, FUN = mean, partial = TRUE))



team_adv_stats <- team_adv_stats %>% 
  group_by(opp_team) %>%  arrange(week) %>% arrange(year) %>%
  mutate(L3_def_stuff_rate = rollapply(def_stuff_rate, width = list(-1:-4), align = 'right', fill = NA, FUN = mean, partial = TRUE))

team_adv_stats <- team_adv_stats %>% 
  group_by(opp_team) %>%  arrange(week) %>% arrange(year) %>%
  mutate(L3_def_line_yds = rollapply(def_line_yds, width = list(-1:-4), align = 'right', fill = NA, FUN = mean, partial = TRUE))



team_adv_stats <- team_adv_stats %>% 
  group_by(opp_team) %>%  arrange(week) %>% arrange(year) %>%
  mutate(L3_def_second_lvl_yds = rollapply(def_second_lvl_yds, width = list(-1:-4), align = 'right', fill = NA, FUN = mean, partial = TRUE))

team_adv_stats <- team_adv_stats %>% 
  group_by(opp_team) %>%  arrange(week) %>% arrange(year) %>%
  mutate(L3_def_open_field_yds = rollapply(def_open_field_yds, width = list(-1:-4), align = 'right', fill = NA, FUN = mean, partial = TRUE))



team_adv_stats <- team_adv_stats %>% 
  group_by(opp_team) %>%  arrange(week) %>% arrange(year) %>%
  mutate(L3_def_pts_per_opp = rollapply(def_pts_per_opp, width = list(-1:-4), align = 'right', fill = NA, FUN = mean, partial = TRUE))

team_adv_stats <- team_adv_stats %>% 
  group_by(opp_team) %>%  arrange(week) %>% arrange(year) %>%
  mutate(L3def_field_pos_avg_predicted_points = rollapply(def_field_pos_avg_predicted_points, width = list(-1:-4), align = 'right', fill = NA, FUN = mean, partial = TRUE))



team_adv_stats <- team_adv_stats %>% 
  group_by(opp_team) %>%  arrange(week) %>% arrange(year) %>%
  mutate(L3_def_standard_downs_rate = rollapply(def_standard_downs_rate, width = list(-1:-4), align = 'right', fill = NA, FUN = mean, partial = TRUE))

team_adv_stats <- team_adv_stats %>% 
  group_by(opp_team) %>%  arrange(week) %>% arrange(year) %>%
  mutate(L3_def_standard_downs_ppa = rollapply(def_standard_downs_ppa, width = list(-1:-4), align = 'right', fill = NA, FUN = mean, partial = TRUE))


team_adv_stats <- team_adv_stats %>% 
  group_by(opp_team) %>%  arrange(week) %>% arrange(year) %>%
  mutate(L3_def_standard_downs_success_rate = rollapply(def_standard_downs_success_rate, width = list(-1:-4), align = 'right', fill = NA, FUN = mean, partial = TRUE))

team_adv_stats <- team_adv_stats %>% 
  group_by(opp_team) %>%  arrange(week) %>% arrange(year) %>%
  mutate(L3_def_standard_downs_explosiveness = rollapply(def_standard_downs_explosiveness, width = list(-1:-4), align = 'right', fill = NA, FUN = mean, partial = TRUE))



team_adv_stats <- team_adv_stats %>% 
  group_by(opp_team) %>%  arrange(week) %>% arrange(year) %>%
  mutate(L3_def_passing_downs_rate = rollapply(def_passing_downs_rate, width = list(-1:-4), align = 'right', fill = NA, FUN = mean, partial = TRUE))

team_adv_stats <- team_adv_stats %>% 
  group_by(opp_team) %>%  arrange(week) %>% arrange(year) %>%
  mutate(L3_def_passing_downs_ppa = rollapply(def_passing_downs_ppa, width = list(-1:-4), align = 'right', fill = NA, FUN = mean, partial = TRUE))



team_adv_stats <- team_adv_stats %>% 
  group_by(opp_team) %>%  arrange(week) %>% arrange(year) %>%
  mutate(L3_def_passing_downs_success_rate = rollapply(def_passing_downs_success_rate, width = list(-1:-4), align = 'right', fill = NA, FUN = mean, partial = TRUE))

team_adv_stats <- team_adv_stats %>% 
  group_by(opp_team) %>%  arrange(week) %>% arrange(year) %>%
  mutate(L3_def_passing_downs_explosiveness = rollapply(def_passing_downs_explosiveness, width = list(-1:-4), align = 'right', fill = NA, FUN = mean, partial = TRUE))



team_adv_stats <- team_adv_stats %>% 
  group_by(opp_team) %>%  arrange(week) %>% arrange(year) %>%
  mutate(L3_def_rushing_plays_rate = rollapply(def_rushing_plays_rate, width = list(-1:-4), align = 'right', fill = NA, FUN = mean, partial = TRUE))

team_adv_stats <- team_adv_stats %>% 
  group_by(opp_team) %>%  arrange(week) %>% arrange(year) %>%
  mutate(L3_def_rushing_plays_ppa = rollapply(def_rushing_plays_ppa, width = list(-1:-4), align = 'right', fill = NA, FUN = mean, partial = TRUE))


team_adv_stats <- team_adv_stats %>% 
  group_by(opp_team) %>%  arrange(week) %>% arrange(year) %>%
  mutate(L3_def_rushing_plays_success_rate = rollapply(def_rushing_plays_success_rate, width = list(-1:-4), align = 'right', fill = NA, FUN = mean, partial = TRUE))

team_adv_stats <- team_adv_stats %>% 
  group_by(opp_team) %>%  arrange(week) %>% arrange(year) %>%
  mutate(L3_def_rushing_plays_explosiveness = rollapply(def_rushing_plays_explosiveness, width = list(-1:-4), align = 'right', fill = NA, FUN = mean, partial = TRUE))



team_adv_stats <- team_adv_stats %>% 
  group_by(opp_team) %>%  arrange(week) %>% arrange(year) %>%
  mutate(L3_def_passing_plays_rate = rollapply(def_passing_plays_rate, width = list(-1:-4), align = 'right', fill = NA, FUN = mean, partial = TRUE))

team_adv_stats <- team_adv_stats %>% 
  group_by(opp_team) %>%  arrange(week) %>% arrange(year) %>%
  mutate(L3_def_passing_plays_ppa = rollapply(def_passing_plays_ppa, width = list(-1:-4), align = 'right', fill = NA, FUN = mean, partial = TRUE))



team_adv_stats <- team_adv_stats %>% 
  group_by(opp_team) %>%  arrange(week) %>% arrange(year) %>%
  mutate(L3_def_passing_plays_success_rate = rollapply(def_passing_plays_success_rate, width = list(-1:-4), align = 'right', fill = NA, FUN = mean, partial = TRUE))

team_adv_stats <- team_adv_stats %>% 
  group_by(opp_team) %>%  arrange(week) %>% arrange(year) %>%
  mutate(L3_def_passing_plays_explosiveness = rollapply(def_passing_plays_explosiveness, width = list(-1:-4), align = 'right', fill = NA, FUN = mean, partial = TRUE))

team_adv_stats <- team_adv_stats[,-c(3:41)]

write.csv(team_adv_stats,'team_adv_stats.csv')
