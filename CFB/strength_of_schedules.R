
library(RSelenium)
library(tidyverse)
library(cfbfastR)
library(data.table)
library(zoo)


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



cfbd_game_info_total <- cfbd_game_info_total[,c(2,3,13,16,21,24)]
cfbd_game_info_total1 <- cfbd_game_info_total[,c(1,2,5,6,3,4)]
colnames(cfbd_game_info_total) <- c('season','week','team','score','opp_team','opp_score')
colnames(cfbd_game_info_total1) <- c('season','week','team','score','opp_team','opp_score')



cfbd_game_info_total <- rbind(cfbd_game_info_total1,cfbd_game_info_total)

cfbd_game_info_total <- cfbd_game_info_total %>% distinct(season,week,team,.keep_all = TRUE)
cfbd_game_info_total <- cfbd_game_info_total %>% distinct(season,week,opp_team,.keep_all = TRUE)


cfbd_game_info_total$win <- ifelse(cfbd_game_info_total$score > cfbd_game_info_total$opp_score,1,0)
cfbd_game_info_total$loss <- ifelse(cfbd_game_info_total$score > cfbd_game_info_total$opp_score,0,1)
cfbd_game_info_total <- cfbd_game_info_total[!duplicated(cfbd_game_info_total),]

cfbd_game_info_total <- cfbd_game_info_total %>% 
  group_by(team) %>% 
  arrange(week) %>% 
  arrange(season) %>% 
  mutate(L4_win_perc = rollapplyr(win, width = list(-1:-4), align = 'right', fill = 0, FUN = mean, partial = TRUE))
cfbd_game_info_total[is.na(cfbd_game_info_total)] <- 0

opp_win_perc <- cfbd_game_info_total[,c(1,2,3,9)]
colnames(opp_win_perc) <- c('season','week','opp_team','L4_opp_win_perc')
cfbd_game_info_total <- left_join(cfbd_game_info_total,opp_win_perc)
cfbd_game_info_total[is.na(cfbd_game_info_total)] <- 0


cfbd_game_info_total <- cfbd_game_info_total %>% 
  group_by(team) %>% 
  arrange(week) %>% 
  arrange(season) %>% 
  mutate(L4_opp_win_perc1 = rollapplyr(L4_opp_win_perc, width = list(-1:-4), align = 'right', fill = 0, FUN = mean, partial = TRUE))
cfbd_game_info_total[is.na(cfbd_game_info_total)] <- 0


opp_opp_win_perc <- cfbd_game_info_total[,c(1,2,3,11)]
colnames(opp_opp_win_perc) <- c('season','week','opp_team','L4_opp_opp_win_perc')
cfbd_game_info_total <- left_join(cfbd_game_info_total,opp_opp_win_perc)
cfbd_game_info_total[is.na(cfbd_game_info_total)] <- 0


cfbd_game_info_total <- cfbd_game_info_total %>% 
  group_by(team) %>% 
  arrange(week) %>% 
  arrange(season) %>% 
  mutate(L4_opp_opp_win_perc1 = rollapplyr(L4_opp_opp_win_perc, width = list(-1:-4), align = 'right', fill = 0, FUN = mean, partial = TRUE))
cfbd_game_info_total$L4_sos <- ((2*cfbd_game_info_total$L4_opp_win_perc1) + (cfbd_game_info_total$L4_opp_opp_win_perc1))/3

opp_sos <- cfbd_game_info_total[,c(1,2,3,14)]
colnames(opp_sos) <- c('season','week','opp_team','L4_opp_sos')
cfbd_game_info_total <- left_join(cfbd_game_info_total,opp_sos)
cfbd_game_info_total[is.na(cfbd_game_info_total)] <- 0

sos <- cfbd_game_info_total[,c(1,2,3,5,14,15)]
write.csv(sos,'strength_of_schedules.csv')
