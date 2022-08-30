library(zoo)
library(cfbfastR)
library(tidyverse)
library(caret)
library(MLmetrics)
library(ggplot2)
library(stringi)
library(data.table)
setwd('~/Documents/CFB')


library(tidyverse)
library(cfbfastR)


coaches <- cfbd_coaches(
  min_year = 2000,
  max_year = 2022
)

coaches$coach <- paste0(coaches$first_name," ",coaches$last_name)

coaches <- coaches[,c(16,4,5)]
colnames(coaches) <- c('team_name','team','year')

team_names <- data.table(team_names)
coaches <- data.table(coaches)
coaches[, team := stri_trans_general(str = team, 
                                   id = "Latin-ASCII")]


setwd("~/Documents/CFB/rushing_summaries")

rushing_summary_2017_week7<- read.csv('rushing_summarya.csv')
rushing_summary_2017_week7$week <- 7
rushing_summary_2017_week7$year <- 2017

rushing_summary_2017_week8<- read.csv('rushing_summaryb.csv')
rushing_summary_2017_week8$week <- 8
rushing_summary_2017_week8$year <- 2017

rushing_summary_2017_week9<- read.csv('rushing_summaryc.csv')
rushing_summary_2017_week9$week <- 9
rushing_summary_2017_week9$year <- 2017

rushing_summary_2017_week10<- read.csv('rushing_summaryd.csv')
rushing_summary_2017_week10$week <- 10
rushing_summary_2017_week10$year <- 2017

rushing_summary_2017_week11<- read.csv('rushing_summarye.csv')
rushing_summary_2017_week11$week <- 11
rushing_summary_2017_week11$year <- 2017

rushing_summary_2017_week12<- read.csv('rushing_summaryf.csv')
rushing_summary_2017_week12$week <- 12
rushing_summary_2017_week12$year <- 2017

rushing_summary_2017_week13<- read.csv('rushing_summaryg.csv')
rushing_summary_2017_week13$week <- 13
rushing_summary_2017_week13$year <- 2017

rushing_summary_2017_week14<- read.csv('rushing_summaryh.csv')
rushing_summary_2017_week14$week <- 14
rushing_summary_2017_week14$year <- 2017

rushing_summary_2017 <- rbind(rushing_summary_2017_week7,
                              rushing_summary_2017_week8,
                              rushing_summary_2017_week9,
                              rushing_summary_2017_week10,
                              rushing_summary_2017_week11,
                              rushing_summary_2017_week12,
                              rushing_summary_2017_week13,
                              rushing_summary_2017_week14)

rushing_summary_2018_week1 <- read.csv('rushing_summary.csv')
rushing_summary_2018_week1$week <- 1
rushing_summary_2018_week1$year <- 2018

rushing_summary_2018_week2 <- read.csv('rushing_summary(1).csv')
rushing_summary_2018_week2$week <- 2
rushing_summary_2018_week2$year <- 2018


rushing_summary_2018_week3<- read.csv('rushing_summary(2).csv')
rushing_summary_2018_week3$week <- 3
rushing_summary_2018_week3$year <- 2018

rushing_summary_2018_week4<- read.csv('rushing_summary(3).csv')
rushing_summary_2018_week4$week <- 4
rushing_summary_2018_week4$year <- 2018

rushing_summary_2018_week5<- read.csv('rushing_summary(4).csv')
rushing_summary_2018_week5$week <- 5
rushing_summary_2018_week5$year <- 2018

rushing_summary_2018_week6<- read.csv('rushing_summary(5).csv')
rushing_summary_2018_week6$week <- 6
rushing_summary_2018_week6$year <- 2018

rushing_summary_2018_week7<- read.csv('rushing_summary(6).csv')
rushing_summary_2018_week7$week <- 7
rushing_summary_2018_week7$year <- 2018

rushing_summary_2018_week8<- read.csv('rushing_summary(7).csv')
rushing_summary_2018_week8$week <- 8
rushing_summary_2018_week8$year <- 2018

rushing_summary_2018_week9<- read.csv('rushing_summary(8).csv')
rushing_summary_2018_week9$week <- 9
rushing_summary_2018_week9$year <- 2018

rushing_summary_2018_week10<- read.csv('rushing_summary(9).csv')
rushing_summary_2018_week10$week <- 10
rushing_summary_2018_week10$year <- 2018

rushing_summary_2018_week11<- read.csv('rushing_summary(10).csv')
rushing_summary_2018_week11$week <- 11
rushing_summary_2018_week11$year <- 2018

rushing_summary_2018_week12<- read.csv('rushing_summary(11).csv')
rushing_summary_2018_week12$week <- 12
rushing_summary_2018_week12$year <- 2018

rushing_summary_2018_week13<- read.csv('rushing_summary(12).csv')
rushing_summary_2018_week13$week <- 13
rushing_summary_2018_week13$year <- 2018

rushing_summary_2018_week14a<- read.csv('rushing_summary(13).csv')
rushing_summary_2018_week14b<- read.csv('rushing_summary(15).csv')
rushing_summary_2018_week14<- rbind(rushing_summary_2018_week14a,rushing_summary_2018_week14b)
rushing_summary_2018_week14$week <- 14
rushing_summary_2018_week14$year <- 2018


rushing_summary_2018_week15<- read.csv('rushing_summarybg.csv')
rushing_summary_2018_week15$week <- 15
rushing_summary_2018_week15$year <- 2018

rushing_summary_2018 <- rbind(rushing_summary_2018_week1,
                                rushing_summary_2018_week2,
                                rushing_summary_2018_week3,
                                rushing_summary_2018_week4,
                                rushing_summary_2018_week5,
                                rushing_summary_2018_week6,
                                rushing_summary_2018_week7,
                                rushing_summary_2018_week8,
                                rushing_summary_2018_week9,
                                rushing_summary_2018_week10,
                                rushing_summary_2018_week11,
                                rushing_summary_2018_week12,
                                rushing_summary_2018_week13,
                                rushing_summary_2018_week14,
                                rushing_summary_2018_week15)

rushing_summary_2019_week1 <- read.csv('rushing_summary(16).csv')
rushing_summary_2019_week1$week <- 1
rushing_summary_2019_week1$year <- 2019

rushing_summary_2019_week2 <- read.csv('rushing_summary(17).csv')
rushing_summary_2019_week2$week <- 2
rushing_summary_2019_week2$year <- 2019


rushing_summary_2019_week3<- read.csv('rushing_summary(18).csv')
rushing_summary_2019_week3$week <- 3
rushing_summary_2019_week3$year <- 2019

rushing_summary_2019_week4<- read.csv('rushing_summary(19).csv')
rushing_summary_2019_week4$week <- 4
rushing_summary_2019_week4$year <- 2019

rushing_summary_2019_week5<- read.csv('rushing_summary2.csv')
rushing_summary_2019_week5$week <- 5
rushing_summary_2019_week5$year <- 2019

rushing_summary_2019_week6<- read.csv('rushing_summary(20).csv')
rushing_summary_2019_week6$week <- 6
rushing_summary_2019_week6$year <- 2019

rushing_summary_2019_week7<- read.csv('rushing_summary(21).csv')
rushing_summary_2019_week7$week <- 7
rushing_summary_2019_week7$year <- 2019

rushing_summary_2019_week8<- read.csv('rushing_summary(22).csv')
rushing_summary_2019_week8$week <- 8
rushing_summary_2019_week8$year <- 2019

rushing_summary_2019_week9<- read.csv('rushing_summary(23).csv')
rushing_summary_2019_week9$week <- 9
rushing_summary_2019_week9$year <- 2019

rushing_summary_2019_week10<- read.csv('rushing_summary(24).csv')
rushing_summary_2019_week10$week <- 10
rushing_summary_2019_week10$year <- 2019

rushing_summary_2019_week11<- read.csv('rushing_summary(25).csv')
rushing_summary_2019_week11$week <- 11
rushing_summary_2019_week11$year <- 2019

rushing_summary_2019_week12<- read.csv('rushing_summary(26).csv')
rushing_summary_2019_week12$week <- 12
rushing_summary_2019_week12$year <- 2019

rushing_summary_2019_week13<- read.csv('rushing_summary(27).csv')
rushing_summary_2019_week13$week <- 13
rushing_summary_2019_week13$year <- 2019

rushing_summary_2019_week14<- read.csv('rushing_summary(28).csv')
rushing_summary_2019_week14$week <- 14
rushing_summary_2019_week14$year <- 2019

rushing_summary_2019_week15<- read.csv('rushing_summary(31).csv')
rushing_summary_2019_week15$week <- 15
rushing_summary_2019_week15$year <- 2019

rushing_summary_2019_week16<- read.csv('rushing_summarybg1.csv')
rushing_summary_2019_week16$week <- 16
rushing_summary_2019_week16$year <- 2019


rushing_summary_2019 <- rbind(rushing_summary_2019_week1,
                                rushing_summary_2019_week2,
                                rushing_summary_2019_week3,
                                rushing_summary_2019_week4,
                                rushing_summary_2019_week5,
                                rushing_summary_2019_week6,
                                rushing_summary_2019_week7,
                                rushing_summary_2019_week8,
                                rushing_summary_2019_week9,
                                rushing_summary_2019_week10,
                                rushing_summary_2019_week11,
                                rushing_summary_2019_week12,
                                rushing_summary_2019_week13,
                                rushing_summary_2019_week14,
                                rushing_summary_2019_week15,
                                rushing_summary_2019_week16)


rushing_summary_2020_week1 <- read.csv('rushing_summary(32).csv')
rushing_summary_2020_week1$week <- 1
rushing_summary_2020_week1$year <- 2020

rushing_summary_2020_week2 <- read.csv('rushing_summary(33).csv')
rushing_summary_2020_week2$week <- 2
rushing_summary_2020_week2$year <- 2020

rushing_summary_2020_week3<- read.csv('rushing_summary(34).csv')
rushing_summary_2020_week3$week <- 3
rushing_summary_2020_week3$year <- 2020

rushing_summary_2020_week4<- read.csv('rushing_summary(35).csv')
rushing_summary_2020_week4$week <- 4
rushing_summary_2020_week4$year <- 2020

rushing_summary_2020_week5<- read.csv('rushing_summary(36).csv')
rushing_summary_2020_week5$week <- 5
rushing_summary_2020_week5$year <- 2020

rushing_summary_2020_week6<- read.csv('rushing_summary(37).csv')
rushing_summary_2020_week6$week <- 6
rushing_summary_2020_week6$year <- 2020

rushing_summary_2020_week7<- read.csv('rushing_summary(38).csv')
rushing_summary_2020_week7$week <- 7
rushing_summary_2020_week7$year <- 2020

rushing_summary_2020_week8<- read.csv('rushing_summary(39).csv')
rushing_summary_2020_week8$week <- 8
rushing_summary_2020_week8$year <- 2020

rushing_summary_2020_week9<- read.csv('rushing_summary(40).csv')
rushing_summary_2020_week9$week <- 9
rushing_summary_2020_week9$year <- 2020

rushing_summary_2020_week10<- read.csv('rushing_summary(41).csv')
rushing_summary_2020_week10$week <- 10
rushing_summary_2020_week10$year <- 2020

rushing_summary_2020_week11<- read.csv('rushing_summary(42).csv')
rushing_summary_2020_week11$week <- 11
rushing_summary_2020_week11$year <- 2020

rushing_summary_2020_week12<- read.csv('rushing_summary(43).csv')
rushing_summary_2020_week12$week <- 12
rushing_summary_2020_week12$year <- 2020

rushing_summary_2020_week13<- read.csv('rushing_summary(44).csv')
rushing_summary_2020_week13$week <- 13
rushing_summary_2020_week13$year <- 2020

rushing_summary_2020_week14<- read.csv('rushing_summary(45).csv')
rushing_summary_2020_week14$week <- 14
rushing_summary_2020_week14$year <- 2020

rushing_summary_2020_week15<- read.csv('rushing_summary(46).csv')
rushing_summary_2020_week15$week <- 15
rushing_summary_2020_week15$year <- 2020

rushing_summary_2020_week16<- read.csv('rushing_summary(47).csv')
rushing_summary_2020_week16$week <- 16
rushing_summary_2020_week16$year <- 2020

rushing_summary_2020_week17<- read.csv('rushing_summarybg2.csv')
rushing_summary_2020_week17$week <- 17
rushing_summary_2020_week17$year <- 2020


rushing_summary_2020 <- rbind(rushing_summary_2020_week1,
                                rushing_summary_2020_week2,
                                rushing_summary_2020_week3,
                                rushing_summary_2020_week4,
                                rushing_summary_2020_week5,
                                rushing_summary_2020_week6,
                                rushing_summary_2020_week7,
                                rushing_summary_2020_week8,
                                rushing_summary_2020_week9,
                                rushing_summary_2020_week10,
                                rushing_summary_2020_week11,
                                rushing_summary_2020_week12,
                                rushing_summary_2020_week13,
                                rushing_summary_2020_week14,
                                rushing_summary_2020_week15,
                                rushing_summary_2020_week16,
                                rushing_summary_2020_week17)


rushing_summary_2021_week1 <- read.csv('rushing_summary(49).csv')
rushing_summary_2021_week1$week <- 1
rushing_summary_2021_week1$year <- 2021

rushing_summary_2021_week2 <- read.csv('rushing_summary(50).csv')
rushing_summary_2021_week2$week <- 2
rushing_summary_2021_week2$year <- 2021

rushing_summary_2021_week3<- read.csv('rushing_summary(51).csv')
rushing_summary_2021_week3$week <- 3
rushing_summary_2021_week3$year <- 2021

rushing_summary_2021_week4<- read.csv('rushing_summary(52).csv')
rushing_summary_2021_week4$week <- 4
rushing_summary_2021_week4$year <- 2021

rushing_summary_2021_week5<- read.csv('rushing_summary(53).csv')
rushing_summary_2021_week5$week <- 5
rushing_summary_2021_week5$year <- 2021

rushing_summary_2021_week6<- read.csv('rushing_summary(54).csv')
rushing_summary_2021_week6$week <- 6
rushing_summary_2021_week6$year <- 2021

rushing_summary_2021_week7<- read.csv('rushing_summary(55).csv')
rushing_summary_2021_week7$week <- 7
rushing_summary_2021_week7$year <- 2021

rushing_summary_2021_week8<- read.csv('rushing_summary(56).csv')
rushing_summary_2021_week8$week <- 8
rushing_summary_2021_week8$year <- 2021

rushing_summary_2021_week9<- read.csv('rushing_summary(57).csv')
rushing_summary_2021_week9$week <- 9
rushing_summary_2021_week9$year <- 2021

rushing_summary_2021_week10<- read.csv('rushing_summary3.csv')
rushing_summary_2021_week10$week <- 10
rushing_summary_2021_week10$year <- 2021

rushing_summary_2021_week11<- read.csv('rushing_summary(58).csv')
rushing_summary_2021_week11$week <- 11
rushing_summary_2021_week11$year <- 2021

rushing_summary_2021_week12<- read.csv('rushing_summary(59).csv')
rushing_summary_2021_week12$week <- 12
rushing_summary_2021_week12$year <- 2021

rushing_summary_2021_week13<- read.csv('rushing_summary(60).csv')
rushing_summary_2021_week13$week <- 13
rushing_summary_2021_week13$year <- 2021

rushing_summary_2021_week14<- read.csv('rushing_summary(64).csv')
rushing_summary_2021_week14$week <- 14
rushing_summary_2021_week14$year <- 2021

rushing_summary_2021_week15<- read.csv('rushing_summarybg3.csv')
rushing_summary_2021_week15$week <- 15
rushing_summary_2021_week15$year <- 2021

rushing_summary_2021 <- rbind(rushing_summary_2021_week1,
                                rushing_summary_2021_week2,
                                rushing_summary_2021_week3,
                                rushing_summary_2021_week4,
                                rushing_summary_2021_week5,
                                rushing_summary_2021_week6,
                                rushing_summary_2021_week7,
                                rushing_summary_2021_week8,
                                rushing_summary_2021_week9,
                                rushing_summary_2021_week10,
                                rushing_summary_2021_week11,
                                rushing_summary_2021_week12,
                                rushing_summary_2021_week13,
                                rushing_summary_2021_week14,
                                rushing_summary_2021_week15)

rushing_summary_total <- rbind(rushing_summary_2017,
                               rushing_summary_2018,
                                 rushing_summary_2019,
                                 rushing_summary_2020,
                                 rushing_summary_2021)


team_rushes <- aggregate(attempts ~ team_name + week + year,data = rushing_summary_total,FUN = sum)
colnames(team_rushes) <- c('team_name','week', 'year','team_rushes')
rushing_summary_total <- left_join(rushing_summary_total,team_rushes)
rushing_summary_total$runshare <- rushing_summary_total$attempts/rushing_summary_total$team_rushes

team_rushes <- aggregate(attempts ~ team_name + year,data = rushing_summary_total,FUN = sum)
player_rushes <- aggregate(attempts ~ player + year,data = rushing_summary_total,FUN = sum)
colnames(player_rushes) <- c('player','year','season_rushes')
colnames(team_rushes) <- c('team_name','year','season_team_rushes')
rushing_summary_total <- left_join(rushing_summary_total,team_rushes)
rushing_summary_total <- left_join(rushing_summary_total,player_rushes)
rushing_summary_total$total_runshare <- rushing_summary_total$season_rushes/rushing_summary_total$season_team_rushes
avg_runshare <- aggregate(runshare ~ player + year, data = rushing_summary_total, FUN = mean)
colnames(avg_runshare) <- c('player','year','avg_runshare')
rushing_summary_total <- left_join(rushing_summary_total,avg_runshare)


setwd('~/Documents/CFB')

write.csv(rushing_summary_total, "runshares_2018_2021.csv")

rushing_summary_total <- read.csv('runshares_2018_2021.csv')
rushing_summary_total <- rushing_summary_total[,-c(1)]


top_4_runshares <- rushing_summary_total
top_4_runshares <- top_4_runshares[!duplicated(top_4_runshares),]


top_4_runshares$dk_pts <- ifelse(top_4_runshares$yards >= 100,3 + (6*top_4_runshares$touchdowns) + (.1*top_4_runshares$yards) - (top_4_runshares$fumbles),(6*top_4_runshares$touchdowns) + (.1*top_4_runshares$yards) - (top_4_runshares$fumbles))
top_4_runshares$fd_pts <- (6*top_4_runshares$touchdowns) + (.1*top_4_runshares$yards) - (2*top_4_runshares$fumbles)

qb_rushing_stats <- top_4_runshares[grepl("QB", top_4_runshares$position),]

qb_rushing_stats <- qb_rushing_stats %>%                                      
  arrange(desc(runshare)) %>% 
  group_by(team_name,week,year) %>%
  slice(1:4)

qb_rushing_stats <- qb_rushing_stats %>%
  group_by(team_name,week,year) %>%
  mutate(string = order(runshare, decreasing=TRUE))  


team_names <- read.csv('coach and pace names.csv')
team_names <- team_names[,c(1,3)]
colnames(team_names) <- c('team','team_name')
team_names <- data.table(team_names)
team_names[, team := stri_trans_general(str = team, 
                                        id = "Latin-ASCII")]

qb_rushing_stats <- left_join(qb_rushing_stats,team_names)
qb_rushing_stats <- left_join(qb_rushing_stats,coaches)
qb_rushing_stats <- qb_rushing_stats[!duplicated(qb_rushing_stats),]


qb_rushing_stats <- qb_rushing_stats %>% 
  group_by(team_name, player) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_attempts = rollapplyr(attempts, width = list(-1:-4), align = 'right', fill = NA, FUN = mean, partial = TRUE))

qb_rushing_stats <- qb_rushing_stats %>% 
  group_by(team_name, player) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_first_downs = rollapplyr(first_downs, width = list(-1:-4), align = 'right', fill = NA, FUN = mean, partial = TRUE))

qb_rushing_stats <- qb_rushing_stats %>% 
  group_by(team_name, player) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_fumbles = rollapplyr(fumbles, width = list(-1:-4), align = 'right', fill = NA, FUN = mean, partial = TRUE))

qb_rushing_stats <- qb_rushing_stats %>% 
  group_by(team_name, player) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_longest = rollapplyr(longest, width = list(-1:-4), align = 'right', fill = NA, FUN = mean, partial = TRUE))

qb_rushing_stats <- qb_rushing_stats %>% 
  group_by(team_name, player) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_touchdowns = rollapplyr(touchdowns, width = list(-1:-4), align = 'right', fill = NA, FUN = mean, partial = TRUE))

qb_rushing_stats <- qb_rushing_stats %>% 
  group_by(team_name, player) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_yards = rollapplyr(yards, width = list(-1:-4), align = 'right', fill = NA, FUN = mean, partial = TRUE))

qb_rushing_stats <- qb_rushing_stats %>% 
  group_by(team_name, player) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_ypa = rollapplyr(ypa, width = list(-1:-4), align = 'right', fill = NA, FUN = mean, partial = TRUE))

qb_rushing_stats <- qb_rushing_stats %>% 
  group_by(team_name, player) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_dk_pts = rollapplyr(dk_pts, width = list(-1:-4), align = 'right', fill = NA, FUN = mean, partial = TRUE))

qb_rushing_stats <- qb_rushing_stats %>% 
  group_by(team_name, player) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_fd_pts = rollapplyr(fd_pts, width = list(-1:-4), align = 'right', fill = NA, FUN = mean, partial = TRUE))

write.csv(qb_rushing_stats, 'qb_rushing_stats.csv')

top_4_runshares <- top_4_runshares[!grepl("QB", top_4_runshares$position),]
top_4_runshares <- top_4_runshares %>%                                      
  arrange(desc(runshare)) %>% 
  group_by(team_name,week,year) %>%
  slice(1:4)
  
top_4_runshares <- top_4_runshares %>%
  group_by(team_name,week,year) %>%
  mutate(string = order(runshare, decreasing=TRUE))  

team_names <- read.csv('coach and pace names.csv')
team_names <- team_names[,c(1,3)]
colnames(team_names) <- c('team','team_name')
team_names <- data.table(team_names)
team_names[, team := stri_trans_general(str = team, 
                                        id = "Latin-ASCII")]


top_4_runshares <- top_4_runshares[!duplicated(top_4_runshares),]


top_4_runshares <- top_4_runshares %>% 
  group_by(team_name, player) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_attempts = rollapplyr(attempts, width = list(-1:-4), align = 'right', fill = NA, FUN = mean, partial = TRUE))

top_4_runshares <- top_4_runshares %>% 
  group_by(team_name, player) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_first_downs = rollapplyr(first_downs, width = list(-1:-4), align = 'right', fill = NA, FUN = mean, partial = TRUE))

top_4_runshares <- top_4_runshares %>% 
  group_by(team_name, player) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_fumbles = rollapplyr(fumbles, width = list(-1:-4), align = 'right', fill = NA, FUN = mean, partial = TRUE))

top_4_runshares <- top_4_runshares %>% 
  group_by(team_name, player) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_longest = rollapplyr(longest, width = list(-1:-4), align = 'right', fill = NA, FUN = mean, partial = TRUE))

top_4_runshares <- top_4_runshares %>% 
  group_by(team_name, player) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_touchdowns = rollapplyr(touchdowns, width = list(-1:-4), align = 'right', fill = NA, FUN = mean, partial = TRUE))

top_4_runshares <- top_4_runshares %>% 
  group_by(team_name, player) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_yards = rollapplyr(yards, width = list(-1:-4), align = 'right', fill = NA, FUN = mean, partial = TRUE))

top_4_runshares <- top_4_runshares %>% 
  group_by(team_name, player) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_ypa = rollapplyr(ypa, width = list(-1:-4), align = 'right', fill = NA, FUN = mean, partial = TRUE))

top_4_runshares <- top_4_runshares %>% 
  group_by(team_name, player) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_dk_pts = rollapplyr(dk_pts, width = list(-1:-4), align = 'right', fill = NA, FUN = mean, partial = TRUE))

top_4_runshares <- top_4_runshares %>% 
  group_by(team_name, player) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_fd_pts = rollapplyr(fd_pts, width = list(-1:-4), align = 'right', fill = NA, FUN = mean, partial = TRUE))

top_4_runshares <- top_4_runshares %>% 
  group_by(team_name, string) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_runshare = rollapplyr(runshare, width = list(-1:-4), align = 'right', fill = NA, FUN = mean, partial = TRUE))


corr <- top_4_runshares[,c("runshare", "L3_runshare"  )]

corr <- corr[complete.cases(corr),]
corr <- corr[!duplicated(corr),]

rb_share <- corr
corr <- cor(corr)
corr <- as.data.frame(corr)
corr$runshare <- corr$runshare *10
corr <- corr %>% filter(!between(corr$runshare,-.5,5))
rownames(corr)

plot(rb_share)

set.seed(100)

TrainingIndex <- createDataPartition(rb_share$runshare, p=0.8, list = FALSE)
TrainingSet <- rb_share[TrainingIndex,] 
TestingSet <- rb_share[-TrainingIndex,] 

TrainControl <- trainControl( method = "repeatedcv", number = 10, repeats = 4)


xgboost_rb_share_model <- train(runshare ~ ., data = TrainingSet,
                                method = "lm",
                                na.action = na.omit,
                                preProcess=c("scale","center"),
                                trControl= TrainControl
)

xgboost_rb_share_model.training <-predict(xgboost_rb_share_model, TrainingSet, interval = 'confidence') 
xgboost_rb_share_model.testing <-predict(xgboost_rb_share_model, TestingSet,  interval = 'confidence') 


plot(TrainingSet$runshare,xgboost_rb_share_model.training, col = "blue" )
plot(TestingSet$runshare,xgboost_rb_share_model.testing, col = "blue" )

summary(xgboost_rb_share_model)

xgboost_rb_share_r.training <- cor(TrainingSet$runshare,xgboost_rb_share_model.training)
xgboost_rb_share_r.testing <- cor(TestingSet$runshare,xgboost_rb_share_model.testing)

xgboost_rb_share_r2.training <- xgboost_rb_share_r.training^2
xgboost_rb_share_r2.testing <- xgboost_rb_share_r.testing^2

xgboost_rb_share_actuals_preds <- data.frame(cbind(actuals=(TestingSet$runshare), predicteds=(xgboost_rb_share_model.testing)))
xgboost_rb_share_actuals_preds$diff <- (xgboost_rb_share_actuals_preds$actuals - xgboost_rb_share_actuals_preds$predicteds)

TestingSet <- cbind(TestingSet,xgboost_rb_share_actuals_preds)

plot(TestingSet$actuals,TestingSet$predicteds, col = "blue" )

xgboost_rb_share_min_max_accuracy <- mean(apply(xgboost_rb_share_actuals_preds, 1, min) / apply(xgboost_rb_share_actuals_preds, 1, max))  
xgboost_rb_share_mape <- MAPE(xgboost_rb_share_actuals_preds$predicteds, xgboost_rb_share_actuals_preds$actuals)
xgboost_rb_share_RMSE <- sqrt(mean((TestingSet$actuals - TestingSet$predicteds)^2))
xgboost_rb_share_MAE <- mean(abs(TestingSet$actuals - TestingSet$predicteds))

TestingSet <- left_join(TestingSet, rb_share)
saveRDS(xgboost_rb_share_model, "xgboost_rb_share_model.rds")

sd(xgboost_rb_share_model.testing)

write.csv(top_4_runshares,'rb_stats.csv')






