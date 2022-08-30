setwd('~/Documents/CFB')


library(tidyverse)
library(cfbfastR)


coaches <- cfbd_coaches(
  min_year = 2000,
  max_year = 2022
)

coaches$coach <- paste0(coaches$first_name," ",coaches$last_name)

coaches <- coaches[,c(16,4,5)]
colnames(coaches) <- c('head_coach','team','year')

team_names <- data.table(team_names)
coaches <- data.table(coaches)
coaches[, team := stri_trans_general(str = team, 
                                     id = "Latin-ASCII")]

setwd("~/Documents/CFB/passing_summaries")

passing_summary_2017_week7<- read.csv('passing_summarya.csv')
passing_summary_2017_week7$week <- 7
passing_summary_2017_week7$year <- 2017

passing_summary_2017_week8<- read.csv('passing_summaryb.csv')
passing_summary_2017_week8$week <- 8
passing_summary_2017_week8$year <- 2017

passing_summary_2017_week9<- read.csv('passing_summaryc.csv')
passing_summary_2017_week9$week <- 9
passing_summary_2017_week9$year <- 2017

passing_summary_2017_week10<- read.csv('passing_summaryd.csv')
passing_summary_2017_week10$week <- 10
passing_summary_2017_week10$year <- 2017

passing_summary_2017_week11<- read.csv('passing_summarye.csv')
passing_summary_2017_week11$week <- 11
passing_summary_2017_week11$year <- 2017

passing_summary_2017_week12<- read.csv('passing_summaryf.csv')
passing_summary_2017_week12$week <- 12
passing_summary_2017_week12$year <- 2017

passing_summary_2017_week13<- read.csv('passing_summaryg.csv')
passing_summary_2017_week13$week <- 13
passing_summary_2017_week13$year <- 2017

passing_summary_2017_week14<- read.csv('passing_summaryh.csv')
passing_summary_2017_week14$week <- 14
passing_summary_2017_week14$year <- 2017

passing_summary_2017 <- rbind(passing_summary_2017_week7,
                              passing_summary_2017_week8,
                              passing_summary_2017_week9,
                              passing_summary_2017_week10,
                              passing_summary_2017_week11,
                              passing_summary_2017_week12,
                              passing_summary_2017_week13,
                              passing_summary_2017_week14)

passing_summary_2018_week1 <- read.csv('passing_summary.csv')
passing_summary_2018_week1$week <- 1
passing_summary_2018_week1$year <- 2018

passing_summary_2018_week2 <- read.csv('passing_summary(1).csv')
passing_summary_2018_week2$week <- 2
passing_summary_2018_week2$year <- 2018


passing_summary_2018_week3<- read.csv('passing_summary(2).csv')
passing_summary_2018_week3$week <- 3
passing_summary_2018_week3$year <- 2018

passing_summary_2018_week4<- read.csv('passing_summary(3).csv')
passing_summary_2018_week4$week <- 4
passing_summary_2018_week4$year <- 2018

passing_summary_2018_week5<- read.csv('passing_summary(4).csv')
passing_summary_2018_week5$week <- 5
passing_summary_2018_week5$year <- 2018

passing_summary_2018_week6<- read.csv('passing_summary(5).csv')
passing_summary_2018_week6$week <- 6
passing_summary_2018_week6$year <- 2018

passing_summary_2018_week7<- read.csv('passing_summary(6).csv')
passing_summary_2018_week7$week <- 7
passing_summary_2018_week7$year <- 2018

passing_summary_2018_week8<- read.csv('passing_summary(7).csv')
passing_summary_2018_week8$week <- 8
passing_summary_2018_week8$year <- 2018

passing_summary_2018_week9<- read.csv('passing_summary(8).csv')
passing_summary_2018_week9$week <- 9
passing_summary_2018_week9$year <- 2018

passing_summary_2018_week10<- read.csv('passing_summary(9).csv')
passing_summary_2018_week10$week <- 10
passing_summary_2018_week10$year <- 2018

passing_summary_2018_week11<- read.csv('passing_summary(10).csv')
passing_summary_2018_week11$week <- 11
passing_summary_2018_week11$year <- 2018

passing_summary_2018_week12<- read.csv('passing_summary(11).csv')
passing_summary_2018_week12$week <- 12
passing_summary_2018_week12$year <- 2018

passing_summary_2018_week13<- read.csv('passing_summary(12).csv')
passing_summary_2018_week13$week <- 13
passing_summary_2018_week13$year <- 2018

passing_summary_2018_week14<- read.csv('passing_summary(13).csv')
passing_summary_2018_week14$week <- 14
passing_summary_2018_week14$year <- 2018


passing_summary_2018_week15<- read.csv('passing_summarybg.csv')
passing_summary_2018_week15$week <- 15
passing_summary_2018_week15$year <- 2018

passing_summary_2018 <- rbind(passing_summary_2018_week1,
                              passing_summary_2018_week2,
                              passing_summary_2018_week3,
                              passing_summary_2018_week4,
                              passing_summary_2018_week5,
                              passing_summary_2018_week6,
                              passing_summary_2018_week7,
                              passing_summary_2018_week8,
                              passing_summary_2018_week9,
                              passing_summary_2018_week10,
                              passing_summary_2018_week11,
                              passing_summary_2018_week12,
                              passing_summary_2018_week13,
                              passing_summary_2018_week14,
                              passing_summary_2018_week15)

passing_summary_2019_week1 <- read.csv('passing_summary(17).csv')
passing_summary_2019_week1$week <- 1
passing_summary_2019_week1$year <- 2019

passing_summary_2019_week2 <- read.csv('passing_summary(18).csv')
passing_summary_2019_week2$week <- 2
passing_summary_2019_week2$year <- 2019


passing_summary_2019_week3<- read.csv('passing_summary(19).csv')
passing_summary_2019_week3$week <- 3
passing_summary_2019_week3$year <- 2019

passing_summary_2019_week4<- read.csv('passing_summary(20).csv')
passing_summary_2019_week4$week <- 4
passing_summary_2019_week4$year <- 2019

passing_summary_2019_week5<- read.csv('passing_summary(21).csv')
passing_summary_2019_week5$week <- 5
passing_summary_2019_week5$year <- 2019

passing_summary_2019_week6<- read.csv('passing_summary(22).csv')
passing_summary_2019_week6$week <- 6
passing_summary_2019_week6$year <- 2019

passing_summary_2019_week7<- read.csv('passing_summary(23).csv')
passing_summary_2019_week7$week <- 7
passing_summary_2019_week7$year <- 2019

passing_summary_2019_week8<- read.csv('passing_summary(24).csv')
passing_summary_2019_week8$week <- 8
passing_summary_2019_week8$year <- 2019

passing_summary_2019_week9<- read.csv('passing_summary(25).csv')
passing_summary_2019_week9$week <- 9
passing_summary_2019_week9$year <- 2019

passing_summary_2019_week10<- read.csv('passing_summary(26).csv')
passing_summary_2019_week10$week <- 10
passing_summary_2019_week10$year <- 2019

passing_summary_2019_week11<- read.csv('passing_summary(27).csv')
passing_summary_2019_week11$week <- 11
passing_summary_2019_week11$year <- 2019

passing_summary_2019_week12<- read.csv('passing_summary(28).csv')
passing_summary_2019_week12$week <- 12
passing_summary_2019_week12$year <- 2019

passing_summary_2019_week13<- read.csv('passing_summary(29).csv')
passing_summary_2019_week13$week <- 13
passing_summary_2019_week13$year <- 2019

passing_summary_2019_week14<- read.csv('passing_summary(30).csv')
passing_summary_2019_week14$week <- 14
passing_summary_2019_week14$year <- 2019

passing_summary_2019_week15<- read.csv('passing_summary(32).csv')
passing_summary_2019_week15$week <- 15
passing_summary_2019_week15$year <- 2019

passing_summary_2019_week16<- read.csv('passing_summarybg1.csv')
passing_summary_2019_week16$week <- 16
passing_summary_2019_week16$year <- 2019


passing_summary_2019 <- rbind(passing_summary_2019_week1,
                              passing_summary_2019_week2,
                              passing_summary_2019_week3,
                              passing_summary_2019_week4,
                              passing_summary_2019_week5,
                              passing_summary_2019_week6,
                              passing_summary_2019_week7,
                              passing_summary_2019_week8,
                              passing_summary_2019_week9,
                              passing_summary_2019_week10,
                              passing_summary_2019_week11,
                              passing_summary_2019_week12,
                              passing_summary_2019_week13,
                              passing_summary_2019_week14,
                              passing_summary_2019_week15,
                              passing_summary_2019_week16)


passing_summary_2020_week1 <- read.csv('passing_summary(33).csv')
passing_summary_2020_week1$week <- 1
passing_summary_2020_week1$year <- 2020

passing_summary_2020_week2 <- read.csv('passing_summary(34).csv')
passing_summary_2020_week2$week <- 2
passing_summary_2020_week2$year <- 2020

passing_summary_2020_week3<- read.csv('passing_summary(35).csv')
passing_summary_2020_week3$week <- 3
passing_summary_2020_week3$year <- 2020

passing_summary_2020_week4<- read.csv('passing_summary(36).csv')
passing_summary_2020_week4$week <- 4
passing_summary_2020_week4$year <- 2020

passing_summary_2020_week5<- read.csv('passing_summary(37).csv')
passing_summary_2020_week5$week <- 5
passing_summary_2020_week5$year <- 2020

passing_summary_2020_week6<- read.csv('passing_summary(38).csv')
passing_summary_2020_week6$week <- 6
passing_summary_2020_week6$year <- 2020

passing_summary_2020_week7<- read.csv('passing_summary(39).csv')
passing_summary_2020_week7$week <- 7
passing_summary_2020_week7$year <- 2020

passing_summary_2020_week8<- read.csv('passing_summary(40).csv')
passing_summary_2020_week8$week <- 8
passing_summary_2020_week8$year <- 2020

passing_summary_2020_week9<- read.csv('passing_summary(41).csv')
passing_summary_2020_week9$week <- 9
passing_summary_2020_week9$year <- 2020

passing_summary_2020_week10<- read.csv('passing_summary(42).csv')
passing_summary_2020_week10$week <- 10
passing_summary_2020_week10$year <- 2020

passing_summary_2020_week11<- read.csv('passing_summary(43).csv')
passing_summary_2020_week11$week <- 11
passing_summary_2020_week11$year <- 2020

passing_summary_2020_week12<- read.csv('passing_summary(44).csv')
passing_summary_2020_week12$week <- 12
passing_summary_2020_week12$year <- 2020

passing_summary_2020_week13<- read.csv('passing_summary(44).csv')
passing_summary_2020_week13$week <- 13
passing_summary_2020_week13$year <- 2020

passing_summary_2020_week14<- read.csv('passing_summary(45).csv')
passing_summary_2020_week14$week <- 14
passing_summary_2020_week14$year <- 2020

passing_summary_2020_week15<- read.csv('passing_summary(46).csv')
passing_summary_2020_week15$week <- 15
passing_summary_2020_week15$year <- 2020

passing_summary_2020_week16<- read.csv('passing_summary(47).csv')
passing_summary_2020_week16$week <- 16
passing_summary_2020_week16$year <- 2020

passing_summary_2020_week17<- read.csv('passing_summarybg2.csv')
passing_summary_2020_week17$week <- 17
passing_summary_2020_week17$year <- 2020


passing_summary_2020 <- rbind(passing_summary_2020_week1,
                              passing_summary_2020_week2,
                              passing_summary_2020_week3,
                              passing_summary_2020_week4,
                              passing_summary_2020_week5,
                              passing_summary_2020_week6,
                              passing_summary_2020_week7,
                              passing_summary_2020_week8,
                              passing_summary_2020_week9,
                              passing_summary_2020_week10,
                              passing_summary_2020_week11,
                              passing_summary_2020_week12,
                              passing_summary_2020_week13,
                              passing_summary_2020_week14,
                              passing_summary_2020_week15,
                              passing_summary_2020_week16,
                              passing_summary_2020_week17)


passing_summary_2021_week1 <- read.csv('passing_summary(49).csv')
passing_summary_2021_week1$week <- 1
passing_summary_2021_week1$year <- 2021

passing_summary_2021_week2 <- read.csv('passing_summary(50).csv')
passing_summary_2021_week2$week <- 2
passing_summary_2021_week2$year <- 2021

passing_summary_2021_week3<- read.csv('passing_summary(51).csv')
passing_summary_2021_week3$week <- 3
passing_summary_2021_week3$year <- 2021

passing_summary_2021_week4<- read.csv('passing_summary(52).csv')
passing_summary_2021_week4$week <- 4
passing_summary_2021_week4$year <- 2021

passing_summary_2021_week5<- read.csv('passing_summary(53).csv')
passing_summary_2021_week5$week <- 5
passing_summary_2021_week5$year <- 2021

passing_summary_2021_week6<- read.csv('passing_summary(54).csv')
passing_summary_2021_week6$week <- 6
passing_summary_2021_week6$year <- 2021

passing_summary_2021_week7<- read.csv('passing_summary(55).csv')
passing_summary_2021_week7$week <- 7
passing_summary_2021_week7$year <- 2021

passing_summary_2021_week8<- read.csv('passing_summary(56).csv')
passing_summary_2021_week8$week <- 8
passing_summary_2021_week8$year <- 2021

passing_summary_2021_week9<- read.csv('passing_summary(57).csv')
passing_summary_2021_week9$week <- 9
passing_summary_2021_week9$year <- 2021

passing_summary_2021_week10<- read.csv('passing_summary(58).csv')
passing_summary_2021_week10$week <- 10
passing_summary_2021_week10$year <- 2021

passing_summary_2021_week11<- read.csv('passing_summary(59).csv')
passing_summary_2021_week11$week <- 11
passing_summary_2021_week11$year <- 2021

passing_summary_2021_week12<- read.csv('passing_summary(60).csv')
passing_summary_2021_week12$week <- 12
passing_summary_2021_week12$year <- 2021

passing_summary_2021_week13<- read.csv('passing_summary(61).csv')
passing_summary_2021_week13$week <- 13
passing_summary_2021_week13$year <- 2021

passing_summary_2021_week14<- read.csv('passing_summary(64).csv')
passing_summary_2021_week14$week <- 14
passing_summary_2021_week14$year <- 2021

passing_summary_2021_week15<- read.csv('passing_summarybg3.csv')
passing_summary_2021_week15$week <- 15
passing_summary_2021_week15$year <- 2021

passing_summary_2021 <- rbind(passing_summary_2021_week1,
                              passing_summary_2021_week2,
                              passing_summary_2021_week3,
                              passing_summary_2021_week4,
                              passing_summary_2021_week5,
                              passing_summary_2021_week6,
                              passing_summary_2021_week7,
                              passing_summary_2021_week8,
                              passing_summary_2021_week9,
                              passing_summary_2021_week10,
                              passing_summary_2021_week11,
                              passing_summary_2021_week12,
                              passing_summary_2021_week13,
                              passing_summary_2021_week14,
                              passing_summary_2021_week15)

passing_summary_total <- rbind(passing_summary_2017,
                               passing_summary_2018,
                               passing_summary_2019,
                               passing_summary_2020,
                               passing_summary_2021)

setwd('~/Documents/CFB')




passing_summary_total$dk_pts <- ifelse(passing_summary_total$yards >= 300,3 + (4*passing_summary_total$touchdowns) + (.04*passing_summary_total$yards) - (passing_summary_total$interceptions), (4*passing_summary_total$touchdowns) + (.04*passing_summary_total$yards) - (passing_summary_total$interceptions))
passing_summary_total$fd_pts <- (4*passing_summary_total$touchdowns) + (.04*passing_summary_total$yards) - (passing_summary_total$interceptions)


top_4_qbs <- passing_summary_total %>%                                      
  arrange(desc(attempts)) %>% 
  group_by(team_name,week,year) %>%
  slice(1:4)


top_4_qbs <- top_4_qbs %>%
  group_by(team_name,week,year) %>%
  mutate(string = order(attempts, decreasing=TRUE))  


team_names <- read.csv('coach and pace names.csv')
team_names <- team_names[,c(1,3)]
colnames(team_names) <- c('team','team_name')
team_names <- data.table(team_names)
team_names[, team := stri_trans_general(str = team, 
                                        id = "Latin-ASCII")]

top_4_qbs <- left_join(top_4_qbs,team_names)
top_4_qbs <- left_join(top_4_qbs,coaches)

top_4_qbs <- top_4_qbs %>% 
  group_by(team_name, player) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_attempts = rollapplyr(attempts, width = list(-1:-4), align = 'right', fill = NA, FUN = mean, partial = TRUE))


top_4_qbs <- top_4_qbs %>% 
  group_by(team_name, player) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_completion_percent = rollapplyr(completion_percent, width = list(-1:-4), align = 'right', fill = NA, FUN = mean, partial = TRUE))


top_4_qbs <- top_4_qbs %>% 
  group_by(team_name, player) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_completions = rollapplyr(completions, width = list(-1:-4), align = 'right', fill = NA, FUN = mean, partial = TRUE))


top_4_qbs <- top_4_qbs %>% 
  group_by(team_name, player) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_first_downs = rollapplyr(first_downs, width = list(-1:-4), align = 'right', fill = NA, FUN = mean, partial = TRUE))


top_4_qbs <- top_4_qbs %>% 
  group_by(team_name, player) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_interceptions = rollapplyr(interceptions, width = list(-1:-4), align = 'right', fill = NA, FUN = mean, partial = TRUE))


top_4_qbs <- top_4_qbs %>% 
  group_by(team_name, player) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_qb_rating = rollapplyr(qb_rating, width = list(-1:-4), align = 'right', fill = NA, FUN = mean, partial = TRUE))


top_4_qbs <- top_4_qbs %>% 
  group_by(team_name, player) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_sacks = rollapplyr(sacks, width = list(-1:-4), align = 'right', fill = NA, FUN = mean, partial = TRUE))


top_4_qbs <- top_4_qbs %>% 
  group_by(team_name, player) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_touchdowns = rollapplyr(touchdowns, width = list(-1:-4), align = 'right', fill = NA, FUN = mean, partial = TRUE))

top_4_qbs <- top_4_qbs %>% 
  group_by(team_name, player) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_yards = rollapplyr(yards, width = list(-1:-4), align = 'right', fill = NA, FUN = mean, partial = TRUE))


top_4_qbs <- top_4_qbs %>% 
  group_by(team_name, player) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_ypa = rollapplyr(ypa, width = list(-1:-4), align = 'right', fill = NA, FUN = mean, partial = TRUE))

top_4_qbs <- top_4_qbs %>% 
  group_by(team_name, player) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_dk_pts = rollapplyr(dk_pts, width = list(-1:-4), align = 'right', fill = NA, FUN = mean, partial = TRUE))

top_4_qbs <- top_4_qbs %>% 
  group_by(team_name, player) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_fd_pts = rollapplyr(fd_pts, width = list(-1:-4), align = 'right', fill = NA, FUN = mean, partial = TRUE))

top_4_qbs <- top_4_qbs[complete.cases(top_4_qbs),]
write.csv(top_4_qbs,'qb_stats.csv')












