library(cfbfastR)
library(RSelenium)
library(rvest)
library(tidyverse)
library(data.table)
library(stringi)
library(ggplot2)
library(dplyr)
library(zoo)

i <- 2013
lines_total <- data.frame()
  while (i <= 2021) {
    for (j in 1:16) {
      lines <- cfbd_betting_lines(
      year = i,
      week = j,
      line_provider = 'teamrankings'
    )
      assign(paste0('lines_',j,'_',i),lines)
      lines_total <- rbind(lines_total,lines)
      
    }
    i <- i + 1
  }

lines_total <- lines_total %>% drop_na(over_under)
lines_total <- lines_total %>% drop_na(spread)


lines_total$implied_total <- (as.numeric(lines_total$over_under)/2) - (as.numeric(lines_total$spread)/2)
lines_total$opp_implied_total <- (as.numeric(lines_total$over_under)/2) + (as.numeric(lines_total$spread)/2)

lines_total$error <- abs(lines_total$implied_total - lines_total$home_score)


lines_total <- lines_total[,c(1,2,4,6,8,9,11,20,21)]
colnames(lines_total) <- c('game_id','season','week','team','pts','opp_team','opp_pts','implied_total','opp_implied_total')
lines_total1 <- lines_total[,c(1:3,6,7,4,5,9,8)]
colnames(lines_total1) <- c('game_id','season','week','team','pts','opp_team','opp_pts','implied_total','opp_implied_total')

lines_total <- rbind(lines_total,lines_total1)

lines_total <- lines_total[,c(2:4,6,8:9)]
lines_total <- rename(lines_total, year = season)
write.csv(lines_total,'imp_totals1.csv')
