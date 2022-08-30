library(zoo)
library(cfbfastR)
library(tidyverse)
library(caret)
library(MLmetrics)
library(ggplot2)
library(stringi)
library(data.table)


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


j = 2010
i = 1 
cfbd_plays_total <- data.frame()

while(j <= 2021){
  for(i in 1:16)
      tryCatch({
  cfbd_plays <- cfbd_plays(year = j, week = i)
  cfbd_plays$week <- i
  cfbd_plays$season <- j
  
  assign(paste0('cfbd_plays',i,"_",j),cfbd_plays)
  cfbd_plays_total <- rbind(cfbd_plays_total,cfbd_plays)
  
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
  j <- j + 1
  
}
avg_down <- aggregate(down ~ offense + week + season, data = cfbd_plays_total, FUN = mean)
colnames(avg_down) <- c('team','week','season','avg_down')
avg_distance <- aggregate(distance ~ offense + week + season, data = cfbd_plays_total, FUN = mean)
colnames(avg_distance) <- c('team','week','season','avg_distance')

avg_def_down <- aggregate(down ~ defense + week + season, data = cfbd_plays_total, FUN = mean)
colnames(avg_def_down) <- c('team','week','season','avg_def_down')
avg_def_distance <- aggregate(distance ~ defense + week + season, data = cfbd_plays_total, FUN = mean)
colnames(avg_def_distance) <- c('team','week','season','avg_def_distance')




j = 2010
i = 1
cfbd_drives_total <- data.frame()

while(j <= 2021){
  for(i in 1:16)
      tryCatch({
  cfbd_drives <- cfbd_drives(j, week = i)
  
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
  cfbd_drives$season <- j 
  assign(paste0('cfbd_drives',i,"_",j),cfbd_drives)
  cfbd_drives_total <- rbind(cfbd_drives_total,cfbd_drives)

    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
  j <- j + 1
  
}


drive_efficiency <- aggregate(epa ~ offense + season + week, data = cfbd_drives_total, FUN = mean)
def_drive_efficiency <- aggregate(epa ~ defense + season + week, data = cfbd_drives_total, FUN = mean)


colnames(drive_efficiency) <- c('team','season','week','avg_drive_efficiency')
colnames(def_drive_efficiency) <- c('team','season','week','avg_def_drive_efficiency')


efficiency <- left_join(avg_down,avg_def_down)
efficiency <- left_join(efficiency,avg_distance)
efficiency <- left_join(efficiency,avg_def_distance)
efficiency <- left_join(efficiency,drive_efficiency)
efficiency <- left_join(efficiency,def_drive_efficiency)
efficiency <- rename(efficiency, year = season)


efficiency <- efficiency %>% 
  group_by(team) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_avg_down = rollapplyr(avg_down, width = list(-1:-4), align = 'right', fill = NA, FUN = mean, partial = TRUE))

efficiency <- efficiency %>% 
  group_by(team) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_avg_def_down = rollapplyr(avg_def_down, width = list(-1:-4), align = 'right', fill = NA, FUN = mean, partial = TRUE))

efficiency <- efficiency %>% 
  group_by(team) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_avg_distance = rollapplyr(avg_distance, width = list(-1:-4), align = 'right', fill = NA, FUN = mean, partial = TRUE))

efficiency <- efficiency %>% 
  group_by(team) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_avg_def_distance = rollapplyr(avg_def_distance, width = list(-1:-4), align = 'right', fill = NA, FUN = mean, partial = TRUE))

efficiency <- efficiency %>% 
  group_by(team) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_avg_drive_efficiency = rollapplyr(avg_drive_efficiency, width = list(-1:-4), align = 'right', fill = NA, FUN = mean, partial = TRUE))

efficiency <- efficiency %>% 
  group_by(team) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_avg_def_drive_efficiency = rollapplyr(avg_def_drive_efficiency, width = list(-1:-4), align = 'right', fill = NA, FUN = mean, partial = TRUE))

efficiency <- efficiency[,-c(4:9)]

write.csv(efficiency,'efficiency.csv')





