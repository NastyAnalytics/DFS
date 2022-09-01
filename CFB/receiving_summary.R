library(tidyverse)
library(cfbfastR)
library(data.table)
library(stringi)
library(MLmetrics)
library(caret) 

setwd('~/Documents/CFB')


coaches <- cfbd_coaches(
  min_year = 2000,
  max_year = 2022
)

coaches$coach <- paste0(coaches$first_name," ",coaches$last_name)

coaches <- coaches[,c(16,4,5)]
colnames(coaches) <- c('head_coach','team','year')



setwd("~/Documents/CFB/receivers_targets")

receiving_summary_2018_week1 <- read.csv('receiving_summary.csv')
receiving_summary_2018_week1$week <- 1
receiving_summary_2018_week1$year <- 2018

receiving_summary_2018_week2 <- read.csv('receiving_summary (1).csv')
receiving_summary_2018_week2$week <- 2
receiving_summary_2018_week2$year <- 2018


receiving_summary_2018_week3<- read.csv('receiving_summary (2).csv')
receiving_summary_2018_week3$week <- 3
receiving_summary_2018_week3$year <- 2018

receiving_summary_2018_week4<- read.csv('receiving_summary (3).csv')
receiving_summary_2018_week4$week <- 4
receiving_summary_2018_week4$year <- 2018

receiving_summary_2018_week5<- read.csv('receiving_summary (4).csv')
receiving_summary_2018_week5$week <- 5
receiving_summary_2018_week5$year <- 2018

receiving_summary_2018_week6<- read.csv('receiving_summary (5).csv')
receiving_summary_2018_week6$week <- 6
receiving_summary_2018_week6$year <- 2018

receiving_summary_2018_week7<- read.csv('receiving_summary (6).csv')
receiving_summary_2018_week7$week <- 7
receiving_summary_2018_week7$year <- 2018

receiving_summary_2018_week8<- read.csv('receiving_summary (7).csv')
receiving_summary_2018_week8$week <- 8
receiving_summary_2018_week8$year <- 2018

receiving_summary_2018_week9<- read.csv('receiving_summary (8).csv')
receiving_summary_2018_week9$week <- 9
receiving_summary_2018_week9$year <- 2018

receiving_summary_2018_week10<- read.csv('receiving_summary (9).csv')
receiving_summary_2018_week10$week <- 10
receiving_summary_2018_week10$year <- 2018

receiving_summary_2018_week11<- read.csv('receiving_summary (10).csv')
receiving_summary_2018_week11$week <- 11
receiving_summary_2018_week11$year <- 2018

receiving_summary_2018_week12<- read.csv('receiving_summary (11).csv')
receiving_summary_2018_week12$week <- 12
receiving_summary_2018_week12$year <- 2018

receiving_summary_2018_week13<- read.csv('receiving_summary (12).csv')
receiving_summary_2018_week13$week <- 13
receiving_summary_2018_week13$year <- 2018

receiving_summary_2018_week14a<- read.csv('receiving_summary (13).csv')
receiving_summary_2018_week14b<- read.csv('receiving_summarycc.csv')
receiving_summary_2018_week14 <- rbind(receiving_summary_2018_week14a,receiving_summary_2018_week14b)
receiving_summary_2018_week14$week <- 14
receiving_summary_2018_week14$year <- 2018


receiving_summary_2018_week15<- read.csv('receiving_summarybg.csv')
receiving_summary_2018_week15$week <- 15
receiving_summary_2018_week15$year <- 2018

receiving_summary_2018 <- rbind(receiving_summary_2018_week1,
                                receiving_summary_2018_week2,
                                receiving_summary_2018_week3,
                                receiving_summary_2018_week4,
                                receiving_summary_2018_week5,
                                receiving_summary_2018_week6,
                                receiving_summary_2018_week7,
                                receiving_summary_2018_week8,
                                receiving_summary_2018_week9,
                                receiving_summary_2018_week10,
                                receiving_summary_2018_week11,
                                receiving_summary_2018_week12,
                                receiving_summary_2018_week13,
                                receiving_summary_2018_week14,
                                receiving_summary_2018_week15)

receiving_summary_2019_week1 <- read.csv('receiving_summary (14).csv')
receiving_summary_2019_week1$week <- 1
receiving_summary_2019_week1$year <- 2019

receiving_summary_2019_week2 <- read.csv('receiving_summary (15).csv')
receiving_summary_2019_week2$week <- 2
receiving_summary_2019_week2$year <- 2019


receiving_summary_2019_week3<- read.csv('receiving_summary (16).csv')
receiving_summary_2019_week3$week <- 3
receiving_summary_2019_week3$year <- 2019

receiving_summary_2019_week4<- read.csv('receiving_summary (17).csv')
receiving_summary_2019_week4$week <- 4
receiving_summary_2019_week4$year <- 2019

receiving_summary_2019_week5<- read.csv('receiving_summary (18).csv')
receiving_summary_2019_week5$week <- 5
receiving_summary_2019_week5$year <- 2019

receiving_summary_2019_week6<- read.csv('receiving_summary (19).csv')
receiving_summary_2019_week6$week <- 6
receiving_summary_2019_week6$year <- 2019

receiving_summary_2019_week7<- read.csv('receiving_summary (20).csv')
receiving_summary_2019_week7$week <- 7
receiving_summary_2019_week7$year <- 2019

receiving_summary_2019_week8<- read.csv('receiving_summary (21).csv')
receiving_summary_2019_week8$week <- 8
receiving_summary_2019_week8$year <- 2019

receiving_summary_2019_week9<- read.csv('receiving_summary (22).csv')
receiving_summary_2019_week9$week <- 9
receiving_summary_2019_week9$year <- 2019

receiving_summary_2019_week10<- read.csv('receiving_summary (23).csv')
receiving_summary_2019_week10$week <- 10
receiving_summary_2019_week10$year <- 2019

receiving_summary_2019_week11<- read.csv('receiving_summary (24).csv')
receiving_summary_2019_week11$week <- 11
receiving_summary_2019_week11$year <- 2019

receiving_summary_2019_week12<- read.csv('receiving_summary (26).csv')
receiving_summary_2019_week12$week <- 12
receiving_summary_2019_week12$year <- 2019

receiving_summary_2019_week13<- read.csv('receiving_summary (27).csv')
receiving_summary_2019_week13$week <- 13
receiving_summary_2019_week13$year <- 2019

receiving_summary_2019_week14<- read.csv('receiving_summary (28).csv')
receiving_summary_2019_week14$week <- 14
receiving_summary_2019_week14$year <- 2019


receiving_summary_2019_week15<- read.csv('receiving_summarycc1.csv')
receiving_summary_2019_week15$week <- 15
receiving_summary_2019_week15$year <- 2019

receiving_summary_2019_week16<- read.csv('receiving_summarybg1.csv')
receiving_summary_2019_week16$week <- 16
receiving_summary_2019_week16$year <- 2019


receiving_summary_2019 <- rbind(receiving_summary_2019_week1,
                                receiving_summary_2019_week2,
                                receiving_summary_2019_week3,
                                receiving_summary_2019_week4,
                                receiving_summary_2019_week5,
                                receiving_summary_2019_week6,
                                receiving_summary_2019_week7,
                                receiving_summary_2019_week8,
                                receiving_summary_2019_week9,
                                receiving_summary_2019_week10,
                                receiving_summary_2019_week11,
                                receiving_summary_2019_week12,
                                receiving_summary_2019_week13,
                                receiving_summary_2019_week14,
                                receiving_summary_2019_week15,
                                receiving_summary_2019_week16)


receiving_summary_2020_week1 <- read.csv('receiving_summary (29).csv')
receiving_summary_2020_week1$week <- 1
receiving_summary_2020_week1$year <- 2020

receiving_summary_2020_week2 <- read.csv('receiving_summary (30).csv')
receiving_summary_2020_week2$week <- 2
receiving_summary_2020_week2$year <- 2020

receiving_summary_2020_week3<- read.csv('receiving_summary (31).csv')
receiving_summary_2020_week3$week <- 3
receiving_summary_2020_week3$year <- 2020

receiving_summary_2020_week4<- read.csv('receiving_summary (32).csv')
receiving_summary_2020_week4$week <- 4
receiving_summary_2020_week4$year <- 2020

receiving_summary_2020_week5<- read.csv('receiving_summary (33).csv')
receiving_summary_2020_week5$week <- 5
receiving_summary_2020_week5$year <- 2020

receiving_summary_2020_week6<- read.csv('receiving_summary (34).csv')
receiving_summary_2020_week6$week <- 6
receiving_summary_2020_week6$year <- 2020

receiving_summary_2020_week7<- read.csv('receiving_summary (35).csv')
receiving_summary_2020_week7$week <- 7
receiving_summary_2020_week7$year <- 2020

receiving_summary_2020_week8<- read.csv('receiving_summary (36).csv')
receiving_summary_2020_week8$week <- 8
receiving_summary_2020_week8$year <- 2020

receiving_summary_2020_week9<- read.csv('receiving_summary (37).csv')
receiving_summary_2020_week9$week <- 9
receiving_summary_2020_week9$year <- 2020

receiving_summary_2020_week10<- read.csv('receiving_summary (38).csv')
receiving_summary_2020_week10$week <- 10
receiving_summary_2020_week10$year <- 2020

receiving_summary_2020_week11<- read.csv('receiving_summary (39).csv')
receiving_summary_2020_week11$week <- 11
receiving_summary_2020_week11$year <- 2020

receiving_summary_2020_week12<- read.csv('receiving_summary (40).csv')
receiving_summary_2020_week12$week <- 12
receiving_summary_2020_week12$year <- 2020

receiving_summary_2020_week13<- read.csv('receiving_summary (41).csv')
receiving_summary_2020_week13$week <- 13
receiving_summary_2020_week13$year <- 2020

receiving_summary_2020_week14<- read.csv('receiving_summary (42).csv')
receiving_summary_2020_week14$week <- 14
receiving_summary_2020_week14$year <- 2020

receiving_summary_2020_week15<- read.csv('receiving_summary (43).csv')
receiving_summary_2020_week15$week <- 15
receiving_summary_2020_week15$year <- 2020

receiving_summary_2020_week16<- read.csv('receiving_summary (44).csv')
receiving_summary_2020_week16$week <- 16
receiving_summary_2020_week16$year <- 2020

receiving_summary_2020_week17<- read.csv('receiving_summarybg2.csv')
receiving_summary_2020_week17$week <- 17
receiving_summary_2020_week17$year <- 2020


receiving_summary_2020 <- rbind(receiving_summary_2020_week1,
                                receiving_summary_2020_week2,
                                receiving_summary_2020_week3,
                                receiving_summary_2020_week4,
                                receiving_summary_2020_week5,
                                receiving_summary_2020_week6,
                                receiving_summary_2020_week7,
                                receiving_summary_2020_week8,
                                receiving_summary_2020_week9,
                                receiving_summary_2020_week10,
                                receiving_summary_2020_week11,
                                receiving_summary_2020_week12,
                                receiving_summary_2020_week13,
                                receiving_summary_2020_week14,
                                receiving_summary_2020_week15,
                                receiving_summary_2020_week16,
                                receiving_summary_2020_week17)


receiving_summary_2021_week1 <- read.csv('receiving_summary (45).csv')
receiving_summary_2021_week1$week <- 1
receiving_summary_2021_week1$year <- 2021

receiving_summary_2021_week2 <- read.csv('receiving_summary (46).csv')
receiving_summary_2021_week2$week <- 2
receiving_summary_2021_week2$year <- 2021

receiving_summary_2021_week3<- read.csv('receiving_summary (47).csv')
receiving_summary_2021_week3$week <- 3
receiving_summary_2021_week3$year <- 2021

receiving_summary_2021_week4<- read.csv('receiving_summary (48).csv')
receiving_summary_2021_week4$week <- 4
receiving_summary_2021_week4$year <- 2021

receiving_summary_2021_week5<- read.csv('receiving_summary (49).csv')
receiving_summary_2021_week5$week <- 5
receiving_summary_2021_week5$year <- 2021

receiving_summary_2021_week6<- read.csv('receiving_summary (50).csv')
receiving_summary_2021_week6$week <- 6
receiving_summary_2021_week6$year <- 2021

receiving_summary_2021_week7<- read.csv('receiving_summary (51).csv')
receiving_summary_2021_week7$week <- 7
receiving_summary_2021_week7$year <- 2021

receiving_summary_2021_week8<- read.csv('receiving_summary (52).csv')
receiving_summary_2021_week8$week <- 8
receiving_summary_2021_week8$year <- 2021

receiving_summary_2021_week9<- read.csv('receiving_summary (53).csv')
receiving_summary_2021_week9$week <- 9
receiving_summary_2021_week9$year <- 2021

receiving_summary_2021_week10<- read.csv('receiving_summary (54).csv')
receiving_summary_2021_week10$week <- 10
receiving_summary_2021_week10$year <- 2021

receiving_summary_2021_week11<- read.csv('receiving_summary (55).csv')
receiving_summary_2021_week11$week <- 11
receiving_summary_2021_week11$year <- 2021

receiving_summary_2021_week12<- read.csv('receiving_summary (56).csv')
receiving_summary_2021_week12$week <- 12
receiving_summary_2021_week12$year <- 2021

receiving_summary_2021_week13<- read.csv('receiving_summary (57).csv')
receiving_summary_2021_week13$week <- 13
receiving_summary_2021_week13$year <- 2021

receiving_summary_2021_week14<- read.csv('receiving_summarycc3.csv')
receiving_summary_2021_week14$week <- 14
receiving_summary_2021_week14$year <- 2021

receiving_summary_2021_week15<- read.csv('receiving_summarybg3.csv')
receiving_summary_2021_week15$week <- 15
receiving_summary_2021_week15$year <- 2021

receiving_summary_2021 <- rbind(receiving_summary_2021_week1,
                                receiving_summary_2021_week2,
                                receiving_summary_2021_week3,
                                receiving_summary_2021_week4,
                                receiving_summary_2021_week5,
                                receiving_summary_2021_week6,
                                receiving_summary_2021_week7,
                                receiving_summary_2021_week8,
                                receiving_summary_2021_week9,
                                receiving_summary_2021_week10,
                                receiving_summary_2021_week11,
                                receiving_summary_2021_week12,
                                receiving_summary_2021_week13,
                                receiving_summary_2021_week14,
                                receiving_summary_2021_week15)

receiving_summary_total <- rbind(receiving_summary_2018,
                                 receiving_summary_2019,
                                 receiving_summary_2020,
                                 receiving_summary_2021)



team_targets <- aggregate(targets ~ team_name + week + year,data = receiving_summary_total,FUN = sum)
colnames(team_targets) <- c('team_name','week','year','team_targets')
receiving_summary_total <- left_join(receiving_summary_total,team_targets)
receiving_summary_total$rec_usage <- receiving_summary_total$targets/receiving_summary_total$team_targets

team_total_targets <- aggregate(targets ~ team_name + year,data = receiving_summary_total,FUN = sum)
total_targets <- aggregate(targets ~ player + team_name + year,data = receiving_summary_total,FUN = sum)
colnames(total_targets) <- c('player','team_name','year','season_targets')
colnames(team_total_targets) <- c('team_name','year','season_team_targets')
receiving_summary_total <- left_join(receiving_summary_total,team_total_targets)
receiving_summary_total <- left_join(receiving_summary_total,total_targets)
receiving_summary_total$season_usage <- receiving_summary_total$season_targets/receiving_summary_total$season_team_targets


setwd('~/Documents/CFB')
team_names <- read.csv('coach and pace names.csv')
team_names <- team_names[,c(1,3)]
colnames(team_names) <- c('team','team_name')
team_names <- data.table(team_names)
team_names[, team := stri_trans_general(str = team, 
                                        id = "Latin-ASCII")]
team_names[, team_name := stri_trans_general(str = team_name, 
                                             id = "Latin-ASCII")]

top_6_usage <- left_join(receiving_summary_total, team_names)

top_6_usage <- top_6_usage[,-c(23)]

top_6_usage$dk_pts <- ifelse(top_6_usage$yards >= 100,3 + (.1*top_6_usage$yards) + (6*top_6_usage$touchdowns) + (top_6_usage$receptions) - (top_6_usage$fumbles),(.1*top_6_usage$yards) + (6*top_6_usage$touchdowns) + (top_6_usage$receptions) - (top_6_usage$fumbles))
top_6_usage$fd_pts <- (.1*top_6_usage$yards) + (6*top_6_usage$touchdowns) + (0.5*top_6_usage$receptions) - (2*top_6_usage$fumbles)


rb_receiving_stats <- top_6_usage[grepl("HB|FB", top_6_usage$position),]

rb_stats <- read.csv('rb_stats.csv')
rb_stats <- rb_stats[,c(2,5,19,20,29)]
rb_receiving_stats <- left_join(rb_receiving_stats,rb_stats)

rb_receiving_stats <- rb_receiving_stats %>% 
  group_by(team_name,string) %>% 
  arrange(week) %>% 
  arrange(year) %>%
  mutate(L3_rec_usage = rollapply(rec_usage, width = list(-1:-4), align = 'right', fill = NA, FUN = mean, partial = TRUE))


rb_receiving_stats <- rb_receiving_stats[complete.cases(rb_receiving_stats),]
rb_receiving_stats <- rb_receiving_stats[!duplicated(rb_receiving_stats),]

corr <- rb_receiving_stats[,c('rec_usage','L3_rec_usage')]
corr <- corr[complete.cases(corr),]
rb_usage <- corr
corr <- cor(corr)
plot(rb_usage)
view(corr)

set.seed(100)

TrainingIndex <- createDataPartition(rb_usage$rec_usage, p=0.8, list = FALSE)
TrainingSet <- rb_usage[TrainingIndex,] 
TestingSet <- rb_usage[-TrainingIndex,] 

TrainControl <- trainControl( method = "repeatedcv", number = 10, repeats = 4)


xgboost_rb_usage_model <- train(rec_usage ~ ., data = TrainingSet,
                                method = "lm",
                                na.action = na.omit,
                                preProcess=c("scale","center"),
                                trControl= TrainControl
)

xgboost_rb_usage_model.training <-predict(xgboost_rb_usage_model, TrainingSet, interval = 'confidence') 
xgboost_rb_usage_model.testing <-predict(xgboost_rb_usage_model, TestingSet,  interval = 'confidence') 


plot(TrainingSet$rec_usage,xgboost_rb_usage_model.training, col = "blue" )
plot(TestingSet$rec_usage,xgboost_rb_usage_model.testing, col = "blue" )

summary(xgboost_rb_usage_model)

xgboost_rb_usage_r.training <- cor(TrainingSet$rec_usage,xgboost_rb_usage_model.training)
xgboost_rb_usage_r.testing <- cor(TestingSet$rec_usage,xgboost_rb_usage_model.testing)

xgboost_rb_usage_r2.training <- xgboost_rb_usage_r.training^2
xgboost_rb_usage_r2.testing <- xgboost_rb_usage_r.testing^2

xgboost_rb_usage_actuals_preds <- data.frame(cbind(actuals=(TestingSet$rec_usage), predicteds=(xgboost_rb_usage_model.testing)))
xgboost_rb_usage_actuals_preds$diff <- (xgboost_rb_usage_actuals_preds$actuals - xgboost_rb_usage_actuals_preds$predicteds)

TestingSet <- cbind(TestingSet,xgboost_rb_usage_actuals_preds)

plot(TestingSet$actuals,TestingSet$predicteds, col = "blue" )

xgboost_rb_usage_min_max_accuracy <- mean(apply(xgboost_rb_usage_actuals_preds, 1, min) / apply(xgboost_rb_usage_actuals_preds, 1, max))  
xgboost_rb_usage_mape <- MAPE(xgboost_rb_usage_actuals_preds$predicteds, xgboost_rb_usage_actuals_preds$actuals)
xgboost_rb_usage_RMSE <- sqrt(mean((TestingSet$actuals - TestingSet$predicteds)^2))
xgboost_rb_usage_MAE <- mean(abs(TestingSet$actuals - TestingSet$predicteds))

TestingSet <- left_join(TestingSet, rb_usage)
saveRDS(xgboost_rb_usage_model, "xgboost_rb_usage_model.rds")

sd(xgboost_rb_usage_model.testing)

write.csv(rb_receiving_stats, 'rb_rec_stats.csv')


top_6_usage <- top_6_usage %>%                                      
  arrange(desc(rec_usage)) %>% 
  group_by(team_name,week,year) %>%
  slice(1:9)

top_6_usage <- top_6_usage %>%
  group_by(team_name,week,year) %>%
  mutate(string = order(rec_usage, decreasing=TRUE))  

top_6_usage <- top_6_usage %>% 
  group_by(team_name,string) %>% 
  arrange(week) %>% 
  arrange(year) %>%
  mutate(L3_rec_usage = rollapply(rec_usage, width = list(-1:-4), align = 'right', fill = NA, FUN = mean, partial = TRUE))

top_6_usage <- top_6_usage %>% 
  group_by(team_name,string) %>% 
  arrange(week) %>% 
  arrange(year) %>%
  mutate(L3_targets = rollapply(targets, width = list(-1:-4), align = 'right', fill = NA, FUN = mean, partial = TRUE))


top_6_usage <- top_6_usage[complete.cases(top_6_usage),]
top_6_usage <- top_6_usage[!duplicated(top_6_usage),]

corr <- top_6_usage[,c('rec_usage','L3_rec_usage')]
corr <- corr[complete.cases(corr),]
wr_share <- corr
corr <- cor(corr)
plot(wr_share)


set.seed(100)

TrainingIndex <- createDataPartition(wr_share$rec_usage, p=0.8, list = FALSE)
TrainingSet <- wr_share[TrainingIndex,] 
TestingSet <- wr_share[-TrainingIndex,] 

TrainControl <- trainControl( method = "repeatedcv", number = 10, repeats = 4)


xgboost_wr_share_model <- train(rec_usage ~ ., data = TrainingSet,
                                method = "lm",
                                na.action = na.omit,
                                preProcess=c("scale","center"),
                                trControl= TrainControl
)

xgboost_wr_share_model.training <-predict(xgboost_wr_share_model, TrainingSet, interval = 'confidence') 
xgboost_wr_share_model.testing <-predict(xgboost_wr_share_model, TestingSet,  interval = 'confidence') 


plot(TrainingSet$rec_usage,xgboost_wr_share_model.training, col = "blue" )
plot(TestingSet$rec_usage,xgboost_wr_share_model.testing, col = "blue" )

summary(xgboost_wr_share_model)

xgboost_wr_share_r.training <- cor(TrainingSet$rec_usage,xgboost_wr_share_model.training)
xgboost_wr_share_r.testing <- cor(TestingSet$rec_usage,xgboost_wr_share_model.testing)

xgboost_wr_share_r2.training <- xgboost_wr_share_r.training^2
xgboost_wr_share_r2.testing <- xgboost_wr_share_r.testing^2

xgboost_wr_share_actuals_preds <- data.frame(cbind(actuals=(TestingSet$rec_usage), predicteds=(xgboost_wr_share_model.testing)))
xgboost_wr_share_actuals_preds$diff <- (xgboost_wr_share_actuals_preds$actuals - xgboost_wr_share_actuals_preds$predicteds)

TestingSet <- cbind(TestingSet,xgboost_wr_share_actuals_preds)

plot(TestingSet$actuals,TestingSet$predicteds, col = "blue" )

xgboost_wr_share_min_max_accuracy <- mean(apply(xgboost_wr_share_actuals_preds, 1, min) / apply(xgboost_wr_share_actuals_preds, 1, max))  
xgboost_wr_share_mape <- MAPE(xgboost_wr_share_actuals_preds$predicteds, xgboost_wr_share_actuals_preds$actuals)
xgboost_wr_share_RMSE <- sqrt(mean((TestingSet$actuals - TestingSet$predicteds)^2))
xgboost_wr_share_MAE <- mean(abs(TestingSet$actuals - TestingSet$predicteds))

TestingSet <- left_join(TestingSet, wr_share)
saveRDS(xgboost_wr_share_model, "xgboost_wr_share_model.rds")

sd(xgboost_wr_share_model.testing)

write.csv(top_6_usage,'wr_stats.csv')










