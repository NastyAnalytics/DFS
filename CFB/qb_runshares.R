library(cfbfastR)
library(RSelenium)
library(rvest)
library(tidyverse)
library(data.table)
library(stringi)
library(ggplot2)
library(dplyr)
library(zoo)

setwd('~/Documents/CFB')

qb_runshares <- read.csv('qb_runshares.csv')
qb_runshares <- qb_runshares %>%                                      
  arrange(desc(runshare)) %>% 
  group_by(team,week,year) %>%
  slice(1)

games_w_coaches <- read.csv('games_w_coaches.csv')
games_w_coaches <- games_w_coaches[,-c(1)]
coaches <- games_w_coaches[,c('team','coach','week','season')]
colnames(coaches) <- c('team','coach','week','year')

qb_runshares <- left_join(qb_runshares,coaches)

qb_runshares <- qb_runshares %>% 
  group_by(coach) %>% 
  arrange(week) %>% 
  arrange(year) %>%
  mutate(L3_runshare = rollapply(runshare, width = list(-1:-4), align = 'right', fill = NA, FUN = mean))


qb_runshares <- qb_runshares %>% 
  group_by(coach) %>%
  arrange(week) %>% 
  arrange(year) %>%
  mutate(L3_rush_att = rollapply(rushing_car, width = list(-1:-4), align = 'right', fill = NA, FUN = mean))

qb_runshares <- qb_runshares[complete.cases(qb_runshares),]

teams_sacks <- read.csv('teams_sacks.csv')
teams_sacks <- teams_sacks[,-c(1)]
qb_runshares <- left_join(qb_runshares,teams_sacks)
qb_runshares <- rename(qb_runshares,season = year)

efficiency <- read.csv('efficiency.csv')
efficiency <- efficiency[,-c(1)]
qb_runshares <- rename(qb_runshares,year = season)
qb_runshares <- left_join(qb_runshares,efficiency)

time_per_play <- read.csv('time_per_play.csv')
time_per_play <- time_per_play[,-c(1)]
qb_runshares <- left_join(qb_runshares,time_per_play)


time_per_play <- read.csv('time_per_play.csv')
time_per_play <- time_per_play[,-c(1)]
colnames(time_per_play) <- c('year','opp_team','week','opp_tpp','L3_opp_tpp')
qb_runshares <- left_join(qb_runshares,time_per_play)

team_adv_stats <- read.csv('team_adv_stats.csv')
team_adv_stats <- team_adv_stats[,-c(1)]
qb_runshares <- left_join(qb_runshares,team_adv_stats)


corr <- qb_runshares
corr <- corr[complete.cases(corr),]

qb_share <- corr

qb_share <- qb_share[!duplicated(qb_share),]
qb_share <- left_join(qb_share,qb_share)

write.csv(qb_share,'qb_shares.csv')

library(MLmetrics)
library(caret) 

corr <- qb_share[,c( "runshare",                             "L3_runshare",                         
                     "L3_rush_att",                          "L3_sacks",                            
                     "L3_hurries",                           "L3_avg_def_down",                     
                     "L3_avg_distance",                      "L3_avg_def_drive_efficiency",         
                     "L3_def_ppa",                           "L3_def_success_rate",                 
                     "L3_def_explosiveness",                 "L3_def_stuff_rate",                   
                     "L3_def_line_yds",                      "L3_def_second_lvl_yds",               
                     "L3_def_pts_per_opp",                   "L3def_field_pos_avg_predicted_points",
                     "L3_def_standard_downs_rate",           "L3_def_standard_downs_ppa",           
                     "L3_def_standard_downs_success_rate",   "L3_def_passing_downs_rate",           
                     "L3_def_passing_downs_ppa",             "L3_def_passing_downs_success_rate",   
                     "L3_def_rushing_plays_rate",            "L3_def_rushing_plays_ppa",            
                     "L3_def_rushing_plays_success_rate",    "L3_def_passing_plays_rate",           
                     "L3_def_passing_plays_ppa",             "L3_def_passing_plays_success_rate"  )]

qb_share <- corr
corr <- cor(corr)

view(corr)
sum(is.na(qb_share))

set.seed(100)

TrainingIndex <- createDataPartition(qb_share$runshare, p=0.8, list = FALSE)
TrainingSet <- qb_share[TrainingIndex,] 
TestingSet <- qb_share[-TrainingIndex,] 

TrainControl <- trainControl( method = "repeatedcv", number = 10, repeats = 4)


xgboost_qb_share_model <- train(runshare ~ ., data = TrainingSet,
                             method = "xgbTree",
                             na.action = na.omit,
                             preProcess=c("scale","center"),
                             trControl= TrainControl
)

xgboost_qb_share_model.training <-predict(xgboost_qb_share_model, TrainingSet, interval = 'confidence') 
xgboost_qb_share_model.testing <-predict(xgboost_qb_share_model, TestingSet,  interval = 'confidence') 


plot(TrainingSet$runshare,xgboost_qb_share_model.training, col = "blue" )
plot(TestingSet$runshare,xgboost_qb_share_model.testing, col = "blue" )

summary(xgboost_qb_share_model)

xgboost_qb_share_r.training <- cor(TrainingSet$runshare,xgboost_qb_share_model.training)
xgboost_qb_share_r.testing <- cor(TestingSet$runshare,xgboost_qb_share_model.testing)

xgboost_qb_share_r2.training <- xgboost_qb_share_r.training^2
xgboost_qb_share_r2.testing <- xgboost_qb_share_r.testing^2

xgboost_qb_share_actuals_preds <- data.frame(cbind(actuals=(TestingSet$runshare), predicteds=(xgboost_qb_share_model.testing)))
xgboost_qb_share_actuals_preds$diff <- (xgboost_qb_share_actuals_preds$actuals - xgboost_qb_share_actuals_preds$predicteds)

TestingSet <- cbind(TestingSet,xgboost_qb_share_actuals_preds)

plot(TestingSet$actuals,TestingSet$predicteds, col = "blue" )

xgboost_qb_share_min_max_accuracy <- mean(apply(xgboost_qb_share_actuals_preds, 1, min) / apply(xgboost_qb_share_actuals_preds, 1, max))  
xgboost_qb_share_mape <- MAPE(xgboost_qb_share_actuals_preds$predicteds, xgboost_qb_share_actuals_preds$actuals)
xgboost_qb_share_RMSE <- sqrt(mean((TestingSet$actuals - TestingSet$predicteds)^2))
xgboost_qb_share_MAE <- mean(abs(TestingSet$actuals - TestingSet$predicteds))

TestingSet <- left_join(TestingSet, qb_share)
saveRDS(xgboost_qb_share_model, "xgboost_qb_share_model.rds")

sd(xgboost_qb_share_model.testing)


