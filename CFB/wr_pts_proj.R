library(cfbfastR)
library(RSelenium)
library(rvest)
library(tidyverse)
library(data.table)
library(stringi)
library(ggplot2)
library(dplyr)
library(zoo)

wr_stats <- read.csv('wr_stats.csv')
wr_stats <- wr_stats[,-c(1,3,4,6)]
wr_stats <- rename(wr_stats, player_name = player)

team_names <- read.csv('coach and pace names.csv')
team_names <-team_names[,c(1,3)]
colnames(team_names) <- c('team','team_name')
team_names <- data.table(team_names)
team_names[, team := stri_trans_general(str = team, 
                                        id = "Latin-ASCII")]
wr_stats <- left_join(wr_stats,team_names)


efficiency <- read.csv('efficiency.csv')
efficiency <- efficiency[,-c(1)]
wr_stats <- left_join(wr_stats,efficiency)


lines_total <- read.csv('imp_totals.csv')
lines_total <- lines_total[,-c(1)]
wr_stats <- left_join(wr_stats,lines_total)
wr_stats$favorite <- ifelse(wr_stats$implied_total > wr_stats$opp_implied_total,1,0)

time_per_play <- read.csv('time_per_play.csv')
time_per_play <- time_per_play[,-c(1)]
wr_stats <- left_join(wr_stats,time_per_play)


time_per_play <- read.csv('time_per_play.csv')
time_per_play <- time_per_play[,-c(1)]
colnames(time_per_play) <- c('year','opp_team','week','opp_tpp','L3_opp_tpp')
wr_stats <- left_join(wr_stats,time_per_play)

team_adv_stats <- read.csv('team_adv_stats.csv')
team_adv_stats <- team_adv_stats[,-c(1)]
wr_stats <- left_join(wr_stats,team_adv_stats)

wr_stats <- wr_stats[complete.cases(wr_stats),]
wr_stats <- wr_stats[!duplicated(wr_stats),]
wr_stats <- wr_stats %>% distinct(team, player_name, week,year, .keep_all=TRUE)

game_info <- read.csv('games_w_coaches1.csv')
game_info <- game_info[,-c(1)]
game_info <- game_info[,c("L3_attempts",                           
                          "L3_ra",                                  "L3_run_perc",                           
                          "L3_pass_perc",                           "L3_opp_pts",                            
                          "L3_pts",                                 "L3_opp_ppg",                            
                          "L3_opp_tpp",                             "L3_ppg",                                
                          "L3_tpp",                                 "L3_off_line_yds",                       
                          "L3_off_second_lvl_yds",                  "L3_off_open_field_yds",                 
                          "L3_off_passing_downs_ppa",               "L3_off_passing_downs_success_rate",     
                          "L3_off_rushing_plays_rate",              "L3_off_rushing_plays_ppa",              
                          "L3_off_rushing_plays_explosiveness",     "L3_off_passing_plays_rate",             
                          "L3_off_passing_plays_success_rate",      "L3_off_passing_plays_explosiveness",    
                          "L3_def_ppa",                             "L3_def_success_rate",                   
                          "L3_def_second_lvl_yds",                  "L3_def_open_field_yds",                 
                          "L3_def_pts_per_opp",                     "L3_def_standard_downs_ppa",             
                          "L3_def_standard_downs_success_rate",     "L3_def_passing_downs_ppa",              
                          "L3_def_passing_downs_explosiveness",     "L3_def_rushing_plays_ppa",              
                          "L3_def_rushing_plays_success_rate",      "L3_def_rushing_plays_explosiveness",    
                          "L3_avg_distance",                        "L3_avg_def_drive_efficiency",           
                          "L4_sos",                                 "opp_L3_opp_run_perc",                   
                          "opp_L3_opp_pass_perc",                   "opp_L3_pts",                            
                          "opp_L3_opp_pos",                         "opp_L3_tpp",                            
                          "opp_L3_off_ppa",                         "opp_L3_off_success_rate",               
                          "opp_L3_off_explosiveness",               "opp_L3_off_power_success",              
                          "opp_L3_off_second_lvl_yds",              "opp_L3_off_standard_downs_ppa",         
                          "opp_L3_off_standard_downs_success_rate", "opp_L3_off_standard_downs_explosiveness",
                          "opp_L3_off_passing_downs_ppa",           "opp_L3_off_passing_downs_explosiveness",
                          "opp_L3_off_rushing_plays_explosiveness", "opp_L3_off_passing_plays_ppa",          
                          "opp_L3_off_passing_plays_success_rate",  "opp_L3_def_line_yds",                   
                          "opp_L3_def_second_lvl_yds",              "opp_L3_def_open_field_yds",             
                          "opp_L3_def_rushing_plays_rate",          "opp_L3_def_passing_plays_rate",         
                          "opp_L3_avg_drive_efficiency",            "opp_L3_avg_distance",                   
                          "opp_L3_avg_def_distance",                "L4_opp_sos",                            
                          "favorite"       )]

game_info_predict <- predict(xgboost_p_att_model,game_info)
game_info <- read.csv('games_w_coaches1.csv')
game_info <- game_info[,-c(1)]
game_info$est_pa <- game_info_predict

game_info <- game_info[,c(2,16,17,328)]
game_info <- rename(game_info, year = season)

wr_stats <- left_join(wr_stats,game_info)
wr_stats <- wr_stats[complete.cases(wr_stats),]

wr_stats <- wr_stats[!duplicated(wr_stats),]
wr_stats <- wr_stats %>% distinct(team, player_name, week,year, .keep_all=TRUE)



wr_stats <- left_join(wr_stats,wr_stats)
wr_stats <- wr_stats[,c("dk_pts", "string", "L3_rec_usage", "L3_targets", "L3_avg_down", "L3_avg_drive_efficiency",
                        "implied_total", "favorite", "L3_tpp", "est_pa")]

corr <- cor(wr_stats)
corr <- as.data.frame(corr)
corr <- corr %>% filter(!between(dk_pts,-.05,.05))
corr = as.data.frame(t(corr))
corr <- left_join(corr,corr)
view(corr)


library(MLmetrics)
library(caret) 



sum(is.na(wr_stats))

set.seed(100)

TrainingIndex <- createDataPartition(wr_stats$dk_pts, p=0.8, list = FALSE)
TrainingSet <- wr_stats[TrainingIndex,] 
TestingSet <- wr_stats[-TrainingIndex,] 

TrainControl <- trainControl( method = "repeatedcv", number = 10, repeats = 4)


xgboost_wr_stats_model <- train(dk_pts ~ ., data = TrainingSet,
                                method = "xgbTree",
                                na.action = na.omit,
                                preProcess=c("scale","center"),
                                trControl= TrainControl,
                                verbosity = 0
)

xgboost_wr_stats_model.training <-predict(xgboost_wr_stats_model, TrainingSet) 
xgboost_wr_stats_model.testing <-predict(xgboost_wr_stats_model, TestingSet) 


plot(TrainingSet$dk_pts,xgboost_wr_stats_model.training, col = "blue" )
plot(TestingSet$dk_pts,xgboost_wr_stats_model.testing, col = "blue" )

summary(xgboost_wr_stats_model)

xgboost_wr_stats_r.training <- cor(TrainingSet$dk_pts,xgboost_wr_stats_model.training)
xgboost_wr_stats_r.testing <- cor(TestingSet$dk_pts,xgboost_wr_stats_model.testing)

xgboost_wr_stats_r2.training <- xgboost_wr_stats_r.training^2
xgboost_wr_stats_r2.testing <- xgboost_wr_stats_r.testing^2

xgboost_wr_stats_actuals_preds <- data.frame(cbind(actuals=(TestingSet$dk_pts), predicteds=(xgboost_wr_stats_model.testing)))
xgboost_wr_stats_actuals_preds$diff <- (xgboost_wr_stats_actuals_preds$actuals - xgboost_wr_stats_actuals_preds$predicteds)

TestingSet <- cbind(TestingSet,xgboost_wr_stats_actuals_preds)

plot(TestingSet$actuals,TestingSet$predicteds, col = "blue" )

xgboost_wr_stats_min_max_accuracy <- mean(apply(xgboost_wr_stats_actuals_preds, 1, min) / apply(xgboost_wr_stats_actuals_preds, 1, max))  
xgboost_wr_stats_mape <- MAPE(xgboost_wr_stats_actuals_preds$predicteds, xgboost_wr_stats_actuals_preds$actuals)
xgboost_wr_stats_RMSE <- sqrt(mean((TestingSet$actuals - TestingSet$predicteds)^2))
xgboost_wr_stats_MAE <- mean(abs(TestingSet$actuals - TestingSet$predicteds))

TestingSet <- left_join(TestingSet, wr_stats)
saveRDS(xgboost_wr_stats_model, "xgboost_wr_stats_model.rds")

comp <- left_join(TestingSet,wr_stats)

sd(xgboost_wr_stats_model.testing)



