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

xgboost_rb_share_model <- readRDS("xgboost_rb_share_model.rds")
xgboost_rb_usage_model <- readRDS("xgboost_rb_usage_model.rds")


rb_stats <- read.csv('rb_stats.csv')
rb_stats <- rb_stats[,-c(1)]
rb_stats <- rb_stats[,c(1,4,6:19,26:38)]
rb_stats <- rename(rb_stats, player_name = player)
rb_rec_stats <- read.csv('rb_rec_stats.csv')
rb_rec_stats <- rb_rec_stats[,c(2,5,17,18,24,25,27)]
rb_rec_stats <- rename(rb_rec_stats, player_name = player)
rb_rec_stats <-  rename(rb_rec_stats, re_dkpts = dk_pts)
rb_rec_stats <-  rename(rb_rec_stats, re_fdpts = fd_pts)
rb_stats <-  rename(rb_stats, ru_dkpts = dk_pts)
rb_stats <-  rename(rb_stats, ru_fdpts = fd_pts)
rb_stats <- left_join(rb_stats,rb_rec_stats)
rb_stats$opportunities <- rb_stats$receptions + rb_stats$attempts
rb_stats$dk_pts <- rb_stats$ru_dkpts + rb_stats$re_dkpts 
rb_stats$fd_pts <- rb_stats$ru_fdpts + rb_stats$re_fdpts 

team_names <- read.csv('coach and pace names.csv')
team_names <-team_names[,c(1,3)]
colnames(team_names) <- c('team','team_name')
team_names <- data.table(team_names)
team_names[, team := stri_trans_general(str = team, 
                                     id = "Latin-ASCII")]
rb_stats <- left_join(team_names,rb_stats)

efficiency <- read.csv('efficiency.csv')
efficiency <- efficiency[,-c(1)]
rb_stats <- left_join(rb_stats,efficiency)


lines_total <- read.csv('imp_totals1.csv')
lines_total <- lines_total[,-c(1)]
rb_stats <- left_join(rb_stats,lines_total)
rb_stats$favorite <- ifelse(rb_stats$implied_total > rb_stats$opp_implied_total,1,0)

time_per_play <- read.csv('time_per_play.csv')
time_per_play <- time_per_play[,-c(1)]
rb_stats <- left_join(rb_stats,time_per_play)


time_per_play <- read.csv('time_per_play.csv')
time_per_play <- time_per_play[,-c(1)]
colnames(time_per_play) <- c('year','opp_team','week','opp_tpp','L3_opp_tpp')
rb_stats <- left_join(rb_stats,time_per_play)

team_adv_stats <- read.csv('team_adv_stats.csv')
team_adv_stats <- team_adv_stats[,-c(1)]
rb_stats <- left_join(rb_stats,team_adv_stats)

rb_stats <- rb_stats[complete.cases(rb_stats),]
rb_stats <- rb_stats[!duplicated(rb_stats),]
rb_stats <- rb_stats %>% distinct(team, player_name, week,year, .keep_all=TRUE)


game_info <- read.csv('games_w_coaches1.csv')
game_info <- game_info[,-c(1)]
game_info <- game_info[,c("L3_attempts", "L3_ra", "L3_run_perc", "L3_pass_perc", "L3_opp_pts", "L3_pts", "L3_tpp",
                          "L3_off_line_yds", "L3_off_second_lvl_yds", "L3_off_open_field_yds", "L3_off_passing_downs_ppa",
                          "L3_off_passing_downs_success_rate", "L3_off_rushing_plays_rate", "L3_off_rushing_plays_ppa",
                          "L3_off_rushing_plays_explosiveness", "L3_off_passing_plays_rate", "L3_off_passing_plays_success_rate",
                          "L3_off_passing_plays_explosiveness", "L3_def_open_field_yds", "L3_avg_distance", "opp_L3_opp_run_perc",
                          "opp_L3_opp_pass_perc", "opp_L3_pts", "opp_L3_tpp", "opp_L3_off_ppa", "opp_L3_off_success_rate",
                          "opp_L3_off_explosiveness", "opp_L3_off_standard_downs_ppa", "opp_L3_off_standard_downs_success_rate",
                          "opp_L3_off_standard_downs_explosiveness", "opp_L3_off_passing_plays_ppa", "opp_L3_off_passing_plays_success_rate",
                          "opp_L3_def_line_yds", "opp_L3_def_rushing_plays_rate", "opp_L3_def_passing_plays_rate", "opp_L3_avg_def_distance",
                          "favorite" )]

game_info_predict <- predict(xgboost_p_att_model,game_info)
game_info <- read.csv('games_w_coaches1.csv')
game_info <- game_info[,-c(1)]
game_info$est_pa <- game_info_predict

game_info <- game_info[,c(3,17,18,319)]
game_info <- rename(game_info, year = season)

rb_stats <- left_join(rb_stats,game_info)
rb_stats <- rb_stats %>% distinct(team, player_name, week,year, .keep_all=TRUE)


game_info <- read.csv('games_w_coaches1.csv')
game_info <- game_info[,-c(1)]
game_info <- game_info[,c("L3_attempts",                          
                          "L3_ra",                        
                          "L3_run_perc",                           "L3_pass_perc",    
                          "L3_tpp",                               
                          "L3_off_success_rate",                  
                          "L3_off_power_success",                  "L3_off_stuff_rate",                    
                          "L3_off_line_yds",                       "L3_off_second_lvl_yds",                
                          "L3_off_open_field_yds",                 "L3_off_standard_downs_rate",           
                          "L3_off_standard_downs_ppa",             "L3_off_standard_downs_success_rate",   
                          "L3_off_passing_downs_rate",             "L3_off_rushing_plays_rate",            
                          "L3_off_rushing_plays_ppa",              "L3_off_rushing_plays_success_rate",    
                          "L3_off_rushing_plays_explosiveness",    "L3_off_passing_plays_rate",            
                          "L3_off_passing_plays_explosiveness",
                          "L3_avg_distance",                       "opp_L3_attempts",                      
                          "opp_L3_ra",                             "opp_L3_run_perc",                      
                          "opp_L3_pass_perc",                      "opp_L3_opp_run_perc",                  
                          "opp_L3_opp_pass_perc",                  "opp_L3_opp_pts",                       
                          "opp_L3_pts",                            "opp_L3_tpp",                           
                          "opp_L3_off_ppa",                        "opp_L3_off_success_rate",              
                          "opp_L3_off_standard_downs_rate",        "opp_L3_off_standard_downs_ppa",        
                          "opp_L3_off_standard_downs_success_rate","opp_L3_off_passing_downs_rate",        
                          "opp_L3_off_passing_downs_ppa",          "opp_L3_off_rushing_plays_rate",        
                          "opp_L3_off_passing_plays_rate",        
                          "opp_L3_off_passing_plays_ppa",          "opp_L3_off_passing_plays_success_rate",
                          "opp_L3_off_passing_plays_explosiveness","opp_L3_def_ppa",                       
                          "opp_L3_def_success_rate",               "opp_L3_def_line_yds",                  
                          "opp_L3_def_second_lvl_yds",             "opp_L3_def_standard_downs_rate",       
                          "opp_L3_def_standard_downs_ppa",         "opp_L3_def_standard_downs_success_rate",
                          "opp_L3_def_passing_downs_rate",         "opp_L3_def_passing_downs_success_rate",
                          "opp_L3_def_rushing_plays_rate",         "opp_L3_def_rushing_plays_ppa",         
                          "opp_L3_def_passing_plays_rate",       
                          "opp_L3_avg_def_drive_efficiency",       "opp_L3_avg_def_down",                  
                          "opp_L3_avg_def_distance"   )]


game_info_predict <- predict(xgboost_r_att_model,game_info)
game_info <- read.csv('games_w_coaches1.csv')
game_info <- game_info[,-c(1)]
game_info$est_ra <- game_info_predict

game_info <- game_info[,c(3,17,18,319)]
game_info <- rename(game_info, year = season)

rb_stats <- left_join(rb_stats,game_info)

rb_stats$est_rshare <- predict(xgboost_rb_share_model, rb_stats)
rb_stats$est_ra <- rb_stats$est_ra * rb_stats$est_rshare

rb_stats$est_rusage <- predict(xgboost_rb_usage_model, rb_stats)
rb_stats$est_re <- rb_stats$est_pa * rb_stats$est_rusage
rb_stats <- rb_stats %>% distinct(team, player_name, week,year, .keep_all=TRUE)




rb_stats <- rb_stats[complete.cases(rb_stats),]
rb_stats <- rb_stats[!duplicated(rb_stats),]



rb_stats <- left_join(rb_stats,rb_stats)
rb_stats <- rb_stats[,c("dk_pts", "string", "L3_attempts", "L3_first_downs", "L3_fumbles",
                        "L3_longest", "L3_touchdowns", "L3_yards", "L3_ypa", "L3_dk_pts", "L3_runshare",
                        "L3_avg_down", "L3_avg_def_distance", "L3_avg_drive_efficiency", "implied_total",
                        "opp_implied_total", "favorite", "L3_def_success_rate", "L3_def_stuff_rate",
                        "L3_def_line_yds", "L3_def_second_lvl_yds", "L3_def_standard_downs_rate",
                        "L3_def_passing_downs_rate", "L3_def_passing_downs_ppa",
                        "L3_def_passing_downs_success_rate", "L3_def_rushing_plays_rate",
                        "L3_def_rushing_plays_ppa", "L3_def_rushing_plays_success_rate",
                        "L3_def_passing_plays_rate", "est_ra", "est_re")]

corr <- cor(rb_stats)
corr <- as.data.frame(corr)
corr <- corr %>% filter(!between(dk_pts,-.05,.05))
corr = as.data.frame(t(corr))
corr <- left_join(corr,corr)
view(corr)


library(MLmetrics)
library(caret) 



sum(is.na(rb_stats))

set.seed(100)

TrainingIndex <- createDataPartition(rb_stats$dk_pts, p=0.8, list = FALSE)
TrainingSet <- rb_stats[TrainingIndex,] 
TestingSet <- rb_stats[-TrainingIndex,] 

TrainControl <- trainControl( method = "repeatedcv", number = 10, repeats = 4)


xgboost_rb_stats2_model <- train(dk_pts ~ ., data = TrainingSet,
                                method = "xgbTree",
                                na.action = na.omit,
                                preProcess=c("scale","center"),
                                trControl= TrainControl,
                                verbosity = 0
)

xgboost_rb_stats2_model.training <-predict(xgboost_rb_stats2_model, TrainingSet) 
xgboost_rb_stats2_model.testing <-predict(xgboost_rb_stats2_model, TestingSet) 


plot(TrainingSet$dk_pts,xgboost_rb_stats2_model.training, col = "blue" )
plot(TestingSet$dk_pts,xgboost_rb_stats2_model.testing, col = "blue" )

summary(xgboost_rb_stats2_model)

xgboost_rb_stats2_r.training <- cor(TrainingSet$dk_pts,xgboost_rb_stats2_model.training)
xgboost_rb_stats2_r.testing <- cor(TestingSet$dk_pts,xgboost_rb_stats2_model.testing)

xgboost_rb_stats2_r2.training <- xgboost_rb_stats2_r.training^2
xgboost_rb_stats2_r2.testing <- xgboost_rb_stats2_r.testing^2

xgboost_rb_stats2_actuals_preds <- data.frame(cbind(actuals=(TestingSet$dk_pts), predicteds=(xgboost_rb_stats2_model.testing)))
xgboost_rb_stats2_actuals_preds$diff <- (xgboost_rb_stats2_actuals_preds$actuals - xgboost_rb_stats2_actuals_preds$predicteds)

TestingSet <- cbind(TestingSet,xgboost_rb_stats2_actuals_preds)

plot(TestingSet$actuals,TestingSet$predicteds, col = "blue" )

xgboost_rb_stats2_min_max_accuracy <- mean(apply(xgboost_rb_stats2_actuals_preds, 1, min) / apply(xgboost_rb_stats2_actuals_preds, 1, max))  
xgboost_rb_stats2_mape <- MAPE(xgboost_rb_stats2_actuals_preds$predicteds, xgboost_rb_stats2_actuals_preds$actuals)
xgboost_rb_stats2_RMSE <- sqrt(mean((TestingSet$actuals - TestingSet$predicteds)^2))
xgboost_rb_stats2_MAE <- mean(abs(TestingSet$actuals - TestingSet$predicteds))

TestingSet <- left_join(TestingSet, rb_stats2)
saveRDS(xgboost_rb_stats1_model, "xgboost_rb_stats1_model.rds")

comp <- left_join(TestingSet,rb_stats2)

sd(xgboost_rb_stats2_model.testing)















