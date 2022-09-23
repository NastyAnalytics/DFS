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
xgboost_p_att_model_alt <- readRDS("xgboost_p_att_model_alt.rds")


qb_stats <- read.csv('qb_stats.csv')
qb_rushing_stats <- read.csv('qb_rushing_stats.csv')
qb_stats <- qb_stats[,-c(1,3,4,6)]
qb_stats <- rename(qb_stats, player_name = player)
qb_rushing_stats <- qb_rushing_stats[,-c(1,3,4,6,8,9,11,15,21:26,29:30,32)]
qb_rushing_stats <- rename(qb_rushing_stats, player_name = player)
qb_rushing_stats <-  rename(qb_rushing_stats, ra = attempts)
qb_rushing_stats <-  rename(qb_rushing_stats, ry = yards)
qb_rushing_stats <-  rename(qb_rushing_stats, rtd = touchdowns)
qb_rushing_stats <-  rename(qb_rushing_stats, rypa = ypa)
qb_rushing_stats <-  rename(qb_rushing_stats, ru_dkpts = dk_pts)
qb_rushing_stats <-  rename(qb_rushing_stats, ru_fdpts = fd_pts)

qb_rushing_stats <-  rename(qb_rushing_stats, L3_ra = L3_attempts)
qb_rushing_stats <-  rename(qb_rushing_stats, L3_ry = L3_yards)
qb_rushing_stats <-  rename(qb_rushing_stats, L3_rtd = L3_touchdowns)
qb_rushing_stats <-  rename(qb_rushing_stats, L3_rypa = L3_ypa)
qb_rushing_stats <-  rename(qb_rushing_stats, L3_ru_dkpts = L3_dk_pts)
qb_rushing_stats <-  rename(qb_rushing_stats, L3_ru_fdpts = L3_fd_pts)

qb_stats <-  rename(qb_stats, pa_dkpts = dk_pts)
qb_stats <-  rename(qb_stats, pa_fdpts = fd_pts)

qb_stats <-  rename(qb_stats, L3_pa_dkpts = L3_dk_pts)
qb_stats <-  rename(qb_stats, L3_pa_fdpts = L3_fd_pts)

qb_stats <- left_join(qb_stats,qb_rushing_stats)
qb_stats <- qb_stats %>%                                      
  arrange(desc(attempts)) %>% 
  group_by(team_name,week,year) %>%
  slice(1:4)

qb_stats <- qb_stats %>%
  group_by(team_name,week,year) %>%
  mutate(string = order(attempts, decreasing=TRUE))  

qb_stats$string <- ifelse(qb_stats$string != 1, 0, 1)

qb_stats$opportunities <- qb_stats$ra + qb_stats$attempts
qb_stats$dk_pts <- qb_stats$pa_dkpts + qb_stats$ru_dkpts 
qb_stats$fd_pts <- qb_stats$pa_fdpts + qb_stats$ru_fdpts 
qb_stats$L3_dk_pts <- qb_stats$L3_pa_dkpts + qb_stats$L3_ru_dkpts 
qb_stats$L3_fd_pts <- qb_stats$L3_pa_fdpts + qb_stats$L3_ru_fdpts 
qb_stats <- qb_stats[complete.cases(qb_stats),]


efficiency <- read.csv('efficiency.csv')
efficiency <- efficiency[,-c(1)]
qb_stats <- left_join(qb_stats,efficiency)


lines_total <- read.csv('imp_totals1.csv')
lines_total <- lines_total[,-c(1)]
qb_stats <- left_join(qb_stats,lines_total)
qb_stats$favorite <- ifelse(qb_stats$implied_total > qb_stats$opp_implied_total,1,0)

time_per_play <- read.csv('time_per_play.csv')
time_per_play <- time_per_play[,-c(1)]
qb_stats <- left_join(qb_stats,time_per_play)

time_per_play <- read.csv('time_per_play.csv')
time_per_play <- time_per_play[,-c(1)]
colnames(time_per_play) <- c('year','opp_team','week','opp_tpp','L3_opp_tpp')
qb_stats <- left_join(qb_stats,time_per_play)


team_adv_stats <- read.csv('team_adv_stats.csv')
team_adv_stats <- team_adv_stats[,-c(1)]
qb_stats <- left_join(qb_stats,team_adv_stats)

qb_stats <- qb_stats[complete.cases(qb_stats),]
qb_stats <- qb_stats[!duplicated(qb_stats),]
qb_stats <- qb_stats %>% distinct(team, player_name, week,year, .keep_all=TRUE)

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

game_info_predict <- predict(xgboost_p_att_model_alt,game_info)
game_info <- read.csv('games_w_coaches1.csv')
game_info <- game_info[,-c(1)]
game_info$est_pa <- game_info_predict

game_info <- game_info[,c(3,17,18,319)]
game_info <- rename(game_info, year = season)
game_info <- game_info[complete.cases(qb_stats),]
game_info <- game_info[!duplicated(qb_stats),]
game_info <- game_info %>% distinct(team, week,year, .keep_all=TRUE)

qb_stats <- left_join(qb_stats,game_info)
qb_stats$est_pa <- qb_stats$est_pa * qb_stats$string


qb_stats <- qb_stats[complete.cases(qb_stats),]
qb_stats <- qb_stats[!duplicated(qb_stats),]
qb_stats <- qb_stats %>% distinct(team, player_name, week,year, .keep_all=TRUE)

game_info <- read.csv('games_w_coaches1.csv')
game_info <- game_info[,-c(1)]
game_info <- game_info[,c( "L3_attempts",                          
                           "L3_ra",                                 "L3_gm_pos",                            
                           "L3_run_perc",                           "L3_pass_perc",                         
                           "L3_ppg",                                "L3_tpp",                               
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
                           "opp_L3_avg_def_distance" )]

game_info_predict <- predict(xgboost_r_att_model_alt,game_info)
game_info <- read.csv('games_w_coaches1.csv')
game_info <- game_info[,-c(1)]
game_info$est_ra <- game_info_predict

game_info <- game_info[,c(3,17,18,319)]
game_info <- rename(game_info, year = season)
game_info <- game_info[complete.cases(qb_stats),]
game_info <- game_info[!duplicated(qb_stats),]
game_info <- game_info %>% distinct(team, week,year, .keep_all=TRUE)

qb_stats <- left_join(qb_stats,game_info)
qb_stats$est_ra <- qb_stats$est_ra * qb_stats$string

qb_stats <- qb_stats[complete.cases(qb_stats),]
qb_stats <- qb_stats[!duplicated(qb_stats),]
qb_stats <- qb_stats %>% distinct(team, player_name, week,year, .keep_all=TRUE)

qb_runshare <- read.csv('qb_shares.csv')
qb_runshare <- qb_runshare[,c("L3_runshare",                         
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
                           "L3_def_passing_plays_ppa",             "L3_def_passing_plays_success_rate"     )]

qb_runshare_predict <- predict(xgboost_qb_share_model,qb_runshare)
qb_runshare <- read.csv('qb_shares.csv')
qb_runshare <- qb_runshare[,-c(1)]
qb_runshare$est_rshare <- qb_runshare_predict

qb_runshare <- qb_runshare[,c(1,4,11,18,64)]
qb_runshare <- rename(qb_runshare, player_name = player)

qb_stats <- left_join(qb_stats,qb_runshare)
qb_stats$est_rshare <- qb_stats$est_rshare * qb_stats$string
qb_stats$est_ra <- qb_stats$est_ra * qb_stats$est_rshare


qb_stats <- qb_stats[complete.cases(qb_stats),]
qb_stats <- qb_stats[!duplicated(qb_stats),]
qb_stats <- qb_stats %>% distinct(team, player_name, week,year, .keep_all=TRUE)



qb_stats <- left_join(qb_stats,qb_stats)
qb_stats <- qb_stats[,c("dk_pts", "string", "L3_attempts", "L3_completion_percent",
                        "L3_completions", "L3_first_downs", "L3_qb_rating", "L3_sacks", "L3_touchdowns",
                        "L3_yards", "L3_ypa", "L3_pa_dkpts", "L3_pa_fdpts", "L3_longest", "L3_rtd",
                        "L3_ry", "L3_rypa", "L3_ru_dkpts", "L3_ru_fdpts", "L3_dk_pts", "L3_avg_down",
                        "L3_avg_distance", "L3_avg_def_distance", "L3_avg_drive_efficiency",
                        "implied_total", "opp_implied_total", "favorite", "L3_tpp", "L3_def_ppa",
                        "L3_def_success_rate", "L3_def_explosiveness", "L3_def_stuff_rate",
                        "L3_def_line_yds", "L3_def_second_lvl_yds", "L3_def_open_field_yds",
                        "L3_def_pts_per_opp", "L3def_field_pos_avg_predicted_points",
                        "L3_def_standard_downs_ppa", "L3_def_standard_downs_success_rate",
                        "L3_def_standard_downs_explosiveness", "L3_def_passing_downs_ppa",
                        "L3_def_passing_downs_success_rate", "L3_def_rushing_plays_rate",
                        "L3_def_rushing_plays_ppa", "L3_def_rushing_plays_success_rate",
                        "L3_def_rushing_plays_explosiveness", "L3_def_passing_plays_rate",
                        "L3_def_passing_plays_ppa", "L3_def_passing_plays_success_rate",
                        "L3_def_passing_plays_explosiveness", "est_pa", "est_ra")]
qb_stats <- qb_stats[complete.cases(qb_stats),]
qb_stats <- qb_stats[!duplicated(qb_stats),]

corr <- cor(qb_stats)
corr <- as.data.frame(corr)
corr <- corr %>% filter(!between(dk_pts,-.05,.05))
corr <- t(corr)
corr <- as.data.frame(corr)
corr <- left_join(corr,corr)
view(corr)




library(MLmetrics)
library(caret) 



sum(is.na(qb_stats))

set.seed(100)

TrainingIndex <- createDataPartition(qb_stats$dk_pts, p=0.8, list = FALSE)
TrainingSet <- qb_stats[TrainingIndex,] 
TestingSet <- qb_stats[-TrainingIndex,] 

TrainControl <- trainControl( method = "repeatedcv", number = 10, repeats = 4)


xgboost_qb_stats2_model <- train(dk_pts ~ ., data = TrainingSet,
                             method = "xgbTree",
                             na.action = na.omit,
                             preProcess=c("scale","center"),
                             trControl= TrainControl,
                             verbosity = 0
)

xgboost_qb_stats2_model.training <-predict(xgboost_qb_stats2_model, TrainingSet) 
xgboost_qb_stats2_model.testing <-predict(xgboost_qb_stats2_model, TestingSet) 


plot(TrainingSet$dk_pts,xgboost_qb_stats2_model.training, col = "blue" )
plot(TestingSet$dk_pts,xgboost_qb_stats2_model.testing, col = "blue" )

summary(xgboost_qb_stats2_model)

xgboost_qb_stats2_r.training <- cor(TrainingSet$dk_pts,xgboost_qb_stats2_model.training)
xgboost_qb_stats2_r.testing <- cor(TestingSet$dk_pts,xgboost_qb_stats2_model.testing)

xgboost_qb_stats2_r2.training <- xgboost_qb_stats2_r.training^2
xgboost_qb_stats2_r2.testing <- xgboost_qb_stats2_r.testing^2

xgboost_qb_stats2_actuals_preds <- data.frame(cbind(actuals=(TestingSet$dk_pts), predicteds=(xgboost_qb_stats2_model.testing)))
xgboost_qb_stats2_actuals_preds$diff <- (xgboost_qb_stats2_actuals_preds$actuals - xgboost_qb_stats2_actuals_preds$predicteds)

TestingSet <- cbind(TestingSet,xgboost_qb_stats2_actuals_preds)

plot(TestingSet$actuals,TestingSet$predicteds, col = "blue" )

xgboost_qb_stats2_min_max_accuracy <- mean(apply(xgboost_qb_stats2_actuals_preds, 1, min) / apply(xgboost_qb_stats2_actuals_preds, 1, max))  
xgboost_qb_stats2_mape <- MAPE(xgboost_qb_stats2_actuals_preds$predicteds, xgboost_qb_stats2_actuals_preds$actuals)
xgboost_qb_stats2_RMSE <- sqrt(mean((TestingSet$actuals - TestingSet$predicteds)^2))
xgboost_qb_stats2_MAE <- mean(abs(TestingSet$actuals - TestingSet$predicteds))

TestingSet <- left_join(TestingSet, qb_stats2)
saveRDS(xgboost_qb_stats2_model, "xgboost_qb_stats2_model.rds")

comp <- left_join(TestingSet,qb_stats2)

sd(xgboost_qb_stats2_model.testing)
