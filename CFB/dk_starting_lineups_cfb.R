library('RSelenium')
library('rvest')
library('tidyverse')
library('httr')
library('data.table')
library('stringi')
library('zoo')
library('cfbfastR')


setwd("~/Documents/CFB")
xgboost_p_att_model <- readRDS("xgboost_p_att_model.rds")
xgboost_r_att_model <- readRDS("xgboost_r_att_model.rds")
xgboost_qb_share_model <- readRDS("xgboost_qb_share_model.rds")
xgboost_qb_stats_model <- readRDS("xgboost_qb_stats2_model.rds")
xgboost_rb_share_model <- readRDS("xgboost_rb_share_model.rds")
xgboost_rb_usage_model <- readRDS("xgboost_rb_usage_model.rds")
xgboost_rb_stats_model <- readRDS("xgboost_rb_stats2_model.rds")
xgboost_wr_share_model <- readRDS("xgboost_wr_share_model.rds")
xgboost_wr_stats_model <- readRDS("xgboost_wr_stats_2_model.rds")


current_slate <- read.csv("DKSalaries.csv")
current_slate <- separate(data = current_slate, col = Game.Info, into = c("away", "right"), sep = "\\@")
current_slate <- separate(data = current_slate, col = right, into = c("home", "date","time","pm"), sep = "\\ ")
current_slate$time <- gsub("([0-9])([A-Z])", "\\1 \\2", current_slate$time)
current_slate$start_time <- as.POSIXct(paste0(current_slate$time," ",current_slate$pm),format= "%H:%M")
current_slate$dk_opp_abbr <- if_else(current_slate$TeamAbbrev == current_slate$away,current_slate$home,current_slate$away)
current_slate <- current_slate[,c(1,3,5,6,8,12,15)]
current_slate <- rename(current_slate, player = Name)
current_slate <- rename(current_slate, position = Position)
currentday = Sys.Date()
year = 2022
currentweek = 7

dk_names <- c('Anthony Tyus III', 'Calvin Tyler Jr.','Winston Wright Jr.','Raymond Niro III','Craig Burt Jr.','Nasjzae Bryant-Lelei','George Pettaway','D.J. Jones','Ray Davis','B.J. Casteel','J.J. Jones','Andre Greene Jr.','Walter Dawn Jr.','Devin Boddie Jr.','Quincy Skinner Jr.',
              'Xazavian Valladay','Harrison Wallace III','Jalen Moreno-Cropper')
injury_names <- c('Anthony Tyus','Calvin Tyler','Winston Wright','Raymond Niro','Craig Burt','Nasjzae Bryant','Gregory Pettaway','DJ Jones',"Re'Mahn Davis",'Brian Casteel','JJ Jones','Andre Greene','Walter Dawn','Devin Boddie','Quincy Skinner','X Valladay','Tre Wallace','Jalen Cropper')
clean_names <- data.frame(dk_names,injury_names)

dk_names <- c("De'zhaun Stribling","Geordan Porter","AJ Toney","Tre'Von Bradley","Lonyatta Alexander Jr.","Dea Dea McDougle","Bub Means","Jalen Moreno-Cropper")
pff_names <- c("De'Zhaun Stribling","Geordon Porter","A.J. Toney","Tre'von Bradley","Lonyatta Alexander","DeaJaun McDougle","Jerrod Means","Jalen Cropper")
clean_names1 <- data.frame(dk_names, pff_names)


rD <- rsDriver(browser = c("firefox"),check=F)

driver <- rD[["client"]]

driver$open()


#set URL
url <- paste0("https://www.boydsbets.com/college-football-injuries/")

# navigate to an URL
driver$navigate(url)
Sys.sleep(1)

#Getting Nodes
html <- driver$getPageSource()[[1]]
team_name <- read_html(html) %>%
  html_nodes("td:nth-child(1)") %>% 
  html_text()
team_name <- as.data.frame(team_name)

player_name <- read_html(html) %>%
  html_nodes("td:nth-child(2)") %>% 
  html_text()
player_name <- as.data.frame(player_name)

pos <- read_html(html) %>%
  html_nodes("td:nth-child(3)") %>% 
  html_text()
pos <- as.data.frame(pos)

status <- read_html(html) %>%
  html_nodes("td:nth-child(4)") %>% 
  html_text()
status <- as.data.frame(status)

date_reported <- read_html(html) %>%
  html_nodes("td:nth-child(5)") %>% 
  html_text()
date_reported <- as.data.frame(date_reported)

notes <- read_html(html) %>%
  html_nodes("td:nth-child(6)") %>% 
  html_text()
notes <- as.data.frame(notes)

#Combining Nodes
injuries <- cbind(team_name,player_name,pos,status,date_reported,notes)
injuries <- injuries[-c(1),]
#close the driver
driver$close()

#close the server
rD[["server"]]$stop()

out_players <- injuries[!grepl('Ques',injuries$status),]
out_players <- out_players[!grepl('Prob',out_players$status),]
out_players <- rename(out_players, player = player_name)
out_players <- rename(out_players, team = team_name)


setwd("~/Documents/CFB/receivers_targets")

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

receiving_summary_2022_week0<- read.csv('receiving_summary_22_0.csv')
receiving_summary_2022_week0$week <- 0
receiving_summary_2022_week0$year <- 2022

receiving_summary_2022_week1<- read.csv('receiving_summary_22_1.csv')
receiving_summary_2022_week1$week <- 1
receiving_summary_2022_week1$year <- 2022

receiving_summary_2022_week2<- read.csv('receiving_summary_22_2.csv')
receiving_summary_2022_week2$week <- 2
receiving_summary_2022_week2$year <- 2022

receiving_summary_2022_week3<- read.csv('receiving_summary_22_3.csv')
receiving_summary_2022_week3$week <- 3
receiving_summary_2022_week3$year <- 2022

receiving_summary_2022_week4<- read.csv('receiving_summary_22_4.csv')
receiving_summary_2022_week4$week <- 4
receiving_summary_2022_week4$year <- 2022

receiving_summary_2022_week5<- read.csv('receiving_summary_22_5.csv')
receiving_summary_2022_week5$week <- 5
receiving_summary_2022_week5$year <- 2022

receiving_summary_2022_week6<- read.csv('receiving_summary_22_6.csv')
receiving_summary_2022_week6$week <- 6
receiving_summary_2022_week6$year <- 2022

receiving_summary_total <- rbind(receiving_summary_2021_week1,
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
                                 receiving_summary_2021_week15,
                                 receiving_summary_2022_week0,
                                 receiving_summary_2022_week1,
                                 receiving_summary_2022_week2,
                                 receiving_summary_2022_week3,
                                 receiving_summary_2022_week4,
                                 receiving_summary_2022_week5,
                                 receiving_summary_2022_week6)

team_targets <- aggregate(targets ~ team_name + week + year,data = receiving_summary_total,FUN = sum)
colnames(team_targets) <- c('team_name','week','year','team_targets')
receiving_summary_total <- left_join(receiving_summary_total,team_targets)
receiving_summary_total$rec_usage <- receiving_summary_total$targets/receiving_summary_total$team_targets

team_2021_targets <- aggregate(targets ~ team_name + year,data = receiving_summary_total,FUN = sum)
total_targets <- aggregate(targets ~ player + team_name + year,data = receiving_summary_total,FUN = sum)
colnames(total_targets) <- c('player','team_name','year','season_targets')
colnames(team_2021_targets) <- c('team_name','year','season_team_targets')
receiving_summary_total <- left_join(receiving_summary_total,team_2021_targets)
receiving_summary_total <- left_join(receiving_summary_total,total_targets)
receiving_summary_total$season_usage <- receiving_summary_total$season_targets/receiving_summary_total$season_team_targets


setwd("~/Documents/CFB/passing_summaries")

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

passing_summary_2022_week0<- read.csv('passing_summary_22_0.csv')
passing_summary_2022_week0$week <- 0
passing_summary_2022_week0$year <- 2022

passing_summary_2022_week1<- read.csv('passing_summary_22_1.csv')
passing_summary_2022_week1$week <- 1
passing_summary_2022_week1$year <- 2022

passing_summary_2022_week2<- read.csv('passing_summary_22_2.csv')
passing_summary_2022_week2$week <- 2
passing_summary_2022_week2$year <- 2022

passing_summary_2022_week3<- read.csv('passing_summary_22_3.csv')
passing_summary_2022_week3$week <- 3
passing_summary_2022_week3$year <- 2022

passing_summary_2022_week4<- read.csv('passing_summary_22_4.csv')
passing_summary_2022_week4$week <- 4
passing_summary_2022_week4$year <- 2022

passing_summary_2022_week5<- read.csv('passing_summary_22_5.csv')
passing_summary_2022_week5$week <- 5
passing_summary_2022_week5$year <- 2022


passing_summary_2022_week6<- read.csv('passing_summary_22_6.csv')
passing_summary_2022_week6$week <- 6
passing_summary_2022_week6$year <- 2022

passing_summary_total <- rbind(passing_summary_2021_week1,
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
                               passing_summary_2021_week15,
                               passing_summary_2022_week0,
                               passing_summary_2022_week1,
                               passing_summary_2022_week2,
                               passing_summary_2022_week3,
                               passing_summary_2022_week4,
                               passing_summary_2022_week5,
                               passing_summary_2022_week6)

setwd("~/Documents/CFB/rushing_summaries")

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

rushing_summary_2022_week0<- read.csv('rushing_summary_22_0.csv')
rushing_summary_2022_week0$week <- 0
rushing_summary_2022_week0$year <- 2022

rushing_summary_2022_week1<- read.csv('rushing_summary_22_1.csv')
rushing_summary_2022_week1$week <- 1
rushing_summary_2022_week1$year <- 2022

rushing_summary_2022_week2<- read.csv('rushing_summary_22_2.csv')
rushing_summary_2022_week2$week <- 2
rushing_summary_2022_week2$year <- 2022

rushing_summary_2022_week3<- read.csv('rushing_summary_22_3.csv')
rushing_summary_2022_week3$week <- 3
rushing_summary_2022_week3$year <- 2022

rushing_summary_2022_week4<- read.csv('rushing_summary_22_4.csv')
rushing_summary_2022_week4$week <- 4
rushing_summary_2022_week4$year <- 2022

rushing_summary_2022_week5<- read.csv('rushing_summary_22_5.csv')
rushing_summary_2022_week5$week <- 5
rushing_summary_2022_week5$year <- 2022

rushing_summary_2022_week6<- read.csv('rushing_summary_22_6.csv')
rushing_summary_2022_week6$week <- 6
rushing_summary_2022_week6$year <- 2022

rushing_summary_total <- rbind(rushing_summary_2021_week1,
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
                               rushing_summary_2021_week15,
                               rushing_summary_2022_week0,
                               rushing_summary_2022_week1,
                               rushing_summary_2022_week2,
                               rushing_summary_2022_week3,
                               rushing_summary_2022_week4,
                               rushing_summary_2022_week5,
                               rushing_summary_2022_week6)

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
avg_runshare <- aggregate(runshare ~ player + team_name + year, data = rushing_summary_total, FUN = mean)
colnames(avg_runshare) <- c('player','team_name','year','avg_runshare')
rushing_summary_total <- left_join(rushing_summary_total,avg_runshare)


setwd("~/Documents/CFB")



passing_summary_total$dk_pts <- ifelse(passing_summary_total$yards >= 300,3 + (4*passing_summary_total$touchdowns) + (.04*passing_summary_total$yards) - (passing_summary_total$interceptions), (4*passing_summary_total$touchdowns) + (.04*passing_summary_total$yards) - (passing_summary_total$interceptions))
passing_summary_total$fd_pts <- (4*passing_summary_total$touchdowns) + (.04*passing_summary_total$yards) - (passing_summary_total$interceptions)


qb_stats <- passing_summary_total %>%
  group_by(team_name,week,year) %>%
  mutate(string = order(attempts, decreasing=TRUE))  


team_names <- read.csv('coach and pace names.csv')
team_names <- team_names[,c(1,3)]
colnames(team_names) <- c('team','team_name')
team_names <- data.table(team_names)
team_names[, team := stri_trans_general(str = team, 
                                        id = "Latin-ASCII")]

qb_stats <- left_join(qb_stats,team_names)

qb_stats1 <- qb_stats %>% 
  group_by(player) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_attempts = rollapplyr(attempts, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))


qb_stats1 <- qb_stats1 %>% 
  group_by(player) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_completion_percent = rollapplyr(completion_percent, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))


qb_stats1 <- qb_stats1 %>% 
  group_by(player) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_completions = rollapplyr(completions, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))


qb_stats1 <- qb_stats1 %>% 
  group_by(player) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_first_downs = rollapplyr(first_downs, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))


qb_stats1 <- qb_stats1 %>% 
  group_by(player) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_interceptions = rollapplyr(interceptions, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))


qb_stats1 <- qb_stats1 %>% 
  group_by(player) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_qb_rating = rollapplyr(qb_rating, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))


qb_stats1 <- qb_stats1 %>% 
  group_by(player) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_sacks = rollapplyr(sacks, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))


qb_stats1 <- qb_stats1 %>% 
  group_by(player) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_touchdowns = rollapplyr(touchdowns, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

qb_stats1 <- qb_stats1 %>% 
  group_by(player) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_yards = rollapplyr(yards, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))


qb_stats1 <- qb_stats1 %>% 
  group_by(player) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_ypa = rollapplyr(ypa, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

qb_stats1 <- qb_stats1 %>% 
  group_by(player) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_dk_pts = rollapplyr(dk_pts, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

qb_stats1 <- qb_stats1 %>% 
  group_by(player) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_fd_pts = rollapplyr(fd_pts, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

qb_stats1 <-  rename(qb_stats1, L3_pa_dkpts = L3_dk_pts)
qb_stats1 <-  rename(qb_stats1, L3_pa_fdpts = L3_fd_pts)

qb_stats1 <- qb_stats1 %>%
  group_by(team_name,week,year) %>%
  mutate(pa_string = order(L3_attempts, decreasing=TRUE))  


qb_stats1 <- qb_stats1 %>% 
  group_by(player) %>% 
  arrange(week) %>% 
  arrange(year) %>%
  slice(n())

qb_stats1 <- qb_stats1[,-c(2,3:20,22,29)]

str_stats_pa <- qb_stats %>% 
  group_by(team_name, string) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_attempts = rollapplyr(attempts, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))


str_stats_pa <- str_stats_pa %>% 
  group_by(team_name, string) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_completion_percent = rollapplyr(completion_percent, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))


str_stats_pa <- str_stats_pa %>% 
  group_by(team_name, string) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_completions = rollapplyr(completions, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))


str_stats_pa <- str_stats_pa %>% 
  group_by(team_name, string) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_first_downs = rollapplyr(first_downs, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))


str_stats_pa <- str_stats_pa %>% 
  group_by(team_name, string) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_interceptions = rollapplyr(interceptions, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))


str_stats_pa <- str_stats_pa %>% 
  group_by(team_name, string) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_qb_rating = rollapplyr(qb_rating, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))


str_stats_pa <- str_stats_pa %>% 
  group_by(team_name, string) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_sacks = rollapplyr(sacks, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))


str_stats_pa <- str_stats_pa %>% 
  group_by(team_name, string) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_touchdowns = rollapplyr(touchdowns, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

str_stats_pa <- str_stats_pa %>% 
  group_by(team_name, string) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_yards = rollapplyr(yards, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))


str_stats_pa <- str_stats_pa %>% 
  group_by(team_name, string) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_ypa = rollapplyr(ypa, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

str_stats_pa <- str_stats_pa %>% 
  group_by(team_name, string) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_dk_pts = rollapplyr(dk_pts, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

str_stats_pa <- str_stats_pa %>% 
  group_by(team_name, string) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_fd_pts = rollapplyr(fd_pts, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

str_stats_pa <-  rename(str_stats_pa, L3_pa_dkpts = L3_dk_pts)
str_stats_pa <-  rename(str_stats_pa, L3_pa_fdpts = L3_fd_pts)

str_stats_pa <- str_stats_pa %>%
  group_by(team_name,week,year) %>%
  mutate(pa_string = order(L3_attempts, decreasing=TRUE))  


str_stats_pa1 <- str_stats_pa %>% 
  group_by(team_name,string) %>% 
  arrange(week) %>% 
  arrange(year) %>%
  slice(n())

str_stats_pa1 <- str_stats_pa1[,-c(1:3,5:20,22)]
colnames(str_stats_pa1) <- c("team_name", "string", "str_L3_attempts", "str_L3_completion_percent", "str_L3_completions", "str_L3_first_downs",
                             "str_L3_interceptions", "str_L3_qb_rating", "str_L3_sacks", "str_L3_touchdowns", "str_L3_yards", "str_L3_ypa", "str_L3_pa_dkpts", "str_L3_pa_fdpts",
                             "str_pa_string") 
#RB Stats

rb_runshares <- rushing_summary_total
rb_runshares <- rb_runshares[!duplicated(rb_runshares),]


rb_runshares$dk_pts <- ifelse(rb_runshares$yards >= 100,3 + (6*rb_runshares$touchdowns) + (.1*rb_runshares$yards) - (rb_runshares$fumbles),(6*rb_runshares$touchdowns) + (.1*rb_runshares$yards) - (rb_runshares$fumbles))
rb_runshares$fd_pts <- (6*rb_runshares$touchdowns) + (.1*rb_runshares$yards) - (2*rb_runshares$fumbles)

qb_rushing_stats <- rb_runshares[grepl("QB", rb_runshares$position),]

qb_rushing_stats <- qb_rushing_stats %>%
  group_by(team_name,week,year) %>%
  mutate(string = order(runshare, decreasing=TRUE))  


team_names <- read.csv('coach and pace names.csv')
team_names <- team_names[,c(1,3)]
colnames(team_names) <- c('team','team_name')
team_names <- data.table(team_names)
team_names[, team := stri_trans_general(str = team, 
                                        id = "Latin-ASCII")]

qb_rushing_stats1 <- left_join(qb_rushing_stats,team_names)
qb_rushing_stats1 <- qb_rushing_stats[!duplicated(qb_rushing_stats),]

qb_rushing_stats1 <- qb_rushing_stats1 %>% 
  group_by(player) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_runshare = rollapplyr(runshare, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

qb_rushing_stats1 <- qb_rushing_stats1 %>% 
  group_by(player) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_attempts = rollapplyr(attempts, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

qb_rushing_stats1 <- qb_rushing_stats1 %>% 
  group_by(player) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_first_downs = rollapplyr(first_downs, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

qb_rushing_stats1 <- qb_rushing_stats1 %>% 
  group_by(player) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_fumbles = rollapplyr(fumbles, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

qb_rushing_stats1 <- qb_rushing_stats1 %>% 
  group_by(player) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_longest = rollapplyr(longest, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

qb_rushing_stats1 <- qb_rushing_stats1 %>% 
  group_by(player) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_touchdowns = rollapplyr(touchdowns, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

qb_rushing_stats1 <- qb_rushing_stats1 %>% 
  group_by(player) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_yards = rollapplyr(yards, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

qb_rushing_stats1 <- qb_rushing_stats1 %>% 
  group_by(player) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_ypa = rollapplyr(ypa, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

qb_rushing_stats1 <- qb_rushing_stats1 %>% 
  group_by(player) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_dk_pts = rollapplyr(dk_pts, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

qb_rushing_stats1 <- qb_rushing_stats1 %>% 
  group_by(player) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_fd_pts = rollapplyr(fd_pts, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))


qb_rushing_stats1 <-  rename(qb_rushing_stats1, L3_rush_att = L3_attempts)
qb_rushing_stats1 <-  rename(qb_rushing_stats1, L3_ry = L3_yards)
qb_rushing_stats1 <-  rename(qb_rushing_stats1, L3_rtd = L3_touchdowns)
qb_rushing_stats1 <-  rename(qb_rushing_stats1, L3_rypa = L3_ypa)
qb_rushing_stats1 <-  rename(qb_rushing_stats1, L3_ru_dkpts = L3_dk_pts)
qb_rushing_stats1 <-  rename(qb_rushing_stats1, L3_ru_fdpts = L3_fd_pts)

qb_rushing_stats1 <- qb_rushing_stats1 %>% 
  group_by(player) %>% 
  arrange(week) %>% 
  arrange(year) %>%
  slice(n())

qb_rushing_stats1 <- qb_rushing_stats1[,-c(2,3:28,31:32)]


str_stats_qb_ru <- qb_rushing_stats %>% 
  group_by(player) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_runshare = rollapplyr(runshare, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

str_stats_qb_ru <- str_stats_qb_ru %>% 
  group_by(team_name, string) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_attempts = rollapplyr(attempts, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

str_stats_qb_ru <- str_stats_qb_ru %>% 
  group_by(team_name, string) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_first_downs = rollapplyr(first_downs, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

str_stats_qb_ru <- str_stats_qb_ru %>% 
  group_by(team_name, string) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_fumbles = rollapplyr(fumbles, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

str_stats_qb_ru <- str_stats_qb_ru %>% 
  group_by(team_name, string) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_longest = rollapplyr(longest, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

str_stats_qb_ru <- str_stats_qb_ru %>% 
  group_by(team_name, string) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_touchdowns = rollapplyr(touchdowns, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

str_stats_qb_ru <- str_stats_qb_ru %>% 
  group_by(team_name, string) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_yards = rollapplyr(yards, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

str_stats_qb_ru <- str_stats_qb_ru %>% 
  group_by(team_name, string) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_ypa = rollapplyr(ypa, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

str_stats_qb_ru <- str_stats_qb_ru %>% 
  group_by(team_name, string) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_dk_pts = rollapplyr(dk_pts, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

str_stats_qb_ru <- str_stats_qb_ru %>% 
  group_by(team_name, string) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_fd_pts = rollapplyr(fd_pts, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))


str_stats_qb_ru <-  rename(str_stats_qb_ru, L3_rush_att = L3_attempts)
str_stats_qb_ru <-  rename(str_stats_qb_ru, L3_ry = L3_yards)
str_stats_qb_ru <-  rename(str_stats_qb_ru, L3_rtd = L3_touchdowns)
str_stats_qb_ru <-  rename(str_stats_qb_ru, L3_rypa = L3_ypa)
str_stats_qb_ru <-  rename(str_stats_qb_ru, L3_ru_dkpts = L3_dk_pts)
str_stats_qb_ru <-  rename(str_stats_qb_ru, L3_ru_fdpts = L3_fd_pts)

str_stats_qb_ru1 <- str_stats_qb_ru %>% 
  group_by(team_name,string) %>% 
  arrange(week) %>% 
  arrange(year) %>%
  slice(n())

str_stats_qb_ru1 <- str_stats_qb_ru1[,-c(1:3,5:27,31:32)]
colnames(str_stats_qb_ru1) <-  c("team_name", "string", "str_L3_runshare", "str_L3_rush_att", "str_L3_longest","str_L3_rtd", "str_L3_ry", "str_L3_rypa", "str_L3_ru_dkpts",
                                 "str_L3_ru_fdpts") 

rb_runshares <- rb_runshares[!grepl("QB", rb_runshares$position),]

rb_runshares <- rb_runshares %>%
  group_by(team_name,week,year) %>%
  mutate(string = order(runshare, decreasing=TRUE))  


rb_runshares <- rb_runshares[!duplicated(rb_runshares),]


rb_runshares1 <- rb_runshares %>% 
  group_by(player) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_attempts = rollapplyr(attempts, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

rb_runshares1 <- rb_runshares1 %>% 
  group_by(player) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_first_downs = rollapplyr(first_downs, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

rb_runshares1 <- rb_runshares1 %>% 
  group_by(player) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_fumbles = rollapplyr(fumbles, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

rb_runshares1 <- rb_runshares1 %>% 
  group_by(player) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_longest = rollapplyr(longest, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

rb_runshares1 <- rb_runshares1 %>% 
  group_by(player) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_touchdowns = rollapplyr(touchdowns, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

rb_runshares1 <- rb_runshares1 %>% 
  group_by(player) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_yards = rollapplyr(yards, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

rb_runshares1 <- rb_runshares1 %>% 
  group_by(player) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_ypa = rollapplyr(ypa, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

rb_runshares1 <- rb_runshares1 %>% 
  group_by(player) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_dk_pts = rollapplyr(dk_pts, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

rb_runshares1 <- rb_runshares1 %>% 
  group_by(player) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_fd_pts = rollapplyr(fd_pts, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

rb_runshares1 <- rb_runshares1 %>% 
  group_by(team_name, string) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_runshare = rollapplyr(runshare, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

rb_runshares1 <- rb_runshares1 %>%
  group_by(team_name,week,year) %>%
  mutate(ru_string = order(L3_attempts, decreasing=TRUE))  

rb_runshares1 <- rb_runshares1 %>% 
  group_by(player) %>% 
  arrange(week) %>% 
  arrange(year) %>%
  slice(n())

rb_runshares1 <- rb_runshares1[,-c(2,3:27)]

str_stats_ru <- rb_runshares %>% 
  group_by(team_name, string) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_attempts = rollapplyr(attempts, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

str_stats_ru <- str_stats_ru %>% 
  group_by(team_name, string) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_first_downs = rollapplyr(first_downs, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

str_stats_ru <- str_stats_ru %>% 
  group_by(team_name, string) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_fumbles = rollapplyr(fumbles, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

str_stats_ru <- str_stats_ru %>% 
  group_by(team_name, string) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_longest = rollapplyr(longest, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

str_stats_ru <- str_stats_ru %>% 
  group_by(team_name, string) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_touchdowns = rollapplyr(touchdowns, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

str_stats_ru <- str_stats_ru %>% 
  group_by(team_name, string) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_yards = rollapplyr(yards, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

str_stats_ru <- str_stats_ru %>% 
  group_by(team_name, string) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_ypa = rollapplyr(ypa, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

str_stats_ru <- str_stats_ru %>% 
  group_by(team_name, string) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_dk_pts = rollapplyr(dk_pts, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

str_stats_ru <- str_stats_ru %>% 
  group_by(team_name, string) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_fd_pts = rollapplyr(fd_pts, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

str_stats_ru <- str_stats_ru %>% 
  group_by(team_name, string) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_runshare = rollapplyr(runshare, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

str_stats_ru <- str_stats_ru %>%
  group_by(team_name,week,year) %>%
  mutate(ru_string = order(L3_attempts, decreasing=TRUE))  

str_stats_ru1 <- str_stats_ru %>% 
  group_by(team_name,string) %>% 
  arrange(week) %>% 
  arrange(year) %>%
  slice(n())

str_stats_ru1 <- str_stats_ru1[,-c(1:3,5:27)]
colnames(str_stats_ru1) <- c("team_name", "string", "str_L3_attempts", "str_L3_first_downs", "str_L3_fumbles", "str_L3_longest", "str_L3_touchdowns",
                             "str_L3_yards", "str_L3_ypa", "str_L3_dk_pts", "str_L3_fd_pts", "str_L3_runshare", "str_ru_string")

runshares <- str_stats_ru1[,c(1,12,2)]

#WR Stats

team_names <- read.csv('coach and pace names.csv')
team_names <- team_names[,c(1,3)]
colnames(team_names) <- c('team','team_name')
team_names <- data.table(team_names)
team_names[, team := stri_trans_general(str = team, 
                                        id = "Latin-ASCII")]
team_names[, team_name := stri_trans_general(str = team_name, 
                                             id = "Latin-ASCII")]

wr_share <- left_join(receiving_summary_total, team_names)

wr_share <- wr_share[,-c(23)]

wr_share$dk_pts <- ifelse(wr_share$yards >= 100,3 + (.1*wr_share$yards) + (6*wr_share$touchdowns) + (wr_share$receptions) - (wr_share$fumbles),(.1*wr_share$yards) + (6*wr_share$touchdowns) + (wr_share$receptions) - (wr_share$fumbles))
wr_share$fd_pts <- (.1*wr_share$yards) + (6*wr_share$touchdowns) + (0.5*wr_share$receptions) - (2*wr_share$fumbles)


rb_receiving_stats <- wr_share[grepl("HB|FB", wr_share$position),]
rb_receiving_stats1 <- rb_receiving_stats %>% 
  group_by(player) %>% 
  arrange(week) %>% 
  arrange(year) %>%
  mutate(L3_targets = rollapply(targets, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

rb_receiving_stats1 <- rb_receiving_stats1 %>% 
  group_by(player) %>% 
  arrange(week) %>% 
  arrange(year) %>%
  mutate(L3_touchdowns = rollapply(touchdowns, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

rb_receiving_stats1 <- rb_receiving_stats1 %>% 
  group_by(player) %>% 
  arrange(week) %>% 
  arrange(year) %>%
  mutate(L3_yards = rollapply(yards, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

rb_receiving_stats1 <- rb_receiving_stats1 %>% 
  group_by(player) %>% 
  arrange(week) %>% 
  arrange(year) %>%
  mutate(L3_yards_per_reception = rollapply(yards_per_reception, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

rb_receiving_stats1 <- rb_receiving_stats1 %>% 
  group_by(player) %>% 
  arrange(week) %>% 
  arrange(year) %>%
  mutate(L3_dk_pts = rollapply(dk_pts, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

rb_receiving_stats1 <- rb_receiving_stats1 %>% 
  group_by(player) %>% 
  arrange(week) %>% 
  arrange(year) %>%
  mutate(L3_fd_pts = rollapply(fd_pts, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

rb_receiving_stats1 <- rb_receiving_stats1 %>% 
  group_by(player) %>% 
  arrange(week) %>% 
  arrange(year) %>%
  mutate(L3_rec_usage = rollapply(rec_usage, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))


rb_receiving_stats1 <- rb_receiving_stats1 %>% 
  group_by(player) %>% 
  arrange(week) %>% 
  arrange(year) %>%
  slice(n())

rb_receiving_stats1 <- rb_receiving_stats1[,-c(2,3,5:24)]
rb_receiving_stats1 <-  rename(rb_receiving_stats1, L3_re_touchdowns = L3_touchdowns)
rb_receiving_stats1 <-  rename(rb_receiving_stats1, L3_re_yds = L3_yards)
rb_receiving_stats1 <-  rename(rb_receiving_stats1, L3_re_dkpts = L3_dk_pts)
rb_receiving_stats1 <-  rename(rb_receiving_stats1, L3_re_fdpts = L3_fd_pts)

str_stats_rb_re <- rb_receiving_stats %>%
  group_by(team_name,week,year) %>%
  mutate(string = order(rec_usage, decreasing=TRUE))  

str_stats_rb_re <- str_stats_rb_re %>% 
  group_by(team_name,string) %>% 
  arrange(week) %>% 
  arrange(year) %>%
  mutate(L3_targets = rollapply(targets, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

str_stats_rb_re <- str_stats_rb_re %>% 
  group_by(team_name,string) %>% 
  arrange(week) %>% 
  arrange(year) %>%
  mutate(L3_touchdowns = rollapply(touchdowns, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

str_stats_rb_re <- str_stats_rb_re %>% 
  group_by(team_name,string) %>% 
  arrange(week) %>% 
  arrange(year) %>%
  mutate(L3_yards = rollapply(yards, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

str_stats_rb_re <- str_stats_rb_re %>% 
  group_by(team_name,string) %>% 
  arrange(week) %>% 
  arrange(year) %>%
  mutate(L3_yards_per_reception = rollapply(yards_per_reception, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

str_stats_rb_re <- str_stats_rb_re %>% 
  group_by(team_name,string) %>% 
  arrange(week) %>% 
  arrange(year) %>%
  mutate(L3_dk_pts = rollapply(dk_pts, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

str_stats_rb_re <- str_stats_rb_re %>% 
  group_by(team_name,string) %>% 
  arrange(week) %>% 
  arrange(year) %>%
  mutate(L3_fd_pts = rollapply(fd_pts, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

str_stats_rb_re <- str_stats_rb_re %>% 
  group_by(team_name,string) %>% 
  arrange(week) %>% 
  arrange(year) %>%
  mutate(L3_rec_usage = rollapply(rec_usage, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))


str_stats_rb_re1 <- str_stats_rb_re %>% 
  group_by(team_name,string) %>% 
  arrange(week) %>% 
  arrange(year) %>%
  slice(n())

str_stats_rb_re1 <- str_stats_rb_re1[,-c(1:3,5:24)]
str_stats_rb_re1 <-  rename(str_stats_rb_re1, L3_re_touchdowns = L3_touchdowns)
str_stats_rb_re1 <-  rename(str_stats_rb_re1, L3_re_yds = L3_yards)
str_stats_rb_re1 <-  rename(str_stats_rb_re1, L3_re_dkpts = L3_dk_pts)
str_stats_rb_re1 <-  rename(str_stats_rb_re1, L3_re_fdpts = L3_fd_pts)
colnames(str_stats_rb_re1) <- c("team_name", "string", "str_L3_targets", "str_L3_re_touchdowns", "str_L3_re_yds", "str_L3_yards_per_reception",
                                "str_L3_re_dkpts", "str_L3_re_fdpts", "str_L3_rec_usage")

wr_share1 <- wr_share %>%
  group_by(team_name,week,year) %>%
  mutate(string = order(rec_usage, decreasing=TRUE))  

wr_share1 <- wr_share1 %>% 
  group_by(player) %>% 
  arrange(week) %>% 
  arrange(year) %>%
  mutate(L3_rec_usage = rollapply(rec_usage, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

wr_share1 <- wr_share1 %>% 
  group_by(player) %>% 
  arrange(week) %>% 
  arrange(year) %>%
  mutate(L3_touchdowns = rollapply(touchdowns, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))


wr_share1 <- wr_share1 %>% 
  group_by(player) %>% 
  arrange(week) %>% 
  arrange(year) %>%
  mutate(L3_yards = rollapply(yards, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))


wr_share1 <- wr_share1 %>% 
  group_by(player) %>% 
  arrange(week) %>% 
  arrange(year) %>%
  mutate(L3_yards_per_reception = rollapply(yards_per_reception, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

wr_share1 <- wr_share1 %>% 
  group_by(player) %>% 
  arrange(week) %>% 
  arrange(year) %>%
  mutate(L3_dk_pts = rollapply(dk_pts, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

wr_share1 <- wr_share1 %>% 
  group_by(player) %>% 
  arrange(week) %>% 
  arrange(year) %>%
  mutate(L3_fd_pts = rollapply(fd_pts, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

wr_share1 <- wr_share1 %>% 
  group_by(player) %>% 
  arrange(week) %>% 
  arrange(year) %>%
  mutate(L3_targets = rollapply(targets, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

wr_share1 <- wr_share1 %>%
  group_by(team_name,week,year) %>%
  mutate(re_string = order(L3_targets, decreasing=TRUE))  

wr_share1 <- wr_share1 %>% 
  group_by(player) %>% 
  arrange(week) %>% 
  arrange(year) %>%
  slice(n())

wr_share1 <- wr_share1[,-c(2,3:24)]


str_stats_re <- wr_share %>%
  group_by(team_name,week,year) %>%
  mutate(string = order(rec_usage, decreasing=TRUE))  

str_stats_re <- str_stats_re %>% 
  group_by(team_name,string) %>% 
  arrange(week) %>% 
  arrange(year) %>%
  mutate(L3_rec_usage = rollapply(rec_usage, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

str_stats_re <- str_stats_re %>% 
  group_by(team_name,string) %>% 
  arrange(week) %>% 
  arrange(year) %>%
  mutate(L3_touchdowns = rollapply(touchdowns, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))


str_stats_re <- str_stats_re %>% 
  group_by(team_name,string) %>% 
  arrange(week) %>% 
  arrange(year) %>%
  mutate(L3_yards = rollapply(yards, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))


str_stats_re <- str_stats_re %>% 
  group_by(team_name,string) %>% 
  arrange(week) %>% 
  arrange(year) %>%
  mutate(L3_yards_per_reception = rollapply(yards_per_reception, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

str_stats_re <- str_stats_re %>% 
  group_by(team_name,string) %>% 
  arrange(week) %>% 
  arrange(year) %>%
  mutate(L3_dk_pts = rollapply(dk_pts, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

str_stats_re <- str_stats_re %>% 
  group_by(team_name,string) %>% 
  arrange(week) %>% 
  arrange(year) %>%
  mutate(L3_fd_pts = rollapply(fd_pts, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

str_stats_re <- str_stats_re %>% 
  group_by(team_name,string) %>% 
  arrange(week) %>% 
  arrange(year) %>%
  mutate(L3_targets = rollapply(targets, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

str_stats_re <- str_stats_re %>%
  group_by(team_name,week,year) %>%
  mutate(re_string = order(L3_targets, decreasing=TRUE))  

str_stats_re1 <- str_stats_re %>% 
  group_by(team_name,string) %>% 
  arrange(week) %>% 
  arrange(year) %>%
  slice(n())

str_stats_re1 <- str_stats_re1[,-c(1:3,5:24)]
colnames(str_stats_re1) <- c("team_name", "string", "str_L3_rec_usage", "str_L3_touchdowns", "str_L3_yards", "str_L3_yards_per_reception",
                             "str_L3_dk_pts", "str_L3_fd_pts", "str_L3_targets", "str_re_string")

usages <- str_stats_re1[,c(1,3,2)]

current_slate_wr <- filter(current_slate, position == "WR")
current_slate_rb <- filter(current_slate, position == "RB")
current_slate_qb <- filter(current_slate, position == "QB")

team_names <- read.csv('coach and pace names.csv')
team_names <- team_names[,c(3,4)]
colnames(team_names) <- c('team_name','TeamAbbrev')
team_names <- data.table(team_names)
team_names[, TeamAbbrev := stri_trans_general(str = TeamAbbrev, 
                                              id = "Latin-ASCII")]

current_slate_qb <- left_join(current_slate_qb, team_names)
clean_names1 <- rename(clean_names1, player = pff_names)
qb_stats1 <- left_join(qb_stats1, clean_names1)
qb_stats1$player <- ifelse(is.na(qb_stats1$dk_names),qb_stats1$player,qb_stats1$dk_names)
qb_stats1 <- qb_stats1[,-c(15)]
current_slate_qb <- left_join(current_slate_qb, qb_stats1)
qb_rushing_stats1 <- left_join(qb_rushing_stats1, clean_names1)
qb_rushing_stats1$player <- ifelse(is.na(qb_rushing_stats1$dk_names),qb_rushing_stats1$player,qb_rushing_stats1$dk_names)
qb_rushing_stats1 <- qb_rushing_stats1[,-c(10)]
current_slate_qb <- left_join(current_slate_qb, qb_rushing_stats1)

current_slate_rb <- left_join(current_slate_rb, team_names)
rb_runshares1 <- left_join(rb_runshares1, clean_names1)
rb_runshares1$player <- ifelse(is.na(rb_runshares1$dk_names),rb_runshares1$player,rb_runshares1$dk_names)
rb_runshares1 <- rb_runshares1[,-c(14)]
current_slate_rb <- left_join(current_slate_rb, rb_runshares1)
rb_receiving_stats1 <- left_join(rb_receiving_stats1, clean_names1)
rb_receiving_stats1$player <- ifelse(is.na(rb_receiving_stats1$dk_names),rb_receiving_stats1$player,rb_receiving_stats1$dk_names)
rb_receiving_stats1 <- rb_receiving_stats1[,-c(10)]
current_slate_rb <- left_join(current_slate_rb, rb_receiving_stats1)

current_slate_wr <- left_join(current_slate_wr, team_names)
wr_share1 <- left_join(wr_share1, clean_names1)
wr_share1$player <- ifelse(is.na(wr_share1$dk_names),wr_share1$player,wr_share1$dk_names)
wr_share1 <- wr_share1[,-c(11)]
current_slate_wr <- left_join(current_slate_wr, wr_share1)


team_names <- read.csv('coach and pace names.csv')
team_names <- team_names[,c(3,5)]
colnames(team_names) <- c('team_name','team')
team_names <- data.table(team_names)
team_names[, team := stri_trans_general(str = team, 
                                        id = "Latin-ASCII")]

out_players <- left_join(out_players,team_names)

current_slate_qb <- left_join(current_slate_qb,out_players)
current_slate_qb <- current_slate_qb[,-c(9)]
current_slate_rb <- left_join(current_slate_rb,out_players)
current_slate_rb <- current_slate_rb[,-c(9)]
current_slate_wr <- left_join(current_slate_wr,out_players)
current_slate_wr <- current_slate_wr[,-c(9)]


depth_charts <- read.csv('depth_charts.csv')
depth_charts <- depth_charts %>% 
  group_by(Team) %>% 
  mutate(string = 1:n())


team_names <- read.csv('coach and pace names.csv')
team_names <- team_names[,c(3,6)]
colnames(team_names) <- c('team_name','Team')
team_names <- data.table(team_names)
team_names[, Team := stri_trans_general(str = Team, 
                                        id = "Latin-ASCII")]

depth_charts <- left_join(depth_charts,team_names)
colnames(clean_names) <- c('dk_name','player')
qb_strings <- depth_charts[,c("Team", "QB", "string", "team_name")]
qb_strings <- rename(qb_strings, player = QB)
qb_strings <- left_join(qb_strings,clean_names)
qb_strings$player <- ifelse(is.na(qb_strings$dk_name),qb_strings$player,qb_strings$dk_name)
qb_strings <- qb_strings[,-c(5)]

rb_strings <- depth_charts[,c("Team", "RB", "string", "team_name")]
rb_strings <- rename(rb_strings, player = RB)
rb_strings <- left_join(rb_strings,clean_names)
rb_strings$player <- ifelse(is.na(rb_strings$dk_name),rb_strings$player,rb_strings$dk_name)
rb_strings <- rb_strings[,-c(5)]

wr_strings <- depth_charts[,c("Team", "WR", "string", "team_name")]
wr_strings <- rename(wr_strings, player = WR)
wr_strings <- left_join(wr_strings,clean_names)
wr_strings$player <- ifelse(is.na(wr_strings$dk_name),wr_strings$player,wr_strings$dk_name)
wr_strings <- wr_strings[,-c(5)]


current_slate_qb <- left_join(current_slate_qb,qb_strings)
current_slate_rb <- left_join(current_slate_rb,rb_strings)
current_slate_wr <- left_join(current_slate_wr,wr_strings)
#Check for NA's
na_qb <- current_slate_qb[!is.na(current_slate_qb$string),]
na_rb <- current_slate_rb[!is.na(current_slate_rb$string),]
na_wr <- current_slate_wr[!is.na(current_slate_wr$string),]

current_slate_qb <- current_slate_qb[!is.na(current_slate_qb$Team),]
current_slate_rb <- current_slate_rb[!is.na(current_slate_rb$Team),]
current_slate_wr <- current_slate_wr[!is.na(current_slate_wr$Team),]

current_slate_qb <- current_slate_qb[is.na(current_slate_qb$team),]
current_slate_rb <- current_slate_rb[is.na(current_slate_rb$team),]
current_slate_wr <- current_slate_wr[is.na(current_slate_wr$team),]


current_slate_qb <- current_slate_qb %>%
  group_by(Team) %>%
  arrange(string) %>%
  mutate(string = order(string, decreasing=FALSE))  
current_slate_qb <- current_slate_qb[,-c(5,29:33)]


current_slate_rb <- current_slate_rb %>%
  group_by(Team) %>%
  arrange(string) %>%
  mutate(string1 = order(string, decreasing=FALSE))  
current_slate_rb <- current_slate_rb[,-c(5,27:32)]

current_slate_wr <- current_slate_wr %>%
  group_by(Team) %>%
  arrange(string) %>%
  mutate(string = order(string, decreasing=FALSE))  
current_slate_wr <- current_slate_wr[,-c(5,17:22)]

current_slate_qb <- left_join(current_slate_qb,str_stats_pa1)
current_slate_qb <- left_join(current_slate_qb,str_stats_qb_ru1)
current_slate_qb$L3_attempts <- ifelse(is.na(current_slate_qb$L3_attempts),current_slate_qb$str_L3_attempts,current_slate_qb$L3_attempts)
current_slate_qb$L3_completion_percent <- ifelse(is.na(current_slate_qb$L3_completion_percent),current_slate_qb$str_L3_completion_percent,current_slate_qb$L3_completion_percent)
current_slate_qb$L3_completions <- ifelse(is.na(current_slate_qb$L3_completions),current_slate_qb$str_L3_completions,current_slate_qb$L3_completions)
current_slate_qb$L3_first_downs <- ifelse(is.na(current_slate_qb$L3_first_downs),current_slate_qb$str_L3_first_downs,current_slate_qb$L3_first_downs)
current_slate_qb$L3_interceptions <- ifelse(is.na(current_slate_qb$L3_interceptions),current_slate_qb$str_L3_interceptions,current_slate_qb$L3_interceptions)
current_slate_qb$L3_qb_rating <- ifelse(is.na(current_slate_qb$L3_qb_rating),current_slate_qb$str_L3_qb_rating,current_slate_qb$L3_qb_rating)
current_slate_qb$L3_touchdowns <- ifelse(is.na(current_slate_qb$L3_touchdowns),current_slate_qb$str_L3_touchdowns,current_slate_qb$L3_touchdowns)
current_slate_qb$L3_yards <- ifelse(is.na(current_slate_qb$L3_yards),current_slate_qb$str_L3_yards,current_slate_qb$L3_yards)
current_slate_qb$L3_ypa <- ifelse(is.na(current_slate_qb$L3_ypa),current_slate_qb$str_L3_ypa,current_slate_qb$L3_ypa)
current_slate_qb$L3_pa_dkpts <- ifelse(is.na(current_slate_qb$L3_pa_dkpts),current_slate_qb$str_L3_pa_dkpts,current_slate_qb$L3_pa_dkpts)
current_slate_qb$L3_pa_fdpts <- ifelse(is.na(current_slate_qb$L3_pa_fdpts),current_slate_qb$str_L3_pa_fdpts,current_slate_qb$L3_pa_fdpts)
current_slate_qb$L3_runshare <- ifelse(is.na(current_slate_qb$L3_runshare),current_slate_qb$str_L3_runshare,current_slate_qb$L3_runshare)
current_slate_qb$L3_rush_att <- ifelse(is.na(current_slate_qb$L3_rush_att),current_slate_qb$str_L3_rush_att,current_slate_qb$L3_rush_att)
current_slate_qb$L3_longest <- ifelse(is.na(current_slate_qb$L3_longest),current_slate_qb$str_L3_longest,current_slate_qb$L3_longest)
current_slate_qb$L3_rtd <- ifelse(is.na(current_slate_qb$L3_rtd),current_slate_qb$str_L3_rtd,current_slate_qb$L3_rtd)
current_slate_qb$L3_ry <- ifelse(is.na(current_slate_qb$L3_ry),current_slate_qb$str_L3_ry,current_slate_qb$L3_ry)
current_slate_qb$L3_rypa <- ifelse(is.na(current_slate_qb$L3_rypa),current_slate_qb$str_L3_rypa,current_slate_qb$L3_rypa)
current_slate_qb$L3_ru_dkpts <- ifelse(is.na(current_slate_qb$L3_ru_dkpts),current_slate_qb$str_L3_ru_dkpts,current_slate_qb$L3_ru_dkpts)
current_slate_qb$L3_ru_fdpts <- ifelse(is.na(current_slate_qb$L3_ru_fdpts),current_slate_qb$str_L3_ru_fdpts,current_slate_qb$L3_ru_fdpts)

current_slate_qb$L3_attempts <- ifelse(is.na(current_slate_qb$L3_attempts),0,current_slate_qb$L3_attempts)
current_slate_qb$L3_completion_percent <- ifelse(is.na(current_slate_qb$L3_completion_percent),0,current_slate_qb$L3_completion_percent)
current_slate_qb$L3_completions <- ifelse(is.na(current_slate_qb$L3_completions),0,current_slate_qb$L3_completions)
current_slate_qb$L3_first_downs <- ifelse(is.na(current_slate_qb$L3_first_downs),0,current_slate_qb$L3_first_downs)
current_slate_qb$L3_interceptions <- ifelse(is.na(current_slate_qb$L3_interceptions),0,current_slate_qb$L3_interceptions)
current_slate_qb$L3_qb_rating <- ifelse(is.na(current_slate_qb$L3_qb_rating),0,current_slate_qb$L3_qb_rating)
current_slate_qb$L3_touchdowns <- ifelse(is.na(current_slate_qb$L3_touchdowns),0,current_slate_qb$L3_touchdowns)
current_slate_qb$L3_yards <- ifelse(is.na(current_slate_qb$L3_yards),0,current_slate_qb$L3_yards)
current_slate_qb$L3_ypa <- ifelse(is.na(current_slate_qb$L3_ypa),0,current_slate_qb$L3_ypa)
current_slate_qb$L3_pa_dkpts <- ifelse(is.na(current_slate_qb$L3_pa_dkpts),0,current_slate_qb$L3_pa_dkpts)
current_slate_qb$L3_pa_fdpts <- ifelse(is.na(current_slate_qb$L3_pa_fdpts),0,current_slate_qb$L3_pa_fdpts)
current_slate_qb$L3_runshare <- ifelse(is.na(current_slate_qb$L3_runshare),0,current_slate_qb$L3_runshare)
current_slate_qb$L3_rush_att <- ifelse(is.na(current_slate_qb$L3_rush_att),0,current_slate_qb$L3_rush_att)
current_slate_qb$L3_longest <- ifelse(is.na(current_slate_qb$L3_longest),0,current_slate_qb$L3_longest)
current_slate_qb$L3_rtd <- ifelse(is.na(current_slate_qb$L3_rtd),0,current_slate_qb$L3_rtd)
current_slate_qb$L3_ry <- ifelse(is.na(current_slate_qb$L3_ry),0,current_slate_qb$L3_ry)
current_slate_qb$L3_rypa <- ifelse(is.na(current_slate_qb$L3_rypa),0,current_slate_qb$L3_rypa)
current_slate_qb$L3_ru_dkpts <- ifelse(is.na(current_slate_qb$L3_ru_dkpts),0,current_slate_qb$L3_ru_dkpts)
current_slate_qb$L3_ru_fdpts <- ifelse(is.na(current_slate_qb$L3_ru_fdpts),0,current_slate_qb$L3_ru_fdpts)

current_slate_rb <- left_join(current_slate_rb,str_stats_ru1)
current_slate_rb <- left_join(current_slate_rb,str_stats_rb_re1)
current_slate_rb$L3_attempts <- ifelse(is.na(current_slate_rb$L3_attempts),current_slate_rb$str_L3_attempts,current_slate_rb$L3_attempts)
current_slate_rb$L3_first_downs <- ifelse(is.na(current_slate_rb$L3_first_downs),current_slate_rb$str_L3_first_downs,current_slate_rb$L3_first_downs)
current_slate_rb$L3_fumbles <- ifelse(is.na(current_slate_rb$L3_fumbles),current_slate_rb$str_L3_fumbles,current_slate_rb$L3_fumbles)
current_slate_rb$L3_longest <- ifelse(is.na(current_slate_rb$L3_longest),current_slate_rb$str_L3_longest,current_slate_rb$L3_longest)
current_slate_rb$L3_touchdowns <- ifelse(is.na(current_slate_rb$L3_touchdowns),current_slate_rb$str_L3_touchdowns,current_slate_rb$L3_touchdowns)
current_slate_rb$L3_yards <- ifelse(is.na(current_slate_rb$L3_yards),current_slate_rb$str_L3_yards,current_slate_rb$L3_yards)
current_slate_rb$L3_ypa <- ifelse(is.na(current_slate_rb$L3_ypa),current_slate_rb$str_L3_ypa,current_slate_rb$L3_ypa)
current_slate_rb$L3_dk_pts <- ifelse(is.na(current_slate_rb$L3_dk_pts),current_slate_rb$str_L3_dk_pts,current_slate_rb$L3_dk_pts)
current_slate_rb$L3_fd_pts <- ifelse(is.na(current_slate_rb$L3_fd_pts),current_slate_rb$str_L3_fd_pts,current_slate_rb$L3_fd_pts)
current_slate_rb$L3_runshare <- ifelse(is.na(current_slate_rb$L3_runshare),current_slate_rb$str_L3_runshare,current_slate_rb$L3_runshare)
current_slate_rb$L3_targets <- ifelse(is.na(current_slate_rb$L3_targets),current_slate_rb$str_L3_targets,current_slate_rb$L3_targets)
current_slate_rb$L3_re_touchdowns <- ifelse(is.na(current_slate_rb$L3_re_touchdowns),current_slate_rb$str_L3_re_touchdowns,current_slate_rb$L3_re_touchdowns)
current_slate_rb$L3_re_yds <- ifelse(is.na(current_slate_rb$L3_re_yds),current_slate_rb$str_L3_re_yds,current_slate_rb$L3_re_yds)
current_slate_rb$L3_yards_per_reception <- ifelse(is.na(current_slate_rb$L3_yards_per_reception),current_slate_rb$str_L3_yards_per_reception,current_slate_rb$L3_yards_per_reception)
current_slate_rb$L3_re_dkpts <- ifelse(is.na(current_slate_rb$L3_re_dkpts),current_slate_rb$str_L3_re_dkpts,current_slate_rb$L3_re_dkpts)
current_slate_rb$L3_re_fdpts <- ifelse(is.na(current_slate_rb$L3_re_fdpts),current_slate_rb$str_L3_re_fdpts,current_slate_rb$L3_re_fdpts)
current_slate_rb$L3_rec_usage <- ifelse(is.na(current_slate_rb$L3_rec_usage),current_slate_rb$str_L3_rec_usage,current_slate_rb$L3_rec_usage)

current_slate_rb$L3_attempts <- ifelse(is.na(current_slate_rb$L3_attempts),0,current_slate_rb$L3_attempts)
current_slate_rb$L3_first_downs <- ifelse(is.na(current_slate_rb$L3_first_downs),0,current_slate_rb$L3_first_downs)
current_slate_rb$L3_fumbles <- ifelse(is.na(current_slate_rb$L3_fumbles),0,current_slate_rb$L3_fumbles)
current_slate_rb$L3_longest <- ifelse(is.na(current_slate_rb$L3_longest),0,current_slate_rb$L3_longest)
current_slate_rb$L3_touchdowns <- ifelse(is.na(current_slate_rb$L3_touchdowns),0,current_slate_rb$L3_touchdowns)
current_slate_rb$L3_yards <- ifelse(is.na(current_slate_rb$L3_yards),0,current_slate_rb$L3_yards)
current_slate_rb$L3_ypa <- ifelse(is.na(current_slate_rb$L3_ypa),0,current_slate_rb$L3_ypa)
current_slate_rb$L3_dk_pts <- ifelse(is.na(current_slate_rb$L3_dk_pts),0,current_slate_rb$L3_dk_pts)
current_slate_rb$L3_fd_pts <- ifelse(is.na(current_slate_rb$L3_fd_pts),0,current_slate_rb$L3_fd_pts)
current_slate_rb$L3_runshare <- ifelse(is.na(current_slate_rb$L3_runshare),0,current_slate_rb$L3_runshare)
current_slate_rb$L3_targets <- ifelse(is.na(current_slate_rb$L3_targets),0,current_slate_rb$L3_targets)
current_slate_rb$L3_re_touchdowns <- ifelse(is.na(current_slate_rb$L3_re_touchdowns),0,current_slate_rb$L3_re_touchdowns)
current_slate_rb$L3_re_yds <- ifelse(is.na(current_slate_rb$L3_re_yds),0,current_slate_rb$L3_re_yds)
current_slate_rb$L3_yards_per_reception <- ifelse(is.na(current_slate_rb$L3_yards_per_reception),0,current_slate_rb$L3_yards_per_reception)
current_slate_rb$L3_re_dkpts <- ifelse(is.na(current_slate_rb$L3_re_dkpts),0,current_slate_rb$L3_re_dkpts)
current_slate_rb$L3_re_fdpts <- ifelse(is.na(current_slate_rb$L3_re_fdpts),0,current_slate_rb$L3_re_fdpts)
current_slate_rb$L3_rec_usage <- ifelse(is.na(current_slate_rb$L3_rec_usage),0,current_slate_rb$L3_rec_usage)

current_slate_wr <- left_join(current_slate_wr,str_stats_re1)
current_slate_wr$L3_rec_usage <- ifelse(is.na(current_slate_wr$L3_rec_usage),current_slate_wr$str_L3_rec_usage,current_slate_wr$L3_rec_usage)
current_slate_wr$L3_touchdowns <- ifelse(is.na(current_slate_wr$L3_touchdowns),current_slate_wr$str_L3_touchdowns,current_slate_wr$L3_touchdowns)
current_slate_wr$L3_yards <- ifelse(is.na(current_slate_wr$L3_yards),current_slate_wr$str_L3_yards,current_slate_wr$L3_yards)
current_slate_wr$L3_yards_per_reception <- ifelse(is.na(current_slate_wr$L3_yards_per_reception),current_slate_wr$str_L3_yards_per_reception,current_slate_wr$L3_yards_per_reception)
current_slate_wr$L3_dk_pts <- ifelse(is.na(current_slate_wr$L3_dk_pts),current_slate_wr$str_L3_dk_pts,current_slate_wr$L3_dk_pts)
current_slate_wr$L3_fd_pts <- ifelse(is.na(current_slate_wr$L3_fd_pts),current_slate_wr$str_L3_fd_pts,current_slate_wr$L3_fd_pts)
current_slate_wr$L3_targets <- ifelse(is.na(current_slate_wr$L3_targets),current_slate_wr$str_L3_targets,current_slate_wr$L3_targets)

current_slate_wr$L3_rec_usage <- ifelse(is.na(current_slate_wr$L3_rec_usage),0,current_slate_wr$L3_rec_usage)
current_slate_wr$L3_touchdowns <- ifelse(is.na(current_slate_wr$L3_touchdowns),0,current_slate_wr$L3_touchdowns)
current_slate_wr$L3_yards <- ifelse(is.na(current_slate_wr$L3_yards),0,current_slate_wr$L3_yards)
current_slate_wr$L3_yards_per_reception <- ifelse(is.na(current_slate_wr$L3_yards_per_reception),0,current_slate_wr$L3_yards_per_reception)
current_slate_wr$L3_dk_pts <- ifelse(is.na(current_slate_wr$L3_dk_pts),0,current_slate_wr$L3_dk_pts)
current_slate_wr$L3_fd_pts <- ifelse(is.na(current_slate_wr$L3_fd_pts),0,current_slate_wr$L3_fd_pts)
current_slate_wr$L3_targets <- ifelse(is.na(current_slate_wr$L3_targets),0,current_slate_wr$L3_targets)

current_slate_qb <- current_slate_qb[,-c(7,19,30:50)]
current_slate_rb <- current_slate_rb[,-c(7,18,27:44)]

team_names <- read.csv('coach and pace names.csv')
team_names <- team_names[,c(3,4)]
colnames(team_names) <- c('team_name','TeamAbbrev')
team_names <- data.table(team_names)
team_names[, team_name := stri_trans_general(str = team_name, 
                                             id = "Latin-ASCII")]
current_slate_rb <- left_join(current_slate_rb,team_names)
current_slate_rb <- left_join(current_slate_rb,runshares)
current_slate_rb$L3_runshare <- current_slate_rb$str_L3_runshare


current_slate_wr <- current_slate_wr[,-c(7,15,17:24)]

team_names <- read.csv('coach and pace names.csv')
team_names <- team_names[,c(3,4)]
colnames(team_names) <- c('team_name','TeamAbbrev')
team_names <- data.table(team_names)
team_names[, team_name := stri_trans_general(str = team_name, 
                                             id = "Latin-ASCII")]
current_slate_wr <- left_join(current_slate_wr,team_names)
current_slate_wr <- left_join(current_slate_wr,usages)
current_slate_wr$L3_rec_usage <- current_slate_wr$str_L3_rec_usage

#implied Totals



odds <- read.csv('imp_totals.csv')
odds <- odds[,-c(1)]
imp_totals <- odds
imp_totals$spread <- as.numeric(imp_totals$spread)
imp_totals$spread <- ifelse(is.na(imp_totals$spread),0,imp_totals$spread)
imp_totals$ou <- as.numeric(imp_totals$ou)
imp_totals$imp_totals <- (imp_totals$ou/2) - (imp_totals$spread/2)
imp_totals <- imp_totals[,c(1,4)]
team_names <- read.csv('coach and pace names.csv')
team_names <- team_names[,c(1,7)]
colnames(team_names) <- c('team','imp_team')
team_names <- data.table(team_names)
team_names[, imp_team := stri_trans_general(str = imp_team, 
                                            id = "Latin-ASCII")]
team_names[, team := stri_trans_general(str = team, 
                                        id = "Latin-ASCII")]
imp_totals <- left_join(imp_totals,team_names)

cfbd_game_info_prev <- cfbd_game_info(year - 1)
cfbd_game_info <- cfbd_game_info(year)
cfbd_game_info_total <- rbind(cfbd_game_info,cfbd_game_info_prev)
cfbd_game_info_total <- cfbd_game_info_total[,c(1,2,3,13,16,21,24)]
week_games <- cfbd_game_info_total
week_games <- week_games %>% filter(season == year)
week_games <- week_games %>% filter(week == currentweek)
week_games <- week_games[,c(4,6)]
cfbd_game_info_total <- cfbd_game_info_total[complete.cases(cfbd_game_info_total),]
cfbd_game_info_total1 <- cfbd_game_info_total[,c(1,2,3,6,7,4,5)]
colnames(cfbd_game_info_total) <- c('game_id','year','week','team','pts','opp_team','opp_pts')
colnames(cfbd_game_info_total1) <- c('game_id','year','week','team','pts','opp_team','opp_pts')
cfbd_game_info_total <- rbind(cfbd_game_info_total,cfbd_game_info_total1)


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

j = year-1
cfbd_plays_total <- data.frame()

while(j <= year){
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




j = year-1
cfbd_drives_total <- data.frame()

while(j <= year){
  for(i in 1:16)
    tryCatch({
      cfbd_drives <- cfbd_drives(year = j, week = i)
      
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
  mutate(L3_avg_down = rollapplyr(avg_down, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

efficiency <- efficiency %>% 
  group_by(team) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_avg_def_down = rollapplyr(avg_def_down, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

efficiency <- efficiency %>% 
  group_by(team) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_avg_distance = rollapplyr(avg_distance, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

efficiency <- efficiency %>% 
  group_by(team) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_avg_def_distance = rollapplyr(avg_def_distance, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

efficiency <- efficiency %>% 
  group_by(team) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_avg_drive_efficiency = rollapplyr(avg_drive_efficiency, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

efficiency <- efficiency %>% 
  group_by(team) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_avg_def_drive_efficiency = rollapplyr(avg_def_drive_efficiency, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

efficiency <- efficiency[,-c(4:9)]
efficiency$L3_avg_drive_efficiency <- ifelse(is.na(efficiency$L3_avg_drive_efficiency),mean(efficiency$L3_avg_drive_efficiency, na.rm = TRUE),efficiency$L3_avg_drive_efficiency)
efficiency$L3_avg_def_drive_efficiency <- ifelse(is.na(efficiency$L3_avg_def_drive_efficiency),mean(efficiency$L3_avg_def_drive_efficiency,na.rm = TRUE),efficiency$L3_avg_def_drive_efficiency)

team_efficiency <- efficiency
cfbd_game_info_total <- left_join(cfbd_game_info_total,team_efficiency)


library(cfbfastR)


j = year-1
i = currentweek
total_team_stats <- data.frame()
while (j <= year-1) {
  for (i in 1:16) {
    tryCatch({
      cfbd_stats_season_team <- cfbd_stats_season_team(
        j,
        season_type = "regular",
        start_week = i,
        end_week = i
      )
      cfbd_stats_season_team$week <- i
      assign(paste0('total_stats_',i,'_',year),cfbd_stats_season_team)
      total_team_stats <- rbind(cfbd_stats_season_team,total_team_stats)
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
  
  j <- j + 1
}

j = year
while (j <= year) {
  for (i in 1:currentweek-1) {
    tryCatch({
      cfbd_stats_season_team <- cfbd_stats_season_team(
        j,
        season_type = "regular",
        start_week = i,
        end_week = i
      )
      cfbd_stats_season_team$week <- i
      assign(paste0('total_stats_',i,'_',year),cfbd_stats_season_team)
      total_team_stats <- rbind(cfbd_stats_season_team,total_team_stats)
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
  
  j <- j + 1
}


team_stats <- total_team_stats
team_stats <- rename(team_stats, year = season)
team_stats$tot_plays <- team_stats$pass_atts + team_stats$rush_atts
team_stats$tpp <- team_stats$time_of_poss_total / team_stats$tot_plays
time_per_play <- team_stats[,c(1,2,33,35)]
time_per_play <- time_per_play[complete.cases(time_per_play),]


time_per_play <- time_per_play %>% 
  group_by(team) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_tpp = rollapplyr(tpp, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

time_per_play <- time_per_play %>% filter(L3_tpp != Inf)

time_per_play <- time_per_play[complete.cases(time_per_play),]

cfbd_game_info_total <- left_join(cfbd_game_info_total, time_per_play)

team_stats <- total_team_stats
team_stats <- rename(team_stats, year = season)
team_stats$tot_plays <- team_stats$pass_atts + team_stats$rush_atts
team_stats$tpp <- team_stats$time_of_poss_total / team_stats$tot_plays
opp_time_per_play <- team_stats[,c(1,2,33,35)]
opp_time_per_play <- opp_time_per_play[complete.cases(opp_time_per_play),]

colnames(opp_time_per_play) <- c('year','opp_team','week','opp_tpp')
opp_time_per_play <- opp_time_per_play %>% 
  group_by(opp_team) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_opp_tpp = rollapplyr(opp_tpp, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))
cfbd_game_info_total <- left_join(cfbd_game_info_total, opp_time_per_play)

adv_stats_total <- read.csv('2021_adv_stats_total.csv')
adv_stats_total <- adv_stats_total[,-c(1)]
i <- year
j <- 1
while (j < currentweek) {
  tryCatch({
    adv_stats <- cfbd_stats_season_advanced(
      year = i,
      start_week = j,
      end_week = j,
    )
    adv_stats$week <- j
    assign(paste0('adv_stats_',j,'_',i),adv_stats)
    adv_stats_total <- rbind(adv_stats_total,adv_stats)
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  j <- j + 1
}






adv_stats_total <- adv_stats_total %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_off_ppa = rollapply(off_ppa, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))


adv_stats_total <- adv_stats_total %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_off_success_rate = rollapply(off_success_rate, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

adv_stats_total <- adv_stats_total %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_off_explosiveness = rollapply(off_explosiveness, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))


adv_stats_total <- adv_stats_total %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_off_power_success = rollapply(off_power_success, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

adv_stats_total <- adv_stats_total %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_off_stuff_rate = rollapply(off_stuff_rate, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))


adv_stats_total <- adv_stats_total %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_off_line_yds = rollapply(off_line_yds, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

adv_stats_total <- adv_stats_total %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_off_second_lvl_yds = rollapply(off_second_lvl_yds, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))


adv_stats_total <- adv_stats_total %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_off_open_field_yds = rollapply(off_open_field_yds, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

adv_stats_total <- adv_stats_total %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_off_field_pos_avg_predicted_points = rollapply(off_field_pos_avg_predicted_points, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))


adv_stats_total <- adv_stats_total %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_off_standard_downs_rate = rollapply(off_standard_downs_rate, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

adv_stats_total <- adv_stats_total %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_off_standard_downs_ppa = rollapply(off_standard_downs_ppa, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))


adv_stats_total <- adv_stats_total %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_off_standard_downs_success_rate = rollapply(off_standard_downs_success_rate, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

adv_stats_total <- adv_stats_total %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_off_standard_downs_explosiveness = rollapply(off_standard_downs_explosiveness, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))


adv_stats_total <- adv_stats_total %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_off_passing_downs_rate = rollapply(off_passing_downs_rate, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

adv_stats_total <- adv_stats_total %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_off_passing_downs_ppa = rollapply(off_passing_downs_ppa, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))


adv_stats_total <- adv_stats_total %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_off_passing_downs_success_rate = rollapply(off_passing_downs_success_rate, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

adv_stats_total <- adv_stats_total %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_off_passing_downs_explosiveness = rollapply(off_passing_downs_explosiveness, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))


adv_stats_total <- adv_stats_total %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_off_rushing_plays_rate = rollapply(off_rushing_plays_rate, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

adv_stats_total <- adv_stats_total %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_off_rushing_plays_ppa = rollapply(off_rushing_plays_ppa, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))


adv_stats_total <- adv_stats_total %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_off_rushing_plays_success_rate = rollapply(off_rushing_plays_success_rate, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

adv_stats_total <- adv_stats_total %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_off_rushing_plays_explosiveness = rollapply(off_rushing_plays_explosiveness, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))


adv_stats_total <- adv_stats_total %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_off_passing_plays_rate = rollapply(off_passing_plays_rate, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

adv_stats_total <- adv_stats_total %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_off_passing_plays_ppa = rollapply(off_passing_plays_ppa, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))


adv_stats_total <- adv_stats_total %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_off_passing_plays_success_rate = rollapply(off_passing_plays_success_rate, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

adv_stats_total <- adv_stats_total %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_off_passing_plays_explosiveness = rollapply(off_passing_plays_explosiveness, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))



adv_stats_total <- adv_stats_total %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_def_ppa = rollapply(def_ppa, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

adv_stats_total <- adv_stats_total %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_def_success_rate = rollapply(def_success_rate, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))



adv_stats_total <- adv_stats_total %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_def_explosiveness = rollapply(def_explosiveness, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

adv_stats_total <- adv_stats_total %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_def_power_success = rollapply(def_power_success, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))



adv_stats_total <- adv_stats_total %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_def_stuff_rate = rollapply(def_stuff_rate, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

adv_stats_total <- adv_stats_total %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_def_line_yds = rollapply(def_line_yds, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))



adv_stats_total <- adv_stats_total %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_def_second_lvl_yds = rollapply(def_second_lvl_yds, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

adv_stats_total <- adv_stats_total %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_def_open_field_yds = rollapply(def_open_field_yds, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))



adv_stats_total <- adv_stats_total %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_def_pts_per_opp = rollapply(def_pts_per_opp, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

adv_stats_total <- adv_stats_total %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3def_field_pos_avg_predicted_points = rollapply(def_field_pos_avg_predicted_points, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))



adv_stats_total <- adv_stats_total %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_def_standard_downs_rate = rollapply(def_standard_downs_rate, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

adv_stats_total <- adv_stats_total %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_def_standard_downs_ppa = rollapply(def_standard_downs_ppa, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))


adv_stats_total <- adv_stats_total %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_def_standard_downs_success_rate = rollapply(def_standard_downs_success_rate, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

adv_stats_total <- adv_stats_total %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_def_standard_downs_explosiveness = rollapply(def_standard_downs_explosiveness, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))



adv_stats_total <- adv_stats_total %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_def_passing_downs_rate = rollapply(def_passing_downs_rate, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

adv_stats_total <- adv_stats_total %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_def_passing_downs_ppa = rollapply(def_passing_downs_ppa, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))



adv_stats_total <- adv_stats_total %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_def_passing_downs_success_rate = rollapply(def_passing_downs_success_rate, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

adv_stats_total <- adv_stats_total %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_def_passing_downs_explosiveness = rollapply(def_passing_downs_explosiveness, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))



adv_stats_total <- adv_stats_total %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_def_rushing_plays_rate = rollapply(def_rushing_plays_rate, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

adv_stats_total <- adv_stats_total %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_def_rushing_plays_ppa = rollapply(def_rushing_plays_ppa, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))


adv_stats_total <- adv_stats_total %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_def_rushing_plays_success_rate = rollapply(def_rushing_plays_success_rate, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

adv_stats_total <- adv_stats_total %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_def_rushing_plays_explosiveness = rollapply(def_rushing_plays_explosiveness, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))



adv_stats_total <- adv_stats_total %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_def_passing_plays_rate = rollapply(def_passing_plays_rate, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

adv_stats_total <- adv_stats_total %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_def_passing_plays_ppa = rollapply(def_passing_plays_ppa, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))



adv_stats_total <- adv_stats_total %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_def_passing_plays_success_rate = rollapply(def_passing_plays_success_rate, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

adv_stats_total <- adv_stats_total %>% 
  group_by(team) %>%  arrange(week) %>% arrange(season) %>%
  mutate(L3_def_passing_plays_explosiveness = rollapply(def_passing_plays_explosiveness, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))


adv_stats_total <- adv_stats_total[,c(1,2,82:133)]
adv_stats_total <- rename(adv_stats_total, year = season)

adv_stats_total$L3_off_ppa <- ifelse(is.na(adv_stats_total$L3_off_ppa),mean(adv_stats_total$L3_off_ppa, na.rm = TRUE),adv_stats_total$L3_off_ppa)
adv_stats_total$L3_off_success_rate <- ifelse(is.na(adv_stats_total$L3_off_success_rate),mean(adv_stats_total$L3_off_success_rate, na.rm = TRUE),adv_stats_total$L3_off_success_rate)
adv_stats_total$L3_off_explosiveness <- ifelse(is.na(adv_stats_total$L3_off_explosiveness),mean(adv_stats_total$L3_off_explosiveness, na.rm = TRUE),adv_stats_total$L3_off_explosiveness)
adv_stats_total$L3_off_power_success <- ifelse(is.na(adv_stats_total$L3_off_power_success),mean(adv_stats_total$L3_off_power_success, na.rm = TRUE),adv_stats_total$L3_off_power_success)
adv_stats_total$L3_off_stuff_rate <- ifelse(is.na(adv_stats_total$L3_off_stuff_rate),mean(adv_stats_total$L3_off_stuff_rate, na.rm = TRUE),adv_stats_total$L3_off_stuff_rate)
adv_stats_total$L3_off_line_yds <- ifelse(is.na(adv_stats_total$L3_off_line_yds),mean(adv_stats_total$L3_off_line_yds, na.rm = TRUE),adv_stats_total$L3_off_line_yds)
adv_stats_total$L3_off_second_lvl_yds <- ifelse(is.na(adv_stats_total$L3_off_second_lvl_yds),mean(adv_stats_total$L3_off_second_lvl_yds, na.rm = TRUE),adv_stats_total$L3_off_second_lvl_yds)
adv_stats_total$L3_off_open_field_yds <- ifelse(is.na(adv_stats_total$L3_off_open_field_yds),mean(adv_stats_total$L3_off_open_field_yds, na.rm = TRUE),adv_stats_total$L3_off_open_field_yds)
adv_stats_total$L3_off_field_pos_avg_predicted_points <- ifelse(is.na(adv_stats_total$L3_off_field_pos_avg_predicted_points),mean(adv_stats_total$L3_off_field_pos_avg_predicted_points, na.rm = TRUE),adv_stats_total$L3_off_field_pos_avg_predicted_points)
adv_stats_total$L3_off_standard_downs_rate <- ifelse(is.na(adv_stats_total$L3_off_standard_downs_rate),mean(adv_stats_total$L3_off_standard_downs_rate, na.rm = TRUE),adv_stats_total$L3_off_standard_downs_rate)
adv_stats_total$L3_off_standard_downs_ppa <- ifelse(is.na(adv_stats_total$L3_off_standard_downs_ppa),mean(adv_stats_total$L3_off_standard_downs_ppa, na.rm = TRUE),adv_stats_total$L3_off_standard_downs_ppa)
adv_stats_total$L3_off_standard_downs_success_rate <- ifelse(is.na(adv_stats_total$L3_off_standard_downs_success_rate),mean(adv_stats_total$L3_off_standard_downs_success_rate, na.rm = TRUE),adv_stats_total$L3_off_standard_downs_success_rate)
adv_stats_total$L3_off_standard_downs_explosiveness <- ifelse(is.na(adv_stats_total$L3_off_standard_downs_explosiveness),mean(adv_stats_total$L3_off_standard_downs_explosiveness, na.rm = TRUE),adv_stats_total$L3_off_standard_downs_explosiveness)
adv_stats_total$L3_off_passing_downs_rate <- ifelse(is.na(adv_stats_total$L3_off_passing_downs_rate),mean(adv_stats_total$L3_off_passing_downs_rate, na.rm = TRUE),adv_stats_total$L3_off_passing_downs_rate)
adv_stats_total$L3_off_passing_downs_ppa <- ifelse(is.na(adv_stats_total$L3_off_passing_downs_ppa),mean(adv_stats_total$L3_off_passing_downs_ppa, na.rm = TRUE),adv_stats_total$L3_off_passing_downs_ppa)
adv_stats_total$L3_off_passing_downs_success_rate <- ifelse(is.na(adv_stats_total$L3_off_passing_downs_success_rate),mean(adv_stats_total$L3_off_passing_downs_success_rate, na.rm = TRUE),adv_stats_total$L3_off_passing_downs_success_rate)
adv_stats_total$L3_off_passing_downs_explosiveness <- ifelse(is.na(adv_stats_total$L3_off_passing_downs_explosiveness),mean(adv_stats_total$L3_off_passing_downs_explosiveness, na.rm = TRUE),adv_stats_total$L3_off_passing_downs_explosiveness)
adv_stats_total$L3_off_rushing_plays_rate <- ifelse(is.na(adv_stats_total$L3_off_rushing_plays_rate),mean(adv_stats_total$L3_off_rushing_plays_rate, na.rm = TRUE),adv_stats_total$L3_off_rushing_plays_rate)
adv_stats_total$L3_off_rushing_plays_ppa <- ifelse(is.na(adv_stats_total$L3_off_rushing_plays_ppa),mean(adv_stats_total$L3_off_rushing_plays_ppa, na.rm = TRUE),adv_stats_total$L3_off_rushing_plays_ppa)
adv_stats_total$L3_off_rushing_plays_success_rate <- ifelse(is.na(adv_stats_total$L3_off_rushing_plays_success_rate),mean(adv_stats_total$L3_off_rushing_plays_success_rate, na.rm = TRUE),adv_stats_total$L3_off_rushing_plays_success_rate)
adv_stats_total$L3_off_rushing_plays_explosiveness <- ifelse(is.na(adv_stats_total$L3_off_rushing_plays_explosiveness),mean(adv_stats_total$L3_off_rushing_plays_explosiveness, na.rm = TRUE),adv_stats_total$L3_off_rushing_plays_explosiveness)
adv_stats_total$L3_off_passing_plays_rate <- ifelse(is.na(adv_stats_total$L3_off_passing_plays_rate),mean(adv_stats_total$L3_off_passing_plays_rate, na.rm = TRUE),adv_stats_total$L3_off_passing_plays_rate)
adv_stats_total$L3_off_passing_plays_ppa <- ifelse(is.na(adv_stats_total$L3_off_passing_plays_ppa),mean(adv_stats_total$L3_off_passing_plays_ppa, na.rm = TRUE),adv_stats_total$L3_off_passing_plays_ppa)
adv_stats_total$L3_off_passing_plays_success_rate <- ifelse(is.na(adv_stats_total$L3_off_passing_plays_success_rate),mean(adv_stats_total$L3_off_passing_plays_success_rate, na.rm = TRUE),adv_stats_total$L3_off_passing_plays_success_rate)
adv_stats_total$L3_off_passing_plays_explosiveness <- ifelse(is.na(adv_stats_total$L3_off_passing_plays_explosiveness),mean(adv_stats_total$L3_off_passing_plays_explosiveness, na.rm = TRUE),adv_stats_total$L3_off_passing_plays_explosiveness)
adv_stats_total$L3_def_ppa <- ifelse(is.na(adv_stats_total$L3_def_ppa),mean(adv_stats_total$L3_def_ppa, na.rm = TRUE),adv_stats_total$L3_def_ppa)
adv_stats_total$L3_def_success_rate <- ifelse(is.na(adv_stats_total$L3_def_success_rate),mean(adv_stats_total$L3_def_success_rate, na.rm = TRUE),adv_stats_total$L3_def_success_rate)
adv_stats_total$L3_def_explosiveness <- ifelse(is.na(adv_stats_total$L3_def_explosiveness),mean(adv_stats_total$L3_def_explosiveness, na.rm = TRUE),adv_stats_total$L3_def_explosiveness)
adv_stats_total$L3_def_power_success <- ifelse(is.na(adv_stats_total$L3_def_power_success),mean(adv_stats_total$L3_def_power_success, na.rm = TRUE),adv_stats_total$L3_def_power_success)
adv_stats_total$L3_def_stuff_rate <- ifelse(is.na(adv_stats_total$L3_def_stuff_rate),mean(adv_stats_total$L3_def_stuff_rate, na.rm = TRUE),adv_stats_total$L3_def_stuff_rate)
adv_stats_total$L3_def_line_yds <- ifelse(is.na(adv_stats_total$L3_def_line_yds),mean(adv_stats_total$L3_def_line_yds, na.rm = TRUE),adv_stats_total$L3_def_line_yds)
adv_stats_total$L3_def_second_lvl_yds <- ifelse(is.na(adv_stats_total$L3_def_second_lvl_yds),mean(adv_stats_total$L3_def_second_lvl_yds, na.rm = TRUE),adv_stats_total$L3_def_second_lvl_yds)
adv_stats_total$L3_def_open_field_yds <- ifelse(is.na(adv_stats_total$L3_def_open_field_yds),mean(adv_stats_total$L3_def_open_field_yds, na.rm = TRUE),adv_stats_total$L3_def_open_field_yds)
adv_stats_total$L3_def_pts_per_opp <- ifelse(is.na(adv_stats_total$L3_def_pts_per_opp),mean(adv_stats_total$L3_def_pts_per_opp, na.rm = TRUE),adv_stats_total$L3_def_pts_per_opp)
adv_stats_total$L3def_field_pos_avg_predicted_points <- ifelse(is.na(adv_stats_total$L3def_field_pos_avg_predicted_points),mean(adv_stats_total$L3def_field_pos_avg_predicted_points, na.rm = TRUE),adv_stats_total$L3def_field_pos_avg_predicted_points)
adv_stats_total$L3_def_standard_downs_rate <- ifelse(is.na(adv_stats_total$L3_def_standard_downs_rate),mean(adv_stats_total$L3_def_standard_downs_rate, na.rm = TRUE),adv_stats_total$L3_def_standard_downs_rate)
adv_stats_total$L3_def_standard_downs_ppa <- ifelse(is.na(adv_stats_total$L3_def_standard_downs_ppa),mean(adv_stats_total$L3_def_standard_downs_ppa, na.rm = TRUE),adv_stats_total$L3_def_standard_downs_ppa)
adv_stats_total$L3_def_standard_downs_success_rate <- ifelse(is.na(adv_stats_total$L3_def_standard_downs_success_rate),mean(adv_stats_total$L3_def_standard_downs_success_rate, na.rm = TRUE),adv_stats_total$L3_def_standard_downs_success_rate)
adv_stats_total$L3_def_standard_downs_explosiveness <- ifelse(is.na(adv_stats_total$L3_def_standard_downs_explosiveness),mean(adv_stats_total$L3_def_standard_downs_explosiveness, na.rm = TRUE),adv_stats_total$L3_def_standard_downs_explosiveness)
adv_stats_total$L3_def_passing_downs_rate <- ifelse(is.na(adv_stats_total$L3_def_passing_downs_rate),mean(adv_stats_total$L3_def_passing_downs_rate, na.rm = TRUE),adv_stats_total$L3_def_passing_downs_rate)
adv_stats_total$L3_def_passing_downs_ppa <- ifelse(is.na(adv_stats_total$L3_def_passing_downs_ppa),mean(adv_stats_total$L3_def_passing_downs_ppa, na.rm = TRUE),adv_stats_total$L3_def_passing_downs_ppa)
adv_stats_total$L3_def_passing_downs_success_rate <- ifelse(is.na(adv_stats_total$L3_def_passing_downs_success_rate),mean(adv_stats_total$L3_def_passing_downs_success_rate, na.rm = TRUE),adv_stats_total$L3_def_passing_downs_success_rate)
adv_stats_total$L3_def_passing_downs_explosiveness <- ifelse(is.na(adv_stats_total$L3_def_passing_downs_explosiveness),mean(adv_stats_total$L3_def_passing_downs_explosiveness, na.rm = TRUE),adv_stats_total$L3_def_passing_downs_explosiveness)
adv_stats_total$L3_def_rushing_plays_rate <- ifelse(is.na(adv_stats_total$L3_def_rushing_plays_rate),mean(adv_stats_total$L3_def_rushing_plays_rate, na.rm = TRUE),adv_stats_total$L3_def_rushing_plays_rate)
adv_stats_total$L3_def_rushing_plays_ppa <- ifelse(is.na(adv_stats_total$L3_def_rushing_plays_ppa),mean(adv_stats_total$L3_def_rushing_plays_ppa, na.rm = TRUE),adv_stats_total$L3_def_rushing_plays_ppa)
adv_stats_total$L3_def_rushing_plays_success_rate <- ifelse(is.na(adv_stats_total$L3_def_rushing_plays_success_rate),mean(adv_stats_total$L3_def_rushing_plays_success_rate, na.rm = TRUE),adv_stats_total$L3_def_rushing_plays_success_rate)
adv_stats_total$L3_def_rushing_plays_explosiveness <- ifelse(is.na(adv_stats_total$L3_def_rushing_plays_explosiveness),mean(adv_stats_total$L3_def_rushing_plays_explosiveness, na.rm = TRUE),adv_stats_total$L3_def_rushing_plays_explosiveness)
adv_stats_total$L3_def_passing_plays_rate <- ifelse(is.na(adv_stats_total$L3_def_passing_plays_rate),mean(adv_stats_total$L3_def_passing_plays_rate, na.rm = TRUE),adv_stats_total$L3_def_passing_plays_rate)
adv_stats_total$L3_def_passing_plays_ppa <- ifelse(is.na(adv_stats_total$L3_def_passing_plays_ppa),mean(adv_stats_total$L3_def_passing_plays_ppa, na.rm = TRUE),adv_stats_total$L3_def_passing_plays_ppa)
adv_stats_total$L3_def_passing_plays_success_rate <- ifelse(is.na(adv_stats_total$L3_def_passing_plays_success_rate),mean(adv_stats_total$L3_def_passing_plays_success_rate, na.rm = TRUE),adv_stats_total$L3_def_passing_plays_success_rate)
adv_stats_total$L3_def_passing_plays_explosiveness <- ifelse(is.na(adv_stats_total$L3_def_passing_plays_explosiveness),mean(adv_stats_total$L3_def_passing_plays_explosiveness, na.rm = TRUE),adv_stats_total$L3_def_passing_plays_explosiveness)


cfbd_game_info_total <- left_join(cfbd_game_info_total, adv_stats_total)




cfbd_game_team_stats_total <- read.csv('2021_cfbd_game_team_stats_total.csv')
cfbd_game_team_stats_total <- cfbd_game_team_stats_total[,c(2,3,5,11,17)]

i = currentweek-1
j = year
while(j <= year){
  for(i in 1:i)
    tryCatch({
      cfbd_game_team_stats <- cfbd_game_team_stats(year = j,week = i)
      cfbd_game_team_stats <- cfbd_game_team_stats[,c(1,2,4,10,16)]
      
      assign(paste0('cfbd_game_team_stats',i,"_",j),cfbd_game_team_stats)
      
      cfbd_game_team_stats_total <- rbind(cfbd_game_team_stats_total,cfbd_game_team_stats)
      
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
  j <- j + 1
  
}

cfbd_game_team_stats_total <- separate(data = cfbd_game_team_stats_total, col = completion_attempts,c('completions','attempts'),sep = '-')
cfbd_game_team_stats_total <- cfbd_game_team_stats_total %>% drop_na()
ra <- as.data.frame(unlist(cfbd_game_team_stats_total$rushing_attempts))
colnames(ra) <- c('ra')
cfbd_game_team_stats_total <- cbind(cfbd_game_team_stats_total[,c(1:5)],ra)
cfbd_game_team_stats_total$ra <- as.numeric(cfbd_game_team_stats_total$ra)
cfbd_game_team_stats_total$attempts <- as.numeric(cfbd_game_team_stats_total$attempts)
cfbd_game_team_stats_total$total_plays <- cfbd_game_team_stats_total$attempts + cfbd_game_team_stats_total$ra
cfbd_game_team_stats_total$run_perc <- cfbd_game_team_stats_total$ra / cfbd_game_team_stats_total$total_plays
cfbd_game_team_stats_total$pass_perc <- cfbd_game_team_stats_total$attempts / cfbd_game_team_stats_total$total_plays

cfbd_game_team_stats_total_away <- filter(cfbd_game_team_stats_total, home_away == 'away')
cfbd_game_team_stats_total_home <- filter(cfbd_game_team_stats_total, home_away == 'home')
colnames(cfbd_game_team_stats_total_away) <- c('game_id','away_team','home_away','away_completions','away_attempts','away_ra','away_total_plays','away_run_perc','away_pass_perc')
colnames(cfbd_game_team_stats_total_home) <- c('game_id','home_team','home_away','home_completions','home_attempts','home_ra','home_total_plays','home_run_perc','home_pass_perc')
cfbd_game_team_stats_total_away <- cfbd_game_team_stats_total_away[,c(1,2,4:9)]
cfbd_game_team_stats_total_home <- cfbd_game_team_stats_total_home[,c(1,2,4:9)]
cfbd_game_team_stats_total <- left_join(cfbd_game_team_stats_total_home, cfbd_game_team_stats_total_away)

colnames(cfbd_game_team_stats_total) <- c('game_id','team','completions','attempts','ra','team_total_plays','run_perc','pass_perc','opp_team','opp_completions','opp_attempts','opp_ra','opp_total_plays','opp_run_perc','opp_pass_perc')

cfbd_game_team_stats_total1 <- cfbd_game_team_stats_total[,c(1,9:15,2:8)]
colnames(cfbd_game_team_stats_total1) <- c('game_id','team','completions','attempts','ra','team_total_plays','run_perc','pass_perc','opp_team','opp_completions','opp_attempts','opp_ra','opp_total_plays','opp_run_perc','opp_pass_perc')
cfbd_game_team_stats_total <- rbind(cfbd_game_team_stats_total,cfbd_game_team_stats_total1)

cfbd_game_team_stats_total <- left_join(cfbd_game_team_stats_total,team_names)
cfbd_game_info_total <- left_join(cfbd_game_info_total,cfbd_game_team_stats_total)

team_names <- read.csv('coach and pace names.csv')
team_names <- team_names[,c(1,8)]
colnames(team_names) <- c('prev_team','team')
team_names <- data.table(team_names)
team_names[, team := stri_trans_general(str = team, 
                                        id = "Latin-ASCII")]
team_names[, prev_team := stri_trans_general(str = prev_team, 
                                             id = "Latin-ASCII")]
prev_team <- cfbd_game_info_total

prev_team <- prev_team[,c(2,3,4,14,16,69:80)]

curr_team <- prev_team %>% filter(year == 2022)
prev_team <- prev_team %>% filter(year == 2021)

prev_team <- left_join(team_names,prev_team)
prev_team <- prev_team[,-c(2)]
prev_team <- rename(prev_team, team = prev_team)
prev_team <- rbind(prev_team,curr_team)



prev_team$tpp <- ifelse(is.na(prev_team$tpp),mean(prev_team$tpp, na.rm = TRUE),prev_team$tpp)
prev_team$opp_tpp <- ifelse(is.na(prev_team$opp_tpp),mean(prev_team$opp_tpp, na.rm = TRUE),prev_team$opp_tpp)
prev_team$ra <- ifelse(is.na(prev_team$ra),mean(prev_team$ra, na.rm = TRUE),prev_team$ra)
prev_team$run_perc <- ifelse(is.na(prev_team$run_perc),mean(prev_team$run_perc, na.rm = TRUE),prev_team$run_perc)
prev_team$pass_perc <- ifelse(is.na(prev_team$pass_perc),mean(prev_team$pass_perc, na.rm = TRUE),prev_team$pass_perc)
prev_team$opp_ra <- ifelse(is.na(prev_team$opp_ra),mean(prev_team$opp_ra, na.rm = TRUE),prev_team$opp_ra)
prev_team$opp_run_perc <- ifelse(is.na(prev_team$opp_run_perc),mean(prev_team$opp_run_perc, na.rm = TRUE),prev_team$opp_run_perc)
prev_team$opp_pass_perc <- ifelse(is.na(prev_team$opp_pass_perc),mean(prev_team$opp_pass_perc, na.rm = TRUE),prev_team$opp_pass_perc)
prev_team$attempts <- ifelse(is.na(prev_team$attempts),mean(prev_team$attempts, na.rm = TRUE),prev_team$attempts)
prev_team$opp_tpp <- ifelse(is.na(prev_team$opp_tpp),mean(prev_team$opp_tpp, na.rm = TRUE),prev_team$opp_tpp)



prev_team <- prev_team %>% 
  group_by(team) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_ra = rollapplyr(ra, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

prev_team <- prev_team %>% 
  group_by(team) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_run_perc = rollapplyr(run_perc, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

prev_team <- prev_team %>% 
  group_by(team) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_pass_perc = rollapplyr(pass_perc, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

prev_team <- prev_team %>% 
  group_by(team) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_opp_ra = rollapplyr(opp_ra, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

prev_team <- prev_team %>% 
  group_by(team) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_opp_run_perc = rollapplyr(opp_run_perc, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

prev_team <- prev_team %>% 
  group_by(team) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_opp_pass_perc = rollapplyr(opp_pass_perc, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

prev_team <- prev_team %>% 
  group_by(team) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_attempts = rollapplyr(attempts, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

cfbd_game_info_total <- cfbd_game_info_total %>% 
  group_by(team) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_pts = rollapplyr(pts, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

cfbd_game_info_total <- cfbd_game_info_total %>% 
  group_by(team) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_opp_pts = rollapplyr(opp_pts, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))


prev_team <- prev_team %>% 
  group_by(team) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_tpp = rollapplyr(tpp, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))


prev_team <- prev_team %>% 
  group_by(team) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_opp_tpp = rollapplyr(opp_tpp, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))




prev_team <- prev_team %>% 
  group_by(team) %>% 
  arrange(week) %>% 
  arrange(year) %>%
  slice(n())

prev_team <- prev_team[,c(1,18:26)]



library(RSelenium)
library(tidyverse)
library(cfbfastR)
library(data.table)
library(zoo)



cfbd_game_info_prev <- cfbd_game_info(year-1)
cfbd_game_info_curr <- cfbd_game_info(year)

sos_tot <- rbind(cfbd_game_info_prev,
                 cfbd_game_info_curr)



sos_tot <- sos_tot[,c(2,3,13,16,21,24)]
sos_tot1 <- sos_tot[,c(1,2,5,6,3,4)]
colnames(sos_tot) <- c('season','week','team','score','opp_team','opp_score')
colnames(sos_tot1) <- c('season','week','team','score','opp_team','opp_score')



sos_tot <- rbind(sos_tot1,sos_tot)
sos_tot <- sos_tot[complete.cases(sos_tot),]


sos_tot$win <- ifelse(sos_tot$score > sos_tot$opp_score,1,0)
sos_tot$loss <- ifelse(sos_tot$score > sos_tot$opp_score,0,1)
sos_tot <- sos_tot[!duplicated(sos_tot),]

sos_tot <- sos_tot %>% 
  group_by(team) %>% 
  arrange(week) %>% 
  arrange(season) %>% 
  mutate(L4_win_perc = rollapplyr(win, width = list(0:-4), align = 'right', fill = NA, FUN = mean, partial = TRUE))
sos_tot[is.na(sos_tot)] <- 0

opp_win_perc <- sos_tot[,c(1,2,3,5,9)]
colnames(opp_win_perc) <- c('season','week','opp_team','team','L4_opp_win_perc')
sos_tot <- left_join(sos_tot,opp_win_perc)
sos_tot[is.na(sos_tot)] <- 0


sos_tot <- sos_tot %>% 
  group_by(team) %>% 
  arrange(week) %>% 
  arrange(season) %>% 
  mutate(L4_opp_win_perc1 = rollapplyr(L4_opp_win_perc, width = list(0:-4), align = 'right', fill = NA, FUN = mean, partial = TRUE))
sos_tot[is.na(sos_tot)] <- 0


opp_opp_win_perc <- sos_tot[,c(1,2,3,5,11)]
colnames(opp_opp_win_perc) <- c('season','week','opp_team','team','L4_opp_opp_win_perc')
sos_tot <- left_join(sos_tot,opp_opp_win_perc)
sos_tot[is.na(sos_tot)] <- 0


sos_tot <- sos_tot %>% 
  group_by(team) %>% 
  arrange(week) %>% 
  arrange(season) %>% 
  mutate(L4_opp_opp_win_perc1 = rollapplyr(L4_opp_opp_win_perc, width = list(0:-4), align = 'right', fill = NA, FUN = mean, partial = TRUE))
sos_tot$L4_sos <- ((2*sos_tot$L4_opp_win_perc1) + (sos_tot$L4_opp_opp_win_perc1))/3

opp_sos <- sos_tot[,c(1,2,3,5,14)]
colnames(opp_sos) <- c('season','week','opp_team','team','L4_opp_sos')
sos_tot <- left_join(sos_tot,opp_sos)
sos_tot[is.na(sos_tot)] <- 0

sos <- sos_tot[,c(1,2,3,5,14,15)]
sos <- sos %>% filter(week <= currentweek | season != year)
sos<- rename(sos, year = season)



cfbd_game_info_total <- left_join(cfbd_game_info_total,sos)
cfbd_game_info_total <- cfbd_game_info_total[!is.na(cfbd_game_info_total$L3_off_ppa),]


cfbd_game_info_total <- cfbd_game_info_total %>% 
  group_by(team) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_ra = rollapplyr(ra, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

cfbd_game_info_total <- cfbd_game_info_total %>% 
  group_by(team) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_run_perc = rollapplyr(run_perc, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

cfbd_game_info_total <- cfbd_game_info_total %>% 
  group_by(team) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_pass_perc = rollapplyr(pass_perc, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

cfbd_game_info_total <- cfbd_game_info_total %>% 
  group_by(team) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(opp_L3_ra = rollapplyr(opp_ra, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

cfbd_game_info_total <- cfbd_game_info_total %>% 
  group_by(team) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(opp_L3_run_perc = rollapplyr(opp_run_perc, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

cfbd_game_info_total <- cfbd_game_info_total %>% 
  group_by(team) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(opp_L3_pass_perc = rollapplyr(opp_pass_perc, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

cfbd_game_info_total <- cfbd_game_info_total %>% 
  group_by(team) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_attempts = rollapplyr(attempts, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

cfbd_game_info_total <- cfbd_game_info_total %>% 
  group_by(team) %>% 
  arrange(week) %>%
  arrange(year) %>%
  mutate(L3_opp_attempts = rollapplyr(opp_attempts, width = list(0:-3), align = 'right', fill = NA, FUN = mean, partial = TRUE))

cfbd_game_info_total <- cfbd_game_info_total %>% 
  group_by(team) %>% 
  arrange(week) %>% 
  arrange(year) %>%
  slice(n())

cfbd_game_info_total$L3_avg_down <- ifelse(is.na(cfbd_game_info_total$L3_avg_down),mean(cfbd_game_info_total$L3_avg_down, na.rm = TRUE),cfbd_game_info_total$L3_avg_down)
cfbd_game_info_total$L3_avg_def_down <- ifelse(is.na(cfbd_game_info_total$L3_avg_def_down),mean(cfbd_game_info_total$L3_avg_def_down, na.rm = TRUE),cfbd_game_info_total$L3_avg_def_down)
cfbd_game_info_total$L3_avg_distance <- ifelse(is.na(cfbd_game_info_total$L3_avg_distance),mean(cfbd_game_info_total$L3_avg_distance, na.rm = TRUE),cfbd_game_info_total$L3_avg_distance)
cfbd_game_info_total$L3_avg_def_distance <- ifelse(is.na(cfbd_game_info_total$L3_avg_def_distance),mean(cfbd_game_info_total$L3_avg_def_distance, na.rm = TRUE),cfbd_game_info_total$L3_avg_def_distance)
cfbd_game_info_total$L3_avg_drive_efficiency <- ifelse(is.na(cfbd_game_info_total$L3_avg_drive_efficiency),mean(cfbd_game_info_total$L3_avg_drive_efficiency, na.rm = TRUE),cfbd_game_info_total$L3_avg_drive_efficiency)
cfbd_game_info_total$L3_avg_def_drive_efficiency <- ifelse(is.na(cfbd_game_info_total$L3_avg_def_drive_efficiency),mean(cfbd_game_info_total$L3_avg_def_drive_efficiency, na.rm = TRUE),cfbd_game_info_total$L3_avg_def_drive_efficiency)
cfbd_game_info_total$L3_tpp <- ifelse(is.na(cfbd_game_info_total$L3_tpp),mean(cfbd_game_info_total$L3_tpp, na.rm = TRUE),cfbd_game_info_total$L3_tpp)
cfbd_game_info_total$L3_opp_tpp <- ifelse(is.na(cfbd_game_info_total$L3_opp_tpp),mean(cfbd_game_info_total$L3_opp_tpp, na.rm = TRUE),cfbd_game_info_total$L3_opp_tpp)

cfbd_game_info_total$L3_off_ppa <- ifelse(is.na(cfbd_game_info_total$L3_off_ppa),mean(cfbd_game_info_total$L3_off_ppa, na.rm = TRUE),cfbd_game_info_total$L3_off_ppa)
cfbd_game_info_total$L3_off_success_rate <- ifelse(is.na(cfbd_game_info_total$L3_off_success_rate),mean(cfbd_game_info_total$L3_off_success_rate, na.rm = TRUE),cfbd_game_info_total$L3_off_success_rate)
cfbd_game_info_total$L3_off_explosiveness <- ifelse(is.na(cfbd_game_info_total$L3_off_explosiveness),mean(cfbd_game_info_total$L3_off_explosiveness, na.rm = TRUE),cfbd_game_info_total$L3_off_explosiveness)
cfbd_game_info_total$L3_off_power_success <- ifelse(is.na(cfbd_game_info_total$L3_off_power_success),mean(cfbd_game_info_total$L3_off_power_success, na.rm = TRUE),cfbd_game_info_total$L3_off_power_success)
cfbd_game_info_total$L3_off_stuff_rate <- ifelse(is.na(cfbd_game_info_total$L3_off_stuff_rate),mean(cfbd_game_info_total$L3_off_stuff_rate, na.rm = TRUE),cfbd_game_info_total$L3_off_stuff_rate)
cfbd_game_info_total$L3_off_line_yds <- ifelse(is.na(cfbd_game_info_total$L3_off_line_yds),mean(cfbd_game_info_total$L3_off_line_yds, na.rm = TRUE),cfbd_game_info_total$L3_off_line_yds)
cfbd_game_info_total$L3_off_second_lvl_yds <- ifelse(is.na(cfbd_game_info_total$L3_off_second_lvl_yds),mean(cfbd_game_info_total$L3_off_second_lvl_yds, na.rm = TRUE),cfbd_game_info_total$L3_off_second_lvl_yds)
cfbd_game_info_total$L3_off_open_field_yds <- ifelse(is.na(cfbd_game_info_total$L3_off_open_field_yds),mean(cfbd_game_info_total$L3_off_open_field_yds, na.rm = TRUE),cfbd_game_info_total$L3_off_open_field_yds)
cfbd_game_info_total$L3_off_field_pos_avg_predicted_points <- ifelse(is.na(cfbd_game_info_total$L3_off_field_pos_avg_predicted_points),mean(cfbd_game_info_total$L3_off_field_pos_avg_predicted_points, na.rm = TRUE),cfbd_game_info_total$L3_off_field_pos_avg_predicted_points)
cfbd_game_info_total$L3_off_standard_downs_rate <- ifelse(is.na(cfbd_game_info_total$L3_off_standard_downs_rate),mean(cfbd_game_info_total$L3_off_standard_downs_rate, na.rm = TRUE),cfbd_game_info_total$L3_off_standard_downs_rate)
cfbd_game_info_total$L3_off_standard_downs_ppa <- ifelse(is.na(cfbd_game_info_total$L3_off_standard_downs_ppa),mean(cfbd_game_info_total$L3_off_standard_downs_ppa, na.rm = TRUE),cfbd_game_info_total$L3_off_standard_downs_ppa)
cfbd_game_info_total$L3_off_standard_downs_success_rate <- ifelse(is.na(cfbd_game_info_total$L3_off_standard_downs_success_rate),mean(cfbd_game_info_total$L3_off_standard_downs_success_rate, na.rm = TRUE),cfbd_game_info_total$L3_off_standard_downs_success_rate)
cfbd_game_info_total$L3_off_standard_downs_explosiveness <- ifelse(is.na(cfbd_game_info_total$L3_off_standard_downs_explosiveness),mean(cfbd_game_info_total$L3_off_standard_downs_explosiveness, na.rm = TRUE),cfbd_game_info_total$L3_off_standard_downs_explosiveness)
cfbd_game_info_total$L3_off_passing_downs_rate <- ifelse(is.na(cfbd_game_info_total$L3_off_passing_downs_rate),mean(cfbd_game_info_total$L3_off_passing_downs_rate, na.rm = TRUE),cfbd_game_info_total$L3_off_passing_downs_rate)
cfbd_game_info_total$L3_off_passing_downs_ppa <- ifelse(is.na(cfbd_game_info_total$L3_off_passing_downs_ppa),mean(cfbd_game_info_total$L3_off_passing_downs_ppa, na.rm = TRUE),cfbd_game_info_total$L3_off_passing_downs_ppa)
cfbd_game_info_total$L3_off_passing_downs_success_rate <- ifelse(is.na(cfbd_game_info_total$L3_off_passing_downs_success_rate),mean(cfbd_game_info_total$L3_off_passing_downs_success_rate, na.rm = TRUE),cfbd_game_info_total$L3_off_passing_downs_success_rate)
cfbd_game_info_total$L3_off_passing_downs_explosiveness <- ifelse(is.na(cfbd_game_info_total$L3_off_passing_downs_explosiveness),mean(cfbd_game_info_total$L3_off_passing_downs_explosiveness, na.rm = TRUE),cfbd_game_info_total$L3_off_passing_downs_explosiveness)
cfbd_game_info_total$L3_off_rushing_plays_rate <- ifelse(is.na(cfbd_game_info_total$L3_off_rushing_plays_rate),mean(cfbd_game_info_total$L3_off_rushing_plays_rate, na.rm = TRUE),cfbd_game_info_total$L3_off_rushing_plays_rate)
cfbd_game_info_total$L3_off_rushing_plays_ppa <- ifelse(is.na(cfbd_game_info_total$L3_off_rushing_plays_ppa),mean(cfbd_game_info_total$L3_off_rushing_plays_ppa, na.rm = TRUE),cfbd_game_info_total$L3_off_rushing_plays_ppa)
cfbd_game_info_total$L3_off_rushing_plays_success_rate <- ifelse(is.na(cfbd_game_info_total$L3_off_rushing_plays_success_rate),mean(cfbd_game_info_total$L3_off_rushing_plays_success_rate, na.rm = TRUE),cfbd_game_info_total$L3_off_rushing_plays_success_rate)
cfbd_game_info_total$L3_off_rushing_plays_explosiveness <- ifelse(is.na(cfbd_game_info_total$L3_off_rushing_plays_explosiveness),mean(cfbd_game_info_total$L3_off_rushing_plays_explosiveness, na.rm = TRUE),cfbd_game_info_total$L3_off_rushing_plays_explosiveness)
cfbd_game_info_total$L3_off_passing_plays_rate <- ifelse(is.na(cfbd_game_info_total$L3_off_passing_plays_rate),mean(cfbd_game_info_total$L3_off_passing_plays_rate, na.rm = TRUE),cfbd_game_info_total$L3_off_passing_plays_rate)
cfbd_game_info_total$L3_off_passing_plays_ppa <- ifelse(is.na(cfbd_game_info_total$L3_off_passing_plays_ppa),mean(cfbd_game_info_total$L3_off_passing_plays_ppa, na.rm = TRUE),cfbd_game_info_total$L3_off_passing_plays_ppa)
cfbd_game_info_total$L3_off_passing_plays_success_rate <- ifelse(is.na(cfbd_game_info_total$L3_off_passing_plays_success_rate),mean(cfbd_game_info_total$L3_off_passing_plays_success_rate, na.rm = TRUE),cfbd_game_info_total$L3_off_passing_plays_success_rate)
cfbd_game_info_total$L3_off_passing_plays_explosiveness <- ifelse(is.na(cfbd_game_info_total$L3_off_passing_plays_explosiveness),mean(cfbd_game_info_total$L3_off_passing_plays_explosiveness, na.rm = TRUE),cfbd_game_info_total$L3_off_passing_plays_explosiveness)
cfbd_game_info_total$L3_def_ppa <- ifelse(is.na(cfbd_game_info_total$L3_def_ppa),mean(cfbd_game_info_total$L3_def_ppa, na.rm = TRUE),cfbd_game_info_total$L3_def_ppa)
cfbd_game_info_total$L3_def_success_rate <- ifelse(is.na(cfbd_game_info_total$L3_def_success_rate),mean(cfbd_game_info_total$L3_def_success_rate, na.rm = TRUE),cfbd_game_info_total$L3_def_success_rate)
cfbd_game_info_total$L3_def_explosiveness <- ifelse(is.na(cfbd_game_info_total$L3_def_explosiveness),mean(cfbd_game_info_total$L3_def_explosiveness, na.rm = TRUE),cfbd_game_info_total$L3_def_explosiveness)
cfbd_game_info_total$L3_def_power_success <- ifelse(is.na(cfbd_game_info_total$L3_def_power_success),mean(cfbd_game_info_total$L3_def_power_success, na.rm = TRUE),cfbd_game_info_total$L3_def_power_success)
cfbd_game_info_total$L3_def_stuff_rate <- ifelse(is.na(cfbd_game_info_total$L3_def_stuff_rate),mean(cfbd_game_info_total$L3_def_stuff_rate, na.rm = TRUE),cfbd_game_info_total$L3_def_stuff_rate)
cfbd_game_info_total$L3_def_line_yds <- ifelse(is.na(cfbd_game_info_total$L3_def_line_yds),mean(cfbd_game_info_total$L3_def_line_yds, na.rm = TRUE),cfbd_game_info_total$L3_def_line_yds)
cfbd_game_info_total$L3_def_second_lvl_yds <- ifelse(is.na(cfbd_game_info_total$L3_def_second_lvl_yds),mean(cfbd_game_info_total$L3_def_second_lvl_yds, na.rm = TRUE),cfbd_game_info_total$L3_def_second_lvl_yds)
cfbd_game_info_total$L3_def_open_field_yds <- ifelse(is.na(cfbd_game_info_total$L3_def_open_field_yds),mean(cfbd_game_info_total$L3_def_open_field_yds, na.rm = TRUE),cfbd_game_info_total$L3_def_open_field_yds)
cfbd_game_info_total$L3_def_pts_per_opp <- ifelse(is.na(cfbd_game_info_total$L3_def_pts_per_opp),mean(cfbd_game_info_total$L3_def_pts_per_opp, na.rm = TRUE),cfbd_game_info_total$L3_def_pts_per_opp)
cfbd_game_info_total$L3def_field_pos_avg_predicted_points <- ifelse(is.na(cfbd_game_info_total$L3def_field_pos_avg_predicted_points),mean(cfbd_game_info_total$L3def_field_pos_avg_predicted_points, na.rm = TRUE),cfbd_game_info_total$L3def_field_pos_avg_predicted_points)
cfbd_game_info_total$L3_def_standard_downs_rate <- ifelse(is.na(cfbd_game_info_total$L3_def_standard_downs_rate),mean(cfbd_game_info_total$L3_def_standard_downs_rate, na.rm = TRUE),cfbd_game_info_total$L3_def_standard_downs_rate)
cfbd_game_info_total$L3_def_standard_downs_ppa <- ifelse(is.na(cfbd_game_info_total$L3_def_standard_downs_ppa),mean(cfbd_game_info_total$L3_def_standard_downs_ppa, na.rm = TRUE),cfbd_game_info_total$L3_def_standard_downs_ppa)
cfbd_game_info_total$L3_def_standard_downs_success_rate <- ifelse(is.na(cfbd_game_info_total$L3_def_standard_downs_success_rate),mean(cfbd_game_info_total$L3_def_standard_downs_success_rate, na.rm = TRUE),cfbd_game_info_total$L3_def_standard_downs_success_rate)
cfbd_game_info_total$L3_def_standard_downs_explosiveness <- ifelse(is.na(cfbd_game_info_total$L3_def_standard_downs_explosiveness),mean(cfbd_game_info_total$L3_def_standard_downs_explosiveness, na.rm = TRUE),cfbd_game_info_total$L3_def_standard_downs_explosiveness)
cfbd_game_info_total$L3_def_passing_downs_rate <- ifelse(is.na(cfbd_game_info_total$L3_def_passing_downs_rate),mean(cfbd_game_info_total$L3_def_passing_downs_rate, na.rm = TRUE),cfbd_game_info_total$L3_def_passing_downs_rate)
cfbd_game_info_total$L3_def_passing_downs_ppa <- ifelse(is.na(cfbd_game_info_total$L3_def_passing_downs_ppa),mean(cfbd_game_info_total$L3_def_passing_downs_ppa, na.rm = TRUE),cfbd_game_info_total$L3_def_passing_downs_ppa)
cfbd_game_info_total$L3_def_passing_downs_success_rate <- ifelse(is.na(cfbd_game_info_total$L3_def_passing_downs_success_rate),mean(cfbd_game_info_total$L3_def_passing_downs_success_rate, na.rm = TRUE),cfbd_game_info_total$L3_def_passing_downs_success_rate)
cfbd_game_info_total$L3_def_passing_downs_explosiveness <- ifelse(is.na(cfbd_game_info_total$L3_def_passing_downs_explosiveness),mean(cfbd_game_info_total$L3_def_passing_downs_explosiveness, na.rm = TRUE),cfbd_game_info_total$L3_def_passing_downs_explosiveness)
cfbd_game_info_total$L3_def_rushing_plays_rate <- ifelse(is.na(cfbd_game_info_total$L3_def_rushing_plays_rate),mean(cfbd_game_info_total$L3_def_rushing_plays_rate, na.rm = TRUE),cfbd_game_info_total$L3_def_rushing_plays_rate)
cfbd_game_info_total$L3_def_rushing_plays_ppa <- ifelse(is.na(cfbd_game_info_total$L3_def_rushing_plays_ppa),mean(cfbd_game_info_total$L3_def_rushing_plays_ppa, na.rm = TRUE),cfbd_game_info_total$L3_def_rushing_plays_ppa)
cfbd_game_info_total$L3_def_rushing_plays_success_rate <- ifelse(is.na(cfbd_game_info_total$L3_def_rushing_plays_success_rate),mean(cfbd_game_info_total$L3_def_rushing_plays_success_rate, na.rm = TRUE),cfbd_game_info_total$L3_def_rushing_plays_success_rate)
cfbd_game_info_total$L3_def_rushing_plays_explosiveness <- ifelse(is.na(cfbd_game_info_total$L3_def_rushing_plays_explosiveness),mean(cfbd_game_info_total$L3_def_rushing_plays_explosiveness, na.rm = TRUE),cfbd_game_info_total$L3_def_rushing_plays_explosiveness)
cfbd_game_info_total$L3_def_passing_plays_rate <- ifelse(is.na(cfbd_game_info_total$L3_def_passing_plays_rate),mean(cfbd_game_info_total$L3_def_passing_plays_rate, na.rm = TRUE),cfbd_game_info_total$L3_def_passing_plays_rate)
cfbd_game_info_total$L3_def_passing_plays_ppa <- ifelse(is.na(cfbd_game_info_total$L3_def_passing_plays_ppa),mean(cfbd_game_info_total$L3_def_passing_plays_ppa, na.rm = TRUE),cfbd_game_info_total$L3_def_passing_plays_ppa)
cfbd_game_info_total$L3_def_passing_plays_success_rate <- ifelse(is.na(cfbd_game_info_total$L3_def_passing_plays_success_rate),mean(cfbd_game_info_total$L3_def_passing_plays_success_rate, na.rm = TRUE),cfbd_game_info_total$L3_def_passing_plays_success_rate)
cfbd_game_info_total$L3_def_passing_plays_explosiveness <- ifelse(is.na(cfbd_game_info_total$L3_def_passing_plays_explosiveness),mean(cfbd_game_info_total$L3_def_passing_plays_explosiveness, na.rm = TRUE),cfbd_game_info_total$L3_def_passing_plays_explosiveness)

cfbd_game_info_total$L3_pts <- ifelse(is.na(cfbd_game_info_total$L3_pts),mean(cfbd_game_info_total$L3_pts, na.rm = TRUE),cfbd_game_info_total$L3_pts)
cfbd_game_info_total$L3_opp_pts <- ifelse(is.na(cfbd_game_info_total$L3_opp_pts),mean(cfbd_game_info_total$L3_opp_pts, na.rm = TRUE),cfbd_game_info_total$L3_opp_pts)
cfbd_game_info_total$L4_sos <- ifelse(is.na(cfbd_game_info_total$L4_sos),mean(cfbd_game_info_total$L4_sos, na.rm = TRUE),cfbd_game_info_total$L4_sos)
cfbd_game_info_total$L4_opp_sos <- ifelse(is.na(cfbd_game_info_total$L4_opp_sos),mean(cfbd_game_info_total$L4_opp_sos, na.rm = TRUE),cfbd_game_info_total$L4_opp_sos)

cfbd_game_info_total <- cfbd_game_info_total[,-c(14:17,86:93)]



cfbd_game_info_total <- left_join(cfbd_game_info_total,prev_team)
cfbd_game_info_total1 <- cfbd_game_info_total[,c(2:3,4,8:64,78:90)]
cfbd_game_info_total1 <- left_join(cfbd_game_info_total1,cfbd_game_info_total1)

colnames(cfbd_game_info_total1) <- c("year", "week", "opp_team", "opp_L3_avg_down", "opp_L3_avg_def_down", "opp_L3_avg_distance", "opp_L3_avg_def_distance", "opp_L3_avg_drive_efficiency",
                                     "opp_L3_avg_def_drive_efficiency", "opp_L3_off_ppa", "opp_L3_off_success_rate", "opp_L3_off_explosiveness", "opp_L3_off_power_success", "opp_L3_off_stuff_rate", "opp_L3_off_line_yds",
                                     "opp_L3_off_second_lvl_yds", "opp_L3_off_open_field_yds", "opp_L3_off_field_pos_avg_predicted_points", "opp_L3_off_standard_downs_rate", "opp_L3_off_standard_downs_ppa",
                                     "opp_L3_off_standard_downs_success_rate", "opp_L3_off_standard_downs_explosiveness", "opp_L3_off_passing_downs_rate", "opp_L3_off_passing_downs_ppa",
                                     "opp_L3_off_passing_downs_success_rate", "opp_L3_off_passing_downs_explosiveness", "opp_L3_off_rushing_plays_rate", "opp_L3_off_rushing_plays_ppa",
                                     "opp_L3_off_rushing_plays_success_rate", "opp_L3_off_rushing_plays_explosiveness", "opp_L3_off_passing_plays_rate", "opp_L3_off_passing_plays_ppa",
                                     "opp_L3_off_passing_plays_success_rate", "opp_L3_off_passing_plays_explosiveness", "opp_L3_def_ppa", "opp_L3_def_success_rate", "opp_L3_def_explosiveness", "opp_L3_def_power_success",
                                     "opp_L3_def_stuff_rate", "opp_L3_def_line_yds", "opp_L3_def_second_lvl_yds", "opp_L3_def_open_field_yds", "opp_L3_def_pts_per_opp", "opp_L3def_field_pos_avg_predicted_points",
                                     "opp_L3_def_standard_downs_rate", "opp_L3_def_standard_downs_ppa", "opp_L3_def_standard_downs_success_rate", "opp_L3_def_standard_downs_explosiveness", "opp_L3_def_passing_downs_rate",
                                     "opp_L3_def_passing_downs_ppa", "opp_L3_def_passing_downs_success_rate", "opp_L3_def_passing_downs_explosiveness", "opp_L3_def_rushing_plays_rate", "opp_L3_def_rushing_plays_ppa",
                                     "opp_L3_def_rushing_plays_success_rate", "opp_L3_def_rushing_plays_explosiveness", "opp_L3_def_passing_plays_rate", "opp_L3_def_passing_plays_ppa",
                                     "opp_L3_def_passing_plays_success_rate", "opp_L3_def_passing_plays_explosiveness", "opp_L3_pts", "opp_L3_opp_pts", "opp_L4_sos", "opp_L4_opp_sos", "opp_L3_ra", "opp_L3_run_perc", "opp_L3_pass_perc",
                                     "opp_L3_opp_ra", "opp_L3_opp_run_perc", "opp_L3_opp_pass_perc", "opp_L3_attempts", "opp_L3_tpp", "opp_L3_opp_tpp")

cfbd_game_info_total <- cfbd_game_info_total[,c(4,8:64,78:90)]
cfbd_game_info_total1 <- cfbd_game_info_total1[,-c(1,2)]

colnames(week_games) <- c('team','opp_team')
week_games1 <- week_games[,c(2,1)]
colnames(week_games1) <- c('team','opp_team')
week_games <- rbind(week_games,week_games1)
week_games <- left_join(week_games,cfbd_game_info_total)
week_games <- left_join(week_games,cfbd_game_info_total1)

team_names <- read.csv('coach and pace names.csv')
team_names <- team_names[,c(1,4)]
colnames(team_names) <- c('team','TeamAbbrev')
team_names <- data.table(team_names)
team_names[, team := stri_trans_general(str = team, 
                                        id = "Latin-ASCII")]
week_games[, team := stri_trans_general(str = team, 
                                        id = "Latin-ASCII")]
week_games <- data.table(week_games)
week_games[, opp_team := stri_trans_general(str = opp_team, 
                                            id = "Latin-ASCII")]
opp_names <- team_names
colnames(opp_names) <- c('opp_team','dk_opp_abbr')

week_games <- left_join(week_games,team_names)
week_games <- left_join(week_games,opp_names)

imp_totals <- data.table(imp_totals)
imp_totals[, team := stri_trans_general(str = team, 
                                        id = "Latin-ASCII")]
week_games <- data.table(week_games)
week_games[, team := stri_trans_general(str = team, 
                                        id = "Latin-ASCII")]
week_games <- data.table(week_games)
week_games[, opp_team := stri_trans_general(str = opp_team, 
                                        id = "Latin-ASCII")]
week_games <- left_join(week_games,imp_totals)
colnames(imp_totals) <- c('opp_imp_team','opp_imp_totals','opp_team')
week_games <- left_join(week_games,imp_totals)

week_games$favorite <- ifelse(week_games$imp_totals > week_games$opp_imp_totals,1,0)
week_games1 <- week_games[,c("L3_attempts", "L3_ra", "L3_run_perc", "L3_pass_perc", "L3_opp_pts", "L3_pts", "L3_tpp",
                             "L3_off_line_yds", "L3_off_second_lvl_yds", "L3_off_open_field_yds", "L3_off_passing_downs_ppa",
                             "L3_off_passing_downs_success_rate", "L3_off_rushing_plays_rate", "L3_off_rushing_plays_ppa",
                             "L3_off_rushing_plays_explosiveness", "L3_off_passing_plays_rate", "L3_off_passing_plays_success_rate",
                             "L3_off_passing_plays_explosiveness", "L3_def_open_field_yds", "L3_avg_distance", "opp_L3_opp_run_perc",
                             "opp_L3_opp_pass_perc", "opp_L3_pts", "opp_L3_tpp", "opp_L3_off_ppa", "opp_L3_off_success_rate",
                             "opp_L3_off_explosiveness", "opp_L3_off_standard_downs_ppa", "opp_L3_off_standard_downs_success_rate",
                             "opp_L3_off_standard_downs_explosiveness", "opp_L3_off_passing_plays_ppa", "opp_L3_off_passing_plays_success_rate",
                             "opp_L3_def_line_yds", "opp_L3_def_rushing_plays_rate", "opp_L3_def_passing_plays_rate", "opp_L3_avg_def_distance",
                             "favorite")]

week_games1 <- week_games1[complete.cases(week_games1),]
week_games_predict <- predict(xgboost_p_att_model,week_games1)
week_games1$est_pa <- week_games_predict

week_games <- left_join(week_games,week_games1)
week_games1 <- week_games[,c(1,2,146,150)]

week_games2 <- week_games[,c("L3_attempts",                          
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


week_games2 <- week_games2[complete.cases(week_games2),]
week_games_predict <- predict(xgboost_r_att_model,week_games2)
week_games2$est_ra <- week_games_predict

week_games <- left_join(week_games,week_games2)
week_games2 <- week_games[,c(1,2,146,148,150,151)]
week_games2 <- week_games2[complete.cases(week_games2),]
write.csv(week_games2,"est_p_att_and_r_att_week_7.csv")

week_games <- data.table(week_games)

week_games[, TeamAbbrev := stri_trans_general(str = TeamAbbrev, 
                                              id = "Latin-ASCII")]
week_games[, dk_opp_abbr := stri_trans_general(str = dk_opp_abbr, 
                                              id = "Latin-ASCII")]
week_games <- week_games[,-c(64,70)]

current_slate_qb1 <- left_join(current_slate_qb,week_games)


cfbd_game_infoprev <- cfbd_game_info(year-1)
cfbd_game_infocurr <- cfbd_game_info(year)

opponents <- rbind(cfbd_game_infoprev,
                   cfbd_game_infocurr)



team_sacks <- read.csv('teams_sacks.csv')
team_sacks <- team_sacks[,-c(1,7:9)]

season <- year
i <- 1
cfbd_stats_season_player_totalseason <- data.frame()

while (i <= currentweek-1) {
  cfbd_stats_season_player1 <-  cfbd_stats_season_player(season,  team = "Air Force", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player2 <- cfbd_stats_season_player(season,  team = "Alabama", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player3 <-  cfbd_stats_season_player(season,  team = "Akron", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player4 <-  cfbd_stats_season_player(season,  team = "Appalachian State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player5 <-  cfbd_stats_season_player(season,  team = "Arizona", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player6 <-  cfbd_stats_season_player(season,  team = "Arizona State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player7 <-  cfbd_stats_season_player(season,  team = "Arkansas", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player8 <-  cfbd_stats_season_player(season,  team = "Arkansas State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player9 <-  cfbd_stats_season_player(season,  team = "Army", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player10 <-  cfbd_stats_season_player(season,  team = "Auburn", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player11 <-  cfbd_stats_season_player(season,  team = "Ball State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player12 <-  cfbd_stats_season_player(season,  team = "Baylor", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player13 <-  cfbd_stats_season_player(season,  team = "Boise State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player14 <-  cfbd_stats_season_player(season,  team = "Boston College", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player15 <-  cfbd_stats_season_player(season,  team = "Bowling Green", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player16 <-  cfbd_stats_season_player(season,  team = "Buffalo", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player17 <-  cfbd_stats_season_player(season,  team = "BYU", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player18 <-  cfbd_stats_season_player(season,  team = "California", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player19 <-  cfbd_stats_season_player(season,  team = "Central Michigan", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player20 <-  cfbd_stats_season_player(season,  team = "Charlotte", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player21 <-  cfbd_stats_season_player(season,  team = "Cincinnati", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player22 <-  cfbd_stats_season_player(season,  team = "Clemson", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player23 <-  cfbd_stats_season_player(season,  team = "Coastal Carolina", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player24 <-  cfbd_stats_season_player(season,  team = "Colorado", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player25 <-  cfbd_stats_season_player(season,  team = "Colorado State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player26 <-  cfbd_stats_season_player(season,  team = "Duke", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player27 <-  cfbd_stats_season_player(season,  team = "East Carolina", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player28 <-  cfbd_stats_season_player(season,  team = "Eastern Michigan", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player29 <-  cfbd_stats_season_player(season,  team = "Florida", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player30 <-  cfbd_stats_season_player(season,  team = "Florida Atlantic", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player31 <-  cfbd_stats_season_player(season,  team = "Florida International", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player32 <-  cfbd_stats_season_player(season,  team = "Florida State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player33 <-  cfbd_stats_season_player(season,  team = "Fresno State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player34 <-  cfbd_stats_season_player(season,  team = "Georgia", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player35 <-  cfbd_stats_season_player(season,  team = "Georgia Southern", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player36 <-  cfbd_stats_season_player(season,  team = "Georgia State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player37 <-  cfbd_stats_season_player(season,  team = "Georgia Tech", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player38 <-  cfbd_stats_season_player(season,  team = "Hawai'i", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player39 <-  cfbd_stats_season_player(season,  team = "Houston", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player40 <-  cfbd_stats_season_player(season,  team = "Illinois", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player41 <-  cfbd_stats_season_player(season,  team = "Indiana", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player42 <-  cfbd_stats_season_player(season,  team = "Iowa", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player43 <-  cfbd_stats_season_player(season,  team = "Iowa State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player44 <-  cfbd_stats_season_player(season,  team = "Kansas", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player45 <-  cfbd_stats_season_player(season,  team = "Kansas State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player46 <-  cfbd_stats_season_player(season,  team = "Kent State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player47 <-  cfbd_stats_season_player(season,  team = "Kentucky", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player48 <-  cfbd_stats_season_player(season,  team = "Liberty", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player49 <-  cfbd_stats_season_player(season,  team = "Louisiana", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player50 <-  cfbd_stats_season_player(season,  team = "Louisiana Monroe", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player51 <-  cfbd_stats_season_player(season,  team = "Louisiana Tech", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player52 <-  cfbd_stats_season_player(season,  team = "Louisville", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player53 <-  cfbd_stats_season_player(season,  team = "LSU", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player54 <-  cfbd_stats_season_player(season,  team = "Marshall", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player55 <-  cfbd_stats_season_player(season,  team = "Maryland", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player56 <-  cfbd_stats_season_player(season,  team = "Memphis", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player57 <-  cfbd_stats_season_player(season,  team = "Miami", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player58 <-  cfbd_stats_season_player(season,  team = "Miami (OH)", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player59 <-  cfbd_stats_season_player(season,  team = "Michigan", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player60 <-  cfbd_stats_season_player(season,  team = "Michigan State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player61 <-  cfbd_stats_season_player(season,  team = "Middle Tennessee", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player62 <-  cfbd_stats_season_player(season,  team = "Minnesota", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player63 <-  cfbd_stats_season_player(season,  team = "Mississippi State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player64 <-  cfbd_stats_season_player(season,  team = "Missouri", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player65 <-  cfbd_stats_season_player(season,  team = "Navy", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player66 <-  cfbd_stats_season_player(season,  team = "NC State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player67 <-  cfbd_stats_season_player(season,  team = "Nebraska", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player68 <-  cfbd_stats_season_player(season,  team = "Nevada", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player69 <-  cfbd_stats_season_player(season,  team = "New Mexico", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player70 <-  cfbd_stats_season_player(season,  team = "North Carolina", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player71 <-  cfbd_stats_season_player(season,  team = "Northern Illinois", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player72 <-  cfbd_stats_season_player(season,  team = "North Texas", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player73 <-  cfbd_stats_season_player(season,  team = "Northwestern", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player74 <-  cfbd_stats_season_player(season,  team = "Notre Dame", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player75 <-  cfbd_stats_season_player(season,  team = "Ohio", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player76 <-  cfbd_stats_season_player(season,  team = "Ohio State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player77 <-  cfbd_stats_season_player(season,  team = "Oklahoma", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player78 <-  cfbd_stats_season_player(season,  team = "Oklahoma State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player79 <-  cfbd_stats_season_player(season,  team = "Ole Miss", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player80 <-  cfbd_stats_season_player(season,  team = "Oregon", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player81 <-  cfbd_stats_season_player(season,  team = "Oregon State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player82 <-  cfbd_stats_season_player(season,  team = "Penn State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player83 <-  cfbd_stats_season_player(season,  team = "Pittsburgh", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player84 <-  cfbd_stats_season_player(season,  team = "Purdue", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player85 <-  cfbd_stats_season_player(season,  team = "Rice", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player86 <-  cfbd_stats_season_player(season,  team = "Rutgers", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player87 <-  cfbd_stats_season_player(season,  team = "San Diego State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player88 <-  cfbd_stats_season_player(season,  team = "San Jose State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player89 <-  cfbd_stats_season_player(season,  team = "SMU", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player90 <-  cfbd_stats_season_player(season,  team = "South Alabama", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player91 <-  cfbd_stats_season_player(season,  team = "South Carolina", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player92 <-  cfbd_stats_season_player(season,  team = "Southern Mississippi", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player93 <-  cfbd_stats_season_player(season,  team = "South Florida", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player94 <-  cfbd_stats_season_player(season,  team = "Stanford", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player95 <-  cfbd_stats_season_player(season,  team = "Syracuse", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player96 <-  cfbd_stats_season_player(season,  team = "TCU", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player97 <-  cfbd_stats_season_player(season,  team = "Temple", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player98 <-  cfbd_stats_season_player(season,  team = "Tennessee", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player99 <-  cfbd_stats_season_player(season,  team = "Texas", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player100 <-  cfbd_stats_season_player(season,  team = "Texas A&M", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player101 <-  cfbd_stats_season_player(season,  team = "Texas State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player102 <-  cfbd_stats_season_player(season,  team = "Texas Tech", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player103 <-  cfbd_stats_season_player(season,  team = "Toledo", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player104 <-  cfbd_stats_season_player(season,  team = "Troy", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player105 <-  cfbd_stats_season_player(season,  team = "Tulane", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player106 <-  cfbd_stats_season_player(season,  team = "Tulsa", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player107 <-  cfbd_stats_season_player(season,  team = "UAB", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player108 <-  cfbd_stats_season_player(season,  team = "UCF", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player109 <-  cfbd_stats_season_player(season,  team = "UCLA", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player110 <-  cfbd_stats_season_player(season,  team = "UMass", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player111 <-  cfbd_stats_season_player(season,  team = "UNLV", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player112 <-  cfbd_stats_season_player(season,  team = "USC", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player113 <-  cfbd_stats_season_player(season,  team = "Utah", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player114 <-  cfbd_stats_season_player(season,  team = "Utah State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player115 <-  cfbd_stats_season_player(season,  team = "UTEP", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player116 <-  cfbd_stats_season_player(season,  team = "UT San Antonio", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player117 <-  cfbd_stats_season_player(season,  team = "Vanderbilt", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player118 <-  cfbd_stats_season_player(season,  team = "Virginia", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player119 <-  cfbd_stats_season_player(season,  team = "Virginia Tech", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player120 <-  cfbd_stats_season_player(season,  team = "Wake Forest", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player121 <-  cfbd_stats_season_player(season,  team = "Washington", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player122 <-  cfbd_stats_season_player(season,  team = "Washington State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player123 <-  cfbd_stats_season_player(season,  team = "Western Kentucky", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player124 <-  cfbd_stats_season_player(season,  team = "Western Michigan", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player125 <-  cfbd_stats_season_player(season,  team = "West Virginia", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player126 <-  cfbd_stats_season_player(season,  team = "Wyoming", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player127 <-  cfbd_stats_season_player(season,  team = "Wisconsin", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player128 <-  cfbd_stats_season_player(season,  team = "Connecticut", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player129 <-  cfbd_stats_season_player(season,  team = "New Mexico State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player130 <-  cfbd_stats_season_player(season,  team = "Old Dominion", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player131 <-  cfbd_stats_season_player(season,  team = "James Madison", start_week = i, end_week = i, category = 'defensive')
  
  cfbd_stats_season_player_week <- bind_rows(cfbd_stats_season_player1, 
                                             cfbd_stats_season_player2, 
                                             cfbd_stats_season_player3, 
                                             cfbd_stats_season_player4, 
                                             cfbd_stats_season_player5, 
                                             cfbd_stats_season_player6, 
                                             cfbd_stats_season_player7, 
                                             cfbd_stats_season_player8, 
                                             cfbd_stats_season_player9, 
                                             cfbd_stats_season_player10, 
                                             cfbd_stats_season_player11, 
                                             cfbd_stats_season_player12, 
                                             cfbd_stats_season_player13, 
                                             cfbd_stats_season_player14, 
                                             cfbd_stats_season_player15, 
                                             cfbd_stats_season_player16, 
                                             cfbd_stats_season_player17, 
                                             cfbd_stats_season_player18, 
                                             cfbd_stats_season_player19, 
                                             cfbd_stats_season_player20, 
                                             cfbd_stats_season_player21, 
                                             cfbd_stats_season_player22, 
                                             cfbd_stats_season_player23, 
                                             cfbd_stats_season_player24, 
                                             cfbd_stats_season_player25, 
                                             cfbd_stats_season_player26, 
                                             cfbd_stats_season_player27, 
                                             cfbd_stats_season_player28, 
                                             cfbd_stats_season_player29, 
                                             cfbd_stats_season_player30, 
                                             cfbd_stats_season_player31, 
                                             cfbd_stats_season_player32, 
                                             cfbd_stats_season_player33, 
                                             cfbd_stats_season_player34, 
                                             cfbd_stats_season_player35, 
                                             cfbd_stats_season_player36, 
                                             cfbd_stats_season_player37, 
                                             cfbd_stats_season_player38, 
                                             cfbd_stats_season_player39, 
                                             cfbd_stats_season_player40, 
                                             cfbd_stats_season_player41, 
                                             cfbd_stats_season_player42, 
                                             cfbd_stats_season_player43, 
                                             cfbd_stats_season_player44, 
                                             cfbd_stats_season_player45, 
                                             cfbd_stats_season_player46, 
                                             cfbd_stats_season_player47, 
                                             cfbd_stats_season_player48, 
                                             cfbd_stats_season_player49, 
                                             cfbd_stats_season_player50, 
                                             cfbd_stats_season_player51, 
                                             cfbd_stats_season_player52, 
                                             cfbd_stats_season_player53, 
                                             cfbd_stats_season_player54, 
                                             cfbd_stats_season_player55, 
                                             cfbd_stats_season_player56, 
                                             cfbd_stats_season_player57, 
                                             cfbd_stats_season_player58, 
                                             cfbd_stats_season_player59, 
                                             cfbd_stats_season_player60, 
                                             cfbd_stats_season_player61, 
                                             cfbd_stats_season_player62, 
                                             cfbd_stats_season_player63, 
                                             cfbd_stats_season_player64, 
                                             cfbd_stats_season_player65, 
                                             cfbd_stats_season_player66, 
                                             cfbd_stats_season_player67, 
                                             cfbd_stats_season_player68, 
                                             cfbd_stats_season_player69, 
                                             cfbd_stats_season_player70, 
                                             cfbd_stats_season_player71, 
                                             cfbd_stats_season_player72, 
                                             cfbd_stats_season_player73, 
                                             cfbd_stats_season_player74, 
                                             cfbd_stats_season_player75, 
                                             cfbd_stats_season_player76, 
                                             cfbd_stats_season_player77, 
                                             cfbd_stats_season_player78, 
                                             cfbd_stats_season_player79, 
                                             cfbd_stats_season_player80, 
                                             cfbd_stats_season_player81, 
                                             cfbd_stats_season_player82, 
                                             cfbd_stats_season_player83, 
                                             cfbd_stats_season_player84, 
                                             cfbd_stats_season_player85, 
                                             cfbd_stats_season_player86, 
                                             cfbd_stats_season_player87, 
                                             cfbd_stats_season_player88, 
                                             cfbd_stats_season_player89, 
                                             cfbd_stats_season_player90, 
                                             cfbd_stats_season_player91, 
                                             cfbd_stats_season_player92, 
                                             cfbd_stats_season_player93, 
                                             cfbd_stats_season_player94, 
                                             cfbd_stats_season_player95, 
                                             cfbd_stats_season_player96, 
                                             cfbd_stats_season_player97, 
                                             cfbd_stats_season_player98, 
                                             cfbd_stats_season_player99, 
                                             cfbd_stats_season_player100, 
                                             cfbd_stats_season_player101, 
                                             cfbd_stats_season_player102, 
                                             cfbd_stats_season_player103, 
                                             cfbd_stats_season_player104, 
                                             cfbd_stats_season_player105, 
                                             cfbd_stats_season_player106, 
                                             cfbd_stats_season_player107, 
                                             cfbd_stats_season_player108, 
                                             cfbd_stats_season_player109, 
                                             cfbd_stats_season_player110, 
                                             cfbd_stats_season_player111, 
                                             cfbd_stats_season_player112, 
                                             cfbd_stats_season_player113, 
                                             cfbd_stats_season_player114, 
                                             cfbd_stats_season_player115, 
                                             cfbd_stats_season_player116, 
                                             cfbd_stats_season_player117, 
                                             cfbd_stats_season_player118, 
                                             cfbd_stats_season_player119, 
                                             cfbd_stats_season_player120, 
                                             cfbd_stats_season_player121, 
                                             cfbd_stats_season_player122, 
                                             cfbd_stats_season_player123, 
                                             cfbd_stats_season_player124, 
                                             cfbd_stats_season_player125, 
                                             cfbd_stats_season_player126, 
                                             cfbd_stats_season_player127, 
                                             cfbd_stats_season_player128, 
                                             cfbd_stats_season_player129, 
                                             cfbd_stats_season_player130,
                                             cfbd_stats_season_player131)
  cfbd_stats_season_player_week <- cfbd_stats_season_player_week[,c(1:5,26:30)]
  cfbd_stats_season_player_week$week <- i 
  
  
  assign(paste0("cfbd_stats_season_player_week",i),cfbd_stats_season_player_week)
  cfbd_stats_season_player_totalseason <- rbind(cfbd_stats_season_player_totalseason,assign(paste0("cfbd_stats_season_player_week",i),cfbd_stats_season_player_week))
  
  
  i <- i+1
}


team_sacks1 <- aggregate(defensive_sacks ~ team + week,data = cfbd_stats_season_player_totalseason,FUN = sum)
colnames(team_sacks1) <- c('team','week','team_sacks')
cfbd_stats_season_player_total <- left_join(cfbd_stats_season_player_totalseason,team_sacks1)

cfbd_stats_season_player_total$year <- season

teams_sacks <- cfbd_stats_season_player_total[,c(1,10:13)]
teams_hurries <- aggregate(defensive_qb_hur ~ team + week + year,data = teams_sacks,FUN = sum)
colnames(teams_hurries) <- c('team','week','year','team_hur')
teams_sacks <- teams_sacks[,-c(2)]
teams_sacks <- left_join(teams_sacks,teams_hurries)
teams_sacks <- teams_sacks[!duplicated(teams_sacks),]

colnames(teams_sacks) <- c('opp_team','week','opp_sacks','year','opp_hur')

team_sacks <- rbind(team_sacks,teams_sacks)

team_sacks <- team_sacks %>% 
  group_by(opp_team) %>% 
  arrange(week) %>% 
  arrange(year) %>%
  mutate(L3_sacks = rollapply(opp_sacks, width = list(0:-3), align = 'right', fill = NA, FUN = mean,partial = TRUE))

team_sacks <- team_sacks %>% 
  group_by(opp_team) %>% 
  arrange(week) %>% 
  arrange(year) %>%
  mutate(L3_hurries = rollapply(opp_hur, width = list(0:-3), align = 'right', fill = NA, FUN = mean,partial = TRUE))

team_sacks <- team_sacks %>% 
  group_by(opp_team) %>% 
  arrange(week) %>% 
  arrange(year) %>%
  slice(n())

team_sacks <- team_sacks[,-c(2:5)]
team_sacks <- data.table(team_sacks)

team_sacks[, opp_team := stri_trans_general(str = opp_team, 
                                              id = "Latin-ASCII")]

current_slate_qb1 <- left_join(current_slate_qb1,team_sacks)
current_slate_qb <- left_join(current_slate_qb,current_slate_qb1)

current_slate_qb1 <- current_slate_qb1[,c("L3_runshare",                         
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

current_slate_qb1 <- current_slate_qb1[complete.cases(current_slate_qb1),]
qb_runshare_predict <- predict(xgboost_qb_share_model,current_slate_qb1)
current_slate_qb2 <- current_slate_qb1[complete.cases(current_slate_qb1),]
current_slate_qb2$est_rshare <- qb_runshare_predict
current_slate_qb <- left_join(current_slate_qb,current_slate_qb2)
current_slate_qb$string <- ifelse(current_slate_qb$string != 1, 0, 1)
current_slate_qb$est_rshare <- current_slate_qb$est_rshare * current_slate_qb$string
current_slate_qb$est_pa <- current_slate_qb$est_pa * current_slate_qb$string
current_slate_qb$est_ra <- current_slate_qb$est_ra * current_slate_qb$est_rshare
current_slate_qb$L3_dk_pts <- current_slate_qb$L3_pa_dkpts + current_slate_qb$L3_ru_dkpts
current_slate_qb <- rename(current_slate_qb,L3_ra = L3_rush_att)
current_slate_qb <- rename(current_slate_qb,implied_total = imp_totals)
current_slate_qb <- rename(current_slate_qb,opp_implied_total = opp_imp_totals)


current_slate_qb1 <- current_slate_qb[,c("string", "L3_attempts", "L3_completion_percent",
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

current_slate_qb1 <- current_slate_qb1[complete.cases(current_slate_qb1),]
qb_stats_predict <- predict(xgboost_qb_stats_model,current_slate_qb1)
current_slate_qb2 <- current_slate_qb1[complete.cases(current_slate_qb1),]
current_slate_qb2$est_dkpts <- qb_stats_predict
current_slate_qb <- left_join(current_slate_qb,current_slate_qb2)
current_slate_qb1 <- current_slate_qb[,c(2,4,5,6,169,171,173,174,179)]
current_slate_qb1 <- current_slate_qb1[!duplicated(current_slate_qb1),]

current_slate_rb1 <- left_join(current_slate_rb,week_games)

current_slate_rb1 <- left_join(current_slate_rb1,team_sacks)
current_slate_rb2 <- left_join(current_slate_rb,current_slate_rb1)
current_slate_rb2 <- current_slate_rb2[complete.cases(current_slate_rb2),]

rb_share_predict <- predict(xgboost_rb_share_model,current_slate_rb2)

rb_usage_predict <- predict(xgboost_rb_usage_model,current_slate_rb2)
current_slate_rb2$est_rshare <- predict(xgboost_rb_share_model, current_slate_rb2)
current_slate_rb2$est_rusage <- predict(xgboost_rb_usage_model, current_slate_rb2)
current_slate_rb2$est_ra <- current_slate_rb2$est_ra * current_slate_rb2$est_rshare
current_slate_rb2$est_re <- current_slate_rb2$est_pa * current_slate_rb2$est_rusage
current_slate_rb1 <- left_join(current_slate_rb,current_slate_rb2)
current_slate_rb1 <- rename(current_slate_rb1, implied_total = imp_totals)
current_slate_rb1 <- rename(current_slate_rb1, opp_implied_total = opp_imp_totals)


current_slate_rb2 <- current_slate_rb1[,c("string", "L3_attempts", "L3_first_downs", "L3_fumbles",
                                          "L3_longest", "L3_touchdowns", "L3_yards", "L3_ypa", "L3_dk_pts", "L3_runshare",
                                          "L3_avg_down", "L3_avg_def_distance", "L3_avg_drive_efficiency", "implied_total",
                                          "opp_implied_total", "favorite", "L3_def_success_rate", "L3_def_stuff_rate",
                                          "L3_def_line_yds", "L3_def_second_lvl_yds", "L3_def_standard_downs_rate",
                                          "L3_def_passing_downs_rate", "L3_def_passing_downs_ppa",
                                          "L3_def_passing_downs_success_rate", "L3_def_rushing_plays_rate",
                                          "L3_def_rushing_plays_ppa", "L3_def_rushing_plays_success_rate",
                                          "L3_def_passing_plays_rate", "est_ra", "est_re")]

current_slate_rb2 <- current_slate_rb2[complete.cases(current_slate_rb2),]
rb_stats_predict <- predict(xgboost_rb_stats_model,current_slate_rb2)
current_slate_rb2$est_dkpts <- rb_stats_predict
current_slate_rb1 <- left_join(current_slate_rb,current_slate_rb2)
current_slate_rb1 <- current_slate_rb1[,c(2,4,5,6,24,31,32,46,47,48)]

current_slate_wr1 <- left_join(current_slate_wr,week_games)

current_slate_wr1 <- left_join(current_slate_wr1,team_sacks)
current_slate_wr2 <- left_join(current_slate_wr,current_slate_wr1)
current_slate_wr2 <- current_slate_wr2[complete.cases(current_slate_wr2),]
wr_share_predict <- predict(xgboost_wr_share_model,current_slate_wr2)

current_slate_wr2$est_wr_share <- wr_share_predict
current_slate_wr2$est_tar <- current_slate_wr2$est_pa * current_slate_wr2$est_wr_share
current_slate_wr1 <- left_join(current_slate_wr,current_slate_wr2)
current_slate_wr1 <- rename(current_slate_wr1, implied_total = imp_totals)
current_slate_wr1 <- rename(current_slate_wr1, opp_implied_total = opp_imp_totals)
current_slate_wr2 <- current_slate_wr1[,c("string", "L3_rec_usage", "L3_targets", "L3_avg_down",
                                          "L3_avg_drive_efficiency", "implied_total", "favorite", "est_wr_share", "est_tar")]

current_slate_wr2 <- current_slate_wr2[complete.cases(current_slate_wr2),]
wr_stats_predict <- predict(xgboost_wr_stats_model,current_slate_wr2)
current_slate_wr2$est_dkpts <- wr_stats_predict
current_slate_wr1 <- left_join(current_slate_wr1,current_slate_wr2)
current_slate_wr1 <- current_slate_wr1[,c(2,4,5,6,14,158,160,166:168)]













setwd("~/Documents/CFB/Slate Projections")



current_slate_qb1 <- current_slate_qb1[complete.cases(current_slate_qb1),]
current_slate_qb1$value <- current_slate_qb1$est_dkpts / (current_slate_qb1$Salary/1000)
current_slate_qb1 <- current_slate_qb1 %>% filter(est_pa != 0)
write.csv(current_slate_qb1,'dk_qb_proj_10-12-22_main.csv')

current_slate_rb1 <- current_slate_rb1[complete.cases(current_slate_rb1),]
current_slate_rb1$value <- current_slate_rb1$est_dkpts / (current_slate_rb1$Salary/1000)
write.csv(current_slate_rb1,'dk_rb_proj-10-12-22_main.csv')

current_slate_wr1 <- current_slate_wr1[complete.cases(current_slate_wr1),]
current_slate_wr1$value <- current_slate_wr1$est_dkpts / (current_slate_wr1$Salary/1000)
write.csv(current_slate_wr1,'dk_wr_projections_10-12-22_main.csv')


