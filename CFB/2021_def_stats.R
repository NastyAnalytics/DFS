
library(cfbfastR)
library(tidyverse)

library(RSelenium)
library(data.table)

cfbd_game_info2004 <- cfbd_game_info(2004)
cfbd_game_info2005 <- cfbd_game_info(2005)
cfbd_game_info2006 <- cfbd_game_info(2006)
cfbd_game_info2007 <- cfbd_game_info(2007)
cfbd_game_info2008 <- cfbd_game_info(2008)
cfbd_game_info2009 <- cfbd_game_info(2009)
cfbd_game_info2010 <- cfbd_game_info(2010)
cfbd_game_info2011 <- cfbd_game_info(2011)
cfbd_game_info2012 <- cfbd_game_info(2012)
cfbd_game_info2013 <- cfbd_game_info(2013)
cfbd_game_info2014 <- cfbd_game_info(2014)
cfbd_game_info2015 <- cfbd_game_info(2015)
cfbd_game_info2016 <- cfbd_game_info(2016)
cfbd_game_info2017 <- cfbd_game_info(2017)
cfbd_game_info2018 <- cfbd_game_info(2018)
cfbd_game_info2019 <- cfbd_game_info(2019)
cfbd_game_info2020 <- cfbd_game_info(2020)
cfbd_game_info2021 <- cfbd_game_info(2021)

cfbd_game_info_total <- rbind(cfbd_game_info2004,
                              cfbd_game_info2005,
                              cfbd_game_info2006,
                              cfbd_game_info2007,
                              cfbd_game_info2008,
                              cfbd_game_info2009,
                              cfbd_game_info2010,
                              cfbd_game_info2011,
                              cfbd_game_info2012,
                              cfbd_game_info2013,
                              cfbd_game_info2014,
                              cfbd_game_info2015,
                              cfbd_game_info2016,
                              cfbd_game_info2017,
                              cfbd_game_info2018,
                              cfbd_game_info2019,
                              cfbd_game_info2020,
                              cfbd_game_info2021)



cfbd_game_info_total <- cfbd_game_info_total[,c(2,3,13,21)]
colnames(cfbd_game_info_total) <- c('year','week','team','opp_team')
cfbd_game_info_total1 <- cfbd_game_info_total
colnames(cfbd_game_info_total1) <- c('year','week','opp_team','team')
cfbd_game_info_total1 <- cfbd_game_info_total1[,c(1,2,4,3)]
cfbd_game_info_total <- rbind(cfbd_game_info_total,cfbd_game_info_total1)

setwd('~/Documents/CFB')

rosters1 <- cfbd_team_roster(2018)
rosters2 <- cfbd_team_roster(2019)
rosters3 <- cfbd_team_roster(2020)
rosters4 <- cfbd_team_roster(2021)
rosters <- rbind(rosters1,rosters2,rosters3,rosters4)
rosters <- rosters[,c(1,9)]

i <- 1
cfbd_stats_season_player_total2018 <- data.frame()

while (i < 15) {
  cfbd_stats_season_player1 <-  cfbd_stats_season_player(2018,  team = "Air Force", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player2 <- cfbd_stats_season_player(2018,  team = "Alabama", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player3 <-  cfbd_stats_season_player(2018,  team = "Akron", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player4 <-  cfbd_stats_season_player(2018,  team = "Appalachian State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player5 <-  cfbd_stats_season_player(2018,  team = "Arizona", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player6 <-  cfbd_stats_season_player(2018,  team = "Arizona State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player7 <-  cfbd_stats_season_player(2018,  team = "Arkansas", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player8 <-  cfbd_stats_season_player(2018,  team = "Arkansas State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player9 <-  cfbd_stats_season_player(2018,  team = "Army", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player10 <-  cfbd_stats_season_player(2018,  team = "Auburn", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player11 <-  cfbd_stats_season_player(2018,  team = "Ball State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player12 <-  cfbd_stats_season_player(2018,  team = "Baylor", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player13 <-  cfbd_stats_season_player(2018,  team = "Boise State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player14 <-  cfbd_stats_season_player(2018,  team = "Boston College", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player15 <-  cfbd_stats_season_player(2018,  team = "Bowling Green", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player16 <-  cfbd_stats_season_player(2018,  team = "Buffalo", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player17 <-  cfbd_stats_season_player(2018,  team = "BYU", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player18 <-  cfbd_stats_season_player(2018,  team = "California", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player19 <-  cfbd_stats_season_player(2018,  team = "Central Michigan", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player20 <-  cfbd_stats_season_player(2018,  team = "Charlotte", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player21 <-  cfbd_stats_season_player(2018,  team = "Cincinnati", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player22 <-  cfbd_stats_season_player(2018,  team = "Clemson", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player23 <-  cfbd_stats_season_player(2018,  team = "Coastal Carolina", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player24 <-  cfbd_stats_season_player(2018,  team = "Colorado", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player25 <-  cfbd_stats_season_player(2018,  team = "Colorado State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player26 <-  cfbd_stats_season_player(2018,  team = "Duke", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player27 <-  cfbd_stats_season_player(2018,  team = "East Carolina", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player28 <-  cfbd_stats_season_player(2018,  team = "Eastern Michigan", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player29 <-  cfbd_stats_season_player(2018,  team = "Florida", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player30 <-  cfbd_stats_season_player(2018,  team = "Florida Atlantic", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player31 <-  cfbd_stats_season_player(2018,  team = "Florida International", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player32 <-  cfbd_stats_season_player(2018,  team = "Florida State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player33 <-  cfbd_stats_season_player(2018,  team = "Fresno State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player34 <-  cfbd_stats_season_player(2018,  team = "Georgia", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player35 <-  cfbd_stats_season_player(2018,  team = "Georgia Southern", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player36 <-  cfbd_stats_season_player(2018,  team = "Georgia State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player37 <-  cfbd_stats_season_player(2018,  team = "Georgia Tech", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player38 <-  cfbd_stats_season_player(2018,  team = "Hawai'i", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player39 <-  cfbd_stats_season_player(2018,  team = "Houston", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player40 <-  cfbd_stats_season_player(2018,  team = "Illinois", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player41 <-  cfbd_stats_season_player(2018,  team = "Indiana", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player42 <-  cfbd_stats_season_player(2018,  team = "Iowa", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player43 <-  cfbd_stats_season_player(2018,  team = "Iowa State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player44 <-  cfbd_stats_season_player(2018,  team = "Kansas", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player45 <-  cfbd_stats_season_player(2018,  team = "Kansas State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player46 <-  cfbd_stats_season_player(2018,  team = "Kent State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player47 <-  cfbd_stats_season_player(2018,  team = "Kentucky", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player48 <-  cfbd_stats_season_player(2018,  team = "Liberty", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player49 <-  cfbd_stats_season_player(2018,  team = "Louisiana", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player50 <-  cfbd_stats_season_player(2018,  team = "Louisiana Monroe", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player51 <-  cfbd_stats_season_player(2018,  team = "Louisiana Tech", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player52 <-  cfbd_stats_season_player(2018,  team = "Louisville", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player53 <-  cfbd_stats_season_player(2018,  team = "LSU", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player54 <-  cfbd_stats_season_player(2018,  team = "Marshall", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player55 <-  cfbd_stats_season_player(2018,  team = "Maryland", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player56 <-  cfbd_stats_season_player(2018,  team = "Memphis", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player57 <-  cfbd_stats_season_player(2018,  team = "Miami", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player58 <-  cfbd_stats_season_player(2018,  team = "Miami (OH)", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player59 <-  cfbd_stats_season_player(2018,  team = "Michigan", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player60 <-  cfbd_stats_season_player(2018,  team = "Michigan State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player61 <-  cfbd_stats_season_player(2018,  team = "Middle Tennessee", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player62 <-  cfbd_stats_season_player(2018,  team = "Minnesota", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player63 <-  cfbd_stats_season_player(2018,  team = "Mississippi State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player64 <-  cfbd_stats_season_player(2018,  team = "Missouri", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player65 <-  cfbd_stats_season_player(2018,  team = "Navy", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player66 <-  cfbd_stats_season_player(2018,  team = "NC State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player67 <-  cfbd_stats_season_player(2018,  team = "Nebraska", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player68 <-  cfbd_stats_season_player(2018,  team = "Nevada", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player69 <-  cfbd_stats_season_player(2018,  team = "New Mexico", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player70 <-  cfbd_stats_season_player(2018,  team = "North Carolina", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player71 <-  cfbd_stats_season_player(2018,  team = "Northern Illinois", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player72 <-  cfbd_stats_season_player(2018,  team = "North Texas", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player73 <-  cfbd_stats_season_player(2018,  team = "Northwestern", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player74 <-  cfbd_stats_season_player(2018,  team = "Notre Dame", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player75 <-  cfbd_stats_season_player(2018,  team = "Ohio", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player76 <-  cfbd_stats_season_player(2018,  team = "Ohio State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player77 <-  cfbd_stats_season_player(2018,  team = "Oklahoma", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player78 <-  cfbd_stats_season_player(2018,  team = "Oklahoma State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player79 <-  cfbd_stats_season_player(2018,  team = "Ole Miss", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player80 <-  cfbd_stats_season_player(2018,  team = "Oregon", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player81 <-  cfbd_stats_season_player(2018,  team = "Oregon State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player82 <-  cfbd_stats_season_player(2018,  team = "Penn State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player83 <-  cfbd_stats_season_player(2018,  team = "Pittsburgh", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player84 <-  cfbd_stats_season_player(2018,  team = "Purdue", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player85 <-  cfbd_stats_season_player(2018,  team = "Rice", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player86 <-  cfbd_stats_season_player(2018,  team = "Rutgers", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player87 <-  cfbd_stats_season_player(2018,  team = "San Diego State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player88 <-  cfbd_stats_season_player(2018,  team = "San Jose State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player89 <-  cfbd_stats_season_player(2018,  team = "SMU", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player90 <-  cfbd_stats_season_player(2018,  team = "South Alabama", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player91 <-  cfbd_stats_season_player(2018,  team = "South Carolina", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player92 <-  cfbd_stats_season_player(2018,  team = "Southern Mississippi", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player93 <-  cfbd_stats_season_player(2018,  team = "South Florida", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player94 <-  cfbd_stats_season_player(2018,  team = "Stanford", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player95 <-  cfbd_stats_season_player(2018,  team = "Syracuse", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player96 <-  cfbd_stats_season_player(2018,  team = "TCU", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player97 <-  cfbd_stats_season_player(2018,  team = "Temple", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player98 <-  cfbd_stats_season_player(2018,  team = "Tennessee", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player99 <-  cfbd_stats_season_player(2018,  team = "Texas", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player100 <-  cfbd_stats_season_player(2018,  team = "Texas A&M", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player101 <-  cfbd_stats_season_player(2018,  team = "Texas State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player102 <-  cfbd_stats_season_player(2018,  team = "Texas Tech", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player103 <-  cfbd_stats_season_player(2018,  team = "Toledo", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player104 <-  cfbd_stats_season_player(2018,  team = "Troy", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player105 <-  cfbd_stats_season_player(2018,  team = "Tulane", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player106 <-  cfbd_stats_season_player(2018,  team = "Tulsa", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player107 <-  cfbd_stats_season_player(2018,  team = "UAB", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player108 <-  cfbd_stats_season_player(2018,  team = "UCF", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player109 <-  cfbd_stats_season_player(2018,  team = "UCLA", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player110 <-  cfbd_stats_season_player(2018,  team = "UMass", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player111 <-  cfbd_stats_season_player(2018,  team = "UNLV", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player112 <-  cfbd_stats_season_player(2018,  team = "USC", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player113 <-  cfbd_stats_season_player(2018,  team = "Utah", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player114 <-  cfbd_stats_season_player(2018,  team = "Utah State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player115 <-  cfbd_stats_season_player(2018,  team = "UTEP", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player116 <-  cfbd_stats_season_player(2018,  team = "UT San Antonio", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player117 <-  cfbd_stats_season_player(2018,  team = "Vanderbilt", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player118 <-  cfbd_stats_season_player(2018,  team = "Virginia", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player119 <-  cfbd_stats_season_player(2018,  team = "Virginia Tech", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player120 <-  cfbd_stats_season_player(2018,  team = "Wake Forest", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player121 <-  cfbd_stats_season_player(2018,  team = "Washington", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player122 <-  cfbd_stats_season_player(2018,  team = "Washington State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player123 <-  cfbd_stats_season_player(2018,  team = "Western Kentucky", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player124 <-  cfbd_stats_season_player(2018,  team = "Western Michigan", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player125 <-  cfbd_stats_season_player(2018,  team = "West Virginia", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player126 <-  cfbd_stats_season_player(2018,  team = "Wyoming", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player127 <-  cfbd_stats_season_player(2018,  team = "Wisconsin", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player128 <-  cfbd_stats_season_player(2018,  team = "Connecticut", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player129 <-  cfbd_stats_season_player(2018,  team = "New Mexico State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player130 <-  cfbd_stats_season_player(2018,  team = "Old Dominion", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player131 <-  cfbd_stats_season_player(2018,  team = "James Madison", start_week = i, end_week = i, category = 'defensive')
  
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
  cfbd_stats_season_player_total2018 <- rbind(cfbd_stats_season_player_total2018,assign(paste0("cfbd_stats_season_player_week",i),cfbd_stats_season_player_week))
  
  
  i <- i+1
}

team_sacks <- aggregate(defensive_sacks ~ team + week,data = cfbd_stats_season_player_total2018,FUN = sum)
colnames(team_sacks) <- c('team','week','team_sacks')
cfbd_stats_season_player_total2018 <- left_join(cfbd_stats_season_player_total2018,team_sacks)

cfbd_stats_season_player_total2018$year <- 2018


i <- 1
cfbd_stats_season_player_total2019 <- data.frame()

while (i < 15) {
  cfbd_stats_season_player1 <-  cfbd_stats_season_player(2019,  team = "Air Force", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player2 <- cfbd_stats_season_player(2019,  team = "Alabama", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player3 <-  cfbd_stats_season_player(2019,  team = "Akron", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player4 <-  cfbd_stats_season_player(2019,  team = "Appalachian State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player5 <-  cfbd_stats_season_player(2019,  team = "Arizona", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player6 <-  cfbd_stats_season_player(2019,  team = "Arizona State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player7 <-  cfbd_stats_season_player(2019,  team = "Arkansas", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player8 <-  cfbd_stats_season_player(2019,  team = "Arkansas State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player9 <-  cfbd_stats_season_player(2019,  team = "Army", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player10 <-  cfbd_stats_season_player(2019,  team = "Auburn", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player11 <-  cfbd_stats_season_player(2019,  team = "Ball State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player12 <-  cfbd_stats_season_player(2019,  team = "Baylor", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player13 <-  cfbd_stats_season_player(2019,  team = "Boise State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player14 <-  cfbd_stats_season_player(2019,  team = "Boston College", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player15 <-  cfbd_stats_season_player(2019,  team = "Bowling Green", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player16 <-  cfbd_stats_season_player(2019,  team = "Buffalo", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player17 <-  cfbd_stats_season_player(2019,  team = "BYU", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player18 <-  cfbd_stats_season_player(2019,  team = "California", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player19 <-  cfbd_stats_season_player(2019,  team = "Central Michigan", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player20 <-  cfbd_stats_season_player(2019,  team = "Charlotte", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player21 <-  cfbd_stats_season_player(2019,  team = "Cincinnati", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player22 <-  cfbd_stats_season_player(2019,  team = "Clemson", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player23 <-  cfbd_stats_season_player(2019,  team = "Coastal Carolina", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player24 <-  cfbd_stats_season_player(2019,  team = "Colorado", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player25 <-  cfbd_stats_season_player(2019,  team = "Colorado State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player26 <-  cfbd_stats_season_player(2019,  team = "Duke", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player27 <-  cfbd_stats_season_player(2019,  team = "East Carolina", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player28 <-  cfbd_stats_season_player(2019,  team = "Eastern Michigan", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player29 <-  cfbd_stats_season_player(2019,  team = "Florida", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player30 <-  cfbd_stats_season_player(2019,  team = "Florida Atlantic", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player31 <-  cfbd_stats_season_player(2019,  team = "Florida International", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player32 <-  cfbd_stats_season_player(2019,  team = "Florida State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player33 <-  cfbd_stats_season_player(2019,  team = "Fresno State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player34 <-  cfbd_stats_season_player(2019,  team = "Georgia", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player35 <-  cfbd_stats_season_player(2019,  team = "Georgia Southern", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player36 <-  cfbd_stats_season_player(2019,  team = "Georgia State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player37 <-  cfbd_stats_season_player(2019,  team = "Georgia Tech", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player38 <-  cfbd_stats_season_player(2019,  team = "Hawai'i", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player39 <-  cfbd_stats_season_player(2019,  team = "Houston", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player40 <-  cfbd_stats_season_player(2019,  team = "Illinois", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player41 <-  cfbd_stats_season_player(2019,  team = "Indiana", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player42 <-  cfbd_stats_season_player(2019,  team = "Iowa", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player43 <-  cfbd_stats_season_player(2019,  team = "Iowa State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player44 <-  cfbd_stats_season_player(2019,  team = "Kansas", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player45 <-  cfbd_stats_season_player(2019,  team = "Kansas State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player46 <-  cfbd_stats_season_player(2019,  team = "Kent State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player47 <-  cfbd_stats_season_player(2019,  team = "Kentucky", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player48 <-  cfbd_stats_season_player(2019,  team = "Liberty", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player49 <-  cfbd_stats_season_player(2019,  team = "Louisiana", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player50 <-  cfbd_stats_season_player(2019,  team = "Louisiana Monroe", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player51 <-  cfbd_stats_season_player(2019,  team = "Louisiana Tech", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player52 <-  cfbd_stats_season_player(2019,  team = "Louisville", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player53 <-  cfbd_stats_season_player(2019,  team = "LSU", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player54 <-  cfbd_stats_season_player(2019,  team = "Marshall", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player55 <-  cfbd_stats_season_player(2019,  team = "Maryland", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player56 <-  cfbd_stats_season_player(2019,  team = "Memphis", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player57 <-  cfbd_stats_season_player(2019,  team = "Miami", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player58 <-  cfbd_stats_season_player(2019,  team = "Miami (OH)", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player59 <-  cfbd_stats_season_player(2019,  team = "Michigan", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player60 <-  cfbd_stats_season_player(2019,  team = "Michigan State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player61 <-  cfbd_stats_season_player(2019,  team = "Middle Tennessee", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player62 <-  cfbd_stats_season_player(2019,  team = "Minnesota", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player63 <-  cfbd_stats_season_player(2019,  team = "Mississippi State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player64 <-  cfbd_stats_season_player(2019,  team = "Missouri", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player65 <-  cfbd_stats_season_player(2019,  team = "Navy", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player66 <-  cfbd_stats_season_player(2019,  team = "NC State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player67 <-  cfbd_stats_season_player(2019,  team = "Nebraska", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player68 <-  cfbd_stats_season_player(2019,  team = "Nevada", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player69 <-  cfbd_stats_season_player(2019,  team = "New Mexico", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player70 <-  cfbd_stats_season_player(2019,  team = "North Carolina", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player71 <-  cfbd_stats_season_player(2019,  team = "Northern Illinois", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player72 <-  cfbd_stats_season_player(2019,  team = "North Texas", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player73 <-  cfbd_stats_season_player(2019,  team = "Northwestern", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player74 <-  cfbd_stats_season_player(2019,  team = "Notre Dame", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player75 <-  cfbd_stats_season_player(2019,  team = "Ohio", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player76 <-  cfbd_stats_season_player(2019,  team = "Ohio State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player77 <-  cfbd_stats_season_player(2019,  team = "Oklahoma", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player78 <-  cfbd_stats_season_player(2019,  team = "Oklahoma State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player79 <-  cfbd_stats_season_player(2019,  team = "Ole Miss", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player80 <-  cfbd_stats_season_player(2019,  team = "Oregon", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player81 <-  cfbd_stats_season_player(2019,  team = "Oregon State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player82 <-  cfbd_stats_season_player(2019,  team = "Penn State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player83 <-  cfbd_stats_season_player(2019,  team = "Pittsburgh", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player84 <-  cfbd_stats_season_player(2019,  team = "Purdue", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player85 <-  cfbd_stats_season_player(2019,  team = "Rice", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player86 <-  cfbd_stats_season_player(2019,  team = "Rutgers", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player87 <-  cfbd_stats_season_player(2019,  team = "San Diego State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player88 <-  cfbd_stats_season_player(2019,  team = "San Jose State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player89 <-  cfbd_stats_season_player(2019,  team = "SMU", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player90 <-  cfbd_stats_season_player(2019,  team = "South Alabama", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player91 <-  cfbd_stats_season_player(2019,  team = "South Carolina", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player92 <-  cfbd_stats_season_player(2019,  team = "Southern Mississippi", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player93 <-  cfbd_stats_season_player(2019,  team = "South Florida", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player94 <-  cfbd_stats_season_player(2019,  team = "Stanford", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player95 <-  cfbd_stats_season_player(2019,  team = "Syracuse", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player96 <-  cfbd_stats_season_player(2019,  team = "TCU", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player97 <-  cfbd_stats_season_player(2019,  team = "Temple", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player98 <-  cfbd_stats_season_player(2019,  team = "Tennessee", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player99 <-  cfbd_stats_season_player(2019,  team = "Texas", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player100 <-  cfbd_stats_season_player(2019,  team = "Texas A&M", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player101 <-  cfbd_stats_season_player(2019,  team = "Texas State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player102 <-  cfbd_stats_season_player(2019,  team = "Texas Tech", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player103 <-  cfbd_stats_season_player(2019,  team = "Toledo", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player104 <-  cfbd_stats_season_player(2019,  team = "Troy", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player105 <-  cfbd_stats_season_player(2019,  team = "Tulane", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player106 <-  cfbd_stats_season_player(2019,  team = "Tulsa", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player107 <-  cfbd_stats_season_player(2019,  team = "UAB", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player108 <-  cfbd_stats_season_player(2019,  team = "UCF", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player109 <-  cfbd_stats_season_player(2019,  team = "UCLA", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player110 <-  cfbd_stats_season_player(2019,  team = "UMass", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player111 <-  cfbd_stats_season_player(2019,  team = "UNLV", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player112 <-  cfbd_stats_season_player(2019,  team = "USC", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player113 <-  cfbd_stats_season_player(2019,  team = "Utah", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player114 <-  cfbd_stats_season_player(2019,  team = "Utah State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player115 <-  cfbd_stats_season_player(2019,  team = "UTEP", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player116 <-  cfbd_stats_season_player(2019,  team = "UT San Antonio", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player117 <-  cfbd_stats_season_player(2019,  team = "Vanderbilt", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player118 <-  cfbd_stats_season_player(2019,  team = "Virginia", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player119 <-  cfbd_stats_season_player(2019,  team = "Virginia Tech", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player120 <-  cfbd_stats_season_player(2019,  team = "Wake Forest", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player121 <-  cfbd_stats_season_player(2019,  team = "Washington", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player122 <-  cfbd_stats_season_player(2019,  team = "Washington State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player123 <-  cfbd_stats_season_player(2019,  team = "Western Kentucky", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player124 <-  cfbd_stats_season_player(2019,  team = "Western Michigan", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player125 <-  cfbd_stats_season_player(2019,  team = "West Virginia", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player126 <-  cfbd_stats_season_player(2019,  team = "Wyoming", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player127 <-  cfbd_stats_season_player(2019,  team = "Wisconsin", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player128 <-  cfbd_stats_season_player(2019,  team = "Connecticut", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player129 <-  cfbd_stats_season_player(2019,  team = "New Mexico State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player130 <-  cfbd_stats_season_player(2019,  team = "Old Dominion", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player131 <-  cfbd_stats_season_player(2019,  team = "James Madison", start_week = i, end_week = i, category = 'defensive')
  
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
  cfbd_stats_season_player_total2019 <- rbind(cfbd_stats_season_player_total2019,assign(paste0("cfbd_stats_season_player_week",i),cfbd_stats_season_player_week))
  
  
  i <- i+1
}

team_sacks <- aggregate(defensive_sacks ~ team + week,data = cfbd_stats_season_player_total2019,FUN = sum)
colnames(team_sacks) <- c('team','week','team_sacks')
cfbd_stats_season_player_total2019 <- left_join(cfbd_stats_season_player_total2019,team_sacks)

cfbd_stats_season_player_total2019$year <- 2019







i <- 1
cfbd_stats_season_player_total2020 <- data.frame()

while (i < 15) {
  cfbd_stats_season_player1 <-  cfbd_stats_season_player(2020,  team = "Air Force", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player2 <- cfbd_stats_season_player(2020,  team = "Alabama", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player3 <-  cfbd_stats_season_player(2020,  team = "Akron", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player4 <-  cfbd_stats_season_player(2020,  team = "Appalachian State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player5 <-  cfbd_stats_season_player(2020,  team = "Arizona", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player6 <-  cfbd_stats_season_player(2020,  team = "Arizona State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player7 <-  cfbd_stats_season_player(2020,  team = "Arkansas", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player8 <-  cfbd_stats_season_player(2020,  team = "Arkansas State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player9 <-  cfbd_stats_season_player(2020,  team = "Army", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player10 <-  cfbd_stats_season_player(2020,  team = "Auburn", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player11 <-  cfbd_stats_season_player(2020,  team = "Ball State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player12 <-  cfbd_stats_season_player(2020,  team = "Baylor", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player13 <-  cfbd_stats_season_player(2020,  team = "Boise State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player14 <-  cfbd_stats_season_player(2020,  team = "Boston College", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player15 <-  cfbd_stats_season_player(2020,  team = "Bowling Green", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player16 <-  cfbd_stats_season_player(2020,  team = "Buffalo", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player17 <-  cfbd_stats_season_player(2020,  team = "BYU", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player18 <-  cfbd_stats_season_player(2020,  team = "California", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player19 <-  cfbd_stats_season_player(2020,  team = "Central Michigan", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player20 <-  cfbd_stats_season_player(2020,  team = "Charlotte", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player21 <-  cfbd_stats_season_player(2020,  team = "Cincinnati", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player22 <-  cfbd_stats_season_player(2020,  team = "Clemson", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player23 <-  cfbd_stats_season_player(2020,  team = "Coastal Carolina", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player24 <-  cfbd_stats_season_player(2020,  team = "Colorado", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player25 <-  cfbd_stats_season_player(2020,  team = "Colorado State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player26 <-  cfbd_stats_season_player(2020,  team = "Duke", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player27 <-  cfbd_stats_season_player(2020,  team = "East Carolina", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player28 <-  cfbd_stats_season_player(2020,  team = "Eastern Michigan", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player29 <-  cfbd_stats_season_player(2020,  team = "Florida", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player30 <-  cfbd_stats_season_player(2020,  team = "Florida Atlantic", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player31 <-  cfbd_stats_season_player(2020,  team = "Florida International", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player32 <-  cfbd_stats_season_player(2020,  team = "Florida State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player33 <-  cfbd_stats_season_player(2020,  team = "Fresno State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player34 <-  cfbd_stats_season_player(2020,  team = "Georgia", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player35 <-  cfbd_stats_season_player(2020,  team = "Georgia Southern", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player36 <-  cfbd_stats_season_player(2020,  team = "Georgia State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player37 <-  cfbd_stats_season_player(2020,  team = "Georgia Tech", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player38 <-  cfbd_stats_season_player(2020,  team = "Hawai'i", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player39 <-  cfbd_stats_season_player(2020,  team = "Houston", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player40 <-  cfbd_stats_season_player(2020,  team = "Illinois", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player41 <-  cfbd_stats_season_player(2020,  team = "Indiana", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player42 <-  cfbd_stats_season_player(2020,  team = "Iowa", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player43 <-  cfbd_stats_season_player(2020,  team = "Iowa State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player44 <-  cfbd_stats_season_player(2020,  team = "Kansas", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player45 <-  cfbd_stats_season_player(2020,  team = "Kansas State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player46 <-  cfbd_stats_season_player(2020,  team = "Kent State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player47 <-  cfbd_stats_season_player(2020,  team = "Kentucky", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player48 <-  cfbd_stats_season_player(2020,  team = "Liberty", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player49 <-  cfbd_stats_season_player(2020,  team = "Louisiana", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player50 <-  cfbd_stats_season_player(2020,  team = "Louisiana Monroe", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player51 <-  cfbd_stats_season_player(2020,  team = "Louisiana Tech", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player52 <-  cfbd_stats_season_player(2020,  team = "Louisville", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player53 <-  cfbd_stats_season_player(2020,  team = "LSU", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player54 <-  cfbd_stats_season_player(2020,  team = "Marshall", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player55 <-  cfbd_stats_season_player(2020,  team = "Maryland", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player56 <-  cfbd_stats_season_player(2020,  team = "Memphis", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player57 <-  cfbd_stats_season_player(2020,  team = "Miami", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player58 <-  cfbd_stats_season_player(2020,  team = "Miami (OH)", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player59 <-  cfbd_stats_season_player(2020,  team = "Michigan", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player60 <-  cfbd_stats_season_player(2020,  team = "Michigan State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player61 <-  cfbd_stats_season_player(2020,  team = "Middle Tennessee", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player62 <-  cfbd_stats_season_player(2020,  team = "Minnesota", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player63 <-  cfbd_stats_season_player(2020,  team = "Mississippi State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player64 <-  cfbd_stats_season_player(2020,  team = "Missouri", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player65 <-  cfbd_stats_season_player(2020,  team = "Navy", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player66 <-  cfbd_stats_season_player(2020,  team = "NC State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player67 <-  cfbd_stats_season_player(2020,  team = "Nebraska", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player68 <-  cfbd_stats_season_player(2020,  team = "Nevada", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player69 <-  cfbd_stats_season_player(2020,  team = "New Mexico", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player70 <-  cfbd_stats_season_player(2020,  team = "North Carolina", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player71 <-  cfbd_stats_season_player(2020,  team = "Northern Illinois", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player72 <-  cfbd_stats_season_player(2020,  team = "North Texas", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player73 <-  cfbd_stats_season_player(2020,  team = "Northwestern", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player74 <-  cfbd_stats_season_player(2020,  team = "Notre Dame", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player75 <-  cfbd_stats_season_player(2020,  team = "Ohio", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player76 <-  cfbd_stats_season_player(2020,  team = "Ohio State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player77 <-  cfbd_stats_season_player(2020,  team = "Oklahoma", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player78 <-  cfbd_stats_season_player(2020,  team = "Oklahoma State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player79 <-  cfbd_stats_season_player(2020,  team = "Ole Miss", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player80 <-  cfbd_stats_season_player(2020,  team = "Oregon", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player81 <-  cfbd_stats_season_player(2020,  team = "Oregon State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player82 <-  cfbd_stats_season_player(2020,  team = "Penn State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player83 <-  cfbd_stats_season_player(2020,  team = "Pittsburgh", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player84 <-  cfbd_stats_season_player(2020,  team = "Purdue", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player85 <-  cfbd_stats_season_player(2020,  team = "Rice", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player86 <-  cfbd_stats_season_player(2020,  team = "Rutgers", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player87 <-  cfbd_stats_season_player(2020,  team = "San Diego State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player88 <-  cfbd_stats_season_player(2020,  team = "San Jose State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player89 <-  cfbd_stats_season_player(2020,  team = "SMU", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player90 <-  cfbd_stats_season_player(2020,  team = "South Alabama", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player91 <-  cfbd_stats_season_player(2020,  team = "South Carolina", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player92 <-  cfbd_stats_season_player(2020,  team = "Southern Mississippi", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player93 <-  cfbd_stats_season_player(2020,  team = "South Florida", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player94 <-  cfbd_stats_season_player(2020,  team = "Stanford", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player95 <-  cfbd_stats_season_player(2020,  team = "Syracuse", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player96 <-  cfbd_stats_season_player(2020,  team = "TCU", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player97 <-  cfbd_stats_season_player(2020,  team = "Temple", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player98 <-  cfbd_stats_season_player(2020,  team = "Tennessee", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player99 <-  cfbd_stats_season_player(2020,  team = "Texas", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player100 <-  cfbd_stats_season_player(2020,  team = "Texas A&M", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player101 <-  cfbd_stats_season_player(2020,  team = "Texas State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player102 <-  cfbd_stats_season_player(2020,  team = "Texas Tech", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player103 <-  cfbd_stats_season_player(2020,  team = "Toledo", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player104 <-  cfbd_stats_season_player(2020,  team = "Troy", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player105 <-  cfbd_stats_season_player(2020,  team = "Tulane", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player106 <-  cfbd_stats_season_player(2020,  team = "Tulsa", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player107 <-  cfbd_stats_season_player(2020,  team = "UAB", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player108 <-  cfbd_stats_season_player(2020,  team = "UCF", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player109 <-  cfbd_stats_season_player(2020,  team = "UCLA", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player110 <-  cfbd_stats_season_player(2020,  team = "UMass", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player111 <-  cfbd_stats_season_player(2020,  team = "UNLV", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player112 <-  cfbd_stats_season_player(2020,  team = "USC", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player113 <-  cfbd_stats_season_player(2020,  team = "Utah", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player114 <-  cfbd_stats_season_player(2020,  team = "Utah State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player115 <-  cfbd_stats_season_player(2020,  team = "UTEP", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player116 <-  cfbd_stats_season_player(2020,  team = "UT San Antonio", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player117 <-  cfbd_stats_season_player(2020,  team = "Vanderbilt", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player118 <-  cfbd_stats_season_player(2020,  team = "Virginia", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player119 <-  cfbd_stats_season_player(2020,  team = "Virginia Tech", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player120 <-  cfbd_stats_season_player(2020,  team = "Wake Forest", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player121 <-  cfbd_stats_season_player(2020,  team = "Washington", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player122 <-  cfbd_stats_season_player(2020,  team = "Washington State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player123 <-  cfbd_stats_season_player(2020,  team = "Western Kentucky", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player124 <-  cfbd_stats_season_player(2020,  team = "Western Michigan", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player125 <-  cfbd_stats_season_player(2020,  team = "West Virginia", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player126 <-  cfbd_stats_season_player(2020,  team = "Wyoming", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player127 <-  cfbd_stats_season_player(2020,  team = "Wisconsin", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player128 <-  cfbd_stats_season_player(2020,  team = "Connecticut", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player129 <-  cfbd_stats_season_player(2020,  team = "New Mexico State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player130 <-  cfbd_stats_season_player(2020,  team = "Old Dominion", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player131 <-  cfbd_stats_season_player(2020,  team = "James Madison", start_week = i, end_week = i, category = 'defensive')
  
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
  cfbd_stats_season_player_total2020 <- rbind(cfbd_stats_season_player_total2020,assign(paste0("cfbd_stats_season_player_week",i),cfbd_stats_season_player_week))
  
  
  i <- i+1
}


team_sacks <- aggregate(defensive_sacks ~ team + week,data = cfbd_stats_season_player_total2020,FUN = sum)
colnames(team_sacks) <- c('team','week','team_sacks')
cfbd_stats_season_player_total2020 <- left_join(cfbd_stats_season_player_total2020,team_sacks)

cfbd_stats_season_player_total2020$year <- 2020




i <- 1
cfbd_stats_season_player_total2021 <- data.frame()

while (i < 15) {
  cfbd_stats_season_player1 <-  cfbd_stats_season_player(2021,  team = "Air Force", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player2 <- cfbd_stats_season_player(2021,  team = "Alabama", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player3 <-  cfbd_stats_season_player(2021,  team = "Akron", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player4 <-  cfbd_stats_season_player(2021,  team = "Appalachian State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player5 <-  cfbd_stats_season_player(2021,  team = "Arizona", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player6 <-  cfbd_stats_season_player(2021,  team = "Arizona State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player7 <-  cfbd_stats_season_player(2021,  team = "Arkansas", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player8 <-  cfbd_stats_season_player(2021,  team = "Arkansas State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player9 <-  cfbd_stats_season_player(2021,  team = "Army", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player10 <-  cfbd_stats_season_player(2021,  team = "Auburn", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player11 <-  cfbd_stats_season_player(2021,  team = "Ball State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player12 <-  cfbd_stats_season_player(2021,  team = "Baylor", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player13 <-  cfbd_stats_season_player(2021,  team = "Boise State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player14 <-  cfbd_stats_season_player(2021,  team = "Boston College", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player15 <-  cfbd_stats_season_player(2021,  team = "Bowling Green", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player16 <-  cfbd_stats_season_player(2021,  team = "Buffalo", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player17 <-  cfbd_stats_season_player(2021,  team = "BYU", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player18 <-  cfbd_stats_season_player(2021,  team = "California", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player19 <-  cfbd_stats_season_player(2021,  team = "Central Michigan", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player20 <-  cfbd_stats_season_player(2021,  team = "Charlotte", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player21 <-  cfbd_stats_season_player(2021,  team = "Cincinnati", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player22 <-  cfbd_stats_season_player(2021,  team = "Clemson", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player23 <-  cfbd_stats_season_player(2021,  team = "Coastal Carolina", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player24 <-  cfbd_stats_season_player(2021,  team = "Colorado", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player25 <-  cfbd_stats_season_player(2021,  team = "Colorado State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player26 <-  cfbd_stats_season_player(2021,  team = "Duke", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player27 <-  cfbd_stats_season_player(2021,  team = "East Carolina", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player28 <-  cfbd_stats_season_player(2021,  team = "Eastern Michigan", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player29 <-  cfbd_stats_season_player(2021,  team = "Florida", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player30 <-  cfbd_stats_season_player(2021,  team = "Florida Atlantic", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player31 <-  cfbd_stats_season_player(2021,  team = "Florida International", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player32 <-  cfbd_stats_season_player(2021,  team = "Florida State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player33 <-  cfbd_stats_season_player(2021,  team = "Fresno State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player34 <-  cfbd_stats_season_player(2021,  team = "Georgia", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player35 <-  cfbd_stats_season_player(2021,  team = "Georgia Southern", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player36 <-  cfbd_stats_season_player(2021,  team = "Georgia State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player37 <-  cfbd_stats_season_player(2021,  team = "Georgia Tech", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player38 <-  cfbd_stats_season_player(2021,  team = "Hawai'i", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player39 <-  cfbd_stats_season_player(2021,  team = "Houston", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player40 <-  cfbd_stats_season_player(2021,  team = "Illinois", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player41 <-  cfbd_stats_season_player(2021,  team = "Indiana", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player42 <-  cfbd_stats_season_player(2021,  team = "Iowa", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player43 <-  cfbd_stats_season_player(2021,  team = "Iowa State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player44 <-  cfbd_stats_season_player(2021,  team = "Kansas", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player45 <-  cfbd_stats_season_player(2021,  team = "Kansas State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player46 <-  cfbd_stats_season_player(2021,  team = "Kent State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player47 <-  cfbd_stats_season_player(2021,  team = "Kentucky", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player48 <-  cfbd_stats_season_player(2021,  team = "Liberty", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player49 <-  cfbd_stats_season_player(2021,  team = "Louisiana", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player50 <-  cfbd_stats_season_player(2021,  team = "Louisiana Monroe", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player51 <-  cfbd_stats_season_player(2021,  team = "Louisiana Tech", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player52 <-  cfbd_stats_season_player(2021,  team = "Louisville", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player53 <-  cfbd_stats_season_player(2021,  team = "LSU", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player54 <-  cfbd_stats_season_player(2021,  team = "Marshall", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player55 <-  cfbd_stats_season_player(2021,  team = "Maryland", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player56 <-  cfbd_stats_season_player(2021,  team = "Memphis", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player57 <-  cfbd_stats_season_player(2021,  team = "Miami", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player58 <-  cfbd_stats_season_player(2021,  team = "Miami (OH)", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player59 <-  cfbd_stats_season_player(2021,  team = "Michigan", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player60 <-  cfbd_stats_season_player(2021,  team = "Michigan State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player61 <-  cfbd_stats_season_player(2021,  team = "Middle Tennessee", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player62 <-  cfbd_stats_season_player(2021,  team = "Minnesota", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player63 <-  cfbd_stats_season_player(2021,  team = "Mississippi State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player64 <-  cfbd_stats_season_player(2021,  team = "Missouri", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player65 <-  cfbd_stats_season_player(2021,  team = "Navy", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player66 <-  cfbd_stats_season_player(2021,  team = "NC State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player67 <-  cfbd_stats_season_player(2021,  team = "Nebraska", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player68 <-  cfbd_stats_season_player(2021,  team = "Nevada", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player69 <-  cfbd_stats_season_player(2021,  team = "New Mexico", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player70 <-  cfbd_stats_season_player(2021,  team = "North Carolina", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player71 <-  cfbd_stats_season_player(2021,  team = "Northern Illinois", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player72 <-  cfbd_stats_season_player(2021,  team = "North Texas", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player73 <-  cfbd_stats_season_player(2021,  team = "Northwestern", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player74 <-  cfbd_stats_season_player(2021,  team = "Notre Dame", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player75 <-  cfbd_stats_season_player(2021,  team = "Ohio", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player76 <-  cfbd_stats_season_player(2021,  team = "Ohio State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player77 <-  cfbd_stats_season_player(2021,  team = "Oklahoma", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player78 <-  cfbd_stats_season_player(2021,  team = "Oklahoma State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player79 <-  cfbd_stats_season_player(2021,  team = "Ole Miss", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player80 <-  cfbd_stats_season_player(2021,  team = "Oregon", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player81 <-  cfbd_stats_season_player(2021,  team = "Oregon State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player82 <-  cfbd_stats_season_player(2021,  team = "Penn State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player83 <-  cfbd_stats_season_player(2021,  team = "Pittsburgh", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player84 <-  cfbd_stats_season_player(2021,  team = "Purdue", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player85 <-  cfbd_stats_season_player(2021,  team = "Rice", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player86 <-  cfbd_stats_season_player(2021,  team = "Rutgers", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player87 <-  cfbd_stats_season_player(2021,  team = "San Diego State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player88 <-  cfbd_stats_season_player(2021,  team = "San Jose State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player89 <-  cfbd_stats_season_player(2021,  team = "SMU", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player90 <-  cfbd_stats_season_player(2021,  team = "South Alabama", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player91 <-  cfbd_stats_season_player(2021,  team = "South Carolina", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player92 <-  cfbd_stats_season_player(2021,  team = "Southern Mississippi", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player93 <-  cfbd_stats_season_player(2021,  team = "South Florida", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player94 <-  cfbd_stats_season_player(2021,  team = "Stanford", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player95 <-  cfbd_stats_season_player(2021,  team = "Syracuse", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player96 <-  cfbd_stats_season_player(2021,  team = "TCU", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player97 <-  cfbd_stats_season_player(2021,  team = "Temple", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player98 <-  cfbd_stats_season_player(2021,  team = "Tennessee", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player99 <-  cfbd_stats_season_player(2021,  team = "Texas", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player100 <-  cfbd_stats_season_player(2021,  team = "Texas A&M", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player101 <-  cfbd_stats_season_player(2021,  team = "Texas State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player102 <-  cfbd_stats_season_player(2021,  team = "Texas Tech", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player103 <-  cfbd_stats_season_player(2021,  team = "Toledo", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player104 <-  cfbd_stats_season_player(2021,  team = "Troy", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player105 <-  cfbd_stats_season_player(2021,  team = "Tulane", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player106 <-  cfbd_stats_season_player(2021,  team = "Tulsa", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player107 <-  cfbd_stats_season_player(2021,  team = "UAB", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player108 <-  cfbd_stats_season_player(2021,  team = "UCF", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player109 <-  cfbd_stats_season_player(2021,  team = "UCLA", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player110 <-  cfbd_stats_season_player(2021,  team = "UMass", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player111 <-  cfbd_stats_season_player(2021,  team = "UNLV", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player112 <-  cfbd_stats_season_player(2021,  team = "USC", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player113 <-  cfbd_stats_season_player(2021,  team = "Utah", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player114 <-  cfbd_stats_season_player(2021,  team = "Utah State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player115 <-  cfbd_stats_season_player(2021,  team = "UTEP", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player116 <-  cfbd_stats_season_player(2021,  team = "UT San Antonio", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player117 <-  cfbd_stats_season_player(2021,  team = "Vanderbilt", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player118 <-  cfbd_stats_season_player(2021,  team = "Virginia", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player119 <-  cfbd_stats_season_player(2021,  team = "Virginia Tech", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player120 <-  cfbd_stats_season_player(2021,  team = "Wake Forest", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player121 <-  cfbd_stats_season_player(2021,  team = "Washington", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player122 <-  cfbd_stats_season_player(2021,  team = "Washington State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player123 <-  cfbd_stats_season_player(2021,  team = "Western Kentucky", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player124 <-  cfbd_stats_season_player(2021,  team = "Western Michigan", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player125 <-  cfbd_stats_season_player(2021,  team = "West Virginia", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player126 <-  cfbd_stats_season_player(2021,  team = "Wyoming", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player127 <-  cfbd_stats_season_player(2021,  team = "Wisconsin", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player128 <-  cfbd_stats_season_player(2021,  team = "Connecticut", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player129 <-  cfbd_stats_season_player(2021,  team = "New Mexico State", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player130 <-  cfbd_stats_season_player(2021,  team = "Old Dominion", start_week = i, end_week = i, category = 'defensive')
  cfbd_stats_season_player131 <-  cfbd_stats_season_player(2021,  team = "James Madison", start_week = i, end_week = i, category = 'defensive')
  
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
  cfbd_stats_season_player_total2021 <- rbind(cfbd_stats_season_player_total2021,assign(paste0("cfbd_stats_season_player_week",i),cfbd_stats_season_player_week))
  
  
  i <- i+1
}


team_sacks <- aggregate(defensive_sacks ~ team + week,data = cfbd_stats_season_player_total2021,FUN = sum)
colnames(team_sacks) <- c('team','week','team_sacks')
cfbd_stats_season_player_total2021 <- left_join(cfbd_stats_season_player_total2021,team_sacks)

cfbd_stats_season_player_total2021$year <- 2021

cfbd_stats_season_player_total <- rbind(cfbd_stats_season_player_total2018,cfbd_stats_season_player_total2019,cfbd_stats_season_player_total2020,cfbd_stats_season_player_total2021)
teams_sacks <- cfbd_stats_season_player_total[,c(1,10:13)]
teams_hurries <- aggregate(defensive_qb_hur ~ team + week + year,data = teams_sacks,FUN = sum)
colnames(teams_hurries) <- c('team','week','year','team_hur')
teams_sacks <- teams_sacks[,-c(2)]
teams_sacks <- left_join(teams_sacks,teams_hurries)
teams_sacks <- teams_sacks[!duplicated(teams_sacks),]

teams_sacks <- left_join(teams_sacks,cfbd_game_info_total)


teams_sacks <- teams_sacks %>% 
  group_by(team) %>% 
  mutate(L3_sacks = rollapply(team_sacks, width = list(-1:-4), align = 'right', fill = NA, FUN = mean))

teams_sacks <- teams_sacks %>% 
  group_by(team) %>% 
  mutate(L3_hurries = rollapply(team_hur, width = list(-1:-4), align = 'right', fill = NA, FUN = mean))

colnames(teams_sacks) <- c('opp_team','week','opp_sacks','year','opp_hur','team','L3_sacks','L3_hurries')













