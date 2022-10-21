library(patchwork)

plays <- cfbd_stats_season_team(2022, start_week = 7, end_week = 7)
plays <- plays[,c(2,7,11)]
plays <- data.table(plays)


logos <- read.csv("https://raw.githubusercontent.com/sportsdataverse/cfbfastR-data/main/themes/logos.csv")
logos$school <- ifelse(logos$team_id == 23, 'San Jose State',logos$school)
logos <- logos %>% select(-.data$conference)
logos <- rename(logos, team = school)
logos <- logos[,c(2,11)]
logos <- data.table(logos)

logos[, team := stri_trans_general(str = team, 
                                   id = "Latin-ASCII")]
plays[, team := stri_trans_general(str = team, 
                                   id = "Latin-ASCII")]

plays <- left_join(plays,logos)


ggplot(plays, aes(pass_atts, rush_atts)) + 
  geom_image(aes(image = plays$logo), size = 0.03) +
  xlab("Pass Attempts") + 
  ylab("Rush Attempts") + 
  theme_minimal() + 
  labs(title = "Pass Attempts and Rush Attempts for Week 7", subtitle = "Data courtesy: cfbfastR") + 
  geom_hline(yintercept=mean(plays$rush_atts), linetype="dashed", color = "red") + 
  geom_vline(xintercept=mean(plays$pass_atts), linetype="dashed", color = "red")
