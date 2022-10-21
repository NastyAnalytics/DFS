week_games3 <- read.csv("est_p_att_and_r_att_week_8.csv")


logos <- read.csv("https://raw.githubusercontent.com/sportsdataverse/cfbfastR-data/main/themes/logos.csv")
logos$school <- ifelse(logos$team_id == 23, 'San Jose State',logos$school)
logos <- logos %>% select(-.data$conference)
logos <- rename(logos, team = school)
logos <- logos[,c(2,11)]
logos <- data.table(logos)

logos[, team := stri_trans_general(str = team, 
                                   id = "Latin-ASCII")]

week_games3 <- left_join(week_games3,logos)

annotations <- data.frame(
  X = c(-Inf,-Inf,Inf,Inf),
  Y =  c(-Inf, Inf,-Inf,Inf),
  text = c("Less Plays - Balanced","Run Heavy",
           "Pass Heavy","More Plays - Balanced"),
  x_adjust = c(0,0,1,1),
  y_adjust = c(-2,1,-2,1))

ggplot(week_games3, aes(est_pa, est_ra)) + 
  geom_image(aes(image = logo), size = 0.03) +
  xlab("Estimated Passing Atttempts") + 
  ylab("Estimated Rushing Attempts") + 
  theme_minimal() + 
  labs(title = "Estimated Passing Attempts vs Rushing Attempts for Week 8", subtitle = "@NastyAnalytics") + 
  geom_hline(yintercept=mean(week_games3$est_ra), linetype="dashed", color = "red") + 
  geom_vline(xintercept=mean(week_games3$est_pa), linetype="dashed", color = "red") + 
  geom_text(data=annotations, aes(x=X,y=Y,hjust=x_adjust,vjust=y_adjust,label=text),color = 'red')
