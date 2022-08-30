library(zoo)

i <- 2009
adv_stats_total <- data.frame()
while (i <= 2021) {
  for (j in 1:16) {
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
  }
  i <- i + 1
}

adv_stats_total <- adv_stats_total[,c(1,2,6:18,20,21,25:42,45:60,64:82)]
adv_stats_total <- adv_stats_total[complete.cases(adv_stats_total),]



















