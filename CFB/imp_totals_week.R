
rD <- rsDriver(browser = c("firefox"),check=F)

driver <- rD[["client"]]

driver$open()


#set URL
url <- paste0("https://sportsbook.draftkings.com/leagues/football/ncaaf")

# navigate to an URL
driver$navigate(url)
Sys.sleep(1)

#Getting Nodes
html <- driver$getPageSource()[[1]]
imp_team <- read_html(html) %>%
  html_nodes(".event-cell__name-text") %>% 
  html_text()
imp_team <- as.data.frame(imp_team)
imp_team <- imp_team[-c(105:120),]

spread <- read_html(html) %>%
  html_nodes(".no-label .sportsbook-outcome-cell__line") %>% 
  html_text()
spread <- as.data.frame(spread)
spread <- spread[-c(105:120),]

ou <- read_html(html) %>%
  html_nodes("span+ .sportsbook-outcome-cell__line") %>% 
  html_text()
ou <- as.data.frame(ou)

#Combining Nodes
odds <- cbind(imp_team,spread,ou)
#close the driver
driver$close()

#close the server
rD[["server"]]$stop()

write.csv(odds,'imp_totals.csv')
