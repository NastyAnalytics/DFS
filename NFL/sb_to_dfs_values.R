library('RSelenium')
library('rvest')
library('tidyverse')
library('httr')
library('data.table')
library('stringi')
library('zoo')
library('cfbfastR')


setwd("~/Documents/NFL")

sb_names <- c('Mitchell Trubisky','Lamar Jackson (BAL)','Mark Ingram','Travis Etienne','Cedrick Wilson Receiving Yards','Michael Thomas (NO)','O.J. Howard Receiving Yards','A.J. Green (ARI)','Robby Anderson','Richie James','Gabriel Davis')
dfs_names <-c('Mitch Trubisky', 'Lamar Jackson','Mark Ingram II','Travis Etienne Jr.','Cedrick Wilson Jr.','Michael Thomas','O.J. Howard','A.J. Green','Robbie Anderson','Richie James Jr.','Gabe Davis')
clean_names <- data_frame(sb_names,dfs_names)
colnames(clean_names) <- c('clean_names','player')

current_slate <- read.csv("DKSalaries.csv")
current_slate <- separate(data = current_slate, col = Game.Info, into = c("away", "right"), sep = "\\@")
current_slate <- separate(data = current_slate, col = right, into = c("home", "date","time","pm"), sep = "\\ ")
current_slate$time <- gsub("([0-9])([A-Z])", "\\1 \\2", current_slate$time)
current_slate$start_time <- as.POSIXct(paste0(current_slate$time," ",current_slate$pm),format= "%H:%M")
current_slate$dk_opp_abbr <- if_else(current_slate$TeamAbbrev == current_slate$away,current_slate$home,current_slate$away)
current_slate <- current_slate[,c(1,3,6,12)]
current_slate <- rename(current_slate, player = Name)
current_slate <- rename(current_slate, position = Position)
current_slate <- left_join(current_slate,clean_names)
current_slate$player <- ifelse(is.na(current_slate$clean_names),current_slate$player,current_slate$clean_names)
current_slate <- current_slate[,-c(5)]


current_slate1 <- read.csv("FD_LU.csv")
current_slate1 <- separate(data = current_slate1, col = Game, into = c("away", "home"), sep = "\\@")
current_slate1 <- current_slate1[,c(2,4,8,11)]
current_slate1 <- rename(current_slate1, player = Nickname)
current_slate1 <- rename(current_slate1, position = Position)
current_slate1 <- left_join(current_slate1,clean_names)
current_slate1$player <- ifelse(is.na(current_slate1$clean_names),current_slate1$player,current_slate1$clean_names)
current_slate1 <- current_slate1[,-c(5)]

rD <- rsDriver(browser = c("firefox"),check=F)

driver <- rD[["client"]]

driver$open()


#set URL
url <- paste0("https://sportsbook.draftkings.com/leagues/football/nfl?category=passing-props&subcategory=pass-+-rush-yds")

# navigate to an URL
driver$navigate(url)
Sys.sleep(1)

#Getting Nodes
html <- driver$getPageSource()[[1]]
player <- read_html(html) %>%
  html_nodes(".sportsbook-row-name") %>% 
  html_text()
player <- as.data.frame(player)

ou <- read_html(html) %>%
  html_nodes("th+ .sportsbook-table__column-row .sportsbook-outcome-cell__line") %>% 
  html_text()
ou <- as.numeric(ou)
ou <- as.data.frame(ou)

#Combining Nodes
pa_odds <- cbind(player,ou)
#close the driver
driver$close()

#close the server
rD[["server"]]$stop()





rD <- rsDriver(browser = c("firefox"),check=F)

driver <- rD[["client"]]

driver$open()


#set URL
url <- paste0("https://sportsbook.draftkings.com/leagues/football/nfl?category=rush/rec-props&subcategory=rush-+-rec-yds")

# navigate to an URL
driver$navigate(url)
Sys.sleep(1)

#Getting Nodes
html <- driver$getPageSource()[[1]]
player <- read_html(html) %>%
  html_nodes(".sportsbook-row-name") %>% 
  html_text()
player <- as.data.frame(player)

ou <- read_html(html) %>%
  html_nodes("th+ .sportsbook-table__column-row .sportsbook-outcome-cell__line") %>% 
  html_text()
ou <- as.numeric(ou)
ou <- as.data.frame(ou)

#Combining Nodes
ru_odds <- cbind(player,ou)
#close the driver
driver$close()

#close the server
rD[["server"]]$stop()




rD <- rsDriver(browser = c("firefox"),check=F)

driver <- rD[["client"]]

driver$open()


#set URL
url <- paste0("https://sportsbook.draftkings.com/leagues/football/nfl?category=rush/rec-props&subcategory=rec-yds")

# navigate to an URL
driver$navigate(url)
Sys.sleep(1)

#Getting Nodes
html <- driver$getPageSource()[[1]]
player <- read_html(html) %>%
  html_nodes(".sportsbook-row-name") %>% 
  html_text()
player <- as.data.frame(player)

ou <- read_html(html) %>%
  html_nodes("th+ .sportsbook-table__column-row .sportsbook-outcome-cell__line") %>% 
  html_text()
ou <- as.numeric(ou)
ou <- as.data.frame(ou)
#Combining Nodes
re_odds <- cbind(player,ou)
#close the driver
driver$close()

#close the server
rD[["server"]]$stop()


pa_odds1 <- left_join(pa_odds, current_slate1)
pa_odds1$value <- pa_odds1$ou / (pa_odds1$Salary/1000)
ru_odds1 <- left_join(ru_odds, current_slate1)
ru_odds1$value <- ru_odds1$ou / (ru_odds1$Salary/1000)
re_odds1 <- left_join(re_odds, current_slate1)
re_odds1$value <- re_odds1$ou / (re_odds1$Salary/1000)

pa_odds <- left_join(pa_odds, current_slate)
pa_odds$value <- pa_odds$ou / (pa_odds$Salary/1000)
ru_odds <- left_join(ru_odds, current_slate)
ru_odds$value <- ru_odds$ou / (ru_odds$Salary/1000)
re_odds <- left_join(re_odds, current_slate)
re_odds$value <- re_odds$ou / (re_odds$Salary/1000)

