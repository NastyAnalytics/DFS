library('RSelenium')
library('tidyverse')
library('rvest')

setwd('~/Documents/CFB')

rD <- rsDriver(browser = c("firefox"),check=F)

driver <- rD[["client"]]

driver$open()

start = 49
end = 81

dk_results_total <- data.frame()
while (start <= end) {
    
  #set URL
  url <- paste0("https://www.linestarapp.com/Projections/Sport/CFB/Site/DraftKings/PID/",start)
  
  # navigate to an URL
  driver$navigate(url)
  Sys.sleep(2)
  
  #Getting Nodes
  html <- driver$getPageSource()[[1]]
    
  player_name <- read_html(html) %>%
      html_nodes(".playername.ng-binding.ng-scope") %>% 
      html_text()
  player_name <- as.data.frame(player_name)
    
  team_name <- read_html(html) %>%
      html_nodes(".playerTeam.ng-binding.ng-scope") %>% 
      html_text()
  team_name <- gsub("• ", "",as.character(team_name))
    
  team_name <- as.data.frame(team_name)
    
  dk_pos <- read_html(html) %>%
      html_nodes(".playerPos") %>% 
      html_text()
    dk_pos <- as.data.frame(dk_pos)
  
  salary <- read_html(html) %>%
    html_nodes("td:nth-child(10)") %>% 
    html_text()
  salary <- as.data.frame(salary)
  
  dk_own <- read_html(html) %>%
    html_nodes("td:nth-child(23)") %>% 
    html_text()
  dk_own <- as.data.frame(dk_own)
  
  week <- read_html(html) %>%
    html_nodes(".periodInner .periodName") %>% 
    html_text()
  week <- as.data.frame(week)
  
  num_teams <- read_html(html) %>%
    html_nodes(".teamName") %>% 
    html_text()
  slate_size <- length(num_teams)/2
  
  dk_results <- cbind(player_name,team_name,dk_pos,salary,dk_own)
  dk_results$slate_size <- paste0(slate_size)
  dk_results$week <- paste0(week)
  
  assign(paste0("dk_results",week),dk_results)
  dk_results_total <- rbind(dk_results_total,assign(paste0("dk_results",week),dk_results))
  
  start = (start + 1)
  
}

#close the driver
driver$close()

#close the server
rD[["server"]]$stop()


rD <- rsDriver(browser = c("firefox"),check=F)

driver <- rD[["client"]]

driver$open()

start = 82
end = 98

while (start <= end) {
  
  #set URL
  url <- paste0("https://www.linestarapp.com/Projections/Sport/CFB/Site/DraftKings/PID/",start)
  
  # navigate to an URL
  driver$navigate(url)
  Sys.sleep(2)
  
  
  dropdown <- driver$findElement(using = 'class',value = 'dropDownSlateBox')
  dropdown = dropdown$clickElement()
  
  #Getting Nodes
  html <- driver$getPageSource()[[1]]
  
  player_name <- read_html(html) %>%
    html_nodes(".playername.ng-binding.ng-scope") %>% 
    html_text()
  player_name <- as.data.frame(player_name)
  
  team_name <- read_html(html) %>%
    html_nodes(".playerTeam.ng-binding.ng-scope") %>% 
    html_text()
  team_name <- gsub("• ", "",as.character(team_name))
  
  team_name <- as.data.frame(team_name)
  
  dk_pos <- read_html(html) %>%
    html_nodes("td:nth-child(5)") %>% 
    html_text()
  dk_pos <- as.data.frame(dk_pos)
  
  salary <- read_html(html) %>%
    html_nodes("td:nth-child(10)") %>% 
    html_text()
  salary <- as.data.frame(salary)
  
  dk_own <- read_html(html) %>%
    html_nodes("td:nth-child(24)") %>% 
    html_text()
  dk_own <- as.data.frame(dk_own)
  
  week <- read_html(html) %>%
    html_nodes(".periodInner .periodName") %>% 
    html_text()
  week <- as.data.frame(week)
  
  num_teams <- read_html(html) %>%
    html_nodes(".teamName") %>% 
    html_text()
  slate_size <- length(num_teams)/2
  
  dk_results <- cbind(player_name,team_name,dk_pos,salary,dk_own)
  dk_results$slate_size <- paste0(slate_size)
  dk_results$week <- paste0(week)
  
  assign(paste0("dk_results",week),dk_results)
  dk_results_total <- rbind(dk_results_total,assign(paste0("dk_results",week),dk_results))
  
  start = (start + 1)
  
}


#close the driver
driver$close()

#close the server
rD[["server"]]$stop()


rD <- rsDriver(browser = c("firefox"),check=F)

driver <- rD[["client"]]

driver$open()

start = 100
end = 113

while (start <= end) {
  
  #set URL
  url <- paste0("https://www.linestarapp.com/Projections/Sport/CFB/Site/DraftKings/PID/",start)
  
  # navigate to an URL
  driver$navigate(url)
  Sys.sleep(2)
  
  
  dropdown <- driver$findElement(using = 'class',value = 'dropDownSlateBox')
  dropdown = dropdown$clickElement()
  
  #Getting Nodes
  html <- driver$getPageSource()[[1]]
  
  player_name <- read_html(html) %>%
    html_nodes(".playername.ng-binding.ng-scope") %>% 
    html_text()
  player_name <- as.data.frame(player_name)
  
  team_name <- read_html(html) %>%
    html_nodes(".playerTeam.ng-binding.ng-scope") %>% 
    html_text()
  team_name <- gsub("• ", "",as.character(team_name))
  
  team_name <- as.data.frame(team_name)
  
  dk_pos <- read_html(html) %>%
    html_nodes("td:nth-child(5)") %>% 
    html_text()
  dk_pos <- as.data.frame(dk_pos)
  
  salary <- read_html(html) %>%
    html_nodes("td:nth-child(10)") %>% 
    html_text()
  salary <- as.data.frame(salary)
  
  dk_own <- read_html(html) %>%
    html_nodes("td:nth-child(24)") %>% 
    html_text()
  dk_own <- as.data.frame(dk_own)
  
  week <- read_html(html) %>%
    html_nodes(".periodInner .periodName") %>% 
    html_text()
  week <- as.data.frame(week)
  
  num_teams <- read_html(html) %>%
    html_nodes(".teamName") %>% 
    html_text()
  slate_size <- length(num_teams)/2
  
  dk_results <- cbind(player_name,team_name,dk_pos,salary,dk_own)
  dk_results$slate_size <- paste0(slate_size)
  dk_results$week <- paste0(week)
  
  assign(paste0("dk_results",week),dk_results)
  dk_results_total <- rbind(dk_results_total,assign(paste0("dk_results",week),dk_results))
  
  start = (start + 1)
  
}


#close the driver
driver$close()

#close the server
rD[["server"]]$stop()

write.csv(dk_results_total,'dk_ownership.csv')

dk_results_total <- read.csv('dk_ownership.csv')
