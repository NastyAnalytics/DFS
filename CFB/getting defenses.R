j <- 2018
i <- 1

rD <- rsDriver(browser = c("firefox"),check=F)

driver <- rD[["client"]]

driver$open()

while (j <= 2021) {
  for (i in 1:16) {
    tryCatch({
      #set URL
      url <- paste0("https://premium.pff.com/ncaa/positions/",j,"/SINGLE/defense?division=fbs&week=",i)
      
      # navigate to an URL
      driver$navigate(url)
      Sys.sleep(1)
      
      #Getting Nodes
      html <- driver$getPageSource()[[1]]
      
      button <- driver$findElement(using = 'xpath',value = '//*[contains(concat( " ", @class, " " ), concat( " ", "mr-2", " " ))]')
      button = button$clickElement()
      week <- week + 1
  
      
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    }
  j <- j+1
}

#close the driver
driver$close()

#close the server
rD[["server"]]$stop()

