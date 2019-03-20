





date <- as.Date("12-30-2012", "%m-%d-%Y")



url <- paste("http://www.dailyfx.com/files/Calendar-", strftime(date, "%m-%d-%Y"),".csv", sep="")













events <- read.csv(url)

events$week <-date





for (i in 1:1000)#as.numeric(round((Sys.Date() - date[1]) / 7,0)+1))
  
{
  
  
  
  if(date >= Sys.Date()) {break}
  
  else
    
  {
    
    date <- date + 7
    
    url <- paste("http://www.dailyfx.com/files/Calendar-", strftime(date, "%m-%d-%Y"),".csv", sep="")
    
    tmp <- read.csv(url)
    
    tmp$week <- date
    
    events <- rbind(events, tmp)
    
    Sys.sleep(0.5)
    
  }
  
}



tail(events)
