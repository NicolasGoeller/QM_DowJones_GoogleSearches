scrape_googletrends <- function(term){
  
require(tidyverse)
require(gtrendsR)
require(reshape2)

googletrends <- function(n, period) {
  for (i in 1:n){
    if (i == 1){
      google.trends <- gtrends(c(term), gprop = "web", time = period)[[1]]
      google.trends = google.trends[,1:2]#dcast(google.trends, date ~ keyword + geo, value.var = "hits")
      rownames(google.trends) <- google.trends$date
      #google.trends$date = NULL
      
      trend <- google.trends
      #Sys.sleep(10)
    } else {
      system.time(google.trends <- gtrends(c(term), gprop = "web", time = period)[[1]])
      google.trends = google.trends[,1:2]#dcast(google.trends, date ~ keyword + geo, value.var = "hits")
      rownames(google.trends) <- google.trends$date
      #google.trends$date = NULL
    
      trend <- cbind(trend, google.trends)
      #Sys.sleep(10)
    }
  }
  return(trend)
}

years <- as.character(c(2010:2019))
months <- c(1:12)
mon_end <- c("01-31", "02-28", "03-31", "04-30", "05-31", "06-30",
            "07-31", "08-31", "09-30", "10-31", "11-30", "12-31")
mon_sta <- c("01-01", "02-01", "03-01", "04-01", "05-01", "06-01",
             "07-01", "08-01", "09-01", "10-01", "11-01", "12-01")

periods <- c()
for (i in years){
  year <- c()
  for (j in months){
    mon <- paste0(i,"-",mon_sta[j]," ", i,"-",mon_end[j])
    year <- c(year, mon)
  }
  periods <- c(periods, year)
}
periods <- c(periods, "2020-01-01 2020-01-31", "2020-02-01 2020-02-28", "2020-03-01 2020-03-31")

data <- googletrends(n=1, period= periods[1])
for (x in periods[2:length(periods)]){
  google <- googletrends(n=1, period= x)
  data <- rbind(data, google)
  print(x)
}
saveRDS(data, paste0(term, ".rds"))

data_tot <- googletrends(n=1, period = "2010-01-01 2020-03-31")
data_tot$date <- as.character(data_tot$date)
data_tot$date <- substring(data_tot$date, 1, 7)
names(data_tot) <- c("join", "mon_rank")

print(dim(data_tot))
print(dim(data))

data$join <- as.character(data$date)
data$join <- substring(data$join, 1, 7)
names(data) <- c("date", "hits", "join")

final <- inner_join(data, data_tot, by= "join")
final$mon_rank <- final$mon_rank/100
final$rank <- final$hits * final$mon_rank
final <- final[,c("date","hits","mon_rank","rank" )]

return(final)

}