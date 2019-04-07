library(tidyverse)
library(ggplot2)
library(ggthemes)
library(lubridate)
library(tidyquant)
library(quantmod)
library(magrittr)
library(twitteR)
library(dygraphs)

# consumer_key <- "OS5PuEpT9sDiRMb2IL0tmweTM"
# consumer_secret <- "donOIjojc7CXWeKKdA4ahect8e5NcyxExRZc0IZ0cLSY2jk5KR"
# access_token <- "829782726844805124-NQjwsrOtzCgpO00kDtiI4TaDJzALNRt"
# access_secret <- "jqn5RWWedOfN7RmnRHk2gUWvSBvS3vTsvUVEPZOIjng7G "
# 
# setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
# 
# potus <- userTimeline("POTUS", n=10, includeRts=T)
# potus_df <- twListToDF(potus)





start_date <- as.Date("2017-01-20")
end_date <- Sys.Date()


getSymbols("DJI", src = "yahoo", from = start_date, to = end_date)
getSymbols("NYA", src = "yahoo", from = start_date, to = end_date)



closing_price <- as.xts(data.frame(DJI = DJI[, "DJI.Close"], NYA = NYA[, "NYA.Close"])) %>%
    na.omit(DJI.Close, NYA.Close, NDAQ.close)

dygraph(closing_price,ylab="Closing Price",
  main="Dow Jones and NASDAQ Closing Stock Prices"
  ) %>%
  dySeries("DJI.Close" ,label="DJI") %>%
  dySeries("NYA.Close" ,label="NYA") %>%
  dyOptions(colors = c("blue","red")) %>%
  dyRangeSelector()