library(tidyverse)
library(ggplot2)
library(ggthemes)
library(lubridate)
library(tidyquant)
library(quantmod)
library(magrittr)
library(twitteR)
library(dygraphs)
library(QuantTools)

# consumer_key <- "OS5PuEpT9sDiRMb2IL0tmweTM"
# consumer_secret <- "donOIjojc7CXWeKKdA4ahect8e5NcyxExRZc0IZ0cLSY2jk5KR"
# access_token <- "829782726844805124-NQjwsrOtzCgpO00kDtiI4TaDJzALNRt"
# access_secret <- "jqn5RWWedOfN7RmnRHk2gUWvSBvS3vTsvUVEPZOIjng7G "
# 
# setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

# potus <- userTimeline("POTUS", n=10, includeRts=T)
# potus_df <- twListToDF(potus)





# start_date <- as.Date("2017-01-20")
# end_date <- Sys.Date()


# getSymbols("DJI", src = "yahoo", from = start_date, to = end_date)
# getSymbols("NYA", src = "yahoo", from = start_date, to = end_date)



closing_price <- get_finam_data("AMZN", from = Sys.Date(), to = Sys.Date(), period = "30min")

# new_price <- getSymbols.av("AMZN", env=environment(), src="yahoo", from=Sys.Date(), to=Sys.Date(),  api.key = "10YCYDZE7YQSOI3R", periodicity="intraday", interval="30min")

price <- closing_price %>%
  mutate(since_midnight = hour(time) * 60 + minute(time)) %>% 
  filter(since_midnight >= 9*60 & since_midnight <= (16 * 60)) %>% 
  select(time,open, high,low,close)

new <- xts(price, order.by = price$time)

new$time <- NULL

dygraph(new) %>%
  dyOptions(useDataTimezone = TRUE) %>%
  dyCandlestick() %>% 
  dyRangeSelector() %>% 
  dyOptions(useDataTimezone = TRUE)


