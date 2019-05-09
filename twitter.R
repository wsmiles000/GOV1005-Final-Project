library(tidyverse)
library(ggplot2)
library(ggthemes)
library(fs)
library(lubridate)
library(tidyquant)
library(quantmod)
library(magrittr)
library(twitteR)
library(jsonlite)
library(devtools)
library(flipTime)
library(stringr)
library(dygraphs)
library(tidytext)


# consumer_key <- "OS5PuEpT9sDiRMb2IL0tmweTM"
# consumer_secret <- "donOIjojc7CXWeKKdA4ahect8e5NcyxExRZc0IZ0cLSY2jk5KR"
# access_token <- "829782726844805124-NQjwsrOtzCgpO00kDtiI4TaDJzALNRt"
# access_secret <- "jqn5RWWedOfN7RmnRHk2gUWvSBvS3vTsvUVEPZOIjng7G"
# 
# setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
# 
# 
# 
# potus <- userTimeline("realDonaldTrump", n=2000, includeRts=TRUE)
# potus_df <- twListToDF(potus)
# 
# potus_2019 <- potus_df %>%
#   filter(year(created) >= 2019) %>%
#   filter(isRetweet==F) %>%
#   select(text, created, retweetCount, favoriteCount)
# 
# #  2018
# download.file(url="https://github.com/bpb27/trump_tweet_data_archive/blob/master/condensed_2018.json.zip?raw=true", destfile ="2018.zip", quiet =TRUE)
# 
# #  2017
# download.file(url="https://github.com/bpb27/trump_tweet_data_archive/blob/master/condensed_2017.json.zip?raw=true", destfile ="2017.zip", quiet =TRUE)
# 
# 
# # 2016
# download.file(url="https://github.com/bpb27/trump_tweet_data_archive/blob/master/condensed_2016.json.zip?raw=true", destfile ="2016.zip", quiet =TRUE)
# 
# unzip("2018.zip")
# unzip("2017.zip")
# unzip("2016.zip")
# 
# file_delete(c("2018.zip", "2017.zip", "2016.zip"))
# 
# potus_2018 <- fromJSON("condensed_2018.json") %>%
#   filter(is_retweet==F) %>%
#   select(text, created = created_at, retweetCount = retweet_count, favoriteCount = favorite_count) %>%
#   mutate(created = substr(created, 4, stop=nchar(created))) %>%
#   mutate(ymd = paste(substr(created, 1,4), substr(created, 6,7), substr(created,24,nchar(created)), sep="-")) %>%
#   mutate(created = paste(ymd, substr(created,8,16))) %>%
#   select(-ymd) %>%
#   mutate(created = ParseDateTime(created)) %>%
#   filter(year(created) == 2018)
# 
# 
# 
# potus_2017 <- fromJSON("condensed_2017.json") %>%
#   filter(is_retweet==F) %>%
#   select(text, created = created_at, retweetCount = retweet_count, favoriteCount = favorite_count) %>%
#   mutate(created = substr(created, 4, stop=nchar(created))) %>%
#   mutate(ymd = paste(substr(created, 1,4), substr(created, 6,7), substr(created,24,nchar(created)), sep="-")) %>%
#   mutate(created = paste(ymd, substr(created,8,16))) %>%
#   select(-ymd) %>%
#   mutate(created = ParseDateTime(created)) %>%
#   filter(year(created) == 2017)
# 
# 
# potus_2016 <- fromJSON("condensed_2016.json") %>%
#   filter(is_retweet==F) %>%
#   select(text, created = created_at, retweetCount = retweet_count, favoriteCount = favorite_count) %>%
#   mutate(created = substr(created, 4, stop=nchar(created))) %>%
#   mutate(ymd = paste(substr(created, 1,4), substr(created, 6,7), substr(created,24,nchar(created)), sep="-")) %>%
#   mutate(created = paste(ymd, substr(created,8,16))) %>%
#   select(-ymd) %>%
#   mutate(created = ParseDateTime(created)) %>%
#   filter(year(created) == 2016)
# 
# 
# 
# file_delete(c("condensed_2018.json", "condensed_2017.json", "condensed_2016.json"))

tweets <- bind_rows(potus_2019,potus_2018,potus_2017,potus_2016) %>% 
  mutate(time = substr(created, 12,16)) %>% 
  mutate(created = as.Date(created)) %>% 
  select(text,created,time,retweetCount,favoriteCount)



write_rds(tweets, "trump_tweets")


