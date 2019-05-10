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

# Twitter API key to pull 2019 data

consumer_key <- "OS5PuEpT9sDiRMb2IL0tmweTM"
consumer_secret <- "donOIjojc7CXWeKKdA4ahect8e5NcyxExRZc0IZ0cLSY2jk5KR"
access_token <- "829782726844805124-NQjwsrOtzCgpO00kDtiI4TaDJzALNRt"
access_secret <- "jqn5RWWedOfN7RmnRHk2gUWvSBvS3vTsvUVEPZOIjng7G"


# Creates authentication to pull twitter data

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)


# Pulls 2000 most recent @realDonaldTrump Tweets

potus <- userTimeline("realDonaldTrump", n=2000, includeRts=TRUE)
potus_df <- twListToDF(potus)

# Filters for 2019 tweets

potus_2019 <- potus_df %>%
  filter(year(created) >= 2019) %>%
  filter(isRetweet==F) %>%
  select(text, created, retweetCount, favoriteCount)

#  2018 Trump github tweet repo

download.file(url="https://github.com/bpb27/trump_tweet_data_archive/blob/master/condensed_2018.json.zip?raw=true", destfile ="2018.zip", quiet =TRUE)

#  2017 Trump github tweet repo

download.file(url="https://github.com/bpb27/trump_tweet_data_archive/blob/master/condensed_2017.json.zip?raw=true", destfile ="2017.zip", quiet =TRUE)


# 2016 Trump github tweet repo

download.file(url="https://github.com/bpb27/trump_tweet_data_archive/blob/master/condensed_2016.json.zip?raw=true", destfile ="2016.zip", quiet =TRUE)

unzip("2018.zip")
unzip("2017.zip")
unzip("2016.zip")

file_delete(c("2018.zip", "2017.zip", "2016.zip"))


# Files pulled from the github must be edited to match type of tweets pulled from twitter API. This
# meant changing the column names and pulling substrings of the created_at object to match the created value
# in the 2019 data. 


potus_2018 <- fromJSON("condensed_2018.json") %>%
  filter(is_retweet==F) %>%
  select(text, created = created_at, retweetCount = retweet_count, favoriteCount = favorite_count) %>%
  mutate(created = substr(created, 4, stop=nchar(created))) %>%
  mutate(ymd = paste(substr(created, 1,4), substr(created, 6,7), substr(created,24,nchar(created)), sep="-")) %>%
  mutate(created = paste(ymd, substr(created,8,16))) %>%
  select(-ymd) %>%
  mutate(created = ParseDateTime(created)) %>%
  filter(year(created) == 2018)


potus_2017 <- fromJSON("condensed_2017.json") %>%
  filter(is_retweet==F) %>%
  select(text, created = created_at, retweetCount = retweet_count, favoriteCount = favorite_count) %>%
  mutate(created = substr(created, 4, stop=nchar(created))) %>%
  mutate(ymd = paste(substr(created, 1,4), substr(created, 6,7), substr(created,24,nchar(created)), sep="-")) %>%
  mutate(created = paste(ymd, substr(created,8,16))) %>%
  select(-ymd) %>%
  mutate(created = ParseDateTime(created)) %>%
  filter(year(created) == 2017)


potus_2016 <- fromJSON("condensed_2016.json") %>%
  filter(is_retweet==F) %>%
  select(text, created = created_at, retweetCount = retweet_count, favoriteCount = favorite_count) %>%
  mutate(created = substr(created, 4, stop=nchar(created))) %>%
  mutate(ymd = paste(substr(created, 1,4), substr(created, 6,7), substr(created,24,nchar(created)), sep="-")) %>%
  mutate(created = paste(ymd, substr(created,8,16))) %>%
  select(-ymd) %>%
  mutate(created = ParseDateTime(created)) %>%
  filter(year(created) == 2016)

file_delete(c("condensed_2018.json", "condensed_2017.json", "condensed_2016.json"))

# Bind Rows -- must create separate time object to record time and separate date and time objects. Date
# must be of type as.Date in order to be used later for interactive updates by clicking the tweet table.

tweets <- bind_rows(potus_2019,potus_2018,potus_2017,potus_2016) %>%
  mutate(time = substr(created, 12,16)) %>%
  mutate(created = as.Date(created)) %>%
  select(text,created,time,retweetCount,favoriteCount)


# Keyword Tweets -- Companies
company_keys <- c("Amazon", "Google", "Facebook", "Merck", "Pfizer", "Wells Fargo", "Disney", "Boeing", "Ford", "Apple", "Alibaba")


# Sentiment analysis is used specifically for tweets with key words in the text. After finding these keywords
# and assigning a relevant keyword value, the text can then be broken up to determine the relevant sentiments
# for all words in the text. After, we can group by each keyword value tally and total the number of positive 
# and negative sentiments
# Create sentiment tally for company_tweets objects

company_tweets <- tweets %>%
  filter(str_detect(text, paste(company_keys, collapse="|"))) %>% 
  mutate(keyword = str_match(text, paste(company_keys, collapse="|"))) %>% 
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("bing")) %>%
  group_by(keyword, sentiment) %>%
  tally()


# Keyword Tweets -- Countries

country_keys <- c("China", "North Korea", "Mexico", "Canada")

# Create sentiment tally for country_tweets objects

country_tweets <- tweets %>%
  filter(str_detect(text, paste(country_keys, collapse="|"))) %>% 
  mutate(keyword = str_match(text, paste(country_keys, collapse="|"))) %>% 
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("bing")) %>%
  group_by(keyword, sentiment) %>%
  tally()


# Keyword Tweets -- Individuals 

individual_keys <- c("Tim Cook", "Kim Jong Un", "the Fed", "Vladimir Putin")

# Create sentiment tally for individual_tweets objects

individual_tweets <- tweets %>%
  filter(str_detect(text, paste(individual_keys, collapse="|"))) %>% 
  mutate(keyword = str_match(text, paste(individual_keys, collapse="|"))) %>% 
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("bing")) %>%
  group_by(keyword, sentiment) %>%
  tally()


# Store Data in tweet_data folder

write_rds(tweets, "tweet_data/trump_tweets.rds")
write_rds(company_tweets, path="tweet_data/company_tweets.rds")
write_rds(country_tweets, "tweet_data/country_tweets.rds")
write_rds(individual_tweets, "tweet_data/individual_tweets.rds")




