library(stringr)
library(rebus)
library(stringi)
library(dplyr)
library(tidytext)


#Reformat data
All_beer<-read.csv("~/text-mining-in-class-wheres-ivy/AllBeerTweets.csv")

#Remove urls, @, and # from text
url_pattern <-"http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
at_pattern <- "@\\w+"
hashtag_pattern <- "#" 
All_beer$text<-str_replace_all(All_beer$text,url_pattern, replacement = "")
All_beer$text<-str_replace_all(All_beer$text,at_pattern, replacement = "")
All_beer$text<-str_replace_all(All_beer$text,hashtag_pattern,replacement = "")

#Create dates
tweets<- All_beer %>%
  mutate(created = as.character(created)) %>% 
  mutate(date.stamp = ymd_hms(created)) %>% 
  mutate(weekday = wday(date.stamp, label = T))

#Select useful columns
tweets<-tweets %>%
  select(text, favoriteCount, replyToSN, screenName, retweetCount,
         date.stamp, weekday)
save(tweets,file="data/tweets.Rda")
