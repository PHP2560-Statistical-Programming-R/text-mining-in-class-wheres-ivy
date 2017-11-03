source("check_packages.R")
check_packages(c("twitteR","ROAuth","stringr","rebus","stringi",
                 "dplyr","tidytext","lubridate","ggplot2"))

library(twitteR)
library(ROAuth)

#Grab tweets of Tsingtao from Twitter 
consumer_key="uxezKTgNY6J9vDV2IN1QgKaHd"
consumer_secret="ezdvqK1w1vgA3PnyioIU56bW73DZmo9AWFfptFNsn7bz0Cs44A"
access_token="3759994332-tdFWvTsGAedAgvvw6vCiPkEFbnWedxCnpR83FUe"
access_secret="LXPvROXMJaN2e9WOcgqLTVTv0XahEM9ksE75QVm0sBxBZ"
setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_secret)

Tsingtao<-userTimeline("@Tsingtao",since='2014-10-18',includeRts = F, excludeReplies = F, n= 3200)
df<-twListToDF(Tsingtao)
df$text <- iconv(df$text, from="UTF-8", to="ASCII", "byte")

dir.create("~/text-mining-in-class-wheres-ivy/Lisha/data", showWarnings = FALSE)
write.csv(df,"~/text-mining-in-class-wheres-ivy/Lisha/data/raw.csv")
