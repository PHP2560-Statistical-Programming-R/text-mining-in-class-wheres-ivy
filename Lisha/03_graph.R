library(lubridate)
library(dplyr)
library(ggplot2)

#QUESTION 3: Most popular words among the tweets that have highest number of retweets in each month?

#Create year-month, and select columns according to the question
tweets$date<-as.Date(tweets$date.stamp)
tweets$yearmon<-format(tweets$date.stamp,"%Y-%m")

retweets <- tweets %>%
  group_by(yearmon) %>%
  filter(retweetCount==max(retweetCount)) %>%
  select(text, screenName,yearmon,retweetCount)

save(retweets,file="data/retweets.Rda")

# Tokenize text into words and remove stop words
retweets <-retweets %>%
  unnest_tokens(word,text) 
clean_tweets <- anti_join(retweets,stop_words,by="word")
clean_tweets <- clean_tweets %>%
  filter(word != "de")
top_tweets <-clean_tweets %>%
  group_by(word) %>%
  count() %>%
  ungroup() %>%
  top_n(10) %>%
  arrange(desc(n))

#ggplot
dir.create("graph/", showWarnings = FALSE)
png('graph/barplot.png')

ggplot(top_tweets) +
  geom_bar(mapping = aes(x=reorder(word,n),y=n),stat="identity")+xlab("words") +ylab("Frequency") + coord_flip()+
  ggtitle("Common words among tweets with highest retweets monthly") + theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5))
dev.off()
