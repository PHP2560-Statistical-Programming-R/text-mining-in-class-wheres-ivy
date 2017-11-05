#load and prep the data, took only variables I was interested in using as metadata 
library(stm)
library(dplyr)
library(readr)

clean.beer <- read_csv("AllCleanedTweets.csv")

beer.data <- clean.beer %>% 
  select(X1, text, screenName, weekday)

#data prep
proc.beer <- textProcessor(documents = beer.data$text, metadata = beer.data)

out <- prepDocuments(proc.beer$documents, proc.beer$vocab, proc.beer$meta)
beer.docs <- out$documents 
beer.vocab <- out$vocab
beer.meta <- out$meta

#run stm, k=0, works now!  

set.seed(123)
beerModel <- stm(document = beer.docs,
                 vocab = beer.vocab,
                 K = 10, max.em.its = 1,
                 prevalence =~ screenName + weekday,
                 data = beer.meta,
                 init.type = "Random")

BeerDocs <- beer.meta$text

thoughts <- findThoughts(beerModel, BeerDocs, topics=10, n=3)

plot(thoughts) #seems to be the same tweet
plotQuote(thoughts) #I want to check where the tweets are coming from 

BeerDocs[2081]

