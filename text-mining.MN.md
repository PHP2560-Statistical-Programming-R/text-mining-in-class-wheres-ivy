Text Analyis
================

Your mission
============

Perform text analysis.

Okay, I need more information
-----------------------------

Perform sentiment analysis or topic modeling using text analysis methods as demonstrated in the pre-class work and in the readings.

Okay, I need even more information.
-----------------------------------

Do the above. Can't think of a data source?

-   `gutenbergr`
-   `AssociatedPress` from the `topicmodels` package
-   `NYTimes` or `USCongress` from the `RTextTools` package
-   Harry Potter Complete 7 Books text

``` r
if (packageVersion("devtools") < 1.6) {
  install.packages("devtools")
}

devtools::install_github("bradleyboehmke/harrypotter")
```

-   [State of the Union speeches](https://pradeepadhokshaja.wordpress.com/2017/03/31/scraping-the-web-for-presdential-inaugural-addresses-using-rvest/)

-   Scrape tweets using [`twitteR`](https://www.r-bloggers.com/setting-up-the-twitter-r-package-for-text-analytics/)

-   [Previous URL](https://www.credera.com/blog/business-intelligence/twitter-analytics-using-r-part-1-extract-tweets/)

Analyze the text for sentiment OR topic. **You do not need to do both**.

The datacamp courses and [Tidy Text Mining with R](http://tidytextmining.com/) are good starting points for templates to perform this type of analysis, but feel free to *expand beyond these examples*.

Timelines and Task
==================

We will spend the next 2 weeks working on analyzing textual data in R. You will do the following:

-   Start with some text based data.
-   Clean data and prepare it for analysis
-   Ask questions about the data
-   Answer these questions with the data using tables and graphics
-   Each group member must have their own unique question that they code the answer for.

Twitter Extraction Libraries
----------------------------

``` r
library(twitteR)
library(ROAuth)
library(tidyverse)
```

    ## Loading tidyverse: ggplot2
    ## Loading tidyverse: tibble
    ## Loading tidyverse: tidyr
    ## Loading tidyverse: readr
    ## Loading tidyverse: purrr
    ## Loading tidyverse: dplyr

    ## Conflicts with tidy packages ----------------------------------------------

    ## filter():   dplyr, stats
    ## id():       dplyr, twitteR
    ## lag():      dplyr, stats
    ## location(): dplyr, twitteR

``` r
library(tidytext)
library(tm)
```

    ## Loading required package: NLP

    ## 
    ## Attaching package: 'NLP'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     annotate

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ## 
    ##     date

``` r
library(stringr)
library(sentimentr)
library(stm)
```

    ## stm v1.3.1 (2017-10-30) successfully loaded. See ?stm for help.

``` r
library(knitr)
```

Check out this website (<http://politicaldatascience.blogspot.com/2015/12/rtutorial-using-r-to-harvest-twitter.html>)

    ## [1] "Using direct authentication"

``` r
guinness <- userTimeline("GuinnessUS", n=2000, since="2014-01-01")

guinness.td <- twListToDF(guinness)

guinness.td$reformattedtext <- iconv(guinness.td$text, from="UTF-8", to="ASCII", "byte")

write_csv(guinness.td, "guinness.csv")
```

From the collected tweets, it seems that what we need is (text, replyTOSN, created).

``` r
tweets.edit <- guinnes.td %>% 
  select(text, replyToSN, created) %>% 
  group_by(created) 

# fix date 
tweets.edit$month <- month(tweets.edit$created)
tweets.edit$day <- day(tweets.edit$created)
tweets.edit$year <- year(tweets.edit$created)
```

Almost done at this point, all we need to do is the sentiment analysis, though I perfer to use sentimentR as opposed to what we did in the datacamp courses because it leverages valence shifters, and views the entire tweet as a whole. But I'll do both...

``` r
#take out text, tidy it, word per row, and run sentiment analysis via inner join
tweets <- tweets.edit$text
tweets.td <- tidy(tweets)

tweet.words <- tweets.td %>%
  unnest_tokens(word, x) %>% 
  anti_join(stop_words)

tweet_sentiment <- tweet.words %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()


tweets.edit.sentiment <- sentiment_by(tweets.edit$text)
```

After playing around, we decided it was best to combine all the raw data and then begin cleaning it.

``` r
clean.beer <- read_csv("AllCleanedTweets.csv")
```

    ## Warning: Missing column names filled in: 'X1' [1]

    ## Parsed with column specification:
    ## cols(
    ##   X1 = col_integer(),
    ##   text = col_character(),
    ##   favoriteCount = col_integer(),
    ##   replyToSN = col_character(),
    ##   screenName = col_character(),
    ##   retweetCount = col_integer(),
    ##   date.stamp = col_datetime(format = ""),
    ##   weekday = col_character()
    ## )

``` r
#just want specific data as meta data --> data that would serve as a covariate: beer company and date 

beer.data <- clean.beer %>% 
  select(X1, text, screenName, weekday, favoriteCount)

#process the data via STM textProcessor 
proc.beer <- textProcessor(documents = beer.data$text, metadata = beer.data)
```

    ## Building corpus... 
    ## Converting to Lower Case... 
    ## Removing punctuation... 
    ## Removing stopwords... 
    ## Removing numbers... 
    ## Stemming... 
    ## Creating Output...

``` r
out <- prepDocuments(proc.beer$documents, proc.beer$vocab, proc.beer$meta)
```

    ## Removing 2696 of 5142 terms (2696 of 54355 tokens) due to frequency 
    ## Removing 86 Documents with No Words 
    ## Your corpus now has 8285 documents, 2446 terms and 51659 tokens.

``` r
beerdocs <- out$documents 
beervocab <- out$vocab
beermeta <- out$meta
```

``` r
#need to instal Rtsne, rsvd, and geometry
  #install.packages("Rtsne")
  #install.packages("rsvd")
  #install.packages("geometry")

#from here, we can run the stm function for topic modelling 
#there was a bug when k=0 it wont run, talked to creator, got it fixed, but need to install dev branch 

install.packages("Rcpp")
install.packages("devtools")
install.packages("data.table")
devtools::install_github("bstewart/stm", ref="development")

set.seed(999)
beerModelFit <- stm(document = beerdocs,
                    vocab = beervocab,
                    K =  0, #we can set k=0 to allow the program to find appropiate K as a starting point, later test K around it with searchK
                    prevalence =~ screenName + weekday,
                    data = beermeta,
                    init.type = "Spectral"
                    ) 


#K determines the number of topics, we can use searchK to to figure out the appropriate number of topics
set.seed(5679305)
K <- c(5,10,15,20)
storage <- searchK(docs,
                   vocab,
                   K,
                   prevalence=~ screenName + weekday,
                   data = meta)

#reasonable K set at 30, model converaged after 423 iterations, saving model to be used for later 
saveRDS(beerModelFit, "beerModelformatted.rds")

#k=0, converged after 80 iterations, 49 topics 
saveRDS(beerModelFit, "beerModel49.rds")
```

We now have two potential models, but still, which one should we used? After running stm() with K=0, we had the algorithim pick out some Ks: 49, 51, 56, 59. So we will set K around that and run the searchK() function. It took forever, but I saved the output as an R object, and we can plot the results and see.

``` r
searchk.test <- read_rds("searchk.rds")

plot(searchk.test)

searchk.df <- as.data.frame(searchk.test$results)

#seems like 44, 49, 51 are all reasonable K, since we already have 49, lets run the model at k = 44 and compare

beermodel44 <- stm(document = beerdocs,
                    vocab = beervocab,
                    K =  44,
                    prevalence =~ screenName + weekday,
                    data = beermeta,
                    init.type = "Spectral",
                    seed = 123456
                    ) 

saveRDS(beermodel44, "beermodel44.rds")

thoughts44 <- findThoughts(beermodel44, beermeta$text, topics = c(25,38), n=3)
plot(thoughts44)
```

There doesn't seem to be much of a difference between 44 topics and 49 topics. So we will stick with 49 topics, especially given the jump in residuals onwards after 49.

The STM package leverages metadata as part of the topic modelling process. This is denoted by the prevalence argument. It takes in the metadata: sceenName (the beer company) & weekday, and provides us with 30 topics for which the tweets can fall under. The two metadata is describing how the prevalence of the topics is influenced by the two covariates -- who is tweeting, and on what day are they tweeting.

The first reason why screenName is an important covariate is quite simple: who is tweeting matters. We also have reason to believe that the topic of their tweets will change based on the day, ex. TGIF, or Monday night Football. Tess, will be the one who will help explore topic prevalance given day of the week.

For now, I will focus on topic prevalence given the beer company. Who is tweeting, and what are they tweeting about? However, I am still using the weekday covariate in order to train my model to be as accurate as possible. Just because I will not focus on weekday does not mean that it is not necessary when developing our model. STM's greatest advantage over other topic modelling techniques is its ability to include metadata to help better describe and train the topic outputs.

In order to investigate the beer companies, we need to first answer some more questions: - "What are the most popular topics?" - "Who tweets about these topics the most?" - "Which topics recieve the most engagement? (Favorites & Retweets)"

Thus we need to identify the topics with the highest frequenct among our tweets, visualize who is tweeting them, and then organize them engagement.

``` r
#First let's just play around with some functions in STM -> findThoughts will help us locate the document most closely associated with each topic, this will come in handy later when we know which topic is most frequent

BeerModelFormatted <- readRDS("beerModelformatted.rds")

thoughts <- findThoughts(BeerModelFormatted, beermeta$text, topics=c(48), n=3)

plot(thoughts)
```

![](text-mining.MN_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-10-1.png)

I want to check to see if the documents pulled up from findThoughts matches the one in the metadata. We can do this with plotQuotes(thoughts), it will let us know which document it is using and then we can index to see if they match.

``` r
#we can see which document plot(thoughts) is using with 
#plotQuote(thoughts)
beermeta$text[c(7722,4150,7625)] #looks good! 
```

    ## [1] "Our Mountain Abbey Ale is currently in hibernation, but we recommend trying our new Cappuccino Oatmeal Stout. It's delicious!"
    ## [2] "Dublin Porter and West Indies Porters are back (for a limited time) in our Brewers Collection 12-pack."                      
    ## [3] "Reward yourself for wrapping up your holidaygiftlist by slowing down with a Cappuccino Oatmeal Stout."

From here, let's add the most popular topic corresponding to each tweet as a new variable for our dataframe. We can simply use the "$theta" from the stm model to see all the values. If we take a look at theta from the model, we see that every document has a corresponding theta value for EACH topic.

``` r
#what does it look like? 
theta <- as.data.frame(BeerModelFormatted$theta)
head(theta, n=2)
```

    ##             V1           V2           V3           V4           V5
    ## 1 0.0012138726 5.354624e-05 6.600276e-06 0.0003862886 8.996256e-05
    ## 2 0.0008069851 4.751433e-05 7.432713e-06 0.0003213132 7.398028e-05
    ##             V6          V7         V8          V9         V10         V11
    ## 1 0.0011154561 0.001726719 0.05891677 0.010534933 0.004976748 0.006928108
    ## 2 0.0006481378 0.001086237 0.01533702 0.006187848 0.003919064 0.002584185
    ##          V12        V13         V14         V15        V16         V17
    ## 1 0.01836288 0.02092130 0.002072807 0.001477274 0.03997116 0.008357849
    ## 2 0.01569480 0.01218232 0.001186805 0.001734603 0.67694868 0.003893657
    ##           V18          V19         V20        V21        V22         V23
    ## 1 0.003663303 0.0004659414 0.001187623 0.02619621 0.43249031 0.007022635
    ## 2 0.002627707 0.0001706386 0.000750880 0.03096827 0.03638756 0.004211418
    ##           V24          V25         V26         V27         V28         V29
    ## 1 0.015247580 0.0001420617 0.026636952 0.002877781 0.012803875 0.009170494
    ## 2 0.007955355 0.0001473962 0.005895344 0.001862763 0.004864842 0.004851111
    ##          V30        V31        V32         V33         V34         V35
    ## 1 0.09898867 0.02093405 0.02031150 0.013332354 0.023438255 0.008406186
    ## 2 0.01482492 0.01193808 0.03317774 0.006728066 0.008837397 0.006994188
    ##           V36         V37         V38         V39         V40         V41
    ## 1 0.002140152 0.001915548 0.006452382 0.001984201 0.014018952 0.010802190
    ## 2 0.002092515 0.001543615 0.004970625 0.001953978 0.008063355 0.007823309
    ##           V42         V43          V44         V45        V46         V47
    ## 1 0.002216970 0.004283433 0.0007304083 0.010488766 0.01068837 0.001766027
    ## 2 0.001972908 0.004488310 0.0005937727 0.007427334 0.01123762 0.002275594
    ##           V48         V49         V50         V51         V52         V53
    ## 1 0.001377499 0.015270428 0.003431975 0.003643007 0.005492203 0.002869433
    ## 2 0.003345014 0.006601138 0.003163851 0.003575031 0.001314345 0.001703424

``` r
#add in function that take the column name with the highest value, and add it to a variable we call "topic" and then create a varaible that gives us the actual value, and then we need to create a variable "X1" so we can do a join 

theta$topic <- apply(theta[,1:49], 1, which.max)
theta$topic.value <- apply(theta[, 1:49], 1, max)
theta$X1 <- 1:nrow(theta)

#take only variables of interest 
theta.clean <- theta %>% 
  select(X1, topic, topic.value)

#prepDoc deleted lines in beermeta, therefore X1 is not numbered correctly, have to delete and renumber, the rows 

beermeta <- within(beermeta, rm(X1))

beermeta$X1 <- 1:nrow(beermeta)

beermeta[8,] #X1 = 8, while row num = 9, but it is displayed correctly, X1 matches order correctly
```

    ##                                                 text screenName weekday
    ## 9 Did you prepare wisely and stock up on Bud Light?!   budlight     Mon
    ##   favoriteCount X1
    ## 9             1  8

``` r
#now that our beermeta's X1 is labeled correctly and corresponds with the actual number of the document, we can join the two datasets using X1 

beer.final <- beermeta %>%
  left_join(theta.clean, by = "X1")
```

Awesome, now we have a data frame with all our variables of interest, based on the documents that STM used for the text processing, notice that we started with 8459, and now we have 8285. So what can we do now exactly?

We know *who* tweeted *what* and *when* (on what day). From this we can do the following: *find the most tweeted topic by beer company *find the most favorited topic

    ## # A tibble: 152 x 3
    ##        screenName topic     n
    ##             <chr> <int> <int>
    ##  1 BlueMoonBrewCo    37   535
    ##  2 BlueMoonBrewCo    45   525
    ##  3     GuinnessUS    14   364
    ##  4 BlueMoonBrewCo    19   335
    ##  5     GuinnessUS    44   317
    ##  6       budlight    30   309
    ##  7       budlight    22   304
    ##  8       DosEquis    18   292
    ##  9       DosEquis    25   253
    ## 10 BlueMoonBrewCo    36   252
    ## # ... with 142 more rows

![](text-mining.MN_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-13-1.png)

    ## # A tibble: 152 x 3
    ##        screenName topic favorites
    ##             <chr> <int>     <int>
    ##  1       DosEquis    18     12349
    ##  2       DosEquis    25      9003
    ##  3     GuinnessUS    44      6672
    ##  4       DosEquis    46      3767
    ##  5       DosEquis    34      3665
    ##  6       DosEquis    15      3503
    ##  7     GuinnessUS    14      2937
    ##  8 BlueMoonBrewCo    39      2699
    ##  9       DosEquis    13      2651
    ## 10       DosEquis    12      2593
    ## # ... with 142 more rows

    ## # A tibble: 5 x 4
    ##       screenName favorited ntweets     ratio
    ##            <chr>     <int>   <int>     <dbl>
    ## 1       DosEquis     58197    1616 36.012995
    ## 2     GuinnessUS     12948     919 14.089227
    ## 3 BlueMoonBrewCo      7146    2996  2.385180
    ## 4       budlight      6796    1917  3.545123
    ## 5       tsingtao      2518     837  3.008363

Sweet, now we know which topic is tweeted the most by our beer companies, and also which topics get favorited the most! With this, we now have some topics of interest that is worth further investigation:

``` r
#okay, lets take the 3 most favorited topics for each company, time to make a function! 
header <- function(data, name) {
  x1 <- data %>%
    filter(screenName == name)
  return(head(x1, n=3))
}

header(favorite.topic.freq, "budlight")
```

    ## # A tibble: 3 x 3
    ##   screenName topic favorites
    ##        <chr> <int>     <int>
    ## 1   budlight    16      1571
    ## 2   budlight    12       767
    ## 3   budlight    32       608

``` r
#most tweeted topic by company? 
header(company.topic.freq, "budlight")
```

    ## # A tibble: 3 x 3
    ##   screenName topic     n
    ##        <chr> <int> <int>
    ## 1   budlight    30   309
    ## 2   budlight    22   304
    ## 3   budlight    21   196

There are multiple ways we can do on about investigating out topics, but since we already have the theta scores for each tweet, we can filter for tweets most associated with each topic!

-   Most Favorited:
    -   DosEquis: 18, 25, 46
    -   BlueMoon: 39, 36, 47
    -   Guinness: 44, 14, 48
    -   TsingTao: 20, 1, 3
    -   Budlight: 16, 12, 32
-   Most Tweeted:
    -   DosEquis: 18, 25, 15
    -   BlueMoon: 37, 45, 19
    -   Guinness: 14, 44, 41
    -   TinsgTao: 3, 20, 1
    -   Budlight: 30, 22, 21

We can see that for DosEquis, their most favorited and most tweeted overlap a lot. While TsingTao overlap completely! So From this pool, let's investigate the 3 companies with the most overlap in topics: DosEquis, Guinness, and TsingTao. We will look at topics: *18, 25, 14, 44, 3, 20, 1*
