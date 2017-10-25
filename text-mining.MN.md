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
#install.packages("twitteR")
#install.packages("ROAuth")
#install.packages("streamR")
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
library(textclean)
library(sentimentr)
```

Check out this website (<http://politicaldatascience.blogspot.com/2015/12/rtutorial-using-r-to-harvest-twitter.html>)

    ## [1] "Using direct authentication"

``` r
tweets <- userTimeline("GuinnessUS", n=3200)

tw.df <- twListToDF(tweets)
```

From the collected tweets, it seems that what we need is (text, replyTOSN, created).

``` r
tweets.edit <- tw.df %>% 
  select(text, replyToSN, created) %>% 
  group_by(created) 

# fix date 
tweets.edit$month <- month(tweets.edit$created)
tweets.edit$day <- day(tweets.edit$created)
tweets.edit$year <- year(tweets.edit$created)
```

``` r
#cleaned text 
tweets.edit$text <- replace_non_ascii(tweets.edit$text)
tweets.edit$text <- mgsub(tweets.edit$text, c("\\s*http\\S+\\s*", "@\\w+"), c("",""), fixed = FALSE)
tweets.edit$text <- mgsub(tweets.edit$text, "#", "ht")
tweets.edit$text <- strip(tweets.edit$text)
```

Almost done at this point, all we need to do is the sentiment analysis, though I perfer to use sentimentR as opposed to what we did in the datacamp courses because it leverages valence shifters, and views the entire tweet as a whole. But I'll do both...

``` r
#take out text, tidy it, word per row, and run sentiment analysis via inner join
tweets <- tweets.edit$text
tweets.td <- tidy(tweets)

tweet.words <- tweets.td %>%
  unnest_tokens(word, x) %>% 
  anti_join(stop_words)
```

    ## Joining, by = "word"

``` r
tweet_sentiment <- tweet.words %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()


tweets.edit.sentiment <- sentiment_by(tweets.edit$text)
```
