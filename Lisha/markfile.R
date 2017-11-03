unlink("data", recursive = TRUE) # where data is stored after clean
unlink("graph", recursive = TRUE) # where graphs are stores

dir.create(file.path("data"), showWarnings = FALSE)
dir.create(file.path("graph"), showWarnings = FALSE)

source("01_get_tweets.R")   # get data
source("02_clean_data.R")     ## clean data
source("03_graph.R")  ## create graph

rmarkdown::render("result.Rmd", output_format = "html_document")
