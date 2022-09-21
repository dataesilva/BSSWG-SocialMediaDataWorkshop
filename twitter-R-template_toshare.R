#####
## Twitter-APIv2-Template_R
## Developed for the Social Media Data Collection and Analysis Workshop
## With The Behavioral and Social Science Working Group BSSWG
## By David E. Silva, PhD:
## https://github.com/dataesilva
## dataesilva@gmail.com
## https://twitter.com/Geckosilva
## 
## This code contains snippets from:
## 1. Github @twitterdev; https://github.com/twitterdev/Twitter-API-v2-sample-code/blob/main/Recent-Search/recent-search.r
## 2. Github @dslabscl; https://github.com/dslabscl/twitter-ranking/blob/4f9c1ff35aeab9be1df59ad409b63c0a04c4a3a0/politics/candidatos.R

#####
## Before beginning, you will need to install the following packages
# install.packages(c("httr", "dplyr", "jsonlite", "ggplot2", "tidyr", "tidytext", "tidyselect", "tokenizers", "topicmodels", "wordcloud"))

## The spot_words dictionary will also require the `textdata` package
## Install this separately to respond to the prompt in the console
# install.packages("textdata")

#####
## Activate the following libraries
library(httr)
library(tidytext)
library(tidyr)
library(topicmodels)
library(dplyr)
library(wordcloud)

## This bearer_token is invalid and will not authenticate
## Replace the string with your bearer token from your Twitter Developer App
bearer_token = "AAAAAAAAAAAAAAAAAAAAABVvgAEAAAAAUdubQ4%2FwpJGdwwz0KuJQTDlTX5A%3DMDZWiaoWnRAeExmFYSsbZdHEEdbFMzFQAlIPlWSccUTEDs3U7U"

#####
## Create header with bearer token to pass to the /2/tweets/search/recent endpoint
headers = c(
  `Authorization` = sprintf('Bearer %s', bearer_token)
)

## Create query parameters to pass to the /2/tweets/search/recent endpoint
## This is a very simple query which should be further developed
params = list(
  `query` = 'monkeypox -is:retweet lang:en',
  `max_results` = '10',
  `tweet.fields` = 'created_at,lang,author_id,context_annotations'
)

## A more complex query parameter might include more specific search query and other response fields
# params = list(
#   `query` = '(monkeypox OR mpx) -is:retweet lang:en',
#   `max_results` = '100',
#   `tweet.fields` = 'created_at,lang,author_id,context_annotations',
#   `expansions` = 'entities.mentions.username',
#   `user.fields` = 'id,description,name,username,public_metrics,verified'
# )

#####
## The first response is used to test the query for errors and get the first `next_token`
response <- httr::GET(url = 'https://api.twitter.com/2/tweets/search/recent', httr::add_headers(.headers=headers), query = params)

## If successful, the response status code will be `200`
response$status_code
response_body <- content(response)

## If any other value, parse the response and view it to find out why the query failed
# bad_response_body <- content(response)
# View(bad_response_body)

#####
## For successful queries, reformat to a dataframe
## The first line specifies which data fields from the response to transform to columns in the dataframe
## If other query parameters are requested (ex: public_metrics, verified, etc.) this will need updating
response_temp <- lapply(response_body$data, "[", c("id", "text", "lang", "author_id", "created_at"))
# View(response_temp)

## Then create a starting dataframe
response_temp <- lapply(response_temp, as.data.frame)
# View(response_temp)
response_temp <- do.call(rbind, response_temp)
# View(response_temp)
recent_search_df = response_temp
## Make sure the resulting dataframe includes the expected information
View(recent_search_df)

#####
## A note on efficiency and data size:
## Binding dataframes can be an inefficient process resulting in long runtimes and memory allocation issues
## The Twitter API results are paginated; this script adds new rows to a main dataframe after each page is requested
## For very large datasets, a more efficient process would be to create a list of response data within the loop
## Then after the loop terminates, bind the datasets
## Explore conversations on dataframe efficiency and solutions including `dplyr::bind_rows`:
## 1. https://stat.ethz.ch/pipermail/r-help/2016-June/439787.html
## 2. https://dplyr.tidyverse.org/reference/bind.html#ref-examples
## 3. https://stackoverflow.com/questions/60865434/efficient-way-of-binding-rows-of-data-frame-to-another-data-frame-in-a-loop

#####
## A loop is used to request additional response pages while updating the `next_token`

next_token_update = response_body$meta$next_token

## `1:10` can be changed to increase the number of pages to request
## Essential access allows 450 requests per 15 min interval
## If requesting >450 pages, add the `Sys.sleep(.5)` line to respect the API request limits
for (i in 1:10) {
  ## If the query parameters above are changed, be sure to update these parameters
  params_page = list(
    `query` = 'monkeypox -is:retweet lang:en',
    `max_results` = '10',
    `tweet.fields` = 'created_at,lang,author_id,context_annotations',
    `next_token` = next_token_update
  )
  
  response_page <- httr::GET(url = 'https://api.twitter.com/2/tweets/search/recent', httr::add_headers(.headers=headers), query = params_page)
  print(response_page$status_code)
  
  rp_body <- content(response_page)
  
  rp_temp <- lapply(rp_body$data, "[", c("id", "text", "lang", "author_id", "created_at"))
  rp_temp <- lapply(rp_temp, as.data.frame)
  rp_temp <- do.call(rbind, rp_temp)
  
  ## If working with large datasets, consider appending `rp_temp` to a list and binding to `recent_search_df` outside the loop
  ## Also consider using `dplyr::bind_rows` here instead of `rbind`
  recent_search_df <- rbind(recent_search_df, rp_temp)

  next_token_update = rp_body$meta$next_token
  # Sys.sleep(1)
  
}

View(recent_search_df)

#####
## Sentiment analysis begins with tokenizing the strings in the `text` column of the dataframe
tidy_twt <- recent_search_df %>%
  unnest_tokens(word, text)

## `stop_words` can be customized to account for Twitter-specific content
## Also consider applying regular expressions to match common Twitter text patterns (retweets, hastags, usernames, URLs, emojis)
## See regex: https://cran.r-project.org/web/packages/stringr/vignettes/regular-expressions.html
data("stop_words")
tidy_twt_clean <- tidy_twt %>% anti_join(stop_words)
## Alternatively if creating custom stop words
# custom_stop_words <- bind_rows(data_frame(word = c("https", "t.co"), lexicon = c("custom")), stop_words)
# tidy_twt <- tidy_twt %>% anti_join(custom_stop_words)

tidy_twt_clean %>% count(word, sort=TRUE) %>% View()

## This is a dictionary-based sentiment analysis, the foundation of natural language processing
## Dictionary-based methods are somewhat dated, but offer a good starting point for NLP tools
## tidtytext relies on several sentiment lexicons (dictionaries) to assign sentiment scores to tokens; `afinn` is one option
## The values used in `afinn` can be accessed through the `textdata` package
# View(textdata::lexicon_afinn())
## For all information on tidytext, I recommend: https://www.tidytextmining.com/index.html
## For the original description of the afinn lexicon, see: https://arxiv.org/pdf/1103.2903.pdf

## From the cleaned tokens, match with the token sentiment scores
sent_twt <- tidy_twt_clean %>% inner_join(get_sentiments("afinn"))
# View(sent_twt)

## After each token has a sentiment score, sum the sentiment values for each tweet in a new column named `sentiment`
sent_twt <- tidy_twt_clean %>% inner_join(get_sentiments("afinn")) %>% group_by(id = id) %>% summarise(sentiment = sum(value))
## Then merge with the original dataset
sent_twt <- merge(recent_search_df, sent_twt)
View(sent_twt)

## Note the `sent_twt` dataframe likely contains fewer rows than the original `recent_searc_df` dataframe
## This is because some tweets do not contain any tokens that associate with a sentiment score and are dropped 
## To view the tweets without sentiment scores:
# recent_search_df %>% filter(!id %in% sent_twt$id) %>% View()

#####
## Code not discussed in the workshop:
## Create a wordcloud from token frequency
par(mar = c(0, 0, 0, 0))
tidy_twt_clean %>% count(word) %>% with(wordcloud(word, n))

## Bigrams for common co-occurring words
gram_twt <- recent_search_df %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

gram_twt_separated <- gram_twt %>%
  separate(bigram, c("word1", "word2"), sep = " ")
gram_twt_filtered <- gram_twt_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

gram_twt <- gram_twt_filtered %>%
  count(word1, word2, sort = TRUE)
View(gram_twt)