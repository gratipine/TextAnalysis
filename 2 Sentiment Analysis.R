# First we need the sentiments datasets
library(tidytext)

get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")

# keep in mind:
# - unigram only, so cannot help for no good, not true, sick in a positive 
# sense and so on
# - cannot help for sarcasm as well
# - size matters - scores even out over longer text, sentences or paragraph
# analysis works better.

# 2.2 Sentiment analysis with inner join --------------
# Prep
library(janeaustenr)
library(dplyr)
library(stringr)

tidy_books <- 
  austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(
           text, regex("^chapter [\\divxlc]", ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)

# grab the joys from the nrc
nrc_joy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")

tidy_books %>%
  filter(book == "Emma") %>%
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)
