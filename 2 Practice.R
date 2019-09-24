# First we need the sentiments datasets
library(tidytext)

get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")

# 2.2 Sentiment analysis with inner join --------------
# Prep
library(gutenbergr)
library(dplyr)
library(stringr)
library(data.table)


# Hoping to find some Agatha Christie books and analyze them
authors_dt <- data.table(gutenberg_authors)
authors_dt[author == "Christie, Agatha"]

# pull all books written by her
gutenberg_metadata_dt <- data.table(gutenberg_metadata)
gutenberg_metadata_dt[author == "Christie, Agatha"]

# A bit of a dissappointment - only two are found. Let's see what they have
ids_christie_books <- gutenberg_metadata_dt[
  author == "Christie, Agatha",
  gutenberg_id]

books <- gutenberg_download(ids_christie_books)

# Need to sort out the chapters. Regex will have to be my friend
tidy_books <- 
  books %>%
  group_by(gutenberg_id) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(
           text, regex("^CHAPTER [\\divxlc]", ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)


# grab the joys from the nrc
nrc_joy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")

# Most common joy words in The Mysterious Affair at Styles
tidy_books %>%
  filter(gutenberg_id == 863) %>%
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)

# Count in 80 line increments what is the sentiment score 
library(tidyr)

sentiments_in_agatha <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(gutenberg_id, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

# Potting the plot trajectory
library(ggplot2)

# They are so similar!!!!
ggplot(sentiments_in_agatha, aes(index, sentiment, fill = gutenberg_id)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~gutenberg_id, ncol = 2, scales = "free_x")


# Comparing the three dictionaries using plot of P&P -----------
styles_affair <- tidy_books %>% 
  filter(gutenberg_id == 863)

afinn <- styles_affair %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(index = linenumber %/% 80) %>% 
  summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN")

bing_and_nrc <- bind_rows(
  styles_affair %>% 
    inner_join(get_sentiments("bing")) %>%
    mutate(method = "Bing et al."),
  styles_affair %>% 
    inner_join(get_sentiments("nrc") %>% 
                 filter(sentiment %in% c("positive", 
                                         "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

# Visualize
bind_rows(afinn, 
          bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")

# The three methods seem to agree over here as well - wit the expected 
# exception that the NRC is more positive than the rest of them

