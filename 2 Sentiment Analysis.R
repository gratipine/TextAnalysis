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

# Most common joy words in Emma
tidy_books %>%
  filter(book == "Emma") %>%
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)


# Count in 80 line increments what is the sentiment score 
library(tidyr)

jane_austen_sentiment <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(book, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

# Potting the plot trajectory
library(ggplot2)

ggplot(jane_austen_sentiment, aes(index, sentiment, fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free_x")

# Comparing the three dictionaries using plot of P&P -----------
pride_prejudice <- tidy_books %>% 
  filter(book == "Pride & Prejudice")

afinn <- pride_prejudice %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(index = linenumber %/% 80) %>% 
  summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN")

bing_and_nrc <- bind_rows(
  pride_prejudice %>% 
    inner_join(get_sentiments("bing")) %>%
    mutate(method = "Bing et al."),
  pride_prejudice %>% 
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

# The NRC is a lot more positive than the others. Having a look at the actual 
# dictionary reveals that it has a more equal spread of positive ~ negative as
# compared to the 

get_sentiments("nrc") %>% 
  filter(sentiment %in% c("positive", 
                          "negative")) %>% 
  count(sentiment)

get_sentiments("bing") %>% 
  count(sentiment)

# Most common positive and negative words
bing_word_counts <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

# the miss is probably way off
bing_word_counts

# The plot of it
bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

# Add miss to the stop words
custom_stop_words <- bind_rows(tibble(word = c("miss"), 
                                      lexicon = c("custom")), 
                               stop_words)

custom_stop_words


# Make a wordcloud
library(wordcloud)

# First time I'm seeing it build like that
tidy_books %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

# A comparison cloud, yay!
library(reshape2)

# Maybe two grays isn't the best thing
tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)

# Units beyond words: ----
# coreNLP (T. Arnold and Tilton 2016),
# cleanNLP (T. B. Arnold 2016)
# sentimentr (Rinker 2017) 

PandP_sentences <- tibble(text = prideprejudice) %>% 
  unnest_tokens(sentence, text, token = "sentences")

# Can also split using Regex
austen_chapters <- austen_books() %>%
  group_by(book) %>%
  unnest_tokens(chapter, text, token = "regex", 
                pattern = "Chapter|CHAPTER [\\dIVXLC]") %>%
  ungroup()

austen_chapters %>% 
  group_by(book) %>% 
  summarise(chapters = n())

# Negative chapters can be classified as chapters having the highest proportion
# of negative word overall
bingnegative <- get_sentiments("bing") %>% 
  filter(sentiment == "negative")

wordcounts <- tidy_books %>%
  group_by(book, chapter) %>%
  summarize(words = n())

tidy_books %>%
  semi_join(bingnegative) %>%
  group_by(book, chapter) %>%
  summarize(negativewords = n()) %>%
  left_join(wordcounts, by = c("book", "chapter")) %>%
  mutate(ratio = negativewords/words) %>%
  filter(chapter != 0) %>%
  top_n(1) %>%
  ungroup()
