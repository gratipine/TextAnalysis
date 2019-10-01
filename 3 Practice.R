# 1. Term frequency in Agatha Christie's books --------------------

library(dplyr)
library(tidytext)
library(ggplot2)
library(data.table)
library(gutenbergr)
options(scipen = 999)

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

# saving in case I need to work while on the road
saveRDS(books, file = "books.RDS")

books <- readRDS("books.RDS")


# Counting the words by book
book_words <- books %>%
  unnest_tokens(word, text) %>%
  count(gutenberg_id, word, sort = TRUE)


total_words <- book_words %>%
  group_by(gutenberg_id) %>%
  summarize(total = sum(n))

book_words <- left_join(book_words, total_words)

# One of the books is a lot longer than the other

# As expected, most of the words are very widespread, there are some, which 
# are not at all
ggplot(book_words, aes(n / total, fill = gutenberg_id)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~gutenberg_id, ncol = 2, scales = "free_y")


# Zipf's Law ------------
freq_by_rank <- book_words %>%
  mutate(rank = row_number(),
         term_frequency = n/total)

# About the same as the previous one
ggplot(freq_by_rank, 
       aes(rank, term_frequency, col = as.factor(gutenberg_id))) +
  geom_line() +
  scale_x_log10() +
  scale_y_log10()

# when we try to fit a line on it, we get about the same
rank_subset <- freq_by_rank %>% 
  filter(rank < 500, rank > 10)

lm(log10(rank) ~ log10(term_frequency), data = rank_subset)

lm(log10(rank) ~ log10(term_frequency), 
   data = rank_subset[rank_subset$gutenberg_id == 1155,])

lm(log10(rank) ~ log10(term_frequency), 
   data = rank_subset[rank_subset$gutenberg_id == 863,])

# Get quite different lines depending on what we fit them on. The two books are
# Quite different in their logged frequencies
ggplot(freq_by_rank, 
       aes(rank, term_frequency, col = as.factor(gutenberg_id))) +
  geom_line() +
  scale_x_log10() +
  scale_y_log10() +
  geom_abline(
    intercept = -0.45, slope = -0.96, color = "gray50", linetype = 2) +
  geom_abline(
    intercept = -0.58, slope = -0.99, color = "gray50", linetype = 2) +
  geom_abline(
    intercept = -0.44, slope = -0.987, color = "gray50", linetype = 2)
  

# Calculating the tf_idf
book_words <- book_words %>%
  bind_tf_idf(word, gutenberg_id, n)

book_words %>%
  select(-total) %>%
  arrange(desc(tf_idf))

book_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(gutenberg_id) %>% 
  top_n(15) %>% 
  ungroup() %>%
  ggplot(aes(word, tf_idf, fill = gutenberg_id)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~gutenberg_id, ncol = 2, scales = "free") +
  coord_flip()

# We do have some words in the Tommu and Tuppence book that we shouldn't have
# Some clean up required

# Fast fix for the encoding problem
book_words <- book_words[-grep(
  "dat2", iconv(book_words$word, "latin1", "ASCII", sub="dat2")),]


book_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(gutenberg_id) %>% 
  top_n(15) %>% 
  ungroup() %>%
  ggplot(aes(word, tf_idf, fill = gutenberg_id)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~gutenberg_id, ncol = 2, scales = "free") +
  coord_flip()

# We still talk about mostly names, which is good. Surprising amount of taxis in 
# the Tommy and Tuppence book as opposed to the Poirot one, but given how he 
# likes to sit tight and let the little gray cells do their job, perhaps not 
# that surprising.