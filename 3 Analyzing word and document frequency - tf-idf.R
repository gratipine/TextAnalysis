# term frequency - how often a word shows up in a document
# inverse document frequency - decreases weight for commonly used word and 
# increases it for words not used a lot in a corpus of documents

# idf(term) = ln(ndocuments / n documents containing term)


# 1. Term frequency in Jane Austen's books --------------------

library(dplyr)
library(janeaustenr)
library(tidytext)
library(ggplot2)

options(scipen = 999)

book_words <- austen_books() %>%
  unnest_tokens(word, text) %>%
  count(book, word, sort = TRUE)

total_words <- book_words %>%
  group_by(book) %>%
  summarize(total = sum(n))

book_words <- left_join(book_words, total_words)

ggplot(book_words, aes(n / total, fill = book)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~book, ncol = 2, scales = "free_y")


# 2. Zipf's Law ------------------------
# Zipf’s law states that the frequency that a word appears is inversely 
# proportional to its rank.

freq_by_rank <- book_words %>%
  group_by(book) %>%
  mutate(rank = row_number(),
         term_frequency = n/total)

freq_by_rank %>% ggplot(
  aes(rank, term_frequency, color = book)) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) +
  scale_x_log10() +
  scale_y_log10()

freq_by_rank %>% ggplot(
  aes(rank, term_frequency, color = book)) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE)
