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

rank_subset <- freq_by_rank %>% 
  filter(rank < 500, rank > 10)

lm(log10(term_frequency) ~ log10(rank), data = rank_subset)

# Zipf's law - frequency is inversely proportional to rank

# When you have a look at it, the slope is very close to -1
freq_by_rank %>% 
  ggplot(aes(rank, term_frequency, color = book)) + 
  geom_abline(intercept = -0.62, slope = -1.1, color = "gray50", linetype = 2) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()

# The bind_tf_idf -----------------------------------------------------
book_words <- book_words %>%
  bind_tf_idf(word, book, n)

# words with high tf_idf 
book_words %>%
  select(-total) %>%
  arrange(desc(tf_idf))

book_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(book) %>% 
  top_n(15) %>% 
  ungroup() %>%
  ggplot(aes(word, tf_idf, fill = book)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~book, ncol = 2, scales = "free") +
  coord_flip()

# Working with physics texts -------------------------------------------
# - Discourse on Floating Bodies by Galileo Galilei, 
# - Treatise on Light by Christiaan Huygens,
# - Experiments with Alternate Currents of High Potential and High Frequency
# by Nikola Tesla
# - Relativity: The Special and General Theory by Albert Einstein.

library(gutenbergr)

physics <- gutenberg_download(c(37729, 14725, 13476, 30155), 
                              meta_fields = "author")

physics_words <- physics %>%
  unnest_tokens(word, text) %>%
  count(author, word, sort = TRUE)

library(forcats)

plot_physics <- physics_words %>%
  bind_tf_idf(word, author, n) %>%
  mutate(word = fct_reorder(word, tf_idf)) %>%
  mutate(author = factor(
    author, levels = 
      c("Galilei, Galileo", "Huygens, Christiaan", "Tesla, Nikola", 
        "Einstein, Albert")))

plot_physics %>%
  group_by(author) %>%
  top_n(15, tf_idf) %>%
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill = author)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~author, ncol = 2, scales = "free") +
  coord_flip()

# the big question of why is there a k in the Einstein text

library(stringr)
physics %>%
  filter(str_detect(text, "_k_")) %>%
  select(text)


physics %>%
  filter(str_detect(text, "RC")) %>%
  select(text)

# dropping some words (names of angles and such) to make more meaningful plots
mystop_words <- tibble(word = c("eq", "co", "rc", "ac", "ak", "bn", "fig", 
                                "file", "cg", "cb", "cm", "ab", "_k", "_k_", 
                                "_x"))

physics_words <- anti_join(physics_words, mystop_words, by = "word")

plot_physics <- physics_words %>%
  bind_tf_idf(word, author, n) %>%
  mutate(word = str_remove_all(word, "_")) %>%
  group_by(author) %>% 
  top_n(15, tf_idf) %>%
  ungroup() %>%
  mutate(word = reorder_within(word, tf_idf, author)) %>%
  mutate(author = factor(author, levels = c("Galilei, Galileo",
                                            "Huygens, Christiaan",
                                            "Tesla, Nikola",
                                            "Einstein, Albert")))

ggplot(plot_physics, aes(word, tf_idf, fill = author)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~author, ncol = 2, scales = "free") +
  coord_flip() +
  scale_x_reordered()
