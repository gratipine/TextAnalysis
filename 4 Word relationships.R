library(dplyr)
library(tidytext)
library(janeaustenr)
library(tidyr)
library(ggplot2)

austen_bigrams <- austen_books() %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

# Counting and filtering n-grams --------------------
# Contains exactly the not useful stuff, at least at the top
austen_bigrams %>%
  count(bigram, sort = TRUE)

# Separate the bigrams to do some cleaning
bigrams_separated <- austen_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts
# Quite interesting, we are still mostly talking aboout the people, this time 
# with their titles as well
bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)

# uniting the words once again
bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

# The trigrams
austen_books() %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE)

# Was Cathering De Bourgh? It sounds better as Debourgh.

# Analysing the bigrams ---------------

# Most often-mentioned streets:
bigrams_filtered %>%
  filter(word2 == "street") %>%
  count(book, word1, sort = TRUE)

bigram_tf_idf <- bigrams_united %>%
  count(book, bigram) %>%
  bind_tf_idf(bigram, book, n) %>%
  arrange(desc(tf_idf))

# We are again looking at mostly the names
bigram_tf_idf %>%
  mutate(word = factor(bigram, levels = rev(unique(bigram)))) %>% 
  group_by(book) %>% 
  arrange(desc(tf_idf)) %>% 
  top_n(15) %>% 
  ungroup() %>%
  arrange(desc(tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill = book)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~book, ncol = 2, scales = "free") +
  coord_flip()

# Analysing a lot of text can be easier with ngrams, since they are sparser data
# than individual words

# Using bigrams for context -----------------------------

# We can use the not the ignore / 
bigrams_separated %>%
  filter(word1 == "not") %>%
  count(word1, word2, sort = TRUE)

AFINN <- get_sentiments("afinn")

# Check which words were tied with a sentiment but also preceeded by a not
not_words <- bigrams_separated %>%
  filter(word1 == "not") %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word2, value, sort = TRUE)

# Which words contributed the most to a wrong score? Multiply the score by the 
# number of times the word appears

library(ggplot2)

not_words %>%
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(word2, n * value, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  xlab("Words preceded by \"not\"") +
  ylab("Sentiment value * number of occurrences") +
  coord_flip()


# There are other misleaders besides not
negation_words <- c("not", "no", "never", "without")

negated_words <- bigrams_separated %>%
  filter(word1 %in% negation_words) %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word1, word2, value, sort = TRUE)

# The plots are fewer than expected. Exploration?
negated_words %>%
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(word2, n * value, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  xlab("Words preceded by negative words") +
  ylab("Sentiment value * number of occurrences") +
  facet_wrap(~word1, scales = "free") +
  coord_flip()

# Visualizing a network of bigrams using ggraph -------------------
library(igraph)

# filter for only relatively common combinations
bigram_graph <- bigram_counts %>%
  filter(n > 20) %>%
  graph_from_data_frame()

bigram_graph

library(ggraph)
set.seed(2017)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

set.seed(2016)
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)


# Visualizing bigrams in other texts --------------------------------------
library(dplyr)
library(tidyr)
library(tidytext)
library(ggplot2)
library(igraph)
library(ggraph)

count_bigrams <- function(dataset) {
  dataset %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word) %>%
    count(word1, word2, sort = TRUE)
}

visualize_bigrams <- function(bigrams) {
  set.seed(2016)
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  
  bigrams %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1)
  # +
    # theme_void()
}


# Download the king James bible and check what that looks like
library(gutenbergr)

kjv <- gutenberg_download(10)

library(stringr)

kjv_bigrams <- kjv %>%
  count_bigrams()

# filter out rare combinations, as well as digits
kjv_bigrams %>%
  filter(n > 40,
         !str_detect(word1, "\\d"),
         !str_detect(word2, "\\d")) %>%
  visualize_bigrams()


# Counting and correlating pairs of words with the widyr package ------

# Trying to figure out what words tend to appear in the same section
austen_section_words <- austen_books() %>%
  filter(book == "Pride & Prejudice") %>%
  mutate(section = row_number() %/% 10) %>%
  filter(section > 0) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word)

library(widyr)

# count words co-occuring within sections
word_pairs <- austen_section_words %>%
  pairwise_count(word, section, sort = TRUE)

# Most often occuring with Darcy
word_pairs %>%
  filter(item1 == "darcy")

# Pairwise correlation --------
# while Darcy and Eilzabeth occur together very often, theeffect is spoiled by 
# the fact that those two words are very common in general. 
# To combat the skewing of that, we calculate the correlation instead.

# Here it is done using the phi coefficient (have a google).

# we need to filter for at least relatively common words first
word_cors <- austen_section_words %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, section, sort = TRUE)

word_cors
# Makes total sense - we always talk about sir William and Lady Catherine and
# The fact that De Bourgh is not a perfect correlation is probably due to an 
# arbitrary cut off point between the sections.

# Mostly talking about pounds in terms of money.
word_cors %>%
  filter(item1 == "pounds")

# What are the most common associations for these words.
word_cors %>%
  filter(item1 %in% c("elizabeth", "pounds", "married", "pride")) %>%
  group_by(item1) %>%
  top_n(6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()

set.seed(2016)

word_cors %>%
  filter(correlation > .15) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) 
