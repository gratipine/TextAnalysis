library(gutenbergr)
library(dplyr)
library(stringr)
library(tidytext)
library(tidyr)
library(data.table)

# Loading the Agatha Christie books
authors_dt <- data.table(gutenberg_authors)
authors_dt[author == "Christie, Agatha"]

# pull all books written by her
gutenberg_metadata_dt <- data.table(gutenberg_metadata)
gutenberg_metadata_dt[author == "Christie, Agatha"]

ids_christie_books <- gutenberg_metadata_dt[
  author == "Christie, Agatha",
  gutenberg_id]

books <- gutenberg_download(ids_christie_books)

books$text <- gsub("_", "", books$text)

tidy_books <- 
  books %>%
  group_by(gutenberg_id) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(
           text, regex("^CHAPTER [\\divxlc]", ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)

# Dropping the "_"
books <- data.table(books)
books_modified <- copy(books)
books_modified[, index := 1:.N]
books_modified[, text := gsub("_", "", text), .(index, gutenberg_id)]
books_modified[, index := NULL,]

# Counting bigrams ----------------
# Create the bigrams
christie_bigrams <- books_modified %>%
  unnest_tokens(bigrams, text, token = "ngrams", n = 2)

christie_bigrams %>%
  count(bigrams, sort = TRUE)

bigrams_separated <- christie_bigrams %>%
  separate(bigrams, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!(word1 %in% stop_words$word)) %>%
  filter(!(word2 %in% stop_words$word))

bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)

# uniting the words once again
bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

# The trigrams
books_modified %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE)

# Interesting that so much time is spent on the extra coffee cup 

# how many hospitals do we talk about?
bigrams_filtered %>%
  filter(word2 == "hospital") %>%
  count(gutenberg_id, word1, sort = TRUE)

# Calculating the tf-idf
bigram_tf_idf <- bigrams_united %>%
  count(gutenberg_id, bigram) %>%
  bind_tf_idf(bigram, gutenberg_id, n) %>%
  arrange(desc(tf_idf))

# Realizing that I can easily spoil a murder mustery by plotting these things
bigram_tf_idf %>%
  mutate(word = factor(bigram, levels = rev(unique(bigram)))) %>% 
  group_by(gutenberg_id) %>% 
  arrange(desc(tf_idf)) %>% 
  top_n(15) %>% 
  ungroup() %>%
  arrange(desc(tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill = gutenberg_id)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~gutenberg_id, ncol = 2, scales = "free") +
  coord_flip()

# Using bigrams for context -----------------------
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

# plot
negated_words %>%
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  group_by(word1) %>%
  top_n(n = 10, wt = abs(contribution)) %>%
  ungroup() %>%
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
}


# Counting and correlating pairs of words with the widyr package ------

# Trying to figure out what words tend to appear in the same section
styles_section_words <- books %>%
  filter(gutenberg_id == 863) %>%
  mutate(section = row_number() %/% 10) %>%
  filter(section > 0) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word)

library(widyr)

# count words co-occuring within sections
word_pairs <- styles_section_words %>%
  pairwise_count(word, section, sort = TRUE)

# Most often occuring with Poirot
word_pairs %>%
  filter(item1 == "poirot")

# Pairwise correlation --------
# Poirot and Inglethorp are very common words ingeneral, so just because they
# occur together often doesn't mean they are actually correlated

# Here it is done using the phi coefficient (have a google).

# we need to filter for at least relatively common words first
word_cors <- styles_section_words %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, section, sort = TRUE)

word_cors
# Makes total sense - we always talk about sir William and Lady Catherine and
# The fact that De Bourgh is not a perfect correlation is probably due to an 
# arbitrary cut off point between the sections.

# Lots of coffee in the book
word_cors %>%
  filter(item1 == "coffee")

# What are the most common associations for these words.
word_cors %>%
  filter(item1 %in% c("poirot", "poison", "cavendish")) %>%
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

# Perhaps I cannot spoil the ending - not enough time is actually spent on the 
# soluion as opposed to the clues. Probably classical searching wouldn't have 
# much of a meaning. 

# Let's see the other one, which I do not remember as much
# Trying to figure out what words tend to appear in the same section
tuppence_section_words <- books %>%
  filter(gutenberg_id == 1155) %>%
  mutate(section = row_number() %/% 10) %>%
  filter(section > 0) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word)

tuppence_section_words <- tuppence_section_words[
  -grep("dat2", 
        iconv(
          tuppence_section_words$word,
          "latin1", "ASCII", sub = "dat2")),]

library(widyr)

# count words co-occuring within sections
word_pairs <- tuppence_section_words %>%
  pairwise_count(word, section, sort = TRUE)

# Most often occuring with Poirot
word_pairs %>%
  filter(item1 == "tuppence")

# Pairwise correlation --------
# Poirot and Inglethorp are very common words ingeneral, so just because they
# occur together often doesn't mean they are actually correlated

# Here it is done using the phi coefficient (have a google).

# we need to filter for at least relatively common words first
word_cors <- tuppence_section_words %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, section, sort = TRUE)

word_cors
# Makes total sense - we always talk about sir William and Lady Catherine and
# The fact that De Bourgh is not a perfect correlation is probably due to an 
# arbitrary cut off point between the sections.

# Lots of coffee in the book
word_cors %>%
  filter(item1 == "jane")

# What are the most common associations for these words.
word_cors %>%
  filter(item1 %in% c("russian", "tuppence", "jane", "tommy")) %>%
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
