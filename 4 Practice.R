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

# This would be better off with 10 words per thing, rather than top 30 words
negated_words %>%
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  head(30) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(word2, n * value, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  xlab("Words preceded by negative words") +
  ylab("Sentiment value * number of occurrences") +
  facet_wrap(~word1, scales = "free") +
  coord_flip()
