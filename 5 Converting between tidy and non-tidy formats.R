# A few explanations ----------------------
# - A document - term matrix is a matrix where:
#     * each row is one document (book, article)
#     * each column is a term
#     * each value is a count of how many times the term appears in the document 

library(tm)
library(topicmodels)
library(dplyr)
library(tidytext)
library(quanteda)

data("AssociatedPress", package = "topicmodels")
AssociatedPress

terms <- Terms(AssociatedPress)
head(terms)

ap_td <- tidy(AssociatedPress)
ap_td

ap_sentiments <- ap_td %>%
  inner_join(get_sentiments("bing"), by = c(term = "word"))

ap_sentiments

library(ggplot2)

ap_sentiments %>%
  count(sentiment, term, wt = count) %>%
  ungroup() %>%
  filter(n >= 200) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(term = reorder(term, n)) %>%
  ggplot(aes(term, n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  ylab("Contribution to sentiment") +
  coord_flip()


# Tidying dfm objects ----------------
data("data_corpus_inaugural", package = "quanteda")
inaug_dfm <- quanteda::dfm(data_corpus_inaugural, verbose = FALSE)

inaug_td <- tidy(inaug_dfm)
inaug_td

inaug_tf_idf <- inaug_td %>%
  bind_tf_idf(term, document, count) %>%
  arrange(desc(tf_idf))

inaug_tf_idf %>%
  filter(document %in% c("1861-Lincoln", "1933-Roosevelt",
                         "1961-Kennedy", "2009-Obama")) %>%
  group_by(document) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(term = reorder(term, tf_idf)) %>%
  ggplot(aes(term, tf_idf, fill = document)) +
  facet_wrap(~document, scales = "free") +
  geom_bar(stat = "identity") + 
  coord_flip() +
  ggtitle("Most distinct terms in 4 documents") + 
  guides(fill = FALSE)

# Total number of words within each year
library(tidyr)

year_term_counts <- inaug_td %>%
  extract(document, "year", "(\\d+)", convert = TRUE) %>%
  complete(year, term, fill = list(count = 0)) %>%
  group_by(year) %>%
  mutate(year_total = sum(count))

year_term_counts %>%
  filter(term %in% c("god", "america", "foreign", "union", 
                     "constitution", "freedom")) %>%
  ggplot(aes(year, count / year_total)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ term, scales = "free_y") +
  scale_y_continuous(labels = scales::percent_format()) +
  ylab("% frequency of word in inaugural address")

# Casting tidy data into a matrix ----------
ap_td %>%
  cast_dtm(document, term, count)

ap_td %>%
  cast_dfm(document, term, count)

library(Matrix)

# cast into a Matrix object
m <- ap_td %>%
  cast_sparse(document, term, count)

class(m)

library(janeaustenr)

austen_dtm <- austen_books() %>%
  unnest_tokens(word, text) %>%
  count(book, word) %>%
  cast_dtm(book, word, n)

austen_dtm

# Tidying corpus objects with metadata -------------------
data("acq")
acq[[1]]

acq_td <- tidy(acq)

# Tokenize to find out most common words, for example
acq_tokens <- acq_td %>%
  select(-places) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word")

# most common words
acq_tokens %>%
  count(word, sort = TRUE)

# tf-idf
acq_tokens %>%
  count(id, word) %>%
  bind_tf_idf(word, id, n) %>%
  arrange(desc(tf_idf))


# Example - mining financial articles -------------------------------
library(tm.plugin.webmining)
library(purrr)

company <- c("Microsoft"#, "Apple", "Google", "Amazon", "Facebook",
             #"Twitter", "IBM", "Yahoo", "Netflix"
             )
symbol <- c("MSFT"#, "AAPL", "GOOG", "AMZN", "FB", "TWTR", "IBM", "YHOO", "NFLX"
            )

download_articles <- function(symbol) {
  WebCorpus(YahooFinanceSource(symbol))
}

stock_articles <- 
  tibble(company = company, symbol = symbol) %>%
  mutate(corpus = map(symbol, download_articles))


company <- c("Microsoft", "Apple", "Google")
symbol <- c("MSFT", "AAPL", "GOOG")

download_articles <- function(symbol) {
  WebCorpus(YahooFinanceSource(symbol))
}

out <- YahooFinanceSource("MSFT")

stock_articles <- data_frame(company = company,
                             symbol = symbol) %>%
  mutate(corpus = map(symbol, download_articles))

stock_tokens <- stock_articles %>%
  mutate(corpus = map(corpus, tidy)) %>%
  unnest(cols = (corpus)) %>%
  unnest_tokens(word, text) %>%
  select(company, datetimestamp, word, id, heading)

stock_tokens