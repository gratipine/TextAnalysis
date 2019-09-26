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

# saving in case I need to work while on the road
saveRDS(books, file = "books.RDS")

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

# curious - there us a lot of death and dead in the dictionary, as well as 
# poison. Given that we could proabbly assume that at least one of the murders 
# was done through poison. 

# To check in which book that is:
bing_word_counts_by_books <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  group_by(gutenberg_id) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

# The plot of it
bing_word_counts_by_books %>%
  group_by(gutenberg_id, sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment + gutenberg_id, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

# Top five positive words are exactly the same!
# One of the books certainly sounds sadder and more active at the same time 
# than the other. Given that we have a Poirot  one and a Tommy and Tuppence one, 
# it seems like a fair guess that it is the Poirot book that has a lot more 
# death in it, while the other talks about being afraid and thinsg being 
# impossible. Let's see



merge(bing_word_counts_by_books, 
      gutenberg_metadata_dt[
        author == "Christie, Agatha", .(gutenberg_id, title)],
      by = "gutenberg_id")  %>%
  group_by(title, sentiment) %>%
  top_n(10, n) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment + title, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

# Yay, I was right! 
# However, in this case I think there are still a lot of Miss Smith and the 
# like, which might get wrongly classified as a negative 


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

# The most often used words are the names, which makes sense

# A comparison cloud, yay!
library(reshape2)

# Maybe two grays isn't the best thing
tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)



# Having a bit of problem with the encoding
text <- data.table(
  text = books[books$gutenberg_id == 863,]$text)

# Dealing with the wrong encoding like this - there is probably a better way
# of handling it
text[, I := 1:.N]
text[, text := gsub("[^[:alnum:][:blank:]?&/\\-]", " ", text), .I]

sentences_863 <- text %>%
  unnest_tokens(sentence, text, token = "sentences")

# Can also split using Regex
chapters <- books %>%
  group_by(gutenberg_id) %>%
  unnest_tokens(chapter, text, token = "regex", 
                pattern = "Chapter|CHAPTER [\\dIVXLC]") %>%
  ungroup()

chapters %>% 
  group_by(gutenberg_id) %>% 
  summarise(chapters = n())

# Negative chapters can be classified as chapters having the highest proportion
# of negative word overall
bingnegative <- get_sentiments("bing") %>% 
  filter(sentiment == "negative")

wordcounts <- tidy_books %>%
  group_by(gutenberg_id, chapter) %>%
  summarize(words = n())

tidy_books %>%
  semi_join(bingnegative) %>%
  group_by(gutenberg_id, chapter) %>%
  summarize(negativewords = n()) %>%
  left_join(wordcounts, by = c("gutenberg_id", "chapter")) %>%
  mutate(ratio = negativewords/words) %>%
  filter(chapter != 0) %>%
  top_n(1) %>%
  ungroup()
