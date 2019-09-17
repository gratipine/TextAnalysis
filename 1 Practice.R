library(dplyr)
library(tidytext)
library(gutenbergr)
library(ggplot2)
library(purrr)
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


# Do a simple count of the words. Should be in only edge cases that we
# worry about chapter, part and so on

christie_words <- books %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

christie_words <- merge(
  christie_words, 
  gutenberg_metadata_dt[author == "Christie, Agatha", .(gutenberg_id, title)],
  by =  "gutenberg_id")

conted_words <- christie_words %>%
  count(word, sort = TRUE)

# A handy reminder that while The Secret Adversary was a Tommy and Tuppence 
# book, The Mysterious Affair at Styles was a Poirot book. Hopefully by the end 
# of this analysis I actually remember what they were about.
counted_words_by_book <- christie_words %>%
  group_by(title) %>%
  count(word, sort = TRUE)

# Should probably sort out the ' words - that's is not necessary
christie_words %>%
  count(word, sort = TRUE) %>%
  filter(n > 100) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

frequencies <- christie_words %>%
  mutate(word = stringr::str_extract(word, "[a-z']+")) %>%
  group_by(title) %>%
  count(word) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>%
  tidyr::spread(title, proportion) %>% 
  tidyr::gather(title, proportion, `The Mysterious Affair at Styles`)

# Make the great plot
library(scales)
ggplot(frequencies, 
       aes(x = proportion, y = `The Secret Adversary`, 
           color = abs(`The Secret Adversary` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(
    limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  theme(legend.position="none") +
  labs(y = "The Secret Adversary", x = "The Mysterious Affair at Styles")

# Nothing jogs my memory, but I was wrong about "chapter" not mattering that 
# much - it is right in the middle and very common for both books, even though 
# it should not be that common.

# Correlations between the two sets 
cor.test(data = frequencies,
         ~ proportion + `The Secret Adversary`)

# Not very strong correlation...