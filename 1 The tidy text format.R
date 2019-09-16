# Beginnings of Tokenization

poem <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")

poem


library(dplyr)
text_df <- tibble(line = 1:4, text = poem)

text_df

library(tidytext)

text_df %>%
  unnest_tokens(word, text)


# Analysis of the Jane Austin works

library(janeaustenr)
library(dplyr)
library(stringr)

original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>%
  ungroup()

original_books

tidy_books <- original_books %>%
  unnest_tokens(word, text)

tidy_books


# Removing stopwords
data(stop_words)

tidy_books <- tidy_books %>%
  anti_join(stop_words)

tidy_books

# Perhaps we are not interested in the numbers following chapters as well?


# Most common words in the books
tidy_books %>%
  count(word, sort = TRUE) 

# Perhaps make a word cloud out of that data frame?

library(ggplot2)

tidy_books %>%
  count(word, sort = TRUE) %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()


# Word frequencies -----

library(gutenbergr)

# Download using the Project Gutenberg ids for the following books:
# - The Time Machine
# - The War of the Worlds
# - The Invisible Man
# - The Island of Doctor Moreau


hgwells <- gutenberg_download(c(35, 36, 5230, 159))

tidy_hgwells <- hgwells %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

# Grouping by book id yields interesting results - The time machine has time 
# mentioned in it 200 times, while in the other ones it is mentioned a 
# collective 254 times. I would have expected more mentions in the first one, 
# for some reason. However, given the distribution, perhaps it is the expected 
# amount of times.

tidy_hgwells %>%
  count(word, sort = TRUE)

tidy_hgwells %>%
  group_by(gutenberg_id) %>%
  count(word, sort = TRUE)

# Similar analysis with the Bronte books
# - Jane Eyre
# - Wuthering Heights
# - The Tenant of Wildfell Hall
# - Villette
# - Agnes Grey

bronte <- gutenberg_download(c(1260, 768, 969, 9182, 767))

tidy_bronte <- bronte %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

# Interestingly enough, the Bronte sisters and H.G. Wells all talk a lot about 
# time.
tidy_bronte %>%
  count(word, sort = TRUE)

# Gather all the authors together
library(tidyr)

frequency <- bind_rows(mutate(tidy_bronte, author = "Brontë Sisters"),
                       mutate(tidy_hgwells, author = "H.G. Wells"), 
                       mutate(tidy_books, author = "Jane Austen")) %>% 
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  spread(author, proportion) %>% 
  gather(author, proportion, `Brontë Sisters`:`H.G. Wells`)

library(scales)

# expect a warning about rows with missing values being removed
ggplot(frequency, 
       aes(x = proportion, y = `Jane Austen`, 
           color = abs(`Jane Austen` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(
    limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  facet_wrap(~author, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "Jane Austen", x = NULL)


# Correlations between the two sets 
cor.test(data = frequency[frequency$author == "Brontë Sisters",],
         ~ proportion + `Jane Austen`)

cor.test(data = frequency[frequency$author == "H.G. Wells",], 
         ~ proportion + `Jane Austen`)
