# 1. Term frequency in Agatha Christie's books --------------------

library(dplyr)
library(tidytext)
library(ggplot2)
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

books <- readRDS("books.RDS")
