# Tidy Tuesday - Aug 11, 2020
# Avatar The Last Airbender

# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

library(tidyverse)
library(tidytext)
library(tidytuesdayR)

tuesdata <- tidytuesdayR::tt_load('2020-08-11')

avatar <- tuesdata$avatar


# Investigate relationship between characters word count and IMDB rating

script <- avatar %>% 
  select(book, chapter, chapter_num, character,
         character_words, imdb_rating) %>% 
  filter(character != "Scene Description") %>% 
  group_by(book) %>% 
  mutate(line_num = row_number()) %>% 
  ungroup() %>% 
  unnest_tokens(word, character_words)

word_count <- script %>% 
  select(book, chapter, character, word) %>% 
  group_by(book, chapter, character) %>% count()

main_characters <- word_count %>% group_by(character) %>% summarize(sum(n))
main_characters[order(main_characters$character, decreasing = TRUE)]
main_characters <- rename(main_characters, "n" = "sum(n)")

names <- main_characters %>% arrange(desc(n)) %>% 
  top_n(20)

main_word_count <- merge(word_count, names, by = "character")
main_word_count <- rename(main_word_count, "total.count" = "n.y", "episode.count" = "n.x")

episode <- avatar %>% 
  select(chapter, chapter_num, director, imdb_rating) %>% 
  distinct()

main_word_count <- merge(main_word_count, episode, by = "chapter")

