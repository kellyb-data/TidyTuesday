# Tidy Tuesday - Aug 11, 2020
# Avatar The Last Airbender

# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

library(tidyverse)
library(tidytext)
library(tidytuesdayR)
library(RColorBrewer)
library(ggthemes)

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

# Take information from top 20 characters in the series

names <- main_characters %>% arrange(desc(n)) %>% 
  top_n(7)

main_word_count <- merge(word_count, names, by = "character")
main_word_count <- rename(main_word_count, "total.count" = "n.y", "episode.count" = "n.x")

episode <- avatar %>% 
  select(chapter, chapter_num, director, imdb_rating) %>% 
  distinct()

main_word_count <- merge(main_word_count, episode, by = "chapter")

# Plotting
main_word_count$chapter <- as.factor(main_word_count$chapter)
main_word_count$character <- factor(main_word_count$character, levels = c('Aang', 'Katara', 'Sokka', 'Zuko', 'Iroh', 'Azula','Toph'))
main_word_count$book <- factor(main_word_count$book, levels = c('Water','Earth',"Fire"))

ggplot(data = main_word_count) + geom_col(aes(x = chapter_num, y = episode.count, fill = character)) +
  geom_point(aes(x = chapter_num, y = imdb_rating*230)) + 
  scale_y_continuous("Episode Word Count", sec.axis = sec_axis(~ ./230, name = "IMBD Rating")) + theme_few() +
  scale_fill_manual(values = c(brewer.pal(3,"Purples")[1], brewer.pal(3, "Blues")[-1], brewer.pal(4, "Reds")[-1], brewer.pal(3, "Greens")[2]), name = "Character") + 
  facet_wrap(~book,ncol = 1) + xlab("Chapter Number") + labs(title = "Avatar: The Last Airbender", subtitle = "Character Word Count vs. IMDB Rating")
                
  


