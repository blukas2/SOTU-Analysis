#################################
# SOTU ADDRESS ANALYSIS BACKEND #
#################################

# SET UP ENVIRONMENT

# load libraries
requiredPackages = c("stringi", "stringr", "tidyverse", "lubridate",
                     "rvest", "scales", "dplyr", 
                     "topicmodels", "sotu", "ngram", "tm", "textstem")

#install.packages("tidytext")
#for (p in requiredPackages){if(!p %in% installed.packages()) install.packages(p)}

for (p in requiredPackages){library(p, character.only = TRUE)}
library("tidytext")

# clear memory
rm(list=ls())

# set working directory
setwd("E:\\Data Science\\R\\SOTU Adress Analysis")

# create data directory
dir_data = file.path(getwd(), "data")
if (!dir.exists(dir_data)){dir.create(dir_data)}

# LOAD DATA

#from 'sotu' package
#sotu_meta: metadata from the state of the union adresses

sotu_database <- sotu_meta %>%
  #sotu_text: actual text data of the adressses
  mutate(address = sotu_text)

#read data about Trump and Biden, it was not in the orgininal database
trump_2017 <- read_file("trump_2017.txt")
trump_2018 <- read_file("trump_2018.txt")
trump_2019 <- read_file("trump_2019.txt")
trump_2020 <- read_file("trump_2020.txt")
biden_2021 <- read_file("biden_2021.txt")
trump_biden_address <- c(trump_2017, trump_2018, trump_2019, trump_2020, biden_2021)

trump_biden_data <- read.csv("trump_biden_sotu.csv", header = TRUE, sep = ";") %>%
  mutate(address = trump_biden_address)

# putting together the database
sotu_database <- full_join(sotu_database, trump_biden_data) %>%
  filter(!duplicated(year))

# get meteadata
sotu_metadata <- sotu_database %>%
  select(-address)

# DATA CLEANING
#unnest tokens
sotu_unnested <- sotu_database %>%
  unnest_tokens(word, address) %>%
  mutate(word = str_extract(word, "[a-z']+")) %>%
  filter(!is.na(word))

#calculating speechlength  
speechlength <- sotu_unnested %>%
  group_by(year) %>%
  summarize_all(funs(sum(!is.na(.)))) %>%
  select("year", "word") %>%
  rename(number_of_words = word)
sotu_metadata <- left_join(sotu_metadata, speechlength)
rm(speechlength)

#text complexity
#how many unique words presidents used compared to the full length of the speech
unique_words <- sotu_unnested %>%
  count(year, word, sort = TRUE) %>%
  ungroup() %>%
  group_by(year) %>%
  summarize_all(funs(sum(!is.na(.)))) %>%
  select("year", "word") %>%
  rename(unique_words = word)
sotu_metadata <- sotu_metadata %>%
  left_join(unique_words) %>%
  mutate(unique_per_total_words = unique_words/number_of_words)
rm(unique_words)

#calculating average sencence lenght as number of characters without spaces
sentencelenght <- sotu_database %>%
  mutate(address = stri_enc_toutf8(address)) %>%
  unnest_tokens(sentence, address, token = stringr::str_split, pattern = "\\.") %>%
  select("year", "sentence") %>%
  mutate(slenght = str_count(sentence, pattern = "") - str_count(sentence, pattern = " ")) %>%
  filter(slenght > 10) %>%
  group_by(year) %>%
  summarize_at(vars(slenght), funs(mean)) %>%
  ungroup() %>%
  rename(sentence_length = slenght)
sotu_metadata <- left_join(sotu_metadata, sentencelenght)
rm(sentencelenght)

#calculating mean word length
word_length <- sotu_unnested %>%
  mutate(wlength = nchar(word)) %>%
  group_by(year) %>%
  summarize_at(vars(wlength), funs(mean)) %>%
  rename(word_length = wlength)
sotu_metadata <- left_join(sotu_metadata, word_length)
rm(word_length)

# Sentiment analysis
sentiment_dataset <- get_sentiments() %>%
  mutate(sentiment = case_when(sentiment=="positive" ~ 1,
                               sentiment=="negative" ~ -1))
sotu_sentiments <- sotu_unnested %>%
  inner_join(sentiment_dataset) %>%
  group_by(year) %>%
  summarize(sentiment_points=sum(sentiment), max_sentiment_points = n()) %>%
  ungroup()
sotu_sentiments <- sotu_sentiments %>%
  mutate(sentiment_score=sentiment_points/max_sentiment_points*100)
sotu_metadata <- left_join(sotu_metadata, sotu_sentiments)
rm(sotu_sentiments)


#filtering stopwords
data("stop_words")
sotu_no_stopwords <- sotu_unnested %>%
  anti_join(stop_words)

#tidy data
sotu_tidy <- sotu_no_stopwords %>%
  count(year, word, sort = TRUE) %>%
  ungroup() %>%
  left_join(sotu_metadata) %>%
  select(president, party, sotu_type, year, word, n)


#tf-idf analysis
tf_idf_data <- sotu_tidy %>%
  bind_tf_idf(word, year, n) %>%
  arrange(desc(tf_idf)) %>%
  select(year, word, tf, idf, tf_idf)
sotu_tidy <- left_join(sotu_tidy, tf_idf_data)
rm(tf_idf_data)


# bigram analysis
sotu_bigram <- sotu_database %>%
  unnest_tokens(bigram, address, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  mutate(bigram = paste0(word1, " ", word2))

sotu_bigram_tidy <- sotu_bigram %>%
  group_by(president, year, party, sotu_type) %>%
  count(bigram, sort = TRUE) %>%
  ungroup()




# WRITE OUTPUTS

write.csv(sotu_metadata, file = file.path(dir_data, "sotu_metadata.csv"), row.names = FALSE)
write.csv(sotu_tidy, file = file.path(dir_data, "sotu_tidy.csv"), row.names = FALSE)
write.csv(sotu_bigram_tidy, file = file.path(dir_data, "sotu_bigram_tidy.csv"), row.names = FALSE)
