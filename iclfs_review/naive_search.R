# HEADER ----------------------------------------------------------------------
#
# Title:        Perform Naive Literature Search
# Description:  This script perform a naive search for literature regarding
#               Integrated Crop Livestock Forestry Systems of production in
#               the scopus database. The title, keywords and abstract section
#               from documents found in the search are analysed to find new
#               keywords related to the subject of interest. Keywords are
#               than saved in a csv table for further analysis.
#
# Author:       Hugo Tameirao Seixas
# Contact:      tameirao.hugo@gmail.com
# Date:         2021-03-16
#
# Notes:
#
# LIBRARIES -------------------------------------------------------------------
#
library(fulltext)
library(tidytext)
library(stopwords)
library(glue)
library(tidyverse)
#
# OPTIONS ---------------------------------------------------------------------
#
stop_words <- # A list of words we want to avoid in our exploration
  tibble(
    word = c(
      stopwords::stopwords(language = "pt"),
      stopwords::stopwords(language = "en")
    )
  )
#
# NAIVE SEARCH ----------------------------------------------------------------

## Set keywords for naive search ----
naive_keywords <-
  c( # Very specific set of words
    "iclfs",
    "crop-livestock-forestry",
    "crop-livestock-forest",
    "lavoura-pecuária-floresta"
  )

## Create naive query ----
naive_query <-
  glue( # We search for our words in the title, abstract and keywords sections
    "TITLE-ABS-KEY({glue_collapse(naive_keywords, sep = ' OR ')})"
  )

## Search in Scopus ----
scopus_naive <-
  scopus_search_loop(
    query = naive_query,
    type = "search",
    view = "COMPLETE",
    count = 300
  )

## Transform to tibble and select columns ----
naive_results_table <-
  scopus_naive$results %>%
  as_tibble() %>%
  select(
    scopus_id = `dc:identifier`, title = `dc:title`,
    abstract = `dc:description`, keywords = `authkeywords`
  )

# KEYWORDS SELECTION ----------------------------------------------------------

## Get common words ----
frequent_words <- naive_results_table %>%
  pivot_longer(
    cols = title:keywords,
    names_to = "section",
    values_to = "text"
  ) %>%
  group_by(scopus_id) %>%
  unnest_tokens(output = word, input = text) %>% # Each word becomes a row
  anti_join(stop_words, by = "word") %>%
  filter(!str_detect(word, "[0-9]"), !str_detect(word, "[[:punct:]]")) %>%
  count(word, sort = TRUE) %>%
  group_by(word) %>%
  summarise(
    n_doc = n(), # Amount of documents presenting a word
    n_word = sum(n) # Total amount of a word count in all documents
  ) %>%
  filter(n_doc >= 5)

## Get common bigrams ----
frequent_bigrams <- naive_results_table %>%
  pivot_longer(
    cols = title:keywords,
    names_to = "section",
    values_to = "text"
  ) %>%
  group_by(scopus_id) %>%
  unnest_tokens( # Each bigram becomes a row
    output = word,
    input = text,
    token = "ngrams",
    n = 2
  ) %>%
  separate(word, c("word1", "word2"), sep = " ") %>% # Separate bigram
  filter( # Remove stop words, numbers and symbols
    !word1 %in% stop_words$word,
    !word2 %in% stop_words$word,
    !str_detect(word1, "[0-9]"),
    !str_detect(word2, "[0-9]"),
    !str_detect(word1, "[[:punct:]]"),
    !str_detect(word2, "[[:punct:]]")
  ) %>%
  unite(word, word1, word2, sep = " ") %>%
  count(word, sort = TRUE) %>%
  group_by(word) %>%
  summarise(n_doc = n(), n_word = sum(n)) %>%
  filter(n_doc >= 5)

## Frequent trigrams ----
frequent_trigrams <- naive_results_table %>%
  pivot_longer(
    cols = title:keywords,
    names_to = "section",
    values_to = "text"
  ) %>%
  group_by(scopus_id) %>%
  unnest_tokens( # Each trigram becomes a row
    output = word,
    input = text,
    token = "ngrams",
    n = 3
  ) %>%
  separate(word, c("word1", "word2", "word3"), sep = " ") %>%
  filter(
    !word1 %in% stop_words$word,
    !word2 %in% stop_words$word,
    !word3 %in% stop_words$word,
    !str_detect(word1, "[0-9]"),
    !str_detect(word2, "[0-9]"),
    !str_detect(word3, "[0-9]"),
    !str_detect(word1, "[[:punct:]]"),
    !str_detect(word2, "[[:punct:]]"),
    !str_detect(word3, "[[:punct:]]")
  ) %>%
  unite(word, word1, word2, word3, sep = " ") %>%
  count(word, sort = TRUE) %>%
  group_by(word) %>%
  summarise(n_doc = n(), n_word = sum(n)) %>%
  filter(n_doc >= 5)

# Bind keywords tables ----
keywords <-
  bind_rows(frequent_words, frequent_bigrams, frequent_trigrams) %>%
  mutate(group = 0) %>%
  arrange(desc(n_doc), desc(n_word))

# Save keywords as a table ----
write_csv(keywords, "iclfs_review/naive_keywords.csv")
