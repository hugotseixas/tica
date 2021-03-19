# HEADER ----------------------------------------------------------------------
#
# Title:        Perform Literature Search and Download Documents
# Description:
#
# Author:       Hugo Tameirao Seixas
# Contact:      tameirao.hugo@gmail.com
# Date:         2021-03-18
#
# Notes:
#
# LIBRARIES -------------------------------------------------------------------
#
library(fulltext)
library(glue)
library(tidyverse)
#
# OPTIONS ---------------------------------------------------------------------
#

#
# LOAD AND CLEAN NAIVE KEYWORDS -----------------------------------------------

naive_keywords_classes <-
  read_csv("iclfs_review/naive_keywords_classes.csv") %>%
  filter(group != 0) # Remove useless keywords

# LITERATURE SEARCH -----------------------------------------------------------

## Get keywords and create query ----

### Filter keywords ----
keywords <- naive_keywords_classes %>%
  filter(group == 1) %>% # Our search will be performed using group 1
  pull(word)

### Concatenate words with OR statement ----
iclfs_query <- glue_collapse(keywords, sep = '" OR "')

### Create query ----
iclfs_query <- glue('TITLE-ABS-KEY("{iclfs_query}")')

## Search documents in Scopus ----

### Core search ----
iclfs_search <-
  scopus_search(query = iclfs_query,content = "core")

### Get total numbrs of found documents ----
total_results <-
  as.numeric(iclfs_search$`search-results`$`opensearch:totalResults`)

### Get the results ----
iclfs_search <-
  scopus_search_loop(
    query = iclfs_query,
    type = "search",
    view = "STANDARD",
    count = total_results
  )

## Transform to tibble and select columns ----
iclfs_results <-
  iclfs_search$results %>%
  as_tibble() %>%
  select(
    link, scopus_id = `dc:identifier`, title = `dc:title`,
    doi = `prism:doi`, openaccess
  )

## Download documents ----

### Set path to save documents ----
cache_options_set(full_path = "iclfs_review/bib/")

### Perform download ----
walk(iclfs_results$doi, ~ { try(ft_get(.x, type = "pdf")) })
