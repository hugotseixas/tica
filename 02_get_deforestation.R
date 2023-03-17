# HEADER ----------------------------------------------------------------------
#
# Title:          Get deforestation data
# Description:
#
# Notes:
#
# LIBRARIES -------------------------------------------------------------------
#
library(conflicted)
library(sf)
library(terra)
library(curl)
library(fs)
library(glue)
library(tidyverse)
#
# CONFLICTS -------------------------------------------------------------------
#

conflicts_prefer(dplyr::filter)

#
# OPTIONS ---------------------------------------------------------------------
#

# Data URL
data_url <-
  glue(
    "http://terrabrasilis.dpi.inpe.br/download/dataset/",
    "brasil-prodes/raster/prodes_brasil_2021.zip"
  )

#
# DOWNLOAD DATA ---------------------------------------------------------------

dir_create("data/temp/")

curl_download(
  url = data_url,
  destfile = "./data/temp/prodes.zip",
  quiet = FALSE
)

unzip(
  zipfile = "./data/temp/prodes.zip",
  files = "prodes_brasil_2021.tif",
  exdir = "./data/temp/"
)

# LOAD DATA -------------------------------------------------------------------

base_grid <-
  read_sf("data/base_grid.fgb") %>%
  arrange(cell_id)

prodes <- read_stars("data/temp/prodes_brasil_2021.tif")

prodes <- rast("data/temp/prodes_brasil_2021.tif")

# TRANSFORM DATA TO BASE GRID ------------------------------------------------

deforastation <-
  map_df(
    .x = base_grid$cell_id,
    .f = ~ {

      cat(.x, "\r")

      cell <- base_grid %>%
        filter(cell_id == .x)

      cell_subset <- crop(prodes, cell)

      classes <- cell_subset %>%
        as_tibble() %>%
        rename("class" = "prodes_brasil_2021")

      area <- cell_subset %>%
        cellSize() %>%
        as_tibble() %>%
        select(area)

      classes %>%
        bind_cols(area) %>%
        summarise(
          area = sum(area, na.rm = TRUE),
          .by = "class"
        ) %>%
        mutate(cell_id = .x)

    }
  )


