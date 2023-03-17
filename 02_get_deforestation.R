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

deforestation <-
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

      total_years <-
        tibble(
          year = 2000:2021
        )

      classes %>%
        bind_cols(area) %>%
        mutate(
          year = case_when( # Add year of deforestation
            class < 50 ~ class + 2000,
            class > 50 & class < 90 ~ class - 40 + 2000,
            class > 90 ~ NA_integer_
          )
        ) %>%
        summarise(
          area = sum(area, na.rm = TRUE),
          .by = "year"
        ) %>%
        full_join(total_years, by = join_by(year)) %>%
        arrange(year) %>%
        mutate(
          cell_id = .x # Add id column
        )

    }
  )
