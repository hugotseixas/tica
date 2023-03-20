# HEADER ----------------------------------------------------------------------
#
# Title:          Get deforestation data
# Description:    This script downloads and process deforestation data from
#                 PRODES (http://terrabrasilis.dpi.inpe.br/). The data
#                 is sliced for each base grid cell, and transformed to
#                 a table with the deforestation area of each year for each
#                 cell.
#
# Notes:          This script download PRODES data to your local machine.
#                 The downloaded files are deleted by the end of the script.
#
# LIBRARIES -------------------------------------------------------------------
#
library(conflicted)
library(arrow)
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

if (!file_exists("data/temp/prodes_brasil_2021.tif")) {

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

}

# LOAD DATA -------------------------------------------------------------------

base_grid <-
  read_sf("data/base_grid.fgb") %>%
  arrange(cell_id)

prodes <- rast("data/temp/prodes_brasil_2021.tif")

# TRANSFORM DATA TO BASE GRID ------------------------------------------------

deforestation <-
  map_df(
    .x = base_grid$cell_id, # Map function to every grid cell
    .f = ~ {

      cat("Cell ", .x, "\r")

      # Filter the cell
      cell <- base_grid %>%
        filter(cell_id == .x)

      # Crop PRODES raster to the cell extent
      cell_subset <- crop(prodes, cell)

      # Get the classes values as a table
      classes <- cell_subset %>%
        as_tibble() %>%
        rename("class" = "prodes_brasil_2021")

      # Calculate the area for each class inside the cell
      area <- cell_subset %>%
        cellSize(mask = TRUE) %>%
        as_tibble() %>%
        select(area)

      # Create table with all years
      total_years <-
        tibble(
          year = 2000:2021
        )

      # Merge classes and areas and process data
      classes %>%
        bind_cols(area) %>%
        mutate( # Transform classes to years of deforestation
          year = case_when( # Add year of deforestation
            class < 50 ~ class + 2000,
            class > 50 & class < 90 ~ class - 40 + 2000,
            class > 90 ~ NA_integer_
          )
        ) %>%
        summarise( # Get the deforestation are by year
          area = sum(area, na.rm = TRUE),
          .by = "year"
        ) %>%
        full_join( # Add all years to the time series
          total_years,
          by = join_by(year)
        ) %>%
        arrange(year) %>%
        mutate(
          cell_id = .x # Add id column
        )

    }
  )

# SAVE RESULTING DATA ---------------------------------------------------------

# Write deforestation table to parquet file
write_parquet(
  x = deforestation,
  sink = "data/deforestation.parquet",
  version = "latest"
)

# CLEAN FILES -----------------------------------------------------------------

# Delete temporary folder with PRODES data
dir_delete("data/temp/")
