# HEADER ----------------------------------------------------------------------
#
# Title:          Get protected areas data
# Description:
#
# Notes:
#
# LIBRARIES -------------------------------------------------------------------
#
library(conflicted)
library(arrow)
library(sf)
library(geobr)
library(janitor)
library(units)
library(glue)
library(tidyverse)
#
# CONFLICTS -------------------------------------------------------------------
#

conflicts_prefer(dplyr::filter)

#
# OPTIONS ---------------------------------------------------------------------
#

#
# LOAD DATA -------------------------------------------------------------------

base_grid <-
  read_sf("data/base_grid.fgb") %>%
  arrange(cell_id)

# Get UC data from geobr
c_units <-
  read_conservation_units() %>%
  st_make_valid()

# TRANSFORM DATA TO BASE GRID -------------------------------------------------

# Select UC columns and fix dates
c_units <- c_units %>%
  select(
    code_conservation_unit, category, group, government_level,
    creation_year, geom
  ) %>%
  mutate(
    creation_year = str_sub(creation_year, start = -4),
    creation_year = as.numeric(creation_year)
  )

st_agr(c_units) = "constant"

st_agr(base_grid) = "constant"

uc_grid <-
  map_df(
    .x = base_grid$cell_id, # Map function to every grid cell
    .f = ~ {

      cat("Cell ", .x, "\r")

      # Filter the cell
      cell <- base_grid %>%
        filter(cell_id == .x)

      cell_units <- c_units %>%
        st_intersection(cell)

      # Create table with all years
      total_years <-
        tibble(
          year = 2000:2021
        )

      if (nrow(cell_units) == 0) {

        cell_units <- tibble_row(
          !!!names(cell_units)
        ) %>%
          clean_names() %>%
          mutate(
            across(.cols = everything(), ~ NA),
            uc_area = set_units(NA_integer_, "m^2", mode = "standard"),
            cell_id = .x
          ) %>%
          select(!geom) %>%
          bind_cols(total_years)

      } else {

        cell_units <- cell_units %>%
          mutate(uc_area = st_area(.)) %>%
          as_tibble() %>%
          select(!geom) %>%
          mutate(year = creation_year) %>%
          full_join(
            total_years,
            by = join_by(year)
          ) %>%
          arrange(year) %>%
          mutate(cell_id = .x)

      }

      return(cell_units)

    }
  )

# SAVE RESULTING DATA ---------------------------------------------------------

# Write deforestation table to parquet file
write_parquet(
  x = uc_grid,
  sink = "data/uc.parquet",
  version = "latest"
)
