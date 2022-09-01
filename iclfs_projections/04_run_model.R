# HEADER ----------------------------------------------------------------------
#
# Title:
# Description:
#
#
#
# Authors:      Hugo Tameirao Seixas
# Contact:      seixas.hugo@protonmail.com
# Date:         2022-07-07
#
# Notes:
#
#
#
#
#
# LIBRARIES -------------------------------------------------------------------
#
library(terra)
library(tidyverse)
#
# OPTIONS ---------------------------------------------------------------------
#
#
# LOAD BASE GRID --------------------------------------------------------------

## Load raster file
r <- rast("iclfs_projections/data/base_grid_pasture.tif")

# GENERATE RANDOM DATA AS DUMMIES ---------------------------------------------

## Generate random system types to be sampled
systems <-
  reduce(
    map2(
      .x = c(2, 3, 4, 5, 6),
      .y = sample(1:30, 5, replace = TRUE),
      .f = ~ { rep(.x, .y) }),
    c
  )

## Generate random cycle lengths to be sampled
cycle <-
  reduce(
    map2(
      .x = c(10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20),
      .y = sample(1:30, 11, replace = TRUE),
      .f = ~ { rep(.x, .y) }),
    c
  )

## Generate random transition system type to be sampled
transition_systems <-
  reduce(
    map2(
      .x = c(1, 2, 3, 4, 5, 6),
      .y = sample(1:30, 6, replace = TRUE),
      .f = ~ { rep(.x, .y) }),
    c
  )

## Generate random transition lengths to be sampled
transition_length <-
  reduce(
    map2(
      .x = c(1, 2, 3, 4),
      .y = sample(1:30, 4, replace = TRUE),
      .f = ~ { rep(.x, .y) }),
    c
  )

## Generate random trees population/ha to be sampled
tree_pop <-
  reduce(
    map2(
      .x = c(333, 400, 476, 500, 370, 690, 715, 900, 1292),
      .y = sample(1:40, 9, replace = TRUE),
      .f = ~ { rep(.x, .y) }),
    c
  )

## Generate ICLFS management to be sampled
system_management <-
  reduce(
    map2(
      .x = c(1, 2, 3, 4, 5),
      .y = sample(1:40, 5, replace = TRUE),
      .f = ~ { rep(.x, .y) }),
    c
  )

## Generate transition management to be sampled
transition_management <-
  reduce(
    map2(
      .x = c(1, 2, 3, 4, 5),
      .y = sample(1:40, 5, replace = TRUE),
      .f = ~ { rep(.x, .y) }),
    c
  )

# PREPARE DATA TO START THE LAND COVER PROJECTIONS ----------------------------

## Get degraded pasture from raster file and transform into a data frame
lulc <- as_tibble(as.data.frame(r, na.rm = FALSE, cells = TRUE)) %>%
  drop_na() %>%
  rename(area = pasture_quality_2020) %>%
  mutate(
    year = 2020,
    cover = 1,
    cycle_lenght = NA_real_,
    cycle_end = NA_real_,
    transition = NA_real_,
    transition_length = NA_real_,
    tree_pop = NA_real_,
    system_management = NA_real_,
    transition_management = NA_real_
  )

## Get number of rows that each year of simulation will have
number_of_rows <- nrow(lulc)

## Calculate a linear expansion rate
annual_increase <- floor(pull(global(r, sum, na.rm = TRUE) / 70))

# START LAND COVER PROJECTIONS ------------------------------------------------

for (y in 2021:2100) {

  sample_pool <- lulc %>%
    filter(
      year == y - 1,
      cover == 1
    )

  sample <- sample_pool %>%
    sample_frac()

  sample_sub <- sample %>%
    mutate(area_sum = cumsum(area)) %>%
    filter(area_sum < annual_increase + 10)

  sample_system <- sample(systems, size = number_of_rows, replace = TRUE)

  sample_cycle <- sample(cycle, size = number_of_rows, replace = TRUE)

  sample_transition <- sample(transition_systems, size = number_of_rows, replace = TRUE)

  sample_transition_length <- sample(transition_length, size = number_of_rows, replace = TRUE)

  sample_tree_pop <- sample(tree_pop, size = number_of_rows, replace = TRUE)

  sample_system_management <- sample(system_management, size = number_of_rows, replace = TRUE)

  sample_transition_management <- sample(transition_management, size = number_of_rows, replace = TRUE)

  new_year <- lulc %>%
    filter(year == y - 1) %>%
    mutate(
      year = y,
      new_cover = sample_system,
      new_cycle = sample_cycle,
      new_transition = sample_transition,
      new_transition_length = sample_transition_length,
      new_tree_pop = sample_tree_pop,
      new_system_management = sample_system_management,
      new_transition_management = sample_transition_management,
      transition = if_else(cell %in% sample_sub$cell, new_transition, transition),
      transition_length = if_else(cell %in% sample_sub$cell, new_transition_length, transition_length),
      cover = if_else(cell %in% sample_sub$cell, new_system, cover),
      cycle_lenght = if_else(cell %in% sample_sub$cell, new_cycle, cycle_lenght),
      cycle_end = if_else(cell %in% sample_sub$cell, year + cycle_lenght + transition_length, cycle_end),
      tree_pop = if_else(cell %in% sample_sub$cell, new_tree_pop, tree_pop),
      system_management = if_else(cell %in% sample_sub$cell, new_system_management, system_management),
      transition_management = if_else(cell %in% sample_sub$cell, new_transition_management, transition_management)
    )

  new_year <- new_year %>%
    mutate(
      cover = if_else(cycle_end - year == 0, new_system, cover, missing = cover),
      tree_pop = if_else(cycle_end - year == 0, new_tree_pop, tree_pop, missing = tree_pop),
      system_management = if_else(cycle_end - year == 0, new_system_management, system_management, missing = system_management),
      transition_management = if_else(cycle_end - year == 0, new_transition_management, transition_management, missing = transition_management),
      transition = if_else(cycle_end - year == 0, new_transition, transition, missing = transition),
      transition_length = if_else(cycle_end - year == 0, new_transition_length, transition_length, missing = transition_length),
      cycle_lenght = if_else(cycle_end - year == 0, new_cycle, cycle_lenght, missing = cycle_lenght),
      cycle_end = if_else(cycle_end - year == 0, year + cycle_lenght + transition_length, cycle_end, missing = cycle_end)
    ) %>%
    select(
      cell, area, year, cover, transition, transition_length, cycle_lenght,
      cycle_end, tree_pop, system_management, transition_management
    )

  lulc <- bind_rows(lulc, new_year)

}

lulc %>%
  filter(cell == sample(lulc$cell, size = 1)) %>%
  view()
