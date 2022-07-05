
library(terra)
library(tidyverse)

r <- rast("iclfs_projections/base_grid_pasture.tif")

# Calculate a linear expansion rate
annual_increase <- floor(pull(global(r, sum, na.rm = TRUE) / 30))

# Generate random system types to be sampled
systems <-
  reduce(
    map2(
      .x = c(2, 3, 4, 5, 6),
      .y = sample(1:30, 5, replace = TRUE),
      .f = ~ { rep(.x, .y) }),
    c
  )

# Generate random cycle lengths to be sampled
cycle <-
  reduce(
    map2(
      .x = c(10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20),
      .y = sample(1:30, 11, replace = TRUE),
      .f = ~ { rep(.x, .y) }),
    c
  )

# Generate random transition system type to be sampled
transition_systems <-
  reduce(
    map2(
      .x = c(1, 2, 3, 4, 5, 6),
      .y = sample(1:30, 6, replace = TRUE),
      .f = ~ { rep(.x, .y) }),
    c
  )

# Generate random transition lengths to be sampled
transition_length <-
  reduce(
    map2(
      .x = c(1, 2, 3, 4),
      .y = sample(1:30, 4, replace = TRUE),
      .f = ~ { rep(.x, .y) }),
    c
  )


# Get degraded pasture from raster file and transform into a data frame
lulc <- as_tibble(as.data.frame(r, na.rm = FALSE, cells = TRUE)) %>%
  drop_na() %>%
  rename(area = pasture_quality_2020) %>%
  mutate(
    year = 2020,
    cover = 1,
    cycle_lenght = NA_real_,
    cycle_end = NA_real_,
    transition = NA_real_,
    transition_length = NA_real_
  )

# Get number of rows that each year of simulation will have
number_of_rows <- nrow(lulc)

for (y in 2021:2050) {

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

  new_system <- sample(systems, size = number_of_rows, replace = TRUE)

  new_cycle <- sample(cycle, size = number_of_rows, replace = TRUE)

  new_transition <- sample(transition_systems, size = number_of_rows, replace = TRUE)

  new_transition_length <- sample(transition_length, size = number_of_rows, replace = TRUE)

  new_year <- lulc %>%
    filter(year == y - 1) %>%
    mutate(
      year = y,
      new_cover = new_system,
      new_cycle = new_cycle,
      new_transition = new_transition,
      mew_transition_length = new_transition_length,
      transition = if_else(cell %in% sample_sub$cell, new_transition, transition),
      transition_length = if_else(cell %in% sample_sub$cell, new_transition_length, transition_length),
      cover = if_else(cell %in% sample_sub$cell, new_system, cover),
      cycle_lenght = if_else(cell %in% sample_sub$cell, new_cycle, cycle_lenght),
      cycle_end = if_else(cell %in% sample_sub$cell, year + cycle_lenght + transition_length, cycle_end)
    )

  new_year <- new_year %>%
    mutate(
      cover = if_else(cycle_end - year == 0, new_system, cover, missing = cover),
      transition = if_else(cycle_end - year == 0, new_transition, transition, missing = transition),
      transition_length = if_else(cycle_end - year == 0, new_transition_length, transition_length, missing = transition_length),
      cycle_lenght = if_else(cycle_end - year == 0, new_cycle, cycle_lenght, missing = cycle_lenght),
      cycle_end = if_else(cycle_end - year == 0, year + cycle_lenght + transition_length, cycle_end, missing = cycle_end)
    ) %>%
    select(cell:transition_length)

  lulc <- bind_rows(lulc, new_year)

}

lulc %>%
  group_by(year, cover) %>%
  summarise(area = sum(area)) %>%
  ggplot() +
  geom_line(aes(x = year, y = area, color = factor(cover)))

r_new <- r

bla <- as_tibble(as.data.frame(r, na.rm = FALSE, cells = TRUE)) %>%
  left_join(lulc %>% filter(year == 2035) %>% select(cell, cover), by = "cell")

values(r_new) <- as.integer(bla$cover)

plot(r_new, type = "classes", col = c("gray", "green", "blue", "red", "purple", "orange"))
