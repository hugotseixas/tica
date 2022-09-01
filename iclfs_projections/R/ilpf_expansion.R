
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

  new_tree_pop <- sample(tree_pop, size = number_of_rows, replace = TRUE)

  new_system_management <- sample(system_management, size = number_of_rows, replace = TRUE)

  new_transition_management <- sample(transition_management, size = number_of_rows, replace = TRUE)

  new_year <- lulc %>%
    filter(year == y - 1) %>%
    mutate(
      year = y,
      new_cover = new_system,
      new_cycle = new_cycle,
      new_transition = new_transition,
      mew_transition_length = new_transition_length,
      new_tree_pop = new_tree_pop,
      new_system_management = new_system_management,
      new_transition_management = new_transition_management,
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
      transition = if_else(cycle_end - year == 0, new_transition, transition, missing = transition),
      transition_length = if_else(cycle_end - year == 0, new_transition_length, transition_length, missing = transition_length),
      cycle_lenght = if_else(cycle_end - year == 0, new_cycle, cycle_lenght, missing = cycle_lenght),
      cycle_end = if_else(cycle_end - year == 0, year + cycle_lenght + transition_length, cycle_end, missing = cycle_end)
    ) %>%
    select(cell:transition_length)

  lulc <- bind_rows(lulc, new_year)

}
