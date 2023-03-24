create_grid <- function(resolution, full_cells) {

  # Load biomes limit
  biomes <-
    read_biomes(
      year = 2019,
      simplified = TRUE, # Biome limits are simplified (topology preserved)
      showProgress = FALSE
    ) %>%
    filter(name_biome %in% c("Amazônia", "Cerrado"))

  # Create grid based on polygons
  grid <-
    st_make_grid(
      x = biomes,
      cellsize = resolution # Resolution of the grid cell
    ) %>%
    st_as_sf() %>%
    st_join(
      y = biomes,
      left = FALSE,
      # Choose filter function based on option above
      join = if (full_cells) {st_within} else {st_intersects}
    ) %>%
    select(name_biome, geometry) %>%
    mutate(
      cell_id = row_number(),
      cell_area = st_area(geometry)
    )

  return(grid)

}
