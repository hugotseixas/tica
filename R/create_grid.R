#' Create base grid for TICA
#'
#' @param resolution Set the resolution of the grid in degrees.
#' @param full_cells Choose if cells should completely inside the biomes.
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' base_grid <- create_grid(resolution = 0.2, full_cells = TRUE)
create_grid <- function(resolution, full_cells) {

  # Load biomes limit
  biomes <-
    geobr::read_biomes(
      year = 2019,
      simplified = TRUE, # Biome limits are simplified (topology preserved)
      showProgress = FALSE
    ) |>
    dplyr::filter(code_biome %in% c(1, 3))

  # Create grid based on polygons
  grid <-
    sf::st_make_grid(
      x = biomes,
      cellsize = resolution # Resolution of the grid cell
    ) |>
    sf::st_as_sf() |>
    sf::st_join(
      y = biomes,
      left = FALSE,
      # Choose filter function based on option above
      join = if (full_cells) { sf::st_within } else { sf::st_intersects }
    ) |>
    dplyr::select(name_biome, geometry) |>
    dplyr::mutate(
      cell_id = dplyr::row_number(),
      cell_area = sf::st_area(geometry)
    )

  return(grid)

}
