#' Create base grid for TICA
#'
#' @param resolution Set the resolution of the grid in degrees.
#' @param full_cells Choose if cells should completely inside the biomes.
#' @param aoi_path Path to file of area of interest.
#' @param shape Shape of the grid cells.
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' \dontrun{
#' base_grid <- create_grid(resolution = 0.2, full_cells = TRUE)
#' }
#'
create_grid <-
  function(
    aoi_path = "./data/external/aoi/aoi.fgb",
    resolution = 0.5,
    full_cells = TRUE,
    shape = "hex"
  ) {

    if (shape == "hex") { shape_option <- FALSE }
    if (shape == "square") { shape_option <- TRUE }

    base::stopifnot(base::exists("shape_option"))

    # Load biomes limit
    aoi <- sf::read_sf(aoi_path)

    # Create grid based on polygons
    grid <-
      sf::st_make_grid(
        x = aoi,
        cellsize = resolution, # Resolution of the grid cell
        square = shape_option
      ) |>
      sf::st_as_sf() |>
      sf::st_join(
        y = aoi,
        left = FALSE,
        # Choose filter function based on option above
        join = if (full_cells) { sf::st_within } else { sf::st_intersects }
      ) |>
      dplyr::mutate(
        cell_id = dplyr::row_number(),
        cell_area = base::as.numeric(sf::st_area(geometry))
      ) |>
      dplyr::relocate(cell_id)

    base::return(grid)

  }
