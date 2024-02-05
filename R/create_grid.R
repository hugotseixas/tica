#' Create base grid for TICA
#'
#' @param resolution Set the resolution of the grid in degrees.
#' @param full_cells Choose if cells should completely inside the biomes.
#' @param aoi Path to file of area of interest.
#' @param crs Coordinate reference system
#' @param shape Shape of the grid cells.
#'
#' @return A tibble.
#'
#' @importFrom rlang :=
#' @importFrom rlang .data
#'
#' @export
#'
create_grid <-
  function(
    aoi,
    crs,
    resolution,
    full_cells = TRUE,
    shape = "hex"
  ) {

    # Set working directory
    base_dir <- here::here()

    if (shape == "hex") { shape_option <- FALSE }
    if (shape == "square") { shape_option <- TRUE }

    base::stopifnot(base::exists("shape_option"))

    # Load biomes limit
    if (is.character(aoi)) {

      spatial_data <- sf::read_sf(aoi)

    } else if (is(aoi, "sf")) {

      spatial_data <- aoi

    }

    spatial_data <- spatial_data |>
      sf::st_transform(sf::st_crs(crs)) |>
      sf::st_set_agr("constant")

    # Create grid based on polygons
    grid <-
      sf::st_make_grid(
        x = spatial_data,
        cellsize = resolution, # Resolution of the grid cell
        square = shape_option
      ) |>
      sf::st_as_sf() |>
      sf::st_filter(spatial_data) |>
      sf::st_join(
        y = spatial_data,
        left = FALSE,
        # Choose filter function based on option above
        join = if (full_cells) { sf::st_within } else { sf::st_intersects }
      ) |>
      dplyr::mutate(cell_id = dplyr::row_number()) |>
      dplyr::relocate("cell_id") |>
      dplyr::select("cell_id", "geometry")

    sf::write_sf(
      obj = grid,
      dsn = glue::glue("{base_dir}/data/grid.fgb"),
      driver = "FlatGeobuf",
      delete_dsn = TRUE,
      append = FALSE
    )

    base::return(grid)

  }
