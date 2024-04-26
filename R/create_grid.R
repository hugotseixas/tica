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
    resolution
  ) {

    # Set working directory
    base_dir <- here::here()

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
        square = FALSE
      ) |>
      sf::st_as_sf() |>
      sf::st_set_geometry("geometry") |>
      sf::st_filter(spatial_data, .predicate = sf::st_intersects) |>
      sf::st_filter(spatial_data, .predicate = sf::st_within) |>
      dplyr::mutate(cell_id = dplyr::row_number()) |>
      dplyr::arrange(dplyr::pick("cell_id")) |>
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
