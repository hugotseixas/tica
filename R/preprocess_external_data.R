#' Process external data to base grid
#'
#' @param external_data a
#' @param preprocess_function a
#'
#' @return An sf object
#'
#' @importFrom rlang :=
#' @importFrom rlang .data
#'
#' @export
preprocess_external_data <-
  function(
    external_data = "conservation_units",
    preprocess_function
  ) {

    # Set working directory
    base_dir <- here::here()

    base_grid <-
      sf::read_sf(
        glue::glue(
          "{base_dir}/data/grid.fgb"
        )
      )

    ext_data <-
      base::eval(
        rlang::parse_expr(
          preprocess_function
        )
      ) |>
      sf::st_transform(sf::st_crs(base_grid)) |>
      sf::st_filter(base_grid)

    sf::write_sf(
      obj = ext_data,
      dsn = glue::glue("{base_dir}/data/{external_data}.fgb"),
      driver = "FlatGeobuf",
      delete_dsn = TRUE,
      append = FALSE
    )

    return(ext_data)

  }