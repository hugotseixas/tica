#' Merge Gridded data
#'
#' @return A tibble
#'
#' @importFrom rlang :=
#' @importFrom rlang .data
#'
#' @export
merge_data <-
  function() {

    # Set working directory
    base_dir <- here::here()

    files_path <- fs::dir_ls(glue::glue("{base_dir}/data/gridded/"))

    base_grid <-
      sf::read_sf(
        glue::glue(
          "{base_dir}/data/grid.fgb"
        )
      ) |>
      sf::st_set_agr("constant") |>
      dplyr::arrange(.data$cell_id)

    tables_list <-
      purrr::map(
        .x = files_path,
        .f = \(file) {

          data_table <-
            arrow::read_parquet(file, as_data_frame = FALSE) |>
            dplyr::mutate(
              dplyr::across(
                .cols = c("cell_id", "year"),
                .fns = ~ arrow::cast(.x, arrow::int16())
              )
            ) |>
            dplyr::collect()

          base_table <-
            tidyr::expand_grid(
              cell_id = base_grid$cell_id,
              year = min(data_table$year):max(data_table$year)
            )

          data_table <- base_table |>
            dplyr::left_join(
              data_table,
              by = dplyr::join_by("cell_id", "year")
            ) |>
            dplyr::mutate(
              dplyr::across(
                dplyr::where(is.numeric), \(x) tidyr::replace_na(x, 0)
              )
            )

        }
      )

    merged_data <-
      purrr::reduce(
        .x = tables_list,
        .f = \(t, ct) dplyr::full_join(
          t, ct,
          by = dplyr::join_by("cell_id", "year")
        )
      ) |>
      dplyr::arrange(.data$cell_id, .data$year) |>
      dplyr::left_join(
        base_grid,
        by = dplyr::join_by("cell_id")
      ) |>
      sf::st_as_sf() |>
      sf::st_set_agr("constant")

    merged_data <-
      merged_data |>
      dplyr::mutate(
        lon = sf::st_coordinates(sf::st_centroid(merged_data))[, 1],
        lat = sf::st_coordinates(sf::st_centroid(merged_data))[, 2]
      )

    sf::write_sf(
      obj = merged_data,
      dsn = glue::glue("{base_dir}/data/merged/merged_data.fgb"),
      driver = "FlatGeobuf",
      delete_dsn = TRUE,
      append = FALSE
    )

    return(merged_data)

  }

#' @export
fill_data <-
  function(merged_data) {

    # Set working directory
    base_dir <- here::here()

    filled_data <- merged_data |>
      dplyr::arrange(.data$cell_id, .data$year) |>
      dplyr::group_by(.data$cell_id) |>
      tidyr::fill(
        dplyr::all_of("name_biome"),
        .direction = "downup"
      ) |>
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(c("monitored", "priority")),
          ~ dplyr::if_else(is.na(.x), 0, .x)
        ),
        dplyr::across(
          dplyr::all_of(
            c(
              "conservation_units", "indigenous_territory",
              "quilombola_territory", "highways"
            )
          ),
          ~ cumsum(dplyr::coalesce(.x, 0)) + .x * 0
        )
      ) |>
      dplyr::ungroup()

    sf::write_sf(
      obj = filled_data,
      dsn = glue::glue("{base_dir}/data/merged/filled_data.fgb"),
      driver = "FlatGeobuf",
      delete_dsn = TRUE,
      append = FALSE
    )

    return(filled_data)

  }