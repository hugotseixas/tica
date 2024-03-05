#' Merge Gridded data
#'
#' @param merged_data a
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

    merged_tables <-
      purrr::reduce(
        .x = tables_list,
        .f = \(t, ct) dplyr::full_join(
          t, ct,
          by = dplyr::join_by("cell_id", "year")
        )
      ) |>
      dplyr::arrange(.data$cell_id, .data$year)

    return(merged_tables)

  }

#' @export
fill_data <-
  function(merged_data) {

    filled_data <- merged_data |>
      tidyr::fill(
        "name_biome",
        .direction = "downup"
      ) |>
      tidyr::fill(
        dplyr::all_of(
          c(
            "conservation_units", "indigenous_territory",
            "highways", "quilombola_territory"
          )
        ),
        .direction = "down"
      )

    return(filled_data)

  }