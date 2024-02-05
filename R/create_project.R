create_project <-
  function() {

    # Set working directory
    base_dir <- here::here()

    purrr::walk(
      .x = list("download_external_data", "preprocess_external_data"),
      .f = \(f) {

        args_table <-
          tibble::tibble(args = formalArgs(f), loop = " ") |>
          dplyr::filter(args != "...") |>
          tidyr::pivot_wider(names_from = "args", values_from = "loop") |>
          dplyr::mutate(id = 1, .before = 1)

        args_table_path <-
          glue::glue(
            "{base_dir}/analysis/arguments_{f}.csv"
          )

        if (fs::file_exists(args_table_path)) {
          return(invisible(NULL))
        }

        readr::write_csv(
          args_table,
          file = args_table_path
        )

        return(invisible(NULL))

      }
    )

  }