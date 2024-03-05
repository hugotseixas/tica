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
process_external_data <-
  function(
    data_name,
    read_function,
    process_function
  ) {

    options(
      cli.progress_bar_style = list(
        complete = cli::col_white("\u25AE"),
        incomplete = cli::col_white("\u25AF")
      ),
      cli.progress_show_after = 0.5
    )

    read_expression <- eval(rlang::parse_expr(read_function))

    process_expression <- eval(rlang::parse_expr(process_function))

    # Set working directory
    base_dir <- here::here()

    base_grid <-
      sf::read_sf(
        glue::glue(
          "{base_dir}/data/grid.fgb"
        )
      ) |>
      sf::st_set_agr("constant") |>
      dplyr::arrange(.data$cell_id)

    cli::cli_h3(
      data_name |>
        stringr::str_replace_all("_", " ") |>
        stringr::str_to_title()
    )

    cat("\n")

    cli::cli_alert_info("Reading data...")

    raw_data <- read_expression()

    cat("\r")

    cli::cli_alert_success("Data reading was completed!")

    cat("\n")

    gridded_data <-
      purrr::map_df(
        .progress = list(
          type = "iterator",
          clear = FALSE,
          format = "Progress: {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}",
          format_done = paste(
            "{cli::col_green(cli::symbol$tick)} Data processing completed",
            "in {cli::pb_elapsed}."
          )
        ),
        .x = base_grid$cell_id,
        .f = \(id) {

          base_cell <- base_grid |>
            dplyr::filter(.data$cell_id == id)

          ext_data <- process_expression(raw_data, base_cell)

          return(ext_data)

        }
      )

    dest_path <- glue::glue("{base_dir}/data/gridded/{data_name}.parquet")

    arrow::write_parquet(
      x = gridded_data,
      sink = dest_path,
      version = "2.6"
    )

    cli::cli_alert_info(
      "Processed data saved to: {.path {dest_path}}"
    )

    cli::cli_par()

  }