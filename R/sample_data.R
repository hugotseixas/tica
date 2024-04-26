#' Sample Gridded data
#'
#' @param filled_data a
#'
#' @return A tibble
#'
#' @importFrom rlang :=
#' @importFrom rlang .data
#'
#' @export
sample_data <-
  function(filled_data, seed) {

    set.seed(seed)

    spatial_data_split <-
      spatialsample::spatial_clustering_cv(
        sf::read_sf("./data/grid.fgb"),
        v = 20,
        buffer = 700000,
        radius = 70000
      )

    spatial_data_split <-
      purrr::map(
        .x = seq_len(nrow(spatial_data_split)),
        .f = \(f) {

          test_cells <-
            spatialsample::get_rsplit(spatial_data_split, f) |>
            spatialsample::assessment() |>
            sf::st_drop_geometry() |>
            dplyr::mutate(split_spatial = "test")

          train_cells <-
            spatialsample::get_rsplit(spatial_data_split, f) |>
            spatialsample::analysis() |>
            sf::st_drop_geometry() |>
            dplyr::mutate(split_spatial = "train")

          test_cells |>
            dplyr::bind_rows(train_cells) |>
            tidyr::nest()

        }
      )

    temporal_data_split <-
      purrr::map(
        .x = c(seed:(seed + 59)),
        .f = \(s) {

          sample_year <- filled_data |>
            tibble::as_tibble() |>
            dplyr::select("year") |>
            dplyr::distinct(.data$year) |>
            dplyr::slice(-c(1:2)) |>
            dplyr::slice(1:(dplyr::n() - 2)) |>
            dplyr::slice_sample(n = 1) |>
            dplyr::pull("year")

          train_years <-
            tibble::tibble(
              year = c(
                (sample_year - 2):sample_year,
                sample_year:(sample_year + 2)
              )
            ) |>
            dplyr::mutate(split_temporal = "train")

          test_years <- filled_data |>
            tibble::as_tibble() |>
            dplyr::select("year") |>
            dplyr::distinct(.data$year) |>
            dplyr::filter(
              !.data$year %in% c(
                (sample_year - 4):sample_year,
                sample_year:(sample_year + 4)
              )
            ) |>
            dplyr::slice_sample(n = 2) |>
            dplyr::mutate(split_temporal = "test")

          train_years |>
            dplyr::bind_rows(test_years) |>
            tidyr::nest()

        }
      )

    temporal_data_split <-
      temporal_data_split[!duplicated(temporal_data_split)][1:40]

    split_list <-
      tidyr::crossing(spatial_data_split, temporal_data_split) |>
      dplyr::mutate(sample = dplyr::row_number())

    data_split <-
      purrr::pmap(
        .l = split_list,
        .f = \(spatial_data_split, temporal_data_split, sample) {

          t <- temporal_data_split

          s <- spatial_data_split |>
            tidyr::unnest("data") |>
            dplyr::mutate(test = t[[1]]) |>
            tidyr::unnest("test") |>
            dplyr::filter(.data$split_spatial == .data$split_temporal) |>
            dplyr::rename("split_class" = "split_spatial") |>
            dplyr::select("cell_id", "year", "split_class") |>
            dplyr::mutate(sample = sample)

          return(s)

        }
      ) |>
      purrr::list_rbind()

    return(data_split)

  }
