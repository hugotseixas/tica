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
  function(filled_data, year_min, year_max, seed) {

    filled_data <- filled_data |>
      dplyr::filter(.data$year >= year_min, .data$year <= year_max)

    set.seed(seed)

    spatial_data_split <-
      spatialsample::spatial_buffer_vfold_cv(
        sf::read_sf("./data/grid.fgb"),
        v = 20,
        repeats = 5,
        buffer = 50000,
        radius = NULL
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
        .x = c(seed:length(spatial_data_split)),
        .f = \(s) {

          set.seed(s)

          train_years <- filled_data |>
            tibble::as_tibble() |>
            dplyr::select("year") |>
            dplyr::distinct(.data$year) |>
            dplyr::arrange(.data$year) |>
            dplyr::mutate(
              triples = sort(
                rep(
                  1:(length(year_min:year_max) / 3),
                  length.out = length(year_min:year_max)
                )
              )
            ) |>
            dplyr::filter(
              .data$triples %in% sample(unique(.data$triples), 6)
            ) |>
            dplyr::mutate(split_temporal = "train") |>
            dplyr::select(!"triples")

          set.seed(s)

          test_years <- filled_data |>
            tibble::as_tibble() |>
            dplyr::select("year") |>
            dplyr::distinct(.data$year) |>
            dplyr::arrange(.data$year) |>
            dplyr::filter(
              !.data$year %in% c(
                train_years$year,
                train_years$year + 1,
                train_years$year - 1
              )
            ) |>
            dplyr::mutate(split_temporal = "test")

          train_years |>
            dplyr::bind_rows(test_years) |>
            tidyr::nest()

        }
      )

    split_list <-
      tibble::tibble(
        spatial_data_split = spatial_data_split,
        temporal_data_split = temporal_data_split
      ) |>
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

    arrow::write_parquet(
      x = data_split,
      sink = "./data/sampled_data.parquet",
      version = "2.6"
    )

    return(data_split)

  }
