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
run_model <-
  function(filled_data, sampled_data) {

    model_pred <-
      purrr::map(
        .x = dplyr::distinct(sampled_data, sample) |> dplyr::pull(),
        .f = \(s) {

          train_data <- sampled_data |>
            dplyr::filter(.data$sample == s, .data$split_class == "train") |>
            dplyr::left_join(
              filled_data |> sf::st_drop_geometry(),
              by = dplyr::join_by("cell_id", "year")
            )

          test_data <- sampled_data |>
            dplyr::filter(.data$sample == s, .data$split_class == "test") |>
            dplyr::left_join(
              filled_data |> sf::st_drop_geometry(),
              by = dplyr::join_by("cell_id", "year")
            )

          veg_suppression_rec <-
            recipes::recipe(veg_suppression ~ ., data = train_data) |>
            recipes::update_role("cell_id", "year", new_role = "ID") |>
            recipes::step_zv(recipes::all_predictors()) |>
            recipes::step_dummy(recipes::all_nominal_predictors())

          veg_suppression_mod <-
            parsnip::rand_forest(trees = 500) |>
            parsnip::set_engine("ranger") |>
            parsnip::set_mode("regression")

          veg_suppression_wflow <-
            workflows::workflow() |>
            workflows::add_model(veg_suppression_mod) |>
            workflows::add_recipe(veg_suppression_rec)

          veg_suppression_fit <- veg_suppression_wflow |>
            parsnip::fit(data = train_data)

          veg_suppression_pred <-
            predict(veg_suppression_fit, test_data) |>
            dplyr::bind_cols(
              test_data |>
                dplyr::select(
                  "veg_suppression", "cell_id", "year", "name_biome"
                )
            ) |>
            dplyr::mutate(sample = s)

          return(veg_suppression_pred)

        }
      ) |>
      purrr::list_rbind()

    arrow::write_parquet(
      model_pred,
      sink = "./data/results/model_pred.parquet"
    )

    return(model_pred)

  }