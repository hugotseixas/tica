#' Sample Gridded data
#'
#' @param filled_data a
#'
#' @return A tibble
#'
#' @import bonsai
#'
#' @importFrom rlang :=
#' @importFrom rlang .data
#'
#' @export
#' @rdname run_model
run_model_ensemble <-
  function(filled_data, sampled_data) {

    model_data <- filled_data |>
      sf::st_drop_geometry() |>
      tidyr::drop_na() |>
      dplyr::select(
        dplyr::any_of(
          c(
            "conservation_units", "highways", "indigenous_territory",
            "pasture", "temporary_crop", "forest_formation",
            "savanna_formation", "priority", "monitored",
            "quilombola_territory", "veg_suppression",
            "lon", "lat", "cell_id", "year", "name_biome"
          )
        )
      )

    veg_suppression_rec <-
      recipes::recipe(
        veg_suppression ~ .,
        data = model_data
      ) |>
      recipes::update_role("cell_id", new_role = "ID") |>
      recipes::step_zv(recipes::all_predictors()) |>
      recipes::step_mutate_at(
        recipes::all_nominal_predictors(),
        fn = ~ forcats::as_factor(
          stringi::stri_trans_general(., "latin-ascii")
        )
      ) |>
      recipes::step_dummy(
        recipes::all_nominal_predictors(),
        one_hot = TRUE
      )

    veg_suppression_mod <-
      parsnip::rand_forest(
        trees = 128,
        mtry = tune::tune(),
        min_n = tune::tune()
      ) |>
      parsnip::set_engine("aorsf") |>
      parsnip::set_mode("regression")

    veg_suppression_wflow <-
      workflows::workflow() |>
      workflows::add_model(veg_suppression_mod) |>
      workflows::add_recipe(veg_suppression_rec)

    sample_split_list <-
      purrr::map(
        .x = dplyr::distinct(sampled_data, sample) |> dplyr::pull(),
        .f = \(s) {

          sample_subset <- sampled_data |>
            dplyr::filter(.data$sample == s)

          sample_split <-
            rsample::make_splits(
              x = list(
                analysis = which(sample_subset$split_class == "train"),
                assessment = which(sample_subset$split_class == "test")
              ),
              data = model_data
            )

          return(sample_split)

        }
      )

    sample_split <-
      rsample::manual_rset(
        splits = sample_split_list,
        ids = paste("sample", seq_along(sample_split_list))
      )

    ctrl_grid <- stacks::control_stack_grid()

    veg_suppression_tune <-
      tune::tune_grid(
        object = veg_suppression_wflow,
        resamples = sample_split,
        grid = 30,
        control = ctrl_grid,
        metrics = yardstick::metric_set(yardstick::rmse, yardstick::mae)
      )

    veg_suppression_fit <- stacks::stacks() |>
      stacks::add_candidates(veg_suppression_tune) |>
      stacks::blend_predictions(
        metric = yardstick::metric_set(yardstick::rmse),
        control = tune::control_grid(allow_par = TRUE),
        mixture = 1
      ) |>
      stacks::fit_members()

    veg_suppression_pred <-
      predict(
        object = veg_suppression_fit,
        new_data = model_data,
        members = TRUE
      ) |>
      dplyr::rename(ridge_pred = ".pred") |>
      dplyr::rowwise() |>
      dplyr::mutate(
        mean_pred = mean(dplyr::c_across(dplyr::contains("tune")))
      ) |>
      dplyr::ungroup() |>
      dplyr::bind_cols(
        model_data |>
          dplyr::select(
            "veg_suppression", "cell_id", "year", "name_biome"
          )
      )

    pfun <- function(object, newdata) {
      predict(object, new_data = newdata, type = "numeric", )$.pred[]
    }

    veg_supression_explain <-
      purrr::imap(
        .x = veg_suppression_fit$member_fits,
        .f = \(m, idm) {

          fastshap::explain(
            object = m,
            X = model_data |>
              dplyr::select(!dplyr::any_of(c("veg_suppression"))),
            pred_wrapper = pfun,
            shap_only = TRUE
          ) |>
            tibble::as_tibble() |>
            dplyr::select(!dplyr::all_of(c("cell_id", "year"))) |>
            dplyr::bind_cols(
              model_data |>
                dplyr::select(dplyr::all_of(c("cell_id", "year")))
            ) |>
            tidyr::pivot_longer(
              cols = !dplyr::all_of(c("cell_id", "year")),
              names_to = "var",
              values_to = "shap_value"
            ) |>
            dplyr::mutate(model = idm)

        }
      ) |>
      purrr::list_rbind()

    arrow::write_parquet(
      veg_supression_explain,
      sink = "./data/results/model_shap.parquet"
    )

    arrow::write_parquet(
      veg_suppression_pred,
      sink = "./data/results/model_pred.parquet"
    )

    return(veg_suppression_pred)

  }


#' @export
#' @rdname run_model
run_model <-
  function(filled_data, sampled_data) {

    veg_suppression_rec <-
      recipes::recipe(
        veg_suppression ~ .,
        data = filled_data |> sf::st_drop_geometry()
      ) |>
      recipes::update_role("cell_id", "year", new_role = "ID") |>
      recipes::step_zv(recipes::all_predictors()) |>
      recipes::step_dummy(recipes::all_nominal_predictors()) |>
      recipes::step_normalize(recipes::all_numeric_predictors())

    veg_suppression_mod <-
      parsnip::rand_forest(
        trees = 128,
        mtry = tune::tune(),
        min_n = tune::tune()
      ) |>
      parsnip::set_engine("aorsf") |>
      parsnip::set_mode("regression")

    veg_suppression_wflow <-
      workflows::workflow() |>
      workflows::add_model(veg_suppression_mod) |>
      workflows::add_recipe(veg_suppression_rec)

    model_pred <-
      purrr::map(
        .x = dplyr::distinct(sampled_data, sample) |> dplyr::pull(),
        .f = \(s) {

          future::plan(future::multisession(workers = 4))

          test_data <- sampled_data |>
            dplyr::filter(.data$sample == s, .data$split_class == "test") |>
            dplyr::left_join(
              filled_data |> sf::st_drop_geometry(),
              by = dplyr::join_by("cell_id", "year")
            ) |>
            dplyr::select(!dplyr::any_of("split_class"))

          train_data <- sampled_data |>
            dplyr::filter(.data$sample == s, .data$split_class == "train") |>
            dplyr::left_join(
              filled_data |> sf::st_drop_geometry(),
              by = dplyr::join_by("cell_id", "year")
            ) |>
            dplyr::select(!dplyr::any_of("split_class"))

          validation_split <- sampled_data |>
            dplyr::filter(.data$sample == s, .data$split_class == "train") |>
            dplyr::select(!dplyr::any_of("split_class")) |>
            dplyr::left_join(
              filled_data |> sf::st_drop_geometry(),
              by = dplyr::join_by("cell_id", "year")
            ) |>
            dplyr::slice_sample(prop = 0.05) |>
            rsample::vfold_cv(strata = "name_biome")

          veg_suppression_tune <-
            finetune::tune_race_anova(
              veg_suppression_mod,
              veg_suppression_rec,
              resamples = validation_split,
              grid = 30,
              control = finetune::control_race(
                pkgs = "bonsai"
              )
            )

          best_param <- tune::select_best(veg_suppression_tune, metric = "rmse")

          model_final <-
            tune::finalize_workflow(veg_suppression_wflow, best_param)

          veg_suppression_fit <- model_final |>
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

          pfun <- function(object, newdata) {  # prediction wrapper
            unname(predict(object, new_data = newdata)$.pred)
          }

          veg_supression_explain <-
            fastshap::explain(
              object = workflows::extract_fit_parsnip(veg_suppression_fit),
              X = recipes::prep(veg_suppression_rec) |>
                recipes::juice() |>
                dplyr::select(-"veg_suppression"),
              pred_wrapper = pfun,
              nsim = 5,
              adjust = TRUE,
              shap_only = FALSE
            )

          veg_supression_explain$feature_values |> tibble::as_tibble()

          veg_supression_explain$shapley_values |> tibble::as_tibble()

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