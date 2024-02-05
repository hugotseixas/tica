#' @importFrom rlang :=
#' @importFrom rlang .data

preprocess_conservation_units <-
  function() {

    # Set working directory
    base_dir <- here::here()

    conservation_units <-
      sf::read_sf(
        dsn = glue::glue(
          "{base_dir}/data/external_raw/",
          "conservation_units/conservation_units.fgb"
        )
      ) |>
      sf::st_set_geometry("geometry") |>
      dplyr::mutate(
        year = lubridate::year(
          lubridate::dmy(
            stringr::str_sub(.data$cria_ato, start = -10)
          )
        )
      ) |>
      dplyr::select("year", "class" = "grupo", "geometry") |>
      dplyr::arrange(.data$year) |>
      sf::st_difference() |>
      sf::st_cast("MULTIPOLYGON", warn = FALSE)

    return(conservation_units)

  }

preprocess_indigenous_territory <-
  function() {

    # Set working directory
    base_dir <- here::here()

    indigenous_territory <-
      sf::read_sf(
        dsn = glue::glue(
          "{base_dir}/data/external_raw/",
          "indigenous_territory/indigenous_territory.fgb"
        )
      ) |>
      dplyr::select( # Select variables
        "terrai_cod", "fase_ti", "modalidade", "data_em_es", "data_delim",
        "data_decla", "data_homol", "data_regul", "geometry"
      ) |>
      tidyr::pivot_longer( # Set dates to one column
        cols = tidyselect::matches("data"),
        names_to = "year_type",
        values_to = "year"
      ) |>
      dplyr::slice_max( # Get the latest date to represent the IL
        order_by = .data$year,
        by = "terrai_cod",
        with_ties = FALSE
      ) |>
      dplyr::rename(type = "modalidade") |>
      dplyr::mutate(year = lubridate::year(.data$year)) |>
      dplyr::filter(
        .data$fase_ti %in% c(
          "Regularizada", "Homologada", "Declarada", "Delimitada"
        )
      ) |>
      dplyr::select("year") |>
      sf::st_transform("EPSG:8857") |>
      sf::st_make_valid()

    indigenous_territory <- indigenous_territory |>
      dplyr::arrange(.data$year) |>
      sf::st_difference() |>
      sf::st_cast("MULTIPOLYGON", warn = FALSE)

    return(indigenous_territory)

  }

preprocess_quilombola_territory <-
  function() {

    # Set working directory
    base_dir <- here::here()

    quilombola_territory <-
      sf::read_sf(
        dsn = glue::glue(
          "{base_dir}/data/external_raw/",
          "quilombola_territory/quilombola_territory.fgb"
        )
      ) |>
      dplyr::filter(!is.na(.data$dt_titulac) | !is.na(.data$dt_decreto)) |>
      dplyr::mutate(
        year = dplyr::if_else(
          !is.na(.data$dt_titulac),
          .data$dt_titulac,
          .data$dt_decreto
        )
      ) |>
      dplyr::filter(.data$fase != "TITULO ANULADO") |>
      dplyr::select( # Select variables
        "year"
      ) |>
      dplyr::mutate(
        year = lubridate::year(lubridate::dmy(.data$year, quiet = TRUE))
      ) |>
      tidyr::drop_na("year") |>
      sf::st_transform("EPSG:8857") |>
      sf::st_make_valid()

    quilombola_territory <- quilombola_territory |>
      dplyr::arrange(.data$year) |>
      sf::st_difference() |>
      sf::st_cast("MULTIPOLYGON", warn = FALSE)

    return(quilombola_territory)

  }

process_deforestation <-
  function(
    base_grid_path,
    external_data_path,
    dest_dir,
    timespan,
    natural_class = c(3, 4, 11, 12),
    pasture_class = c(15),
    temporary_crop_class = c(39, 20, 40, 62, 41),
    perennial_crop_class = c(46, 47, 48),
    forest_plantation_class = c(9),
    mosaic_class = c(21)
  ) {

    terra::terraOptions(progress = 0)

    natural_dict <-
      tibble::tibble(
        from = natural_class,
        to = base::seq(
          from = 100,
          to = base::length(natural_class) * 100,
          by = 100
        )
      )

    human_dict <-
      tibble::tibble(
        from = c(
          pasture_class, temporary_crop_class, perennial_crop_class,
          forest_plantation_class, mosaic_class
        ),
        to = c(
          rep(1, times = base::length(pasture_class)),
          rep(2, times = base::length(temporary_crop_class)),
          rep(3, times = base::length(perennial_crop_class)),
          rep(4, times = base::length(forest_plantation_class)),
          rep(5, times = base::length(mosaic_class))
        )
      )

    class_dict <-
      tidyr::crossing(natural_dict$to, human_dict$to) |>
      janitor::clean_names() |>
      dplyr::rename(
        natural_class = natural_dict_to,
        human_class = human_dict_to
      ) |>
      dplyr::mutate(class_diff = human_class - natural_class)

    base_grid <-
      sf::read_sf(base_grid_path) |>
      dplyr::arrange(cell_id) |>
      dplyr::select(cell_id, geometry)

    cell_list <- base_grid |>
      dplyr::pull(cell_id)

    files_list <-
      fs::dir_info(
        external_data_path,
        glob = "*.tif"
      ) |>
      dplyr::filter(
        stringr::str_detect(
          .data$path,
          base::paste(timespan, collapse = "|")
        )
      )

    lulc <- terra::rast(files_list$path)

    terra::set.names(lulc, timespan)

    # Create table with all years
    total_years <-
      tibble::tibble(
        year = timespan
      )

    deforestation_grid <-
      purrr::map_df(
        .x = cell_list, # Map function to every grid cell
        .f = ~ {

          cat(
            "Deforestation | Progress: ",
            round(.x / base::length(cell_list) * 100),
            "%",
            "\r"
          )

          # Filter the cell
          cell <- base_grid |>
            dplyr::filter(cell_id == .x)

          lulc_subset <- terra::crop(lulc, cell)

          cell_mask <- terra::rasterize(cell, lulc_subset, fun = min)

          lulc_subset <- terra::mask(lulc_subset, cell_mask)

          lulc_subset <-
            terra::classify(
              lulc_subset,
              base::matrix(
                c(
                  natural_dict$from, human_dict$from, # From
                  natural_dict$to, human_dict$to # To
                ),
                ncol = 2
              ),
              others = 9999
            )

          lulc_diff <- terra::diff(lulc_subset)

          area <-
            terra::cellSize(
              lulc_diff[[1]],
              mask = TRUE
            )

          lulc_table <- c(lulc_diff, area) |>
            terra::as.data.frame(cells = TRUE) |>
            tidyr::pivot_longer(
              cols = !c(cell, area), # All columns except id and area
              names_to = "year",
              names_transform = as.integer,
              values_to = "lulc"
            )

          deforestation_table <- lulc_table |>
            dplyr::filter(lulc %in% class_dict$class_diff) |>
            dplyr::left_join(
              class_dict,
              by = dplyr::join_by(lulc == class_diff)
            ) |>
            dplyr::select(!lulc) |>
            dplyr::summarise(
              deforestation_area = base::sum(area),
              .by = c("year", "natural_class", "human_class")
            ) |>
            dplyr::mutate(
              deforestation_total = base::sum(deforestation_area),
              .by = c("year")
            ) |>
            dplyr::slice_max(
              deforestation_area,
              by = "year",
              with_ties = FALSE
            ) |>
            dplyr::select(
              "deforestation_area", "natural_class",
              "human_class", "year"
            ) |>
            dplyr::full_join(
              total_years,
              by = dplyr::join_by(year)
            ) |>
            dplyr::arrange(.data$year) |>
            dplyr::mutate(
              cell_id = cell$cell_id
            )

          base::return(deforestation_table)

        }
      )

    arrow::write_parquet(
      x = deforestation_grid,
      sink = glue::glue("{dest_dir}/deforestation.parquet"),
      version = "latest"
    )

    cat("\n")

    return(deforestation_grid)

  }

process_deforestation <-
  function(
    base_grid_path,
    external_data_path,
    dest_dir,
    timespan,
    natural_class = c(3, 4, 11, 12),
    pasture_class = c(15),
    temporary_crop_class = c(39, 20, 40, 62, 41),
    perennial_crop_class = c(46, 47, 48),
    forest_plantation_class = c(9),
    mosaic_class = c(21)
  ) {

    terra::terraOptions(progress = 0)

    natural_dict <-
      tibble::tibble(
        from = natural_class,
        to = base::seq(
          from = 100,
          to = base::length(natural_class) * 100,
          by = 100
        )
      )

    human_dict <-
      tibble::tibble(
        from = c(
          pasture_class, temporary_crop_class, perennial_crop_class,
          forest_plantation_class, mosaic_class
        ),
        to = c(
          rep(1, times = base::length(pasture_class)),
          rep(2, times = base::length(temporary_crop_class)),
          rep(3, times = base::length(perennial_crop_class)),
          rep(4, times = base::length(forest_plantation_class)),
          rep(5, times = base::length(mosaic_class))
        )
      )

    class_dict <-
      tidyr::crossing(natural_dict$to, human_dict$to) |>
      janitor::clean_names() |>
      dplyr::rename(
        natural_class = natural_dict_to,
        human_class = human_dict_to
      ) |>
      dplyr::mutate(class_diff = human_class - natural_class)

    base_grid <-
      sf::read_sf(base_grid_path) |>
      dplyr::arrange(cell_id) |>
      dplyr::select(cell_id, geometry)

    cell_list <- base_grid |>
      dplyr::pull(cell_id)

    files_list <-
      fs::dir_info(
        external_data_path,
        glob = "*.tif"
      ) |>
      dplyr::filter(
        stringr::str_detect(
          .data$path,
          base::paste(timespan, collapse = "|")
        )
      )

    lulc <- terra::rast(files_list$path)

    terra::set.names(lulc, timespan)

    deforestation_grid <-
      purrr::map_df(
        .x = cell_list, # Map function to every grid cell
        .f = ~ {

          cat(
            "Deforestation | Progress: ",
            round(.x / base::length(cell_list) * 100),
            "%",
            "\r"
          )

          # Filter the cell
          cell <- base_grid |>
            dplyr::filter(cell_id == .x)

          lulc_subset <- terra::crop(lulc, cell)

          cell_mask <- terra::rasterize(cell, lulc_subset, fun = min)

          lulc_subset <- terra::mask(lulc_subset, cell_mask)

          lulc_subset <-
            terra::classify(
              lulc_subset,
              base::matrix(
                c(
                  natural_dict$from, human_dict$from, # From
                  natural_dict$to, human_dict$to # To
                ),
                ncol = 2
              ),
              others = 9999
            )

          lulc_diff <- terra::diff(lulc_subset)

          area <-
            terra::cellSize(
              lulc_diff[[1]],
              mask = TRUE
            )

          lulc_table <- c(lulc_diff, area) |>
            terra::as.data.frame(cells = TRUE) |>
            tidyr::pivot_longer(
              cols = !c(cell, area), # All columns except id and area
              names_to = "year",
              names_transform = as.integer,
              values_to = "lulc"
            )

          deforestation_table <- lulc_table |>
            dplyr::filter(lulc %in% class_dict$class_diff) |>
            dplyr::left_join(
              class_dict,
              by = dplyr::join_by(lulc == class_diff)
            ) |>
            dplyr::select(!lulc) |>
            dplyr::summarise(
              deforestation_area = base::sum(area),
              .by = c("year", "natural_class", "human_class")
            ) |>
            dplyr::mutate(
              deforestation_total = base::sum(deforestation_area),
              .by = c("year")
            ) |>
            dplyr::mutate(
              cell_id = cell$cell_id
            ) |>
            dplyr::slice_max(
              deforestation_area,
              by = "year",
              with_ties = FALSE
            ) |>
            dplyr::select(
              "cell_id", "deforestation_area",
              "natural_class", "human_class",
              "year"
            ) |>
            dplyr::arrange(.data$year)

          base::return(deforestation_table)

        }
      )

    arrow::write_parquet(
      x = deforestation_grid,
      sink = glue::glue("{dest_dir}/deforestation.parquet"),
      version = "latest"
    )

    cat("\n")

    return(deforestation_grid)

  }

process_land_use <-
  function(
    base_grid_path,
    external_data_path,
    dest_dir,
    timespan,
    natural_class = c(3, 4, 11, 12),
    pasture_class = c(15),
    temporary_crop_class = c(39, 20, 40, 62, 41),
    perennial_crop_class = c(46, 47, 48),
    forest_plantation_class = c(9),
    mosaic_class = c(21)
  ) {

    terra::terraOptions(progress = 0)

    natural_dict <-
      tibble::tibble(
        from = natural_class,
        to = base::seq(
          from = 100,
          to = base::length(natural_class) * 100,
          by = 100
        )
      )

    human_dict <-
      tibble::tibble(
        from = c(
          pasture_class, temporary_crop_class, perennial_crop_class,
          forest_plantation_class, mosaic_class
        ),
        to = c(
          rep(1, times = base::length(pasture_class)),
          rep(2, times = base::length(temporary_crop_class)),
          rep(3, times = base::length(perennial_crop_class)),
          rep(4, times = base::length(forest_plantation_class)),
          rep(5, times = base::length(mosaic_class))
        )
      )

    base_grid <-
      sf::read_sf(base_grid_path) |>
      dplyr::arrange(cell_id) |>
      dplyr::select(cell_id, geometry)

    cell_list <- base_grid |>
      dplyr::pull(cell_id)

    files_list <-
      fs::dir_info(
        external_data_path,
        glob = "*.tif"
      ) |>
      dplyr::filter(
        stringr::str_detect(
          .data$path,
          base::paste(timespan, collapse = "|")
        )
      )

    lulc <- terra::rast(files_list$path)

    terra::set.names(lulc, timespan)

    lulc_grid <-
      purrr::map_df(
        .x = cell_list, # Map function to every grid cell
        .f = ~ {

          cat(
            "LULC | Progress: ",
            round(.x / base::length(cell_list) * 100),
            "%",
            "\r"
          )

          # Filter the cell
          cell <- base_grid |>
            dplyr::filter(cell_id == .x)

          lulc_subset <- terra::crop(lulc, cell)

          cell_mask <- terra::rasterize(cell, lulc_subset, fun = min)

          lulc_subset <- terra::mask(lulc_subset, cell_mask)

          lulc_subset <-
            terra::classify(
              lulc_subset,
              base::matrix(
                c(
                  natural_dict$from, human_dict$from, # From
                  natural_dict$to, human_dict$to # To
                ),
                ncol = 2
              ),
              others = 9999
            )

          area <-
            terra::cellSize(
              lulc_subset[[1]],
              mask = TRUE
            )

          lulc_table <- c(lulc_subset, area) |>
            terra::as.data.frame(cells = TRUE) |>
            tidyr::pivot_longer(
              cols = !c(cell, area), # All columns except id and area
              names_to = "year",
              names_transform = as.integer,
              values_to = "lulc"
            )

          lulc_table <- lulc_table |>
            dplyr::filter(lulc != 9999) |>
            dplyr::summarise(
              lulc_area = base::sum(area),
              .by = c("year", "lulc")
            ) |>
            dplyr::mutate(
              cell_id = cell$cell_id,
              lulc = stringr::str_c("class_", as.character(lulc))
            ) |>
            tidyr::pivot_wider(
              names_from = "lulc",
              values_from = "lulc_area"
            ) |>
            dplyr::arrange(.data$year)

          base::return(lulc_table)

        }
      )

    arrow::write_parquet(
      x = lulc_grid,
      sink = glue::glue("{dest_dir}/lulc.parquet"),
      version = "latest"
    )

    cat("\n")

    return(lulc_grid)

  }

process_federal_roads <-
  function(
    base_grid_path = "./data/base_grid.fgb",
    external_data_path = "./data/external/fr/fr_raw.fgb",
    timespan = 1985:2021
  ) {

    base_grid <-
      sf::read_sf(base_grid_path) |>
      dplyr::arrange(cell_id) |>
      dplyr::select(cell_id, geometry)

    cell_list <- base_grid |>
      dplyr::pull(cell_id)

    fr_data <-
      purrr::map_df(
        .x = fs::dir_ls("./data/external/fr/", regexp = "PNV|SNV"),
        .f = ~ {

          table_version <-
            as.integer(
              stringr::str_sub(stringr::str_extract(.x, "(\\d)+"), 1, 4)
            )

          if (stringr::str_detect(.x, "PNV")) {

            if (table_version < 2007) {

              readxl::read_excel(.x, sheet = 1) |>
                janitor::clean_names() |>
                tidyr::drop_na(br) |>
                dplyr::rename(year = versao) |>
                dplyr::select(br, uf, codigo, superficie, year)

            } else if (table_version > 2006 & table_version < 2010) {

              readxl::read_excel(.x, sheet = 1) |>
                janitor::clean_names() |>
                tidyr::drop_na(br) |>
                dplyr::rename(year = versao) |>
                dplyr::mutate(
                  superficie = dplyr::if_else(
                    is.na(superficie_estadual),
                    superficie,
                    superficie_estadual
                  )
                ) |>
                dplyr::select(br, uf, codigo, superficie, year)

            } else if (table_version == 2010) {

              readxl::read_excel(
                .x,
                skip = 2,
                sheet = 1,
                guess_max = 10000,
                .name_repair = "unique_quiet"
              ) |>
                janitor::clean_names() |>
                tidyr::drop_na(br) |>
                dplyr::rename(superficie = superficie_federal) |>
                dplyr::mutate(year = 2010) |>
                dplyr::mutate(
                  superficie = dplyr::if_else(
                    is.na(superficie_estadual_coincidente),
                    superficie,
                    superficie_estadual_coincidente
                  )
                ) |>
                dplyr::select(br, uf, codigo, superficie, year)

            }

          } else {

            if (table_version < 2016) {

              readxl::read_excel(
                .x,
                skip = 2,
                sheet = 1,
                guess_max = 10000,
                .name_repair = "unique_quiet"
              ) |>
                janitor::clean_names() |>
                tidyr::drop_na(br) |>
                dplyr::mutate(
                  year = table_version,
                  superficie = dplyr::if_else(
                    is.na(superficie_estadual_coincidente),
                    superficie,
                    superficie_estadual_coincidente
                  )
                ) |>
                dplyr::select(br, uf, codigo, superficie, year)

            } else {

              readxl::read_excel(.x, skip = 2, sheet = 1) |>
                janitor::clean_names() |>
                tidyr::drop_na(br) |>
                dplyr::mutate(
                  year = table_version,
                  superficie = dplyr::if_else(
                    is.na(superficie_est_coincidente),
                    superficie,
                    superficie_est_coincidente
                  )
                ) |>
                dplyr::select(br, uf, codigo, superficie, year)

            }

          }

        }
      )

    fr_data <- fr_data |>
      dplyr::filter(superficie != "PLA") |>
      dplyr::slice_min(year, by = c("codigo"), na_rm = TRUE)

    federal_roads <-
      sf::read_sf("./data/external/fr/fr_raw.fgb") |>
      dplyr::filter(!leg_multim %in% c("Planejada", "Travessia")) |>
      dplyr::select(vl_codigo, vl_extensa) |>
      dplyr::rename(codigo = vl_codigo, length = vl_extensa) |>
      dplyr::inner_join(fr_data, by = "codigo")

    sf::st_agr(federal_roads) <- "constant"

    sf::st_agr(base_grid) <- "constant"

    fr_grid <-
      purrr::map_df(
        .x = cell_list, # Map function to every grid cell
        .f = ~ {

          cat(
            "Progress: ",
            round(.x / length(cell_list) * 100),
            "%",
            "\r"
          )

          # Filter the cell
          cell <- base_grid |>
            dplyr::filter(cell_id == .x)

          cell_roads <- federal_roads |>
            sf::st_intersection(cell) |>
            dplyr::rename(creation_year = year) |>
            dplyr::select(cell_id, length, creation_year)

          # Create table with all years
          total_years <-
            tibble::tibble(
              year = timespan
            )

          if (nrow(cell_roads) == 0) {

            cell_roads <-
              tibble::tibble_row(
                !!!names(cell_roads)
              ) |>
              janitor::clean_names() |>
              dplyr::mutate(
                cell_id = cell$cell_id,
                length = 0,
                creation_year = NA_integer_
              ) |>
              dplyr::select(!geometry) |>
              dplyr::bind_cols(total_years)

          } else {

            cell_roads <- cell_roads |>
              dplyr::distinct(geometry, .keep_all = TRUE) |>
              dplyr::mutate(length = as.numeric(sf::st_length(geometry))) |>
              tibble::as_tibble() |>
              dplyr::select(!geometry) |>
              dplyr::mutate(
                creation_year = as.integer(creation_year),
                year = creation_year
              ) |>
              dplyr::full_join(
                total_years,
                by = dplyr::join_by(year)
              ) |>
              dplyr::arrange(year) |>
              dplyr::mutate(
                cell_id = cell$cell_id,
                length = dplyr::if_else(
                  base::is.na(length),
                  0,
                  length
                )
              )

          }

          return(cell_roads)

        }
      )

    return(fr_grid)

  }