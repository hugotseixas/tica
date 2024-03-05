#' @importFrom rlang :=
#' @importFrom rlang .data

create_wkt_filter <-
  function(
    data_path
  ) {

    # Set working directory
    base_dir <- here::here()

    layer_name <- sf::st_layers(data_path[1])[[1]]

    data_crs <-
      sf::read_sf(
        data_path,
        query = glue::glue(
          "select * from \"{layer_name}\" limit 0"
        )
      ) |>
      sf::st_crs()

    cell_wkt <-
      sf::read_sf(
        dsn = glue::glue("{base_dir}/data/grid.fgb")
      ) |>
      sf::st_transform(data_crs) |>
      sf::st_bbox() |>
      sf::st_as_sfc() |>
      sf::st_as_text()

    return(cell_wkt)

  }

read_municipality_priority <-
  function() {

    # Set working directory
    base_dir <- here::here()

    priority <-
      readr::read_csv(
        file = glue::glue(
          "{base_dir}/data/raw/",
          "municipality_priority/municipality_priority.csv"
        ),
        show_col_types = FALSE
      ) |>
      dplyr::rename("since" = "year") |>
      dplyr::arrange(.data$since)

    file_path <-
      fs::dir_ls(
        glue::glue("{base_dir}/data/raw/municipality"),
        glob = "*.fgb"
      )

    muni <-
      purrr::map_df(
        .x = file_path,
        .f = \(path) {

          sf::read_sf(dsn = path) |>
            dplyr::select("code_muni") |>
            dplyr::mutate(
              year = as.double(stringr::str_extract(path, "\\d+"))
            ) |>
            sf::st_transform(
              sf::st_crs(
                readr::read_file(glue::glue("{base_dir}/data/project_crs.txt"))
              )
            )

        }
      ) |>
      dplyr::arrange(.data$year)

    municipality_priority <-
      dplyr::left_join(
        priority, muni,
        multiple = "last",
        by = dplyr::join_by("code_muni", "since" >= "year")
      ) |>
      sf::st_as_sf()

    return(municipality_priority)

  }

process_municipality_priority <-
  function(spatial_data, spatial_subset) {

    municipality_priority <- spatial_data |>
      sf::st_filter(spatial_subset)

    if (nrow(municipality_priority) == 0) return(NULL)

    municipality_priority <- municipality_priority |>
      dplyr::select( # Select variables
        dplyr::all_of(c("year" = "since", "priority", "monitored"))
      ) |>
      sf::st_make_valid() |>
      dplyr::arrange(.data$year) |>
      sf::st_set_agr("constant") |>
      sf::st_intersection(
        spatial_subset |> sf::st_set_agr("constant")
      ) |>
      sf::st_cast("MULTIPOLYGON", warn = FALSE)

    if (nrow(municipality_priority) == 0) return(NULL)

    municipality_priority <- municipality_priority |>
      dplyr::mutate(
        dplyr::across(
          c("priority", "monitored"),
          \(x) {

            dplyr::if_else(x == 1, as.numeric(sf::st_area(.data$geometry)), 0)

          }
        )
      ) |>
      sf::st_drop_geometry() |>
      dplyr::summarise(
        dplyr::across(
          c("priority", "monitored"),
          \(x) sum(x, na.rm = TRUE)
        ),
        .by = c("year", "cell_id")
      )

    return(municipality_priority)

  }

read_conservation_units <-
  function() {

    # Set working directory
    base_dir <- here::here()

    file_path <-
      glue::glue(
        "{base_dir}/data/raw/",
        "conservation_units/conservation_units.fgb"
      )

    cell_wkt <-
      create_wkt_filter(
        file_path
      )

    conservation_units <-
      sf::read_sf(
        dsn = file_path,
        wkt_filter = cell_wkt
      ) |>
      sf::st_transform(
        sf::st_crs(
          readr::read_file(glue::glue("{base_dir}/data/project_crs.txt"))
        )
      )

    return(conservation_units)

  }

process_conservation_units <-
  function(spatial_data, spatial_subset) {

    conservation_units <- spatial_subset |>
      sf::st_set_agr("constant") |>
      sf::st_intersection(
        spatial_data |> sf::st_set_agr("constant")
      ) |>
      dplyr::mutate(
        year = lubridate::year(
          lubridate::dmy(
            stringr::str_sub(.data$cria_ato, start = -10)
          )
        )
      ) |>
      dplyr::select("cell_id", "year", "class" = "grupo", "geometry") |>
      sf::st_make_valid() |>
      dplyr::arrange(.data$year) |>
      sf::st_set_agr("constant") |>
      sf::st_difference()

    if (nrow(conservation_units) > 0) {

      conservation_units <- conservation_units |>
        dplyr::mutate(
          conservation_units = as.numeric(sf::st_area(.data$geometry))
        ) |>
        sf::st_drop_geometry() |>
        dplyr::summarise(
          conservation_units = sum(.data$conservation_units, na.rm = TRUE),
          .by = c("year", "cell_id")
        ) |>
        dplyr::mutate(conservation_units = cumsum(.data$conservation_units))

      return(conservation_units)

    } else {

      return(NULL)

    }

  }

read_indigenous_territory <-
  function() {

    # Set working directory
    base_dir <- here::here()

    file_path <-
      glue::glue(
        "{base_dir}/data/raw/",
        "indigenous_territory/indigenous_territory.fgb"
      )

    cell_wkt <-
      create_wkt_filter(
        file_path
      )

    indigenous_territory <-
      sf::read_sf(
        dsn = file_path,
        wkt_filter = cell_wkt
      ) |>
      sf::st_transform(
        sf::st_crs(
          readr::read_file(glue::glue("{base_dir}/data/project_crs.txt"))
        )
      )

    return(indigenous_territory)

  }

process_indigenous_territory <-
  function(spatial_data, spatial_subset) {

    indigenous_territory <- spatial_data |>
      sf::st_filter(spatial_subset)

    if (nrow(indigenous_territory) == 0) return(NULL)

    indigenous_territory <- indigenous_territory |>
      dplyr::select( # Select variables
        dplyr::all_of(
          c(
            "terrai_cod", "fase_ti", "modalidade", "data_em_es", "data_delim",
            "data_decla", "data_homol", "data_regul", "geometry"
          )
        )
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
      dplyr::arrange("terrai_cod") |>
      tidyr::fill("year", .direction = "down") |>
      dplyr::rename(type = "modalidade") |>
      dplyr::mutate(year = lubridate::year(.data$year)) |>
      dplyr::filter(
        !is.na(.data$year),
        .data$fase_ti %in% c(
          "Regularizada", "Homologada", "Declarada", "Delimitada"
        )
      ) |>
      dplyr::select("year") |>
      sf::st_make_valid() |>
      dplyr::arrange(.data$year) |>
      sf::st_set_agr("constant") |>
      sf::st_intersection(
        spatial_subset |> sf::st_set_agr("constant")
      ) |>
      sf::st_difference() |>
      sf::st_cast("MULTIPOLYGON", warn = FALSE)

    if (nrow(indigenous_territory) == 0) return(NULL)

    indigenous_territory <- indigenous_territory |>
      dplyr::mutate(
        indigenous_territory = as.numeric(sf::st_area(.data$geometry))
      ) |>
      sf::st_drop_geometry() |>
      dplyr::summarise(
        indigenous_territory = sum(.data$indigenous_territory, na.rm = TRUE),
        .by = c("year", "cell_id")
      ) |>
      dplyr::mutate(indigenous_territory = cumsum(.data$indigenous_territory))

    return(indigenous_territory)

  }

read_quilombola_territory <-
  function() {

    # Set working directory
    base_dir <- here::here()

    file_path <-
      glue::glue(
        "{base_dir}/data/raw/",
        "quilombola_territory/quilombola_territory.fgb"
      )

    cell_wkt <-
      create_wkt_filter(
        file_path
      )

    quilombola_territory <-
      sf::read_sf(
        dsn = file_path,
        wkt_filter = cell_wkt
      ) |>
      sf::st_transform(
        sf::st_crs(
          readr::read_file(glue::glue("{base_dir}/data/project_crs.txt"))
        )
      )

    return(quilombola_territory)

  }

process_quilombola_territory <-
  function(spatial_data, spatial_subset) {

    quilombola_territory <- spatial_data |>
      sf::st_filter(spatial_subset)

    if (nrow(quilombola_territory) == 0) return(NULL)

    quilombola_territory <- quilombola_territory |>
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
      sf::st_make_valid() |>
      dplyr::arrange(.data$year) |>
      sf::st_set_agr("constant") |>
      sf::st_intersection(
        spatial_subset |> sf::st_set_agr("constant")
      ) |>
      sf::st_difference() |>
      sf::st_cast("MULTIPOLYGON", warn = FALSE)

    if (nrow(quilombola_territory) == 0) return(NULL)

    quilombola_territory <- quilombola_territory |>
      dplyr::mutate(
        area = as.numeric(sf::st_area(.data$geometry))
      ) |>
      sf::st_drop_geometry() |>
      dplyr::summarise(
        area = sum(.data$area, na.rm = TRUE), .by = c("cell_id", "year")
      ) |>
      dplyr::mutate(area = cumsum(.data$area)) |>
      dplyr::rename("quilombola_territory" = "area")

    return(quilombola_territory)

  }

read_biomes <-
  function() {

    # Set working directory
    base_dir <- here::here()

    file_path <-
      fs::dir_ls(
        glue::glue("{base_dir}/data/raw/biomes"),
        glob = "*.fgb"
      )

    biomes <-
      purrr::map_df(
        .x = file_path,
        .f = \(path) {

          sf::read_sf(dsn = path) |>
            dplyr::select("name_biome", "year") |>
            dplyr::filter(.data$name_biome %in% c("Amazônia", "Cerrado")) |>
            sf::st_transform(
              sf::st_crs(
                readr::read_file(glue::glue("{base_dir}/data/project_crs.txt"))
              )
            )

        }
      )

    return(biomes)

  }

process_biomes <-
  function(spatial_data, spatial_subset) {

    biomes <- spatial_subset |>
      sf::st_set_agr("constant") |>
      sf::st_intersection(
        spatial_data |> sf::st_set_agr("constant")
      )

    if (nrow(biomes) == 0) return(NULL)

    biomes <- biomes |>
      dplyr::mutate(area = sf::st_area(.data$geometry)) |>
      sf::st_drop_geometry() |>
      dplyr::slice_max(order_by = .data$area, by = c("year")) |>
      dplyr::select("cell_id", "year", "name_biome")

    return(biomes)

  }

read_veg_suppression <-
  function() {

    # Set working directory
    base_dir <- here::here()

    file_path <-
      fs::dir_ls(
        glue::glue("{base_dir}/data/raw/veg_suppression"),
        glob = "*.tif"
      )

    raster_meta <-
      stars::read_stars(file_path, proxy = TRUE)

    return(raster_meta)

  }

process_veg_suppression <-
  function(spatial_data, spatial_subset) {

    base_cell_bbox <- spatial_subset |>
      sf::st_transform(sf::st_crs(spatial_data)) |>
      sf::st_bbox()

    raster_meta <- spatial_data |>
      sf::st_crop(base_cell_bbox)

    raster_dim <- stars::st_dimensions(raster_meta)

    rasterio <-
      list(
        nXOff = raster_dim$x$from,
        nYOff = raster_dim$y$from,
        nXSize = raster_dim$x$to - raster_dim$x$from,
        nYSize = raster_dim$y$to - raster_dim$y$from,
        nBufXSize = (raster_dim$x$to - raster_dim$x$from) / 5,
        nBufYSize = (raster_dim$y$to - raster_dim$y$from) / 5
      )

    veg_suppression <-
      stars::read_stars(
        names(raster_meta),
        RasterIO = rasterio,
        NA_value = 0
      )

    poly_veg_suppression <-
      purrr::map_df(
        .x = names(veg_suppression),
        .f = \(x) {

          poly_veg_suppression <-
            sf::st_as_sf(
              veg_suppression[x],
              as_points = FALSE,
              merge = TRUE,
              use_integer = TRUE
            ) |>
            sf::st_transform(
              crs = sf::st_crs(spatial_subset)
            ) |>
            dplyr::rename(
              "class" = dplyr::all_of(x)
            ) |>
            dplyr::mutate(year = x)

          return(poly_veg_suppression)

        }
      )

    if (nrow(poly_veg_suppression) == 0) return(NULL)

    poly_veg_suppression <- poly_veg_suppression |>
      sf::st_set_agr("constant") |>
      sf::st_intersection(
        spatial_subset |> sf::st_set_agr("constant")
      ) |>
      dplyr::filter(
        dplyr::between(class, 400, 499) | dplyr::between(class, 600, 699)
      ) |>
      dplyr::mutate(
        area = as.numeric(sf::st_area(.data$geometry))
      ) |>
      sf::st_drop_geometry() |>
      dplyr::summarise(
        area = sum(.data$area, na.rm = TRUE),
        .by = c("cell_id", "year")
      ) |>
      dplyr::rename("veg_suppression" = "area")


    return(poly_veg_suppression)

  }

read_lulc <-
  function() {

    # Set working directory
    base_dir <- here::here()

    file_path <-
      fs::dir_ls(
        glue::glue("{base_dir}/data/raw/lulc"),
        glob = "*.tif"
      )

    raster_meta <-
      stars::read_stars(file_path, proxy = TRUE)

    return(raster_meta)

  }

process_lulc <-
  function(spatial_data, spatial_subset) {

    base_cell_bbox <- spatial_subset |>
      sf::st_transform(sf::st_crs(spatial_data)) |>
      sf::st_bbox()

    raster_meta <- spatial_data |>
      sf::st_crop(base_cell_bbox)

    raster_dim <- stars::st_dimensions(raster_meta)

    rasterio <-
      list(
        nXOff = raster_dim$x$from,
        nYOff = raster_dim$y$from,
        nXSize = raster_dim$x$to - raster_dim$x$from,
        nYSize = raster_dim$y$to - raster_dim$y$from,
        nBufXSize = (raster_dim$x$to - raster_dim$x$from) / 10,
        nBufYSize = (raster_dim$y$to - raster_dim$y$from) / 10
      )

    lulc <-
      stars::read_stars(
        names(raster_meta),
        RasterIO = rasterio,
        NA_value = 0
      )

    poly_lulc <-
      purrr::map_df(
        .x = names(lulc),
        .f = \(x) {

          poly_lulc <-
            sf::st_as_sf(
              lulc[x],
              as_points = FALSE,
              merge = TRUE,
              use_integer = TRUE
            ) |>
            sf::st_transform(
              crs = sf::st_crs(spatial_subset)
            ) |>
            dplyr::rename(
              "class" = dplyr::all_of(x)
            ) |>
            dplyr::mutate(year = x)

          return(poly_lulc)

        }
      )

    if (nrow(poly_lulc) == 0) return(NULL)

    poly_lulc <- poly_lulc |>
      sf::st_set_agr("constant") |>
      sf::st_intersection(
        spatial_subset |> sf::st_set_agr("constant")
      ) |>
      dplyr::mutate(
        class = dplyr::case_when(
          class == 3 ~ "forest_formation",
          class == 4 ~ "savanna_formation",
          class == 12 ~ "grasslands",
          class %in% c(5, 6, 49, 11, 32, 29, 50, 13) ~ "other_vegetation",
          class == 15 ~ "pasture",
          class %in% c(39, 20, 40, 62, 41) ~ "temporary_crop",
          class %in% c(46, 47, 35, 48) ~ "perennial_crop",
          class == 9 ~ "forest_plantation",
          class == 21 ~ "agriculture_mosaic",
          class == 24 ~ "urban_area",
          class == 30 ~ "mining"
        )
      ) |>
      tidyr::drop_na(dplyr::all_of("class")) |>
      dplyr::mutate(area = as.numeric(sf::st_area(.data$geometry))) |>
      sf::st_drop_geometry() |>
      dplyr::summarise(
        area = sum(.data$area, na.rm = TRUE),
        .by = c("cell_id", "year", "class")
      ) |>
      tidyr::pivot_wider(names_from = "class", values_from = "area")


    return(poly_lulc)

  }

read_highways <-
  function() {

    # Set working directory
    base_dir <- here::here()

    file_path <-
      fs::dir_ls(
        glue::glue("{base_dir}/data/raw/highways"),
        regexp = "xls"
      )

    highways <-
      purrr::map_df(
        .x = file_path,
        .f = \(file) {

          file_version <-
            as.integer(
              stringr::str_extract(file, "(\\d)+")
            )

          if (file_version <= 2010) {

            if (file_version < 2007) {

              highways <-
                readxl::read_excel(file, sheet = 1, progress = FALSE) |>
                janitor::clean_names() |>
                tidyr::drop_na(dplyr::all_of("br")) |>
                dplyr::rename(year = "versao") |>
                dplyr::select(
                  dplyr::all_of(c("br", "uf", "codigo", "superficie", "year"))
                )

            } else if (file_version > 2006 & file_version < 2010) {

              highways <-
                readxl::read_excel(file, sheet = 1, progress = FALSE) |>
                janitor::clean_names() |>
                tidyr::drop_na(dplyr::all_of("br")) |>
                dplyr::rename(year = "versao") |>
                dplyr::mutate(
                  superficie = dplyr::if_else(
                    is.na(.data$superficie_estadual),
                    .data$superficie,
                    .data$superficie_estadual
                  )
                ) |>
                dplyr::select(
                  dplyr::all_of(c("br", "uf", "codigo", "superficie", "year"))
                )

            } else if (file_version == 2010) {

              highways <-
                readxl::read_excel(
                  file,
                  skip = 2,
                  sheet = 1,
                  guess_max = 10000,
                  .name_repair = "unique_quiet",
                  progress = FALSE
                ) |>
                janitor::clean_names() |>
                tidyr::drop_na(dplyr::all_of("br")) |>
                dplyr::rename(superficie = "superficie_federal") |>
                dplyr::mutate(year = 2010) |>
                dplyr::mutate(
                  superficie = dplyr::if_else(
                    is.na(.data$superficie_estadual_coincidente),
                    .data$superficie,
                    .data$superficie_estadual_coincidente
                  )
                ) |>
                dplyr::select(
                  dplyr::all_of(c("br", "uf", "codigo", "superficie", "year"))
                )

            }

          } else {

            if (file_version < 2016) {

              highways <-
                readxl::read_excel(
                  file,
                  skip = 2,
                  sheet = 1,
                  guess_max = 10000,
                  .name_repair = "unique_quiet",
                  progress = FALSE
                ) |>
                janitor::clean_names() |>
                tidyr::drop_na(dplyr::all_of("br")) |>
                dplyr::mutate(
                  year = file_version,
                  superficie = dplyr::if_else(
                    is.na(.data$superficie_estadual_coincidente),
                    .data$superficie,
                    .data$superficie_estadual_coincidente
                  )
                ) |>
                dplyr::select(
                  dplyr::all_of(c("br", "uf", "codigo", "superficie", "year"))
                )

            } else {

              highways <-
                readxl::read_excel(
                  file, skip = 2,
                  sheet = 1,
                  progress = FALSE
                ) |>
                janitor::clean_names() |>
                tidyr::drop_na(dplyr::all_of("br")) |>
                dplyr::mutate(
                  year = file_version,
                  superficie = dplyr::if_else(
                    is.na(.data$superficie_est_coincidente),
                    .data$superficie,
                    .data$superficie_est_coincidente
                  )
                ) |>
                dplyr::select(
                  dplyr::all_of(c("br", "uf", "codigo", "superficie", "year"))
                )

            }

          }

          return(highways)

        }
      )

    highways <- highways |>
      dplyr::filter(.data$superficie != "PLA") |>
      dplyr::slice_min(.data$year, by = c("codigo"), na_rm = TRUE)

    spatial_highways <-
      sf::read_sf("./data/raw/highways/highways_2023.fgb") |>
      dplyr::filter(!.data$leg_multim %in% c("Planejada", "Travessia")) |>
      dplyr::select(dplyr::all_of(c("vl_codigo", "vl_extensa"))) |>
      dplyr::rename(codigo = "vl_codigo", length = "vl_extensa") |>
      dplyr::inner_join(highways, by = "codigo") |>
      sf::st_transform(
        sf::st_crs(
          readr::read_file(glue::glue("{base_dir}/data/project_crs.txt"))
        )
      )

    return(spatial_highways)

  }

process_highways <-
  function(spatial_data, spatial_subset) {

    highways <- spatial_data |>
      sf::st_filter(spatial_subset)

    if (nrow(highways) == 0) return(NULL)

    highways <- spatial_subset |>
      sf::st_set_agr("constant") |>
      sf::st_intersection(
        spatial_data |> sf::st_set_agr("constant")
      ) |>
      sf::st_make_valid() |>
      dplyr::arrange(.data$year) |>
      sf::st_set_agr("constant") |>
      sf::st_difference()

    if (nrow(highways) == 0) return(NULL)

    highways <- highways |>
      dplyr::mutate(
        length = as.numeric(sf::st_length(.data$geometry))
      ) |>
      sf::st_drop_geometry() |>
      dplyr::summarise(
        length = sum(.data$length, na.rm = TRUE), .by = c("year", "cell_id")
      ) |>
      dplyr::mutate(length = cumsum(.data$length)) |>
      dplyr::rename("highways" = "length")

    return(highways)

  }