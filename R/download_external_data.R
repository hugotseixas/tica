#' Download external data
#'
#' @param dest_dir Destination directory for external files
#' @param timespan Set the years to download data
#'
#' @return A tibble
#'
#' @export
download_external_data <-
  function(
    dest_dir = "./data/external/",
    timespan = 1985:2021
  ) {

    # Create dir to store data
    fs::dir_create(dest_dir)

  }

#' @export
#' @rdname download_external_data
download_aoi <-
  function(
    dest_dir = "./data/external/"
  ) {

    cat(
      "Download AOI data",
      "\n"
    )

    fs::dir_create(glue::glue(dest_dir, "aoi/"))

    aoi_data <-
      sf::read_sf(
        glue::glue(
          "https://www.ipea.gov.br/",
          "geobr/data_gpkg/biomes/2019/biomes_2019_simplified.gpkg"
        )
      ) |>
      dplyr::rename(
        region_name = name_biome,
        region_code = code_biome,
        geometry = geom
      ) |>
      dplyr::filter(region_code %in% c(1, 3)) |>
      dplyr::select(!year)

    if (fs::file_exists(glue::glue(dest_dir, "aoi/aoi.fgb"))) {

      fs::file_delete(glue::glue(dest_dir, "aoi/aoi.fgb"))

    }

    # Save grid polygons as FlatGeobuf file
    sf::write_sf(
      obj = aoi_data,
      dsn = glue::glue(dest_dir, "aoi/aoi.fgb"),
      driver = "FlatGeobuf",
      append = FALSE
    )

  }

#' @export
#' @rdname download_external_data
download_land_use <-
  function(
    dest_dir = "./data/external/",
    timespan = 1985:2021
  ) {

    cat(
      "Download LULC data",
      "\n"
    )

    fs::dir_create(glue::glue(dest_dir, "lulc/"))

    purrr::walk(
      .x = timespan,
      ~ {

        terra::terraOptions(progress = 0)

        lulc <-
          terra::rast(
            glue::glue(
              "https://storage.googleapis.com/mapbiomas-public/",
              "brasil/collection-7/lclu/coverage/",
              "brasil_coverage_{.x}.tif"
            )
          )

        lulc <- terra::crop(lulc, terra::vect(aoi_data))

        terra::aggregate(
          x = lulc,
          factor = 10,
          fun = "modal",
          na.rm = TRUE,
          filename = glue::glue(dest_dir, "lulc/lulc_{.x}.tif"),
          overwrite = TRUE
        )

      }
    )

  }

#' @export
#' @rdname download_external_data
download_conservation_units <-
  function(
    dest_dir = "./data/external/"
  ) {

    cat(
      "Download Conservation Units data",
      "\n"
    )

    fs::dir_create(glue::glue(dest_dir, "uc/"))

    uc_data <-
      sf::read_sf(
        glue::glue(
          "https://www.ipea.gov.br/",
          "geobr/data_gpkg/conservation_units/201909/",
          "conservation_units_201909_simplified.gpkg"
        )
      ) |>
      dplyr::select( # Select variables
        category, group, government_level,
        creation_year, geom
      ) |>
      dplyr::rename(
        geometry = geom
      ) |>
      dplyr::mutate( # Fix dates
        creation_year = stringr::str_sub(creation_year, start = -4),
        creation_year = base::as.numeric(creation_year)
      )

    if (fs::file_exists(glue::glue(dest_dir, "uc/uc.fgb"))) {

      fs::file_delete(glue::glue(dest_dir, "uc/uc.fgb"))

    }

    # Save grid polygons as FlatGeobuf file
    sf::write_sf(
      obj = uc_data,
      dsn = glue::glue(dest_dir, "uc/uc.fgb"),
      driver = "FlatGeobuf",
      append = FALSE
    )

  }

#' @export
#' @rdname download_external_data
download_indigenous_lands <-
  function(
    dest_dir = "./data/external/"
  ) {

    cat(
      "Download Indigenous Lands data",
      "\n"
    )

    fs::dir_create(glue::glue(dest_dir, "il/"))

    h <- curl::new_handle()

    curl::handle_setopt(
      h,
      ssl_verifypeer = FALSE
    )

    curl::curl_download(
      url = glue::glue(
        "https://geoserver.funai.gov.br/geoserver/Funai/",
        "ows?service=WFS&version=1.0.0",
        "&request=GetFeature&typeName=Funai%3Atis_poligonais_portarias&",
        "maxFeatures=10000&outputFormat=SHAPE-ZIP"
      ),
      destfile = glue::glue(dest_dir, "il/il.zip"),
      handle = h
    )

    utils::unzip(
      zipfile = glue::glue(dest_dir, "il/il.zip"),
      exdir = glue::glue(dest_dir, "il/")
    )

    il_data <-
      sf::read_sf(
        glue::glue(dest_dir, "il/tis_poligonais_portariasPolygon.shp")
      ) |>
      dplyr::select( # Select variables
        terrai_cod, fase_ti, modalidade, data_em_es, data_delim,
        data_decla, data_homol, data_regul, geometry
      ) |>
      tidyr::pivot_longer(
        cols = tidyr::matches("data"),
        names_to = "year_type",
        values_to = "creation_year"
      ) |>
      dplyr::slice_max(
        order_by = creation_year,
        by = "terrai_cod",
        with_ties = FALSE
      ) |>
      dplyr::rename(type = modalidade) |>
      dplyr::mutate(creation_year = lubridate::year(creation_year))

    if (fs::file_exists(glue::glue(dest_dir, "il/il.fgb"))) {

      fs::file_delete(glue::glue(dest_dir, "il/il.fgb"))

    }

    # Save grid polygons as FlatGeobuf file
    sf::write_sf(
      obj = il_data,
      dsn = glue::glue(dest_dir, "il/il.fgb"),
      driver = "FlatGeobuf",
      append = FALSE
    )

    fs::file_delete(
      fs::dir_ls(
        path = glue::glue(dest_dir, "il/"),
        glob = "*.fgb",
        invert = TRUE
      )
    )

  }
