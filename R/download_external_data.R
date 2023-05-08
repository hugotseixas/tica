#' Download external data
#'
#' @param dest_dir Destination directory for external files.
#' @param timespan Set the years to download data.
#' @param aoi_path Set path to data with area of interest limits.
#' @param crs Set the Coordinate Reference System to be used
#' @param agg_fact Set the factor to aggregate raster cells
#'
#' @return A tibble
#'
#' @export
download_external_data <-
  function(
    dest_dir = "./data/external/",
    timespan = 1985:2021,
    crs,
    ...
  ) {

    # Create dir to store data
    fs::dir_create(dest_dir)

  }

#' @export
#' @rdname download_external_data
download_aoi <-
  function(
    dest_dir = "./data/external/",
    crs = "EPSG:5880"
  ) {

    cat(
      "Download AOI data",
      "\n"
    )

    fs::dir_create(glue::glue(dest_dir, "aoi/"))

    aoi_data <-
      sf::read_sf( # Get data from IPEA and read as sf
        glue::glue(
          "https://www.ipea.gov.br/",
          "geobr/data_gpkg/biomes/2019/biomes_2019_simplified.gpkg"
        )
      ) |>
      sf::st_transform( # Transform to desired coordinate system
        crs = crs
      ) |>
      dplyr::rename( # Rename columns
        region_name = name_biome,
        region_code = code_biome,
        geometry = geom
      ) |>
      dplyr::filter( # Filter Amazon and Cerrado biomes
        region_code %in% c(1, 3)
      ) |>
      dplyr::select(
        !year
      ) |>
      sf::st_simplify( # Simplify polygon
        dTolerance = 5000
      ) |>
      smoothr::smooth( # Smooth polygon
        method = "ksmooth",
        smoothness = 3
      )

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
    aoi_path = "./data/external/aoi/aoi.fgb",
    timespan = 1985:2021,
    crs = "EPSG:5880",
    agg_fact = 5
  ) {

    cat(
      "Download LULC data",
      "\n"
    )

    fs::dir_create(glue::glue(dest_dir, "lulc/"))

    aoi_data <-
      sf::read_sf( # Read region of interest
        dsn = aoi_path
      )

    purrr::walk( # Download raster files, year by year
      .x = timespan,
      ~ {

        cat(
          .x,
          "\r"
        )

        terra::terraOptions(progress = 0)

        lulc <-
          terra::rast(
            glue::glue( # Download and read data from MapBiomas
              "https://storage.googleapis.com/mapbiomas-public/",
              "brasil/collection-7/lclu/coverage/",
              "brasil_coverage_{.x}.tif"
            )
          )

        lulc <-
        terra::crop( # Crop raster extents to area of interest
          lulc,
          terra::vect(aoi_data)
        )

        lulc <-
          terra::project( # Transform raster to desired coordinate system
            lulc,
            crs,
            method = "near",
            threads = TRUE
          )

        terra::aggregate( # Aggregate cells to coarser resolution
          x = lulc,
          fact = agg_fact,
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
    dest_dir = "./data/external/",
    crs = "EPSG:5880",
    ...
  ) {

    cat(
      "Download Conservation Units data",
      "\n"
    )

    fs::dir_create(glue::glue(dest_dir, "uc/"))

    uc_data <-
      sf::read_sf( # Download data from IPEA and read as sf
        glue::glue(
          "https://www.ipea.gov.br/",
          "geobr/data_gpkg/conservation_units/201909/",
          "conservation_units_201909_simplified.gpkg"
        )
      ) |>
      sf::st_transform( # Transform polygons to desired coordinate system
        crs = crs
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
    dest_dir = "./data/external/",
    crs = "EPSG:5880",
    ...
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

    curl::curl_download( # Download data from FUNAI
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
      sf::read_sf( # Read data as sf
        glue::glue(dest_dir, "il/tis_poligonais_portariasPolygon.shp")
      ) |>
      sf::st_transform( # Transform polygons to the desired coordinate system
        crs = crs
      )

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
