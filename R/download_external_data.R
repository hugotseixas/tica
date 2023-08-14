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

    utils::globalVariables(
      "name_biome", "code_biome",
      "geom", "region_code", "year"
    )

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
              "brasil/collection-71/lclu/coverage/",
              "brasil_coverage_{.x}.tif"
            )
          )

        lulc <-
          terra::crop( # Crop raster extents to area of interest
            lulc,
            terra::vect(sf::st_transform(aoi_data, sf::st_crs(lulc)))
          )

        lulc <- terra::aggregate( # Aggregate cells to coarser resolution
          x = lulc,
          fact = agg_fact,
          fun = "modal",
          na.rm = TRUE
        )

        lulc <-
          terra::project( # Transform raster to desired coordinate system
            lulc,
            terra::crs(crs),
            method = "near",
            threads = TRUE,
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

#' @export
#' @rdname download_external_data
download_federal_roads <-
  function(
    dest_dir = "./data/external/",
    crs = "EPSG:5880",
    ...
  ) {

    cat(
      "Download Federal Roads data",
      "\n"
    )

    fs::dir_create(glue::glue(dest_dir, "fr/"))

    h <- curl::new_handle()

    curl::handle_setopt(
      h,
      ssl_verifypeer = FALSE
    )

    curl::curl_download( # Download data from SNV (1994 - 2010)
      url = glue::glue(
        "http://servicos.dnit.gov.br/dnitcloud/index.php/s/oTpPRmYs5AAdiNr/",
        "download?path=%2FHist%C3%B3rico%20Planilhas%20(1994-2010)",
        "%2FPNV%20Planilhas%20(1994-2010)%20(XLS)&files=%5B",
        "%22PNV2005.xlsx%22%2C%22PNV2006.xlsx%22%2C%22PNV2007.xlsx",
        "%22%2C%22PNV2008.xlsx%22%2C%22PNV2009.xlsx%22%2C%22PNV1994.xlsx",
        "%22%2C%22PNV1996.xlsx%22%2C%22PNV1998.xlsx%22%2C%22PNV1999.xlsx",
        "%22%2C%22PNV2000.xlsx%22%2C%22PNV2001.xlsx%22%2C%22PNV2002.xlsx",
        "%22%2C%22PNV2003.xlsx%22%2C%22PNV2004.xlsx%22%2C%22PNV2010.xlsx",
        "%22%5D&downloadStartSecret=72ezfy19fpf"
      ),
      destfile = glue::glue(dest_dir, "fr/fr_hist.zip"),
      handle = h
    )

    utils::unzip(
      zipfile = glue::glue(dest_dir, "fr/fr_hist.zip"),
      exdir = glue::glue(dest_dir, "fr/")
    )

    curl::curl_download( # Download data from SNV (2011 - 2023)
      url = glue::glue(
        "http://servicos.dnit.gov.br/dnitcloud/index.php/s/oTpPRmYs5AAdiNr/",
        "download?path=%2FSNV%20Planilhas%20(2011-Atual)",
        "%20(XLS)&files=%5B%22SNV_201811A.xls%22%2C%22SNV_201710B.xls",
        "%22%2C%22SNV_201612A.xls%22%2C%22SNV_201503A.xls",
        "%22%2C%22SNV_2011.xlsx%22%2C%22SNV_2012.xlsx",
        "%22%2C%22SNV_2013.xlsx%22%2C%22SNV_2014.xlsx",
        "%22%2C%22SNV_201910A.xls%22%2C%22SNV_202010A.xls",
        "%22%2C%22SNV_202110A.xls%22%2C%22SNV_202210C.xls",
        "%22%2C%22SNV_202308A.xlsx%22%5D&downloadStartSecret=d45aldp5i3v"
      ),
      destfile = glue::glue(dest_dir, "fr/fr.zip"),
      handle = h
    )

    utils::unzip(
      zipfile = glue::glue(dest_dir, "fr/fr.zip"),
      exdir = glue::glue(dest_dir, "fr/")
    )

    curl::curl_download( # Download spatial data from SNV (2023)
      url = glue::glue(
        "http://servicos.dnit.gov.br/dnitcloud/index.php/s/oTpPRmYs5AAdiNr/",
        "download?path=%2FSNV%20Bases%20Geom%C3%A9tricas%20(2013-Atual)",
        "%20(SHP)&files=202308A.zip&downloadStartSecret=qpotl2vvwrm"
      ),
      destfile = glue::glue(dest_dir, "fr/fr_spatial.zip"),
      handle = h
    )

    utils::unzip(
      zipfile = glue::glue(dest_dir, "fr/fr_spatial.zip"),
      exdir = glue::glue(dest_dir, "fr/")
    )

    rf_spatial_data <-
      sf::read_sf( # Read data as sf
        glue::glue(dest_dir, "fr/SNV_202308A.shp")
      ) |>
      sf::st_transform( # Transform lines to the desired coordinate system
        crs = crs
      )

    if (fs::file_exists(glue::glue(dest_dir, "fr/fr.fgb"))) {

      fs::file_delete(glue::glue(dest_dir, "fr/fr.fgb"))

    }

    # Save grid polygons as FlatGeobuf file
    sf::write_sf(
      obj = rf_spatial_data,
      dsn = glue::glue(dest_dir, "fr/fr_raw.fgb"),
      driver = "FlatGeobuf",
      append = FALSE
    )

    fs::file_delete(
      purrr::map(
        .x = c("zip", "cpg", "dbf", "prj", "sbn", "sbx", "shp", "shx"),
        .f = ~ {

          fs::dir_ls(
            path = glue::glue(dest_dir, "fr/"),
            glob = glue::glue("*.{.x}")
          )

        }
      ) |>
        purrr::flatten_chr()
    )

  }