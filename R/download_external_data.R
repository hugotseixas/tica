#' Download external data
#'
#' @param dest_dir Destination directory for external files
#' @param timespan Set the years to download data
#' @param lulc Download land use data
#' @param aoi Download area of interest
#'
#' @return A tibble
#' @export
#'
#' @examples
#' \dontrun{
#'   download_external_data(lulc = TRUE)
#' }
#'
download_external_data <- function(
    dest_dir = "./data/external/",
    timespan = 1985:2021,
    aoi = TRUE,
    lulc = TRUE
) {

  # Create dir to store data
  fs::dir_create(dest_dir)

  # Download area of interest (AOI) data
  if (aoi) {

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

  # Download land use data
  if (lulc) {

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

}
