#' Download external data
#'
#' @param data_name Identification of the variable.
#' @param data_url The url link of the download.
#' @param data_url_function Function that returns a named list of more than one download url.
#' @param data_source Who are the authors of the data, or who they belong to (used for metadata).
#' @param data_description Brief description of the data (used for metadata).
#' @param source_url The url link to the website where data can be found.
#' @param file_pattern A keyword to identify the data file in case there are many inside the download (common with compressed files).
#' @param ... Not used
#'
#' @return NULL
#'
#' @importFrom rlang :=
#' @importFrom rlang .data
#' 
#' @export
download_external_data <-
  function(
    data_name,
    data_url,
    data_source,
    data_description,
    source_url,
    ...,
    data_url_function = NA_character_,
    file_pattern = NA_character_
  ) {

    # Set working directory
    base_dir <- here::here()

    # Create dir for downloaded file
    fs::dir_create(
      path = glue::glue("{base_dir}/data/external_raw/{data_name}")
    )

    if (!is.na(data_url_function)) {
      
      data_url <-
        base::eval(
          rlang::parse_expr(
            data_url_function
          )
        )

    } else {

      names(data_url) <- data_name

    }

    purrr::iwalk(
      .x = data_url,
      .f = \(url, url_name) {

        cli::cli_par()
        cli::cli_alert_info(
          "Downloading {.strong {url_name}} from: {.url {url}}"
        )

        cat("\n")

        # Create temporary directory
        file_dir <- tempdir()
        fs::dir_create(glue::glue("{tempdir()}/{url_name}"))
        file_dir <- glue::glue("{tempdir()}/{url_name}")

        url_ext <- tools::file_ext(sub('^(.*)\\?.*', '\\1', url))

        if (url_ext == "") {
          
          url_ext <- 
            stringr::str_extract_all(
              stringr::str_to_lower(url), 
              "zip|gpkg|csv|xlsx|xls"
            )[[1]] |>
            tail(n = 1)

        }

        # Crate path for downloaded data
        file_path <- 
          glue::glue(
            "{file_dir}/{url_name}.{url_ext}"
          )
        
        # Create path to the downloaded data to be saved
        dest_path <- glue::glue("{base_dir}/data/external_raw/{data_name}/{url_name}")

        # Download data ----
        # Create curl handle
        h <- curl::new_handle()

        curl::handle_setopt(
          h,
          ssl_verifypeer = FALSE
        )

        # Download data to temporary dir
        curl::curl_download(
          url = glue::glue(url),
          destfile = file_path,
          handle = h
        )

        # Extract compressed files
        if (url_ext == "zip") {

          utils::unzip(
            zipfile = file_path,
            exdir = glue::glue("{file_dir}/")
          )

          # Get path of the downloaded data
          file_path <-
            fs::dir_info(
              path = glue::glue("{file_dir}/"),
              recurse = TRUE
            )

          if (!is.na(file_pattern)) {
            
            file_path <- file_path |>
              dplyr::filter(stringr::str_detect(path, file_pattern))

          }

          file_path <- file_path$path

        }
          
        # Save data ----
        if (tools::file_ext(file_path[1]) %in% c("gpkg", "kml", "kmz", "shp", "fgb")) {

          # Read downloaded data, clean names
          spatial_data <-
            sf::read_sf(file_path[1]) |>
            sf::st_set_geometry("geom") |> # Standardize geometry name
            janitor::clean_names() # Standardize columns names

          # Save data ----
          # Write downloaded data to project directory
          sf::write_sf(
            obj = spatial_data,
            dsn = glue::glue("{dest_path}.fgb"),
            driver = "FlatGeobuf",
            delete_dsn = TRUE,
            append = FALSE
          )

        } else {

          fs::file_move(
            file_path[1],
            glue::glue("{dest_path}.{tools::file_ext(file_path[1])}")
          )

        }

        cli::cli_alert_success(
          "Data saved to: {.path {dest_path}}"
        )


        unlink(file_dir)

      }
    )

    # Create metadata
    readr::write_lines(
      x = list(
        glue::glue("DESCRIPTION:   {data_description}"),
        " ",
        glue::glue(
          "SOURCE:        {
            stringr::str_replace_all(
              data_source, 
              pattern = '\n', 
              replacement = '\n               ')
            }"
        ),
        " ",
        glue::glue(
          "URL:           {
            stringr::str_replace_all(
              source_url, 
              pattern = '\n', 
              replacement = '\n               ')
            }"
        ),
        " ",
        glue::glue("ACCESS DATE:   {lubridate::today()}")
      ),
      file = glue::glue("{base_dir}/data/external_raw/{data_name}/{data_name}_metadata.txt")
    )

    cli::cli_alert_success("Download successful.")
    cli::cli_end()

    cli::cli_rule()

    return(invisible(data_url))

}