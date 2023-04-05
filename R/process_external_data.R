#' Process external data to base grid
#'
#' @param base_grid_path Path to the base grid file
#' @param external_data_path Set path to external data file
#' @param timespan Set the years for the data in the base grid
#'
#' @return A tibble
#'
#' @examples
#' \dontrun{
#'   uc_grid <- process_conservation_units("./inst/base_grid.fgb", 1985:2021)
#' }
#'
#' @export
process_external_data <- function() {}

#' @export
#' @rdname process_external_data
process_conservation_units <-
  function(
    base_grid_path = "./data/base_grid.fgb",
    external_data_path = "./data/external/uc/uc.fgb",
    timespan = 1985:2021
  ) {

    base_grid <-
      sf::read_sf(base_grid_path) |>
      dplyr::arrange(cell_id) |>
      dplyr::select(cell_id, geometry)

    cell_list <- base_grid |>
      dplyr::pull(cell_id)

    conservation_units <-
      sf::read_sf(external_data_path) |>
      sf::st_make_valid()

    sf::st_agr(conservation_units) = "constant"

    sf::st_agr(base_grid) = "constant"

    uc_grid <-
      purrr::map_df(
        .x = cell_list, # Map function to every grid cell
        .f = ~ {

          cat(
            "Progress: ",
            round(.x/base::length(cell_list) * 100),
            "%",
            "\r"
          )

          # Filter the cell
          cell <- base_grid |>
            dplyr::filter(cell_id == .x)

          cell_units <- conservation_units |>
            sf::st_intersection(cell) |>
            dplyr::relocate(cell_id)

          # Create table with all years
          total_years <-
            tibble::tibble(
              year = timespan
            )

          if (base::nrow(cell_units) == 0) {

            cell_units <-
              tibble::tibble_row(
                !!!base::names(cell_units)
              ) |>
              janitor::clean_names() |>
              dplyr::mutate(
                cell_id = cell$cell_id,
                dplyr::across(
                  category:government_level,
                  ~ NA_character_
                ),
                creation_year = NA_integer_,
                uc_area = 0
              ) |>
              dplyr::select(!geometry) |>
              dplyr::bind_cols(total_years)

          } else {

            cell_units <- cell_units |>
              dplyr::mutate(uc_area = as.numeric(sf::st_area(geometry))) |>
              tibble::as_tibble() |>
              dplyr::select(!geometry) |>
              dplyr::mutate(
                creation_year = base::as.integer(creation_year),
                year = creation_year
              ) |>
              dplyr::full_join(
                total_years,
                by = dplyr::join_by(year)
              ) |>
              dplyr::arrange(year) |>
              dplyr::mutate(
                cell_id = cell$cell_id,
                uc_area = dplyr::if_else(
                  base::is.na(uc_area),
                  0,
                  uc_area
                )
              )

          }

          base::return(cell_units)

        }
      )

    base::return(uc_grid)

  }

#' @export
#' @rdname process_external_data
process_indigenous_lands <-
  function(
    base_grid_path = "./data/base_grid.fgb",
    external_data_path = "./data/external/il/il.fgb",
    timespan = 1985:2021
  ) {

    base_grid <-
      sf::read_sf(base_grid_path) |>
      dplyr::arrange(cell_id) |>
      dplyr::select(cell_id, geometry)

    cell_list <- base_grid |>
      dplyr::pull(cell_id)

    indigenous_lands <-
      sf::read_sf(external_data_path) |>
      sf::st_make_valid()

    sf::st_agr(indigenous_lands) = "constant"

    sf::st_agr(base_grid) = "constant"

    il_grid <-
      purrr::map_df(
        .x = cell_list, # Map function to every grid cell
        .f = ~ {

          cat(
            "Progress: ",
            round(.x/base::length(cell_list) * 100),
            "%",
            "\r"
          )

          # Filter the cell
          cell <- base_grid |>
            dplyr::filter(cell_id == .x)

          cell_lands <- indigenous_lands |>
            sf::st_intersection(cell) |>
            dplyr::relocate(cell_id)

          # Create table with all years
          total_years <-
            tibble::tibble(
              year = timespan
            )

          if (base::nrow(cell_lands) == 0) {

            cell_lands <-
              tibble::tibble_row(
                !!!base::names(cell_lands)
              ) |>
              janitor::clean_names() |>
              dplyr::mutate(
                cell_id = cell$cell_id,
                terrai_cod = NA_integer_,
                dplyr::across(
                  fase_ti:year_type,
                  ~ NA_character_
                ),
                creation_year = NA_integer_,
                uc_area = 0
              ) |>
              dplyr::select(!geometry) |>
              dplyr::bind_cols(total_years)

          } else {

            cell_lands <- cell_lands |>
              dplyr::mutate(il_area = as.numeric(sf::st_area(geometry))) |>
              tibble::as_tibble() |>
              dplyr::select(!geometry) |>
              dplyr::mutate(
                creation_year = base::as.integer(creation_year),
                year = creation_year
              ) |>
              dplyr::full_join(
                total_years,
                by = dplyr::join_by(year)
              ) |>
              dplyr::arrange(year) |>
              dplyr::mutate(
                cell_id = cell$cell_id,
                il_area = dplyr::if_else(
                  base::is.na(il_area),
                  0,
                  il_area
                )
              )

          }

          base::return(cell_lands)

        }
      )

    base::return(il_grid)

  }

#' @param base_grid_path Path to the base grid file
#' @param external_data_path Set path to external data file
#' @param timespan Set the years for the data in the base grid
#' @param natural_class Set classes of natural cover
#' @param pasture_class Set classes for pasture cover
#' @param temporary_crop_class Set classes for temporary crops cover
#' @param perennial_crop_class Set classes for perennial crops cover
#' @param forest_plantation_class Set classes for forest plantation cover
#' @param mosaic_class Set classes for mixed cover
#'
#' @export
#' @rdname process_external_data
process_land_use <-
  function(
    base_grid_path = "../tica-book/data/base_grid.fgb",
    external_data_path = "../tica-book/data/external/lulc/",
    timespan = 1985:2021,
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
          base::rep(1, times = base::length(pasture_class)),
          base::rep(2, times = base::length(temporary_crop_class)),
          base::rep(3, times = base::length(perennial_crop_class)),
          base::rep(4, times = base::length(forest_plantation_class)),
          base::rep(5, times = base::length(mosaic_class))
        )
      )

    class_dict <-
      tidyr::crossing(natural_dict$to, human_dict$to) |>
      janitor::clean_names() |>
      dplyr::rename(
        natural_class = natural_dict_to,
        human_class = human_dict_to
      ) |>
      dplyr::mutate(class_diff = human_class - natural_class )

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
          path,
          base::paste(timespan, collapse = "|")
        )
      )

    lulc <- terra::rast(files_list$path)

    terra::set.names(lulc, timespan)

    deforestation_grid <-
      purrr::map_df(
        .x = cell_list[[1667]], # Map function to every grid cell
        .f = ~ {

          cat(
            "Progress: ",
            round(.x/base::length(cell_list) * 100),
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
              cell_id = cell$cell_id
            )

          base::return(deforestation_table)

        }
      )

    return(deforestation_grid)

  }
