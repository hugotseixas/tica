#' Process external data to base grid
#'
#' @param base_grid_path Path to the base grid file
#' @param timespan Set the years for the data in the base grid
#' @param external_data_path Set path to external data file
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

    base::return(uc_grid)

  }
