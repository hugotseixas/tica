#' Process conservation units data to base grid
#'
#' @param base_grid_path Path to the base grid file
#' @param timespan Set the years for the data in the base grid
#'
#' @return A tibble
#' @export
#'
#' @examples
#' \dontrun{
#'   uc_grid <- process_conservation_units("./inst/base_grid.fgb", 1985:2021)
#' }
#'
process_conservation_units <- function(base_grid_path, timespan) {

  base_grid <-
    sf::read_sf(base_grid_path) |>
    dplyr::arrange(cell_id) |>
    dplyr::select(cell_id, geometry)

  cell_list <- base_grid |>
    dplyr::pull(cell_id)

  conservation_units <-
    geobr::read_conservation_units(showProgress = FALSE) |>
    sf::st_make_valid() |>
    dplyr::select( # Select variables
      category, group, government_level,
      creation_year, geom
    ) |>
    dplyr::mutate( # Fix dates
      creation_year = stringr::str_sub(creation_year, start = -4),
      creation_year = base::as.numeric(creation_year)
    )

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
              uc_area = units::set_units(
                0,
                "m^2",
                mode = "standard"
              )
            ) |>
            dplyr::select(!geom) |>
            dplyr::bind_cols(total_years)

        } else {

          cell_units <- cell_units |>
            dplyr::mutate(uc_area = sf::st_area(geom)) |>
            tibble::as_tibble() |>
            dplyr::select(!geom) |>
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
                units::set_units(0, "m^2", mode = "standard"),
                uc_area
              )
            )

        }

        base::return(cell_units)

      }
    )

  base::return(uc_grid)

}
