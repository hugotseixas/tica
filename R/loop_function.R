#' @importFrom rlang .data

#' @export
loop_function <-
  function(
    function_name,
    arguments_subset = 1:1000
  ) {

    # Set working directory
    base_dir <- here::here()

    cli::cli_div(
      id = "main",
      theme = list(
        rule = list("line-type" = "double", "before" = "\n")
      )
    )

    cli::cli_rule(
      "{
        as.character(substitute(function_name)) |> 
          stringr::str_replace_all(pattern = '_', replacement = ' ') |>
          stringr::str_to_title()
      }",
      id = "main"
    )

    arguments_table <-
      readr::read_csv(
        file = glue::glue(
          "{base_dir}/analysis/arguments_{substitute(function_name)}.csv"
        ),
        show_col_types = FALSE,
        na = c("", "NA")
      ) |>
      dplyr::filter(.data$id %in% arguments_subset) |>
      dplyr::select(-dplyr::all_of(c("id")))

    purrr::pwalk(
      .l = arguments_table,
      .f = eval(rlang::parse_expr(function_name))
    )

    cli::cli_div(theme = list(rule = list("line-type" = "double")))
    cli::cli_rule(id = "main")

    return(invisible(NULL))

  }