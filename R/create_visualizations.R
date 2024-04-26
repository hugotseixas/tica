#' Title Create data visualizations
#'
#' @return A ggplot
#'
#' @param f Type of visualization
#' @param data A tibble to provide data
#' @param group_variable Set variable to create facet label
#' @param viz_title Visualization title
#' @param x_title Horizontal axis title
#' @param y_title Vertical axis title
#' @param out_format The format of the visualization output
#' @param out_filename Name of the file
#' @param n_bins Number of bins of histogram
#' @param x_lim Limit of x axis
#' @param variable_label Set the labels of the table variables
#' @param cat_variable Set the catgorical variable
#' @param rowname_variable Set the variable used as table row name
#' @param ... Parameters passed to visualization functions
#'
#' @examples
#' \dontrun{
#'   create_visualizations()
#' }
#'
#' @export
#' @rdname create_visualizations
create_visualization <-
  function(
    f,
    group_variable = NULL,
    viz_title = NULL,
    x_title = NULL,
    y_title = NULL,
    out_filename = NULL,
    ...
  ) {

    # Set working directory
    base_dir <- here::here()

    data <-
      sf::read_sf(
        glue::glue("{base_dir}/data/merged/filled_data.fgb")
      ) |>
      tibble::as_tibble() |>
      tidyr::drop_na()

    if (!is.null(group_variable)) {

      group_list <- data |>
        dplyr::distinct(.data[[group_variable]]) |>
        dplyr::arrange(.data[[group_variable]]) |>
        dplyr::pull(dplyr::any_of(.data[[group_variable]]))

    } else {

      group_list <- NA

    }

    viz_list <-
      purrr::map(
        group_list,
        \(group) {

          viz_expr <-
            rlang::eval_tidy(
              rlang::parse_expr(glue::glue("tica::{f}"))
            )

          if (!is.null(group_variable)) {

            viz_data <- data |>
              dplyr::filter(.data[[group_variable]] == group)

            viz <-
              viz_expr(data = viz_data, ...) +
              ggplot2::facet_wrap(
                facets = ggplot2::vars(.data[[group_variable]]),
                nrow = 1,
                strip.position = "right"
              )

          } else {

            viz_data <- data

            viz <- viz_expr(data = viz_data, ...)

          }

          return(viz)

        }
      )

    custom_title <- cowplot::ggdraw() +
      cowplot::draw_label(
        {{ viz_title }},
        size = 15,
        fontface = "bold",
        x = 0,
        hjust = 0
      ) +
      ggplot2::theme(
        plot.margin = ggplot2::margin(0, 0, 0, 0)
      )

    viz_grid <-
      cowplot::plot_grid(
        custom_title,
        plotlist =  viz_list,
        ncol = 1,
        scale = 0.9,
        rel_heights = c(0.1, rep(1, length(group_list)))
      ) +
      cowplot::draw_label(
        {{ x_title }},
        x = 0.5, y = 0,
        vjust = -0.2,
        angle = 0,
        size = 13
      ) +
      cowplot::draw_label(
        {{ y_title }},
        x = 0, y = 0.5,
        vjust = 1.5,
        angle = 90,
        size = 13
      )

    ggplot2::ggsave(
      filename = glue::glue("{out_filename}.png"),
      path = "./figs/eda/",
      plot = viz_grid,
      device = ragg::agg_png,
      width = 15,
      height = 11,
      units = "cm",
      dpi = 300
    )

    obj_name <- out_filename

    assign(obj_name, viz_grid)

    save(
      list = obj_name,
      file = glue::glue("./figs/eda/{out_filename}.rdata")
    )

    return(viz_grid)

  }

#' @export
#' @rdname create_visualizations
eda_histogram <-
  function(
    data,
    variable,
    n_bins,
    scale_transform,
    x_lim,
    ...
  ) {

    if (scale_transform == "log") {

      custom_breaks <- scales::breaks_log()

    } else if (scale_transform == "identity") {

      custom_breaks <- scales::breaks_pretty()

    } else {

      stop("Invalid transform.")

    }

    viz <-
      ggplot2::ggplot(
        data = data,
        mapping = ggplot2::aes(
          x = {{ variable }},
          y = ggplot2::after_stat(dplyr::count / max(dplyr::count))
        )
      ) +
      ggplot2::geom_histogram(
        color = "#000000",
        fill = "#e8e8e8",
        bins = n_bins,
        linewidth = 0.4
      ) +
      ggplot2::scale_x_continuous(
        trans = scale_transform,
        breaks = custom_breaks,
        labels = scales::label_number(accuracy = 1),
        limits = x_lim,
        expand = c(0.001, 0.001)
      ) +
      ggplot2::coord_cartesian(clip = "off") +
      cowplot::theme_minimal_grid() +
      ggplot2::theme(
        text = ggplot2::element_text(size = 12),
        axis.title = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        plot.margin = ggplot2::margin(-0.1, 0, 0.3, 0, "cm"),
        strip.text.y = ggplot2::element_text(size = 13, face = "bold")
      )

    return(viz)

  }

#' @param data A tibble to provide data
#' @param variable The variable to be represented in the visualization
#' @param quantiles_list List of percentiles to be calculated
#' @param scale_transform A transform for the horizontal axis
#'
#' @export
#' @rdname create_visualizations
eda_cumulative_distribution <-
  function(
    data,
    variable,
    quantiles_list,
    scale_transform,
    ...
  ) {

    # Calculate percentiles
    quantile_table <- data |>
      dplyr::rename(viz_variable = {{ variable }}) |>
      dplyr::reframe(
        quant = quantile(.data$viz_variable, quantiles_list),
        probs = quantiles_list,
        .by = "name_biome"
      )

    # Get cumulative sum for each percentile
    cumsum_table <- data |>
      dplyr::rename(viz_variable = {{ variable} }) |>
      dplyr::select("viz_variable", "name_biome") |>
      dplyr::arrange(.data$viz_variable) |>
      dplyr::mutate(
        cumulative = cumsum(.data$viz_variable / sum(.data$viz_variable))
      ) |>
      dplyr::inner_join(
        quantile_table,
        by = dplyr::join_by(
          dplyr::closest("viz_variable" >= "quant"),
          "name_biome"
        )
      ) |>
      dplyr::mutate(dif = .data$viz_variable - .data$quant) |>
      dplyr::slice_min(order_by = .data$dif, by = c("probs", "name_biome")) |>
      dplyr::mutate(
        cumulative = round(.data$cumulative, digits = 2),
        viz_variable = round(.data$viz_variable)
      ) |>
      dplyr::distinct(
        .data$cumulative, .data$viz_variable,
        .data$probs, .data$name_biome
      )

    viz_table <- data |>
      dplyr::rename(viz_variable = {{ variable }}) |>
      dplyr::arrange(.data$viz_variable) |>
      dplyr::mutate(
        cumulative = cumsum(.data$viz_variable / sum(.data$viz_variable))
      ) |>
      dplyr::left_join(
        quantile_table,
        by = dplyr::join_by("viz_variable" <= "quant", "name_biome"),
        multiple = "first"
      ) |>
      dplyr::filter(
        dplyr::if_all(
          "viz_variable",
          ~.x >= quantile(.x, dplyr::first(quantiles_list)) &
            .x <= quantile(.x, dplyr::last(quantiles_list))
        ),
        .by = "name_biome"
      )

    viz <-
      ggplot2::ggplot(
        data = viz_table,
        ggplot2::aes(x = .data$viz_variable, y = .data$cumulative)
      ) +
      ggplot2::facet_wrap(
        facets = ggplot2::vars(.data$name_biome),
        scales = "free_x",
        nrow = 2
      ) +
      ggplot2::geom_ribbon(
        ggplot2::aes(
          ymin = 0,
          ymax = .data$cumulative,
          fill = factor(.data$probs)
        ),
        alpha = 0.7
      ) +
      ggplot2::geom_line(
        linewidth = 0.5
      ) +
      ggplot2::geom_segment(
        data = cumsum_table,
        mapping = ggplot2::aes(
          x = .data$viz_variable,
          y = 0,
          yend = .data$cumulative
        ),
        linetype = 2
      ) +
      ggplot2::geom_label(
        data = cumsum_table,
        ggplot2::aes(label = glue::glue("{probs * 100}th")),
        alpha = 0.9,
        size = 3
      ) +
      ggplot2::scale_x_continuous(
        #breaks = ~ quantile(.x, c(0.005, 0.5, 1)),
        labels = scales::label_number(
          scale_cut = scales::cut_short_scale(),
          accuracy = 1
        ),
        guide = ggplot2::guide_axis(angle = 55),
        expand = c(0, 0),
        transform = scale_transform
      ) +
      ggplot2::scale_y_continuous(
        labels = scales::label_percent(),
        breaks = c(cumsum_table$cumulative, 1),
        guide = ggplot2::guide_axis(check.overlap = TRUE)
      ) +
      scico::scale_fill_scico_d(palette = "bilbao", direction = -1) +
      ggplot2::guides(fill = "none") +
      ggplot2::coord_cartesian(clip = "off") +
      cowplot::theme_minimal_grid() +
      ggplot2::theme(
        text = ggplot2::element_text(size = 12),
        axis.title = ggplot2::element_blank(),
        strip.text.y = ggplot2::element_text(size = 13, face = "bold")
      )

    return(viz)

  }

#' @param data A tibble to provide data
#' @param variable The variable to be represented in the visualization
#' @param base_map An sf object to serve as base map
#'
#' @export
#' @rdname create_visualizations
eda_spatial_distribution <-
  function(
    data,
    variable,
    variable_label,
    ...
  ) {

    viz <- data |>
      dplyr::summarise(
        {{ variable }} := sum(.data[[variable]]),
        .by = "geometry"
      ) |>
      sf::st_as_sf() |>
      ggplot2::ggplot()

    viz <- viz +
      ggplot2::geom_sf(
        mapping = ggplot2::aes(
          geometry = .data$geometry,
          fill = .data[[variable]]
        )
      ) +
      scico::scale_fill_scico(
        palette = "bilbao",
        begin = 0.1,
        end = 0.9,
        direction = -1,
        breaks = \(x) c(min(x), max(x)),
        labels = scales::label_number(scale_cut = scales::cut_short_scale())
      ) +
      ggplot2::labs(fill = variable_label) +
      ggplot2::guides(
        fill = ggplot2::guide_colourbar(
          title.position = "top",
          title.hjust = 0.5
        )
      ) +
      ggplot2::coord_sf() +
      cowplot::theme_map() +
      ggplot2::theme(
        text = ggplot2::element_text(size = 12),
        legend.position = "bottom",
        legend.key.height = ggplot2::unit(2, "mm"),
        legend.key.width = ggplot2::unit(25, "mm"),
        legend.justification = c(0.5, 0.5),
        plot.margin = ggplot2::margin(-0.8, -0.3, 0, -0.3, "cm")
      )

    return(viz)

  }

#' @param data A tibble to provide data
#' @param variable The variable to be represented in the visualization
#' @param ts_type The type of the time series visualization
#'
#' @export
#' @rdname create_visualizations
eda_timeseries <-
  function(
    data,
    variable,
    ...
  ) {

    viz <- data |>
      ggplot2::ggplot(
        mapping = ggplot2::aes(
          x = lubridate::ymd(.data$year, truncated = 2),
          y = .data[[variable]],
          color = ggplot2::after_stat(.data$y)
        ),
      ) +
      ggplot2::stat_summary(
        geom = "line",
        fun = sum,
        linewidth = 1.5
      ) +
      ggplot2::stat_summary(
        geom = "point",
        fun = sum,
        size = 3
      ) +
      ggplot2::scale_x_date(
        date_labels = "%Y",
        breaks = scales::date_breaks("2 years")
      ) +
      ggplot2::scale_y_continuous(
        labels = scales::label_number(scale_cut = scales::cut_short_scale())
      ) +
      scico::scale_color_scico(
        palette = "bilbao",
        labels = scales::label_number(scale_cut = scales::cut_short_scale()),
        begin = 0,
        end = 0.7,
        direction = -1
      ) +
      ggplot2::guides(fill = "none", color = "none") +
      cowplot::theme_minimal_grid() +
      ggplot2::theme(
        text = ggplot2::element_text(size = 12),
        axis.title = ggplot2::element_blank(),
        plot.margin = ggplot2::margin(-0.1, 0, 0.3, 0, "cm"),
        strip.text.y = ggplot2::element_text(size = 13, face = "bold"),
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
      )

    return(viz)

  }

#' @export
#' @rdname create_visualizations
eda_colsum <-
  function(
    data,
    variable,
    cat_variable,
    scale_transform,
    ...
  ) {

    if (scale_transform == "log") {

      custom_breaks <- scales::breaks_log()

    } else if (scale_transform == "identity") {

      custom_breaks <- scales::breaks_pretty()

    } else {

      stop("Invalid transform.")

    }

    viz <- data |>
      ggplot2::ggplot(
        ggplot2::aes(
          x = forcats::fct_reorder(
            {{cat_variable}},
            {{variable}},
            .desc = TRUE
          ),
          y = {{variable}},
          fill = {{variable}},
          group = {{cat_variable}}
        )
      ) +
      ggplot2::geom_col(
        color = "#000000"
      ) +
      ggplot2::geom_label(
        ggplot2::aes(
          label = scales::number(
            {{variable}},
            accuracy = 1,
            scale_cut = scales::cut_short_scale()
          )
        ),
        nudge_y = -2,
        label.r = ggplot2::unit(0, "lines"),
        fill = "#ffffff"
      ) +
      ggplot2::scale_y_continuous(
        trans = scale_transform,
        breaks = custom_breaks,
        labels = scales::label_number(scale_cut = scales::cut_short_scale())
      ) +
      ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(n.dodge = 2)) +
      scico::scale_fill_scico(
        palette = "bilbao",
        labels = scales::label_number(
          data,
          scale_cut = scales::cut_short_scale()
        ),
        begin = 0.2
      ) +
      ggplot2::guides(fill = "none") +
      ggplot2::coord_cartesian(clip = "off") +
      cowplot::theme_minimal_grid() +
      ggplot2::theme(
        text = ggplot2::element_text(size = 12),
        axis.title = ggplot2::element_blank(),
        plot.margin = ggplot2::margin(-0.1, 0, 0.3, 0, "cm"),
        strip.text.y = ggplot2::element_text(size = 13, face = "bold")
      )

    return(viz)

  }

#' @export
#' @rdname create_visualizations
eda_summary_table <-
  function(
    data,
    variable,
    rowname_variable,
    viz_title,
    out_filename = NULL,
    out_path = NULL
  ) {

    viz <- data |>
      dplyr::summarise(
        dplyr::across(
          .cols = {{variable}},
          .fns = list(
            min = min,
            mean = mean,
            median = median,
            max = max,
            std = sd,
            n_miss = ~ sum(is.na(.x))
          ),
          .names = "{.fn}"
        ),
        .by = {{rowname_variable}}
      ) |>
      dplyr::rename("rowname" = {{rowname_variable}}) |>
      gt::gt(
        rowname_col = "rowname"
      ) |>
      gt::tab_header(
        title = viz_title
      ) |>
      gt::fmt_number(
        columns = tidyselect::everything(),
        suffixing = TRUE,
        decimals = 0
      ) |>
      gt::cols_label(
        min = gt::md("**Minimum**"),
        mean = gt::md("**Mean**"),
        median = gt::md("**Median**"),
        max = gt::md("**Maximum**"),
        std = gt::md("**Standard Deviation**"),
        n_miss = gt::md("**Missing Values**")
      ) |>
      gt::tab_options(
        table.font.size = 14
      ) |>
      gt::opt_stylize(style = 1, color = "gray")

    gt::gtsave(
      data = viz,
      filename = glue::glue("{out_path}{out_filename}.html")
    )

    return(viz)

  }

#' @export
#' @rdname create_visualizations
eda_observations <-
  function(
    data
  ) {

    viz <- data |>
      tibble::as_tibble() |>
      dplyr::group_by(.data$year) |>
      dplyr::summarise(
        dplyr::across(
          -c("cell_id", "geometry", "lon", "lat"),
          ~ forcats::as_factor(dplyr::if_else(sum(!is.na(.x)) > 0, 1, 0))
        )
      ) |>
      tidyr::pivot_longer(!.data$year) |>
      dplyr::mutate(year = lubridate::ymd(.data$year, truncated = 2)) |>
      ggplot2::ggplot() +
      ggplot2::geom_tile(
        ggplot2::aes(x = .data$year, y = .data$name, fill = .data$value),
        color = "#000000"
      ) +
      scico::scale_fill_scico_d(
        palette = "bilbao",
        labels = c("Observed", "Not Observed")
      ) +
      ggplot2::scale_x_date(
        date_breaks = "5 years", date_labels = "%Y",
        guide = ggplot2::guide_axis(n.dodge = 2)
      ) +
      ggplot2::labs(
        title = "Observed Data Time Series",
        fill = NULL,
        x = "Year",
        y = "Variables"
      ) +
      cowplot::theme_minimal_grid() +
      ggplot2::theme(
        text = ggplot2::element_text(size = 12),
        plot.margin = ggplot2::margin(0, 0, 0, 0),
        axis.title = ggplot2::element_text(size = 13, face = "bold"),
        legend.position = "bottom",
        legend.justification = c(0.5, 0.5),
        plot.title.position = "plot",
        plot.title = ggplot2::element_text(hjust = 0.02)
      )

    return(viz)

  }

timeseries <-
  function(data, variable) {

    data |>
      ggplot2::ggplot(
        mapping = ggplot2::aes(
          x = lubridate::ymd(.data$year, truncated = 2),
          y = .data[[variable]],
          color = ggplot2::after_stat(.data$y)
        ),
      ) +
      ggplot2::facet_grid(
        rows = ggplot2::vars(.data$name_biome),
        scales = "free"
      ) +
      ggplot2::stat_summary(
        geom = "line",
        fun = sum,
        linewidth = 1.5
      ) +
      ggplot2::stat_summary(
        geom = "point",
        fun = sum,
        size = 3
      ) +
      cowplot::theme_minimal_grid() +
      ggplot2::scale_x_date(
        date_labels = "%Y",
        breaks = scales::date_breaks("2 years")
      ) +
      scico::scale_color_scico(
        palette = "bilbao",
        labels = scales::label_number(scale_cut = scales::cut_short_scale()),
        begin = 0,
        end = 0.7,
        direction = -1
      )

  }