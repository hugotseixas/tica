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
    group_variable = NA_character_,
    viz_title = NA_character_,
    x_title = NA_character_,
    y_title = NA_character_,
    out_filename = NA_character_,
    ...
  ) {

    # Set working directory
    base_dir <- here::here()

    if (stringr::str_detect(f, "eda_")) {

      data <-
        sf::read_sf(
          glue::glue("{base_dir}/data/merged/filled_data.fgb")
        ) |>
        tibble::as_tibble()

      save_folder <- "eda"

    } else if (stringr::str_detect(f, "results_")) {

      data <-
        arrow::read_parquet(
          glue::glue("{base_dir}/data/results/model_pred.parquet")
        )

      save_folder <- "assessment"

    } else if (stringr::str_detect(f, "method_")) {

      data <-
        sf::read_sf(
          glue::glue("{base_dir}/data/grid.fgb")
        )

      save_folder <- "methods"

    } else if (stringr::str_detect(f, "explain_")) {

      data <-
        sf::read_sf(
          glue::glue("{base_dir}/data/merged/filled_data.fgb")
        ) |>
        tibble::as_tibble()

      save_folder <- "explain"

    }

    if (!is.na(group_variable)) {

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

          if (!is.na(group_variable)) {

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

    if (stringr::str_detect(f, "table")) {

      gt::gtsave(
        data = viz_list[[1]],
        filename = glue::glue("./figs/{save_folder}/{out_filename}.html")
      )

      obj_name <- out_filename

      assign(obj_name, viz_list[[1]])

      save(
        list = obj_name,
        file = glue::glue("./figs/{save_folder}/{out_filename}.rdata")
      )

      return(viz_list[[1]])

    }

    custom_title <-
      cowplot::ggdraw() +
      cowplot::draw_label(
        stringr::str_wrap({{ viz_title }}, width = 50),
        size = 12,
        fontface = "bold",
        x = 0,
        hjust = 0
      ) +
      ggplot2::theme(
        plot.margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0)
      )

    viz_grid <-
      cowplot::plot_grid(
        custom_title,
        plotlist =  viz_list,
        ncol = 1,
        scale = 0.9,
        rel_heights = c(0.1, rep(1, length(group_list)))
      )

    if (!is.na(x_title)) {

      viz_grid <- viz_grid +
        cowplot::draw_label(
          {{ x_title }},
          x = 0.5, y = 0,
          vjust = -0.2,
          angle = 0,
          fontface = "bold",
          size = 10
        )

    }

    if (!is.na(y_title)) {

      viz_grid <- viz_grid +
        cowplot::draw_label(
          {{ y_title }},
          x = 0, y = 0.5,
          vjust = 1.5,
          angle = 90,
          fontface = "bold",
          size = 10
        )

    }

    ggplot2::ggsave(
      filename = glue::glue("{out_filename}.png"),
      path = glue::glue("./figs/{save_folder}/"),
      plot = viz_grid,
      device = ragg::agg_png,
      width = 15,
      height = 11,
      units = "cm",
      dpi = 300
    )

    viz_grob <- ggplot2::ggplotGrob(viz_grid)

    obj_name <- out_filename

    assign(obj_name, viz_grob)

    save(
      list = obj_name,
      file = glue::glue("./figs/{save_folder}/{out_filename}.rdata")
    )

    return(NULL)

  }

#' @export
#' @rdname create_visualizations
eda_histogram <-
  function(
    data,
    variable,
    ...
  ) {

    data_obs <- data |>
      dplyr::select(dplyr::all_of(variable)) |>
      tidyr::drop_na() |>
      dplyr::pull(.data[[variable]])

    n <- data_obs |> length()

    skewness <-
      (sum((data_obs - mean(data_obs))^3) / n) /
      (sum((data_obs - mean(data_obs))^2) / n)^(3 / 2)

    count <- dplyr::count

    if (skewness > 1) {

      custom_breaks <- scales::breaks_log()

      scale_transform <- "log"

    } else {

      custom_breaks <- scales::breaks_pretty()

      scale_transform <- "identity"

    }

    viz <- data |>
      tibble::as_tibble() |>
      dplyr::filter(.data[[variable]] > 0) |>
      tidyr::drop_na() |>
      ggplot2::ggplot() +
      ggplot2::geom_histogram(
        mapping = ggplot2::aes(
          x = .data[[variable]],
          y = ggplot2::after_stat(count / max(count))
        ),
        color = "#000000",
        fill = "#e8e8e8",
        bins = 40,
        linewidth = 0.4
      ) +
      ggplot2::scale_x_continuous(
        trans = scale_transform,
        breaks = custom_breaks,
        labels = scales::label_number(accuracy = 1),
        expand = c(0.001, 0.001),
        limits = c(0.1, max(data[[variable]], na.rm = TRUE))
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

#' @export
#' @rdname create_visualizations
eda_spatial_distribution <-
  function(
    data,
    variable,
    variable_label,
    summary,
    ...
  ) {

    func_expr <-
      rlang::eval_tidy(
        rlang::parse_expr(glue::glue("{summary}"))
      )

    viz <- data |>
      tidyr::drop_na() |>
      dplyr::summarise(
        {{ variable }} := func_expr(.data[[variable]], na.rm = TRUE),
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
        begin = 0,
        end = 1,
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
      cowplot::theme_map(font_size = 10) +
      ggplot2::theme(
        text = ggplot2::element_text(size = 9),
        legend.position = "bottom",
        legend.key.height = ggplot2::unit(2, "mm"),
        legend.key.width = ggplot2::unit(25, "mm"),
        legend.justification = c(0.5, 0.5),
        legend.title = ggplot2::element_text(face = "bold"),
        plot.margin = ggplot2::margin(-0.8, -0.3, 0, -0.3, "cm")
      )

    return(viz)

  }

#' @export
#' @rdname create_visualizations
eda_timeseries <-
  function(
    data,
    variable,
    ...
  ) {

    viz <- data |>
      tidyr::drop_na() |>
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
      cowplot::theme_minimal_grid(font_size = 10) +
      ggplot2::theme(
        text = ggplot2::element_text(size = 9),
        axis.title = ggplot2::element_blank(),
        plot.margin = ggplot2::margin(-0.1, 0, 0.3, 0, "cm"),
        strip.text.y = ggplot2::element_text(size = 10, face = "bold"),
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
    table_title = parent.frame()$viz_title,
    ...
  ) {

    viz <- data |>
      tibble::as_tibble() |>
      tidyr::drop_na() |>
      dplyr::select(
        !dplyr::any_of(
          c("cell_id", "year", "name_biome", "lon", "lat", "geometry")
        )
      ) |>
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ dplyr::na_if(., 0))) |>
      tidyr::pivot_longer(dplyr::everything()) |>
      dplyr::summarise(
        dplyr::across(
          .cols = dplyr::everything(),
          .fns = list(
            min = min,
            mean = mean,
            median = median,
            max = max,
            std = sd
          ),
          na.rm = TRUE,
          .names = "{.fn}"
        ),
        .by = "name"
      ) |>
      gt::gt(
        rowname_col = "name"
      ) |>
      gt::tab_header(
        title = table_title
      ) |>
      gt::tab_options(
        table.font.size = 14,
        table.width = gt::pct(90)
      ) |>
      gt::fmt_number(
        columns = tidyselect::everything(),
        suffixing = TRUE,
        decimals = 0
      ) |>
      gt::cols_label(
        min = gt::md("**Mínimo**"),
        mean = gt::md("**Média**"),
        median = gt::md("**Mediana**"),
        max = gt::md("**Máximo**"),
        std = gt::md("**Desvio Padrão**")
      ) |>
      gt::tab_options(
        table.font.size = 14
      ) |>
      gt::opt_stylize(style = 1, color = "gray")

    return(viz)

  }

#' @export
#' @rdname create_visualizations
eda_observations <-
  function(
    data,
    variable = NA_character_,
    ...
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
        labels = c("Observado", "Não Observado")
      ) +
      ggplot2::scale_x_date(
        date_breaks = "5 years", date_labels = "%Y"
      ) +
      ggplot2::labs(
        fill = NULL
      ) +
      cowplot::theme_minimal_grid(font_size = 10) +
      ggplot2::theme(
        text = ggplot2::element_text(size = 9),
        axis.title = ggplot2::element_blank(),
        plot.margin = ggplot2::margin(-0.1, 0, 0.3, 0, "cm"),
        strip.text.y = ggplot2::element_text(size = 11, face = "bold"),
        legend.position = "bottom",
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
      )

    return(viz)

  }

#' @export
#' @rdname create_visualizations
results_scatterplot <-
  function(
    data,
    ...
  ) {

    set.seed(1)

    viz <- data |>
      dplyr::ungroup() |>
      dplyr::filter(.data$veg_suppression > 100) |>
      dplyr::summarise(
        mean_pred = median(.data$mean_pred),
        veg_suppression = median(.data$veg_suppression),
        .by = c("year", "cell_id", "name_biome")
      ) |>
      dplyr::slice_sample(prop = 0.05) |>
      ggplot2::ggplot(
        mapping = ggplot2::aes(
          x = .data$veg_suppression,
          y = .data$mean_pred,
        ),
      ) +
      ggplot2::geom_point(
        alpha = 0.2
      ) +
      ggplot2::geom_abline(intercept = 0, slope = 1) +
      cowplot::theme_minimal_grid(font_size = 10) +
      ggplot2::theme(
        text = ggplot2::element_text(size = 9),
        axis.title = ggplot2::element_blank(),
        plot.margin = ggplot2::margin(-0.1, 0, 0.3, 0, "cm"),
        strip.text.y = ggplot2::element_text(size = 10, face = "bold")
      )

    return(viz)

  }

#' @export
#' @rdname create_visualizations
results_mae_distribution <-
  function(
    data,
    ...
  ) {

    viz <- data |>
      dplyr::group_by(.data$name_biome) |>
      yardstick::mae("veg_suppression", "mean_pred") |>
      ggplot2::ggplot(
        mapping = ggplot2::aes(x = .data$.estimate),
      ) +
      ggplot2::geom_density() +
      cowplot::theme_minimal_grid(font_size = 10) +
      ggplot2::theme(
        text = ggplot2::element_text(size = 9),
        axis.title = ggplot2::element_blank(),
        plot.margin = ggplot2::margin(-0.1, 0, 0.3, 0, "cm"),
        strip.text.y = ggplot2::element_text(size = 10, face = "bold"),
        axis.text.y = ggplot2::element_blank()
      )

    return(viz)

  }

#' @export
#' @rdname create_visualizations
results_mae_timeseries <-
  function(
    data,
    ...
  ) {

    viz <- data |>
      dplyr::ungroup() |>
      dplyr::group_by(.data$year, .data$name_biome) |>
      yardstick::mae("veg_suppression", "mean_pred") |>
      ggplot2::ggplot(
        mapping = ggplot2::aes(x = .data$.estimate),
      ) +
      ggplot2::stat_summary(
        ggplot2::aes(
          x = lubridate::ymd(.data$year, truncated = 2),
          y = .data$.estimate,
          group = lubridate::ymd(.data$year, truncated = 2)
        ),
        fun.data = ggplot2::mean_se
      ) +
      ggplot2::scale_x_date(
        date_labels = "%Y",
        breaks = scales::date_breaks("2 years")
      ) +
      cowplot::theme_minimal_grid(font_size = 10) +
      ggplot2::theme(
        text = ggplot2::element_text(size = 9),
        axis.title = ggplot2::element_blank(),
        plot.margin = ggplot2::margin(-0.1, 0, 0.3, 0, "cm"),
        strip.text.y = ggplot2::element_text(size = 10, face = "bold"),
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
      )

    return(viz)

  }

#' @export
#' @rdname create_visualizations
results_timeseries <-
  function(
    data,
    ...
  ) {

    viz <- data |>
      dplyr::ungroup() |>
      dplyr::summarise(
        mean_pred = median(.data$mean_pred),
        .by = c("year", "cell_id", "name_biome")
      ) |>
      ggplot2::ggplot(
        mapping = ggplot2::aes(
          x = lubridate::ymd(.data$year, truncated = 2),
          y = .data$mean_pred,
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
      cowplot::theme_minimal_grid(font_size = 10) +
      ggplot2::theme(
        text = ggplot2::element_text(size = 9),
        axis.title = ggplot2::element_blank(),
        plot.margin = ggplot2::margin(-0.1, 0, 0.3, 0, "cm"),
        strip.text.y = ggplot2::element_text(size = 10, face = "bold"),
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
      )

    return(viz)

  }

#' @export
#' @rdname create_visualizations
results_spatial <-
  function(
    data,
    ...
  ) {

    base_dir <- here::here()

    viz <- data |>
      dplyr::ungroup() |>
      dplyr::summarise(
        mean_pred = median(.data$mean_pred),
        .by = c("cell_id", "year", "name_biome")
      ) |>
      dplyr::summarise(veg_sup = sum(.data$mean_pred), .by = c("cell_id")) |>
      dplyr::left_join(
        sf::read_sf(glue::glue("{base_dir}/data/grid.fgb")),
        by = dplyr::join_by("cell_id")
      ) |>
      sf::st_as_sf() |>
      ggplot2::ggplot() +
      ggplot2::geom_sf(
        mapping = ggplot2::aes(
          fill = .data$veg_sup
        )
      ) +
      scico::scale_fill_scico(
        palette = "bilbao",
        begin = 0,
        end = 1,
        direction = -1,
        labels = scales::label_number(scale_cut = scales::cut_short_scale())
      ) +
      ggplot2::labs(fill = "Valores Estimados") +
      ggplot2::guides(
        fill = ggplot2::guide_colourbar(
          title.position = "top",
          title.hjust = 0.5
        )
      ) +
      ggplot2::coord_sf() +
      cowplot::theme_map(font_size = 10) +
      ggplot2::theme(
        text = ggplot2::element_text(size = 9),
        legend.position = "bottom",
        legend.key.height = ggplot2::unit(2, "mm"),
        legend.key.width = ggplot2::unit(25, "mm"),
        legend.justification = c(0.5, 0.5),
        legend.title = ggplot2::element_text(face = "bold"),
        plot.margin = ggplot2::margin(-0.8, -0.3, 0, -0.3, "cm")
      )

    return(viz)

  }

#' @export
#' @rdname create_visualizations
results_mae_spatial <-
  function(
    data,
    ...
  ) {

    base_dir <- here::here()

    viz <- data |>
      dplyr::ungroup() |>
      dplyr::group_by(.data$cell_id) |>
      yardstick::mae("veg_suppression", "mean_pred") |>
      dplyr::left_join(
        sf::read_sf(glue::glue("{base_dir}/data/grid.fgb")),
        by = dplyr::join_by("cell_id")
      ) |>
      sf::st_as_sf() |>
      ggplot2::ggplot() +
      ggplot2::geom_sf(
        mapping = ggplot2::aes(
          fill = .data$.estimate
        )
      ) +
      scico::scale_fill_scico(
        palette = "bilbao",
        begin = 0,
        end = 1,
        direction = -1,
        labels = scales::label_number(scale_cut = scales::cut_short_scale())
      ) +
      ggplot2::labs(fill = "Erro Médio Absoluto") +
      ggplot2::guides(
        fill = ggplot2::guide_colourbar(
          title.position = "top",
          title.hjust = 0.5
        )
      ) +
      ggplot2::coord_sf() +
      cowplot::theme_map(font_size = 10) +
      ggplot2::theme(
        text = ggplot2::element_text(size = 9),
        legend.position = "bottom",
        legend.key.height = ggplot2::unit(2, "mm"),
        legend.key.width = ggplot2::unit(25, "mm"),
        legend.justification = c(0.5, 0.5),
        legend.title = ggplot2::element_text(face = "bold"),
        plot.margin = ggplot2::margin(-0.8, -0.3, 0, -0.3, "cm")
      )

    return(viz)

  }

#' @export
#' @rdname create_visualizations
method_samples <-
  function(
    data,
    ...
  ) {

    base_dir <- here::here()

    data <- data <-
      arrow::read_parquet(
        glue::glue("{base_dir}/data/sampled_data.parquet"),
        as_data_frame = TRUE
      )

    spatial_viz <- data |>
      tibble::as_tibble() |>
      dplyr::filter(sample %in% c(1, 2)) |>
      dplyr::distinct(.data$cell_id, .data$sample, .data$split_class) |>
      dplyr::mutate(
        sample = dplyr::if_else(
          .data$sample == 1, "Amostragem 1", "Amostragem 2"
        ),
        split_class = dplyr::if_else(
          .data$split_class == "test", "Teste", "Treino"
        )
      ) |>
      dplyr::left_join(
        sf::read_sf(glue::glue("{base_dir}/data/grid.fgb")),
        by = dplyr::join_by("cell_id")
      ) |>
      sf::st_as_sf() |>
      ggplot2::ggplot() +
      ggplot2::facet_grid(cols = ggplot2::vars(.data$sample)) +
      ggplot2::geom_sf(
        mapping = ggplot2::aes(
          fill = .data$split_class
        )
      ) +
      scico::scale_fill_scico_d(
        palette = "corkO",
        begin = 0.2,
        end = 0.9,
        direction = -1
      ) +
      ggplot2::labs(fill = "Grupo Amostral") +
      ggplot2::coord_sf() +
      cowplot::theme_map(font_size = 10) +
      ggplot2::theme(
        text = ggplot2::element_text(size = 9),
        legend.position = "",
        legend.justification = c(0.5, 0.5),
        legend.title = ggplot2::element_text(face = "bold"),
        plot.margin = ggplot2::margin(-3, -0.3, -3, -0.3, "cm"),
        strip.text = ggplot2::element_text(size = 9, face = "bold")
      )

    temporal_viz <- data |>
      tibble::as_tibble() |>
      dplyr::filter(sample %in% c(1, 2)) |>
      dplyr::distinct(.data$year, .data$sample, .data$split_class) |>
      dplyr::mutate(
        sample = dplyr::if_else(
          .data$sample == 1, "Amostragem 1", "Amostragem 2"
        ),
        split_class = dplyr::if_else(
          .data$split_class == "test", "Teste", "Treino"
        ),
        year = lubridate::ymd(.data$year, truncated = 2),
        dummy = "Amostras"
      ) |>
      ggplot2::ggplot() +
      ggplot2::facet_grid(cols = ggplot2::vars(.data$sample)) +
      ggplot2::geom_tile(
        mapping = ggplot2::aes(
          x = .data$year,
          y = .data$dummy,
          fill = .data$split_class
        ),
        color = "#000000"
      ) +
      scico::scale_fill_scico_d(
        palette = "corkO",
        begin = 0.2,
        end = 0.9,
        direction = -1
      ) +
      ggplot2::scale_x_date(
        date_labels = "%Y",
        breaks = scales::date_breaks("2 years")
      ) +
      ggplot2::labs(fill = "Grupo Amostral") +
      cowplot::theme_minimal_grid(font_size = 10) +
      ggplot2::theme(
        text = ggplot2::element_text(size = 9),
        legend.position = "bottom",
        legend.justification = c(0.5, 0.5),
        legend.title = ggplot2::element_text(
          face = "bold",
          margin = ggplot2::margin(r = 30)
        ),
        axis.title = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
        strip.text = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        plot.margin = ggplot2::margin(0, -0.3, 0, -0.3, "cm"),
      )

    viz <-
      cowplot::plot_grid(
        spatial_viz, temporal_viz,
        ncol = 1,
        rel_heights = c(1, 0.3)
      )

    return(viz)

  }

#' @export
#' @rdname create_visualizations
explain_summary <-
  function(
    data,
    variable,
    ...
  ) {

    base_dir <- here::here()

    set.seed(1)

    viz <- data |>
      sf::st_drop_geometry() |>
      tidyr::drop_na() |>
      dplyr::filter(.data[[variable]] > 0) |>
      dplyr::select(
        dplyr::all_of(
          c(
            "cell_id", "year", "veg_suppression",
            "name_biome", variable
          )
        )
      ) |>
      dplyr::inner_join(
        arrow::read_parquet(
          glue::glue("{base_dir}/data/results/model_shap.parquet")
        ) |>
          dplyr::summarise(
            shap_value = mean(.data$shap_value),
            .by = c("cell_id", "year", "var")
          ) |>
          dplyr::filter(
            var %in% c(variable),
            .data$shap_value != 0
          ),
        by = dplyr::join_by("cell_id", "year")
      ) |>
      dplyr::arrange(.data$cell_id, .data$year) |>
      dplyr::slice_sample(n = 3000) |>
      ggplot2::ggplot() +
      ggforce::geom_sina(
        mapping = ggplot2::aes(
          y = var,
          x = .data$shap_value,
          color = .data[[variable]]
        ),
        alpha = 0.5
      ) +
      scico::scale_color_scico(
        palette = "bilbao",
        labels = scales::label_number(scale_cut = scales::cut_short_scale()),
        begin = 0,
        end = 0.7,
        direction = -1
      ) +
      ggplot2::stat_summary(
        mapping = ggplot2::aes(
          y = var,
          x = .data$shap_value,
          group = var
        ),
        fun = mean,
        geom = "point",
        shape = 73, size = 15
      ) +
      ggplot2::geom_vline(
        mapping = ggplot2::aes(
          xintercept = 0
        )
      ) +
      ggplot2::guides(color = "none") +
      cowplot::theme_minimal_grid(font_size = 10) +
      ggplot2::theme(
        text = ggplot2::element_text(size = 9),
        axis.title = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        plot.margin = ggplot2::margin(-0.1, 0, 0.3, 0, "cm"),
        strip.text.y = ggplot2::element_text(size = 10, face = "bold")
      )

    return(viz)

  }

#' @export
#' @rdname create_visualizations
explain_dependence <-
  function(
    data,
    variable,
    ...
  ) {

    base_dir <- here::here()

    set.seed(1)

    viz <- data |>
      sf::st_drop_geometry() |>
      tidyr::drop_na() |>
      dplyr::filter(.data[[variable]] > 0) |>
      dplyr::select(
        dplyr::all_of(
          c(
            "cell_id", "year", "veg_suppression",
            "name_biome", variable
          )
        )
      ) |>
      dplyr::inner_join(
        arrow::read_parquet(
          glue::glue("{base_dir}/data/results/model_shap.parquet")
        ) |>
          dplyr::summarise(
            shap_value = mean(.data$shap_value),
            .by = c("cell_id", "year", "var")
          ) |>
          dplyr::filter(
            var %in% c(variable),
            .data$shap_value != 0
          ),
        by = dplyr::join_by("cell_id", "year")
      ) |>
      dplyr::arrange(.data$cell_id, .data$year) |>
      dplyr::slice_sample(n = 3000) |>
      ggplot2::ggplot() +
      ggplot2::geom_point(
        mapping = ggplot2::aes(
          x = .data[[variable]],
          y = .data$shap_value,
          color = .data$veg_suppression
        ),
        alpha = 0.5
      ) +
      scico::scale_color_scico(
        palette = "bilbao",
        labels = scales::label_number(scale_cut = scales::cut_short_scale()),
        begin = 0,
        end = 0.7,
        direction = -1
      ) +
      ggplot2::geom_hline(
        mapping = ggplot2::aes(
          yintercept = 0
        )
      ) +
      ggplot2::guides(color = "none") +
      cowplot::theme_minimal_grid(font_size = 10) +
      ggplot2::theme(
        text = ggplot2::element_text(size = 9),
        axis.title = ggplot2::element_blank(),
        plot.margin = ggplot2::margin(-0.1, 0, 0.3, 0, "cm"),
        strip.text.y = ggplot2::element_text(size = 10, face = "bold")
      )

    return(viz)

  }

#' @export
#' @rdname create_visualizations
method_grid <-
  function(
    data,
    ...
  ) {

    aoi <- sf::read_sf("./data/raw/biomes/biomes_2019.fgb") |>
      dplyr::filter(.data$code_biome %in% c(3, 1))

    viz <- ggplot2::ggplot() +
      ggplot2::geom_sf(
        data = aoi,
      ) +
      ggplot2::geom_sf(
        data = data,
        fill = "transparent"
      ) +
      ggplot2::coord_sf() +
      cowplot::theme_map(font_size = 10) +
      ggplot2::theme(
        text = ggplot2::element_text(size = 9),
        legend.position = "",
        legend.justification = c(0.5, 0.5),
        legend.title = ggplot2::element_text(face = "bold"),
        plot.margin = ggplot2::margin(-0.8, -0.3, 0, -0.3, "cm")
      )

    return(viz)

  }

#' @export
#' @rdname create_visualizations
method_grid_area <-
  function(
    data,
    ...
  ) {

    aoi <- sf::read_sf("./data/raw/biomes/biomes_2019.fgb") |>
      dplyr::filter(.data$code_biome %in% c(3, 1))

    viz_grid <- ggplot2::ggplot() +
      ggplot2::geom_sf(
        data = aoi,
      ) +
      ggplot2::geom_sf(
        data = data,
        fill = "transparent"
      ) +
      ggplot2::coord_sf() +
      cowplot::theme_map(font_size = 10) +
      ggplot2::theme(
        text = ggplot2::element_text(size = 9),
        legend.position = "",
        legend.justification = c(0.5, 0.5),
        legend.title = ggplot2::element_text(face = "bold"),
        plot.margin = ggplot2::margin(-0.8, -0.3, 0, -0.3, "cm")
      )

    cell_area <- data |>
      dplyr::mutate(
        cell_area = as.numeric(sf::st_area(.data$geometry)) / 10000
      ) |>
      dplyr::slice_min(
        order_by = abs(cell_area - mean(cell_area)),
        with_ties = FALSE
      ) |>
      dplyr::select(cell_area) |>
      dplyr::mutate(
        key = 1
      ) |>
      dplyr::left_join(
        tibble::tibble(
          key = rep(1, 7),
          percentage = c(0.01, 0.05, 0.1, 0.25, 0.5, 0.9, 1)
        ),
        by = dplyr::join_by("key")
      ) |>
      dplyr::mutate(percentage_area = .data$cell_area * .data$percentage)

    sub_cell_area <-
      purrr::map_df(
        cell_area$percentage,
        \(p) {

          sub_cell_area <-
            sf::st_make_grid(
              sf::st_geometry(dplyr::first(cell_area)),
              cellsize = 1000,
              square = FALSE
            ) |>
            sf::st_intersection(
              sf::st_geometry(dplyr::first(cell_area))
            ) |>
            sf::st_as_sf() |>
            dplyr::filter(
              sf::st_is(.data$x, c("POLYGON", "GEOMETRYCOLLECTION"))
            ) |>
            dplyr::slice_sample(prop = p) |>
            dplyr::mutate(percentage = p)

          return(sub_cell_area)

        }
      )

    viz_cell_area <- cell_area |>
      ggplot2::ggplot() +
      ggplot2::facet_wrap(
        facets = ggplot2::vars(.data$percentage),
        nrow = 1,
        labeller = ggplot2::label_bquote(.(scales::percent(percentage)))
      ) +
      ggplot2::geom_sf(
        data = sub_cell_area,
        mapping = ggplot2::aes(fill = percentage, color = percentage)
      ) +
      ggplot2::geom_sf(
        fill = "transparent",
        color = "#000000",
        linewidth = 0.5
      ) +
      ggplot2::geom_sf_text(
        ggplot2::aes(
          label = scales::number(
            .data$percentage_area,
            suffix = " mil ha",
            accuracy = 1,
            scale = 1e-3
          )
        ),
        size = 2,
        nudge_y = -28000
      ) +
      scico::scale_fill_scico(end = 0.7, direction = -1) +
      scico::scale_color_scico(end = 0.7, direction = -1) +
      ggplot2::coord_sf(clip = "off") +
      ggplot2::theme_void() +
      ggplot2::theme(
        legend.position = "",
        text = ggplot2::element_text(size = 13, face = "bold"),
        plot.margin = ggplot2::unit(c(-0.7, 0, 0, 0), "cm")
      )

    viz <-
      cowplot::plot_grid(
        viz_grid,
        viz_cell_area,
        ncol = 1,
        scale = 0.9,
        rel_heights = c(1, 0.15)
      )

    return(viz)

  }