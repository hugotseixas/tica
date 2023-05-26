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
#' @param out_path Path of the file
#' @param out_width Width in centimeters of the .png file
#' @param out_height Height in centimeters of the .png file
#' @param ... Parameters passed to visualization functions
#'
#' @examples
#' \dontrun{
#'   create_visualizations()
#' }
#'
#' @export
#' @rdname create_visualizations
create_visualizations <-
  function(
    f,
    data,
    group_facet = FALSE,
    group_variable = NULL,
    viz_title = NULL,
    x_title = NULL,
    y_title = NULL,
    out_format = c("png", "rdata"),
    out_filename = NULL,
    out_path = NULL,
    out_width = 15,
    out_height = 11,
    ...
  ) {

    if (group_facet) {
      group_list <- data |>
        dplyr::distinct({{group_variable}}) |>
        dplyr::pull(dplyr::any_of({{group_variable}}))

    } else {

      group_list <- NA

    }

    viz_list <-
      purrr::map(
        group_list,
        \(group) {

          if (group_facet) {

            viz_data <- data |>
              dplyr::filter({{group_variable}} == group)

            viz <- f(data = viz_data, ...) +
              ggplot2::facet_wrap(
                facets = ggplot2::vars({{group_variable}}),
                nrow = 1,
                strip.position = "right"
              )

          } else {

            viz_data <- data

            viz <- f(data = viz_data, ...)

          }

          return(viz)

        }
      )

    custom_title <- cowplot::ggdraw() +
      cowplot::draw_label(
        {{viz_title}},
        size = 15,
        fontface = 'bold',
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
        {{x_title}},
        x = 0.5, y = 0,
        vjust = -0.2,
        angle = 0,
        size = 13
      ) +
      cowplot::draw_label(
        {{y_title}},
        x = 0, y = 0.5,
        vjust = 1.5,
        angle = 90,
        size = 13
      )

    if ("png" %in% out_format) {

      ggplot2::ggsave(
        filename = glue::glue("{out_filename}.png"),
        path = out_path,
        plot = viz_grid,
        device = ragg::agg_png,
        width = out_width,
        height = out_height,
        units = "cm",
        dpi = 300
      )

    }

    if ("rdata" %in% out_format) {

      obj_name <- out_filename

      assign(obj_name, viz_grid)

      save(
        list = obj_name,
        file = glue::glue("{out_path}{out_filename}.rdata")
      )

    }

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

    } else { stop("Invalid transform.") }

    viz <-
      ggplot2::ggplot(
        data = data,
        mapping = ggplot2::aes(
          x = {{variable}},
          y = ggplot2::after_stat(count / max(count))
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
      dplyr::rename(viz_variable = {{variable}}) |>
      dplyr::reframe(
        quant = quantile(viz_variable, quantiles_list),
        probs = quantiles_list
      )

    # Get cumulative sum for each percentile
    cumsum_table <- data |>
      dplyr::rename(viz_variable = {{variable}}) |>
      dplyr::arrange(viz_variable) |>
      dplyr::mutate(cumulative = cumsum(viz_variable/sum(viz_variable))) |>
      dplyr::inner_join(
        quantile_table,
        by = dplyr::join_by(closest(viz_variable >= quant))
      ) |>
      dplyr::mutate(dif = viz_variable - quant) |>
      dplyr::slice_min(order_by = dif, by = c(probs)) |>
      dplyr::mutate(
        cumulative = round(cumulative, digits = 2),
        viz_variable = round(viz_variable)
      ) |>
      dplyr::distinct(cumulative, viz_variable, probs)

    viz_table <- data |>
      dplyr::rename(viz_variable = {{variable}}) |>
      dplyr::arrange(viz_variable) |>
      dplyr::mutate(cumulative = cumsum(viz_variable/sum(viz_variable))) |>
      dplyr::left_join(
        quantile_table,
        by = dplyr::join_by(viz_variable <= quant),
        multiple = "first"
      )

    viz <-
      ggplot2::ggplot(
        data = viz_table,
        ggplot2::aes(x = viz_variable, y = cumulative)
      ) +
      ggplot2::geom_ribbon(
        ggplot2::aes(
          ymin = 0,
          ymax = cumulative,
          fill = factor(probs)
        ),
        alpha = 0.7
      ) +
      ggplot2::geom_line(
        linewidth = 0.5
      ) +
      ggplot2::annotate(
        geom = "segment",
        x = cumsum_table$viz_variable,
        xend = cumsum_table$viz_variable,
        y = 0,
        yend = cumsum_table$cumulative,
        linetype = 2
      ) +
      ggplot2::geom_label(
        data = cumsum_table,
        ggplot2::aes(label = glue::glue("{probs * 100}th")),
        alpha = 0.9,
        size = 3
      ) +
      ggplot2::scale_x_continuous(
        breaks = c(cumsum_table$viz_variable),
        labels = scales::label_number(
          scale_cut = scales::cut_short_scale(),
          accuracy = 1
        ),
        guide = ggplot2::guide_axis(angle = 55),
        trans = scale_transform
      ) +
      ggplot2::scale_y_continuous(
        labels = scales::label_percent(),
        breaks = c(cumsum_table$cumulative, 1),
        guide = ggplot2::guide_axis(check.overlap = TRUE)
      ) +
      scico::scale_fill_scico_d(palette = "bilbao") +
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

#' @param data A tibble to provide data
#' @param variable The variable to be represented in the visualization
#' @param base_map An sf object to serve as base map
#' @param animated Should the visualization be animated?
#'
#' @export
#' @rdname create_visualizations
eda_spatial_distribution <-
  function(
    data,
    variable,
    variable_label,
    base_map,
    animated = FALSE,
    ...
  ) {

    viz <- data |>
      ggplot2::ggplot()

    if (!is.null(base_map)) {

      viz <- viz +
        ggplot2::geom_sf(
          data = base_map,
          fill = "transparent"
        )

    }

    break_values <-
      c(
        min(dplyr::pull(data, {{variable}})),
        max(dplyr::pull(data, {{variable}}))
      )

    viz <- viz +
      ggplot2::geom_sf(
        mapping = ggplot2::aes(
          geometry = geometry,
          fill = {{variable}}
        )
      ) +
      scico::scale_fill_scico(
        palette = "bilbao",
        begin = 0.1,
        end = 0.9,
        breaks = break_values,
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
        legend.key.height = ggplot2::unit(2, 'mm'),
        legend.key.width = ggplot2::unit(25, 'mm'),
        legend.justification = c(0.5, 0.5),
        plot.margin = ggplot2::margin(-0.8, -0.3, 0, -0.3, "cm")
      )

    if (animated) {

      viz <- viz +
        gganimate::transition_time(time = year)

    }

    return(viz)

  }

#' @param data A tibble to provide data
#' @param variable The variable to be represented in the visualization
#' @param ts_type The type of the time series visualization
#'
#' @export
#' @rdname create_visualizations
eda_time_series <-
  function(
    data,
    variable,
    ts_type = c("step", "bar", "line"),
    ...
  ) {

    viz <- data |>
      ggplot2::ggplot(
        mapping = aes(
          x = date,
          y = {{variable}}
        )
      )

    if (ts_type == "step") {

      viz <- viz +
        ggplot2::geom_step(
          mapping = ggplot2::aes(color = {{variable}}),
          linewidth = 1.5
        ) +
        ggplot2::geom_point(
          mapping = ggplot2::aes(color = {{variable}}),
          size = 3
        ) +
        scico::scale_color_scico(
          palette = "bilbao",
          labels = scales::label_number(scale_cut = scales::cut_short_scale()),
          begin = 0.2
        )

    } else if (ts_type == "bar") {

      viz <- viz +
        ggplot2::geom_col(
          mapping = ggplot2::aes(fill = {{variable}}),
          color = "#000000"
        ) +
        scico::scale_fill_scico(
          palette = "bilbao",
          labels = scales::label_number(scale_cut = scales::cut_short_scale()),
          begin = 0.2
        )

    } else if (ts_type == "line") {

      viz <- viz +
        ggplot2::geom_line(
          mapping = aes(color = {{variable}}),
          linewidth = 1.5,
        ) +
        ggplot2::geom_point(
          mapping = aes(color = {{variable}}),
          size = 3
        ) +
        scico::scale_color_scico(
          palette = "bilbao",
          labels = scales::label_number(scale_cut = scales::cut_short_scale()),
          begin = 0.2
        )

    }

    viz <- viz +
      ggplot2::scale_y_continuous(
        labels = scales::label_number(scale_cut = scales::cut_short_scale())
      ) +
      ggplot2::scale_x_continuous(
        labels = scales::label_date(format = "%Y")
      ) +
      ggplot2::guides(fill = "none", color = "none") +
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

    } else { stop("Invalid transform.") }

    viz <- data |>
      ggplot2::ggplot(
        ggplot2::aes(
          x = {{cat_variable}},
          y = {{variable}},
          fill = {{variable}},
          group = {{cat_variable}}
        )
      ) +
      ggplot2::geom_col(
        color = "#000000"
      ) +
      ggplot2::geom_label(
        aes(
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
      scico::scale_fill_scico(
        palette = "bilbao",
        labels = scales::label_number(
          data,scale_cut = scales::cut_short_scale()),
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
            qu_1st = ~ quantile(.x, 0.25),
            mean = mean,
            median = median,
            qu_3rd = ~ quantile(.x, 0.75),
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
        qu_1st = gt::md("**1st Quantile**"),
        mean = gt::md("**Mean**"),
        median = gt::md("**Median**"),
        qu_3rd = gt::md("**3rd Quantile**"),
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
