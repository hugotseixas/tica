#' Title Create data visualizations
#'
#' @return A ggplot
#'
#' @examples
#' \dontrun{
#'   eda_histogram()
#' }
#'
#' @export
create_visualizations <-
  function(

  ) {



  }

#' @param data A tibble to provide data
#' @param variable The variable to be represented in the visualization
#' @param n_bins Number of bins for the visualization
#' @param scale_transform A transform for the horizontal axis
#' @param xlim The limits for the horizontal axis
#'
#' @export
#' @rdname create_visualizations
eda_histogram <-
  function(
    data = NULL,
    variable = NULL,
    n_bins = 30,
    scale_transform = "identity",
    xlim = NULL
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
          y = ggplot2::after_stat(count)
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
        limits = xlim,
        expand = c(0.001, 0.001)
      ) +
      cowplot::theme_nothing() +
      ggplot2::theme(text = ggplot2::element_text(size = 13))

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
    data = NULL,
    variable = NULL,
    quantiles_list = c(0, 0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99, 1),
    scale_transform = "identity"
  ) {

    # Calculate percentiles
    quantile_table <- data |>
      tidyr::drop_na() |>
      dplyr::rename(viz_variable = {{variable}}) |>
      dplyr::reframe(
        quant = quantile(viz_variable, quantiles_list),
        probs = quantiles_list
      )

    # Get cumulative sum for each percentile
    cumsum_table <- data |>
      tidyr::drop_na() |>
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

    viz_table <- data %>%
      tidyr::drop_na() %>%
      dplyr::rename(viz_variable = {{variable}}) |>
      dplyr::arrange(viz_variable) %>%
      dplyr::mutate(cumulative = cumsum(viz_variable/sum(viz_variable))) %>%
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
      ggplot2::geom_step(
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
        labels = scales::label_number(accuracy = 1, scale = 0.0001),
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
      cowplot::theme_nothing() +
      ggplot2::theme(text = ggplot2::element_text(size = 13))

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
    data = NULL,
    variable = NULL,
    base_map = NULL,
    animated = FALSE
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

    viz <- viz +
      ggplot2::geom_sf(
        mapping = ggplot2::aes(
          geometry = geometry,
          fill = {{variable}}
        )
      ) +
      ggplot2::coord_sf() +
      scico::scale_fill_scico(
        palette = "bilbao",
        labels = scales::label_number(scale_cut = scales::cut_short_scale())
      ) +
      cowplot::theme_nothing() +
      ggplot2::theme(text = ggplot2::element_text(size = 13))

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
    data = NULL,
    variable = NULL,
    ts_type = c("step", "bar", "line")
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
          mapping = ggplot2::aes(color = {{variable}})
        ) +
        ggplot2::geom_point(
          mapping = ggplot2::aes(color = {{variable}}),
          size = 1.5
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

    }else if (ts_type == "line") {

      viz <- viz +
        ggplot2::geom_line(
          mapping = aes(color = {{variable}})
        ) +
        ggplot2::geom_point(
          mapping = aes(color = {{variable}}),
          size = 1.5
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
      cowplot::theme_nothing() +
      ggplot2::theme(text = ggplot2::element_text(size = 13))

    return(viz)

  }
