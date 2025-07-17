#' Create IPCA forecast model dashboard
#'
#' @param y Column name of the variable of interest
#' @param data A tsibble object, similar to the output of `ts_transform(get_data(), y)`
#' @param accuracy A tibble object, similar and with the same column names as the output of `train_models`
#' @param forecast A tsibble object, similar to the output of `forecast_ensemble`
#' @param ensemble_col Column name of ensemble forecasts in `forecast`
#' @param title_fanchart Title for the forecast fanchart
#' @param y_lab_fanchart Fanchart Y-axis title
#' @param caption_lab Text for caption
#' @param title_accuracy Title for predictive horizon accuracy graph (`accuracy` data)
#' @param subtitle_accuracy Subtitle for the accuracy graph by predictive horizon (`accuracy` data)
#' @param x_lab_accuracy X-axis text in accuracy graph by predictive horizon (`accuracy` data)
#' @param save_at Directory name to save the HTML file, if it does not exist it will be created
#'
#' @return Flexdashboard HTML file named as `index.html`.
#' @export
#' @encoding UTF-8
#'
#' @examples
#' td <- tempdir()
#' build_dashboard(save_at = td)
build_dashboard <- function (
  y                    = "ipca",
  data                 = ts_transform(get_data(), y),
  accuracy             = train_models(data, y),
  forecast             = forecast_ensemble(ts_transform(get_data(), y, seasonal_dummies = FALSE), y),
  ensemble_col         = "ensemble",
  title_fanchart       = rlang::as_utf8_character("**Fanchart**: Previs\u00e3o do IPCA"),
  y_lab_fanchart       = "% a.m.",
  caption_lab          = rlang::as_utf8_character("**Elabora\u00e7\u00e3o:** FIESP"),
  title_accuracy       = rlang::as_utf8_character("**Acur\u00e1cia**: performance por horizonte"),
  subtitle_accuracy    = rlang::as_utf8_character("Modelos de previs\u00e3o do IPCA"),
  x_lab_accuracy       = "Horizonte (meses)",
  save_at              = "docs"
  ) {

  my_df <- data
  fcst <- forecast

  if (!all(c(tsibble::is_tsibble(my_df), tsibble::is_tsibble(fcst)))) {
    stop("data and forecast must be tsibbles.", call. = FALSE)
  } else {
    date_col_dt <- tsibble::index_var(my_df)
    date_col_fc <- tsibble::index_var(fcst)
    }

  rlang::check_installed(
    pkg = c("knitr", "flexdashboard"),
    reason = "to use `build_dashboard()`"
    )

  my_df_tbl <- my_df %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(date = lubridate::as_date(.data[[date_col_dt]]))

  fcst_tbl <- fcst %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(date = lubridate::as_date(.data[[date_col_fc]]))


  # Cores para gráficos
  colors <- c(
    "#282f6b", # blue
    "#b22200", # red
    "#224f20", # green
    "#eace3f", # yellow
    "#5f487c", # purple
    "#b35c1e", # orange
    "black",
    "#419391", # turquoise
    "#839c56", # light green
    "#3b89bc", # light blue
    "#666666"  # gray
    )


  # |-- Fanchart ----

  # Juntar dados observados com pontos de previsão
  df_fanchart <- my_df_tbl %>%
    dplyr::select(dplyr::all_of(c("date", y))) %>%
    dplyr::full_join(y = fcst_tbl, by = "date") %>%
    dplyr::mutate(
      ensemble = dplyr::if_else(
        date == max(my_df_tbl$date),
        !!rlang::sym(y),
        !!rlang::sym(ensemble_col)
        )
      )

  # Gerar gráfico de linha (fanchart)
  plt_fanchart <- df_fanchart %>%
    ggplot2::ggplot() +
    ggplot2::aes(x = date) +
    ggplot2::geom_line(
      mapping = ggplot2::aes(y = !!rlang::sym(y)),
      size    = 1.5,
      color   = colors[7],
      data    = dplyr::slice_tail(my_df_tbl, n = 36)
      ) +
    ggplot2::geom_ribbon(
      mapping = ggplot2::aes(ymin = -Inf, ymax = Inf),
      fill    = colors[1],
      alpha   = 0.35,
      data    = dplyr::filter(df_fanchart, date >= max(my_df_tbl$date))
      ) +
    ggplot2::geom_line(
      mapping = ggplot2::aes(y = !!rlang::sym(ensemble_col)),
      size    = 2,
      color   = colors[2],
      data    = dplyr::filter(df_fanchart, date >= max(my_df_tbl$date))
      ) +
    ggplot2::geom_vline(
      xintercept = max(my_df_tbl$date),
      linetype   = "dashed"
      ) +
    ggplot2::scale_y_continuous(
      labels = scales::number_format(
        suffix       = "%",
        accuracy     = 0.1,
        decimal.mark = ","
        )
      ) +
    ggplot2::scale_x_date(
      breaks = scales::breaks_width("3 months"),
      labels = ym_label
      ) +
    ggplot2::theme_light() +
    ggplot2::labs(
      title    = title_fanchart,
      y        = y_lab_fanchart,
      x        = NULL,
      caption  = caption_lab
      ) +
    ggplot2::theme(
      plot.title       = ggtext::element_markdown(size = 25, colour = colors[1]),
      axis.text        = ggtext::element_markdown(size = 12, face = "bold"),
      axis.title       = ggtext::element_markdown(size = 12, face = "bold"),
      panel.grid.minor = ggplot2::element_blank(),
      plot.caption     = ggtext::element_textbox_simple(
        size   = 12,
        colour = "grey20",
        margin = ggplot2::margin(10, 5.5, 10, 5.5)
        )
      )


  # |-- Gráfico de acurácia ----

  # Dados de acurácia de todos os modelos
  acc_rmse <- accuracy %>%
    dplyr::arrange(horizon, rmse, model) %>%
    dplyr::select("Horizonte" = "horizon", "Modelo" = "model", "RMSE" = "rmse")


  # Gráfico do RMSE por horizonte de previsão
  plt_rmse <- acc_rmse %>%
    ggplot2::ggplot() +
    ggplot2::aes(x = Horizonte, y = RMSE, colour = Modelo) +
    ggplot2::geom_line(size = 1.5) +
    ggplot2::geom_point(size = 3) +
    ggplot2::scale_colour_manual(values = colors) +
    ggplot2::scale_y_continuous(
      breaks = scales::breaks_extended(n = 8),
      labels = scales::number_format(
        accuracy     = 0.001,
        decimal.mark = ",",
        big.mark     = "."
        )
      ) +
    ggplot2::scale_x_continuous(
      labels = scales::number_format(accuracy = 1),
      breaks = 1:max(acc_rmse$Horizonte)
      ) +
    ggplot2::theme_light() +
    ggplot2::labs(
      title    = title_accuracy,
      subtitle = subtitle_accuracy,
      x        = x_lab_accuracy,
      color    = NULL,
      caption  = caption_lab
      ) +
    ggplot2::theme(
      plot.title       = ggtext::element_markdown(size = 25, colour = colors[1]),
      plot.subtitle    = ggtext::element_markdown(size = 16),
      axis.text        = ggtext::element_markdown(size = 12, face = "bold"),
      axis.title       = ggtext::element_markdown(size = 12, face = "bold"),
      legend.position  = "bottom",
      legend.text      = ggplot2::element_text(size = 12, face = "bold"),
      legend.key.width = ggplot2::unit(1, "cm"),
      panel.grid.minor = ggplot2::element_blank(),
      plot.caption     = ggtext::element_textbox_simple(
        size   = 12,
        colour = "grey20",
        margin = ggplot2::margin(10, 5.5, 10, 5.5)
        )
      )


  # |-- Tabelas ----

  # Tabela de pontos de previsão
  fc_tbl <- df_fanchart %>%
    dplyr::filter(date > max(my_df_tbl$date)) %>%
    dplyr::mutate(date = lubridate::as_date(date) %>% format("%b/%Y")) %>%
    dplyr::select("Data" = "date", "Previs\u00e3o" = !!rlang::sym(ensemble_col)) %>%
    DT::datatable(
      options = list(dom = "tip", pageLength = 6, scrollX = TRUE, scrollY = TRUE),
      rownames = FALSE
    ) %>%
    DT::formatRound(columns = 2, digits = 2, dec.mark = ",", mark = ".") %>%
    DT::formatStyle(columns = 2, fontWeight = "bold")


  # Tabela com valores do RMSE vs. horizonte/modelos
  rmse_tbl <- acc_rmse %>%
    DT::datatable(
      options = list(dom = "tip", pageLength = 5, scrollX = TRUE, scrollY = TRUE),
      rownames = FALSE
    ) %>%
    DT::formatRound(columns = 3, digits = 2, dec.mark = ",", mark = ".")



  # Dashboard ---------------------------------------------------------------

  # Verificar se pasta save_at existe
  if (!dir.exists(save_at)) { dir.create(save_at) }

  # Renderizar dashboard
  rmd <- "rmarkdown/templates/dashboard/skeleton/skeleton.Rmd"
  file <- system.file(rmd, package = "ipca")
  rmarkdown::render(input = file, output_file = "index", output_dir = save_at)

}

