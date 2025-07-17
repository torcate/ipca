#' Black box function to prepare time series
#'
#' This function performs a number of conditional treatments (see arguments) commonly useful for use in time series forecasting models.
#'
#' @param data A tsibble object
#' @param y Column name of the variable of interest
#' @param stationary If `TRUE`, use `report_ndiffs()` to differentiate series
#' @param lags Numeric vector indicating how many lags should be used to expand the data.
#' @param fill_down_na If `TRUE`, fill NA observations with the last observed value downwards.
#' @param seasonal_dummies If `TRUE`, add seasonal dummies to the data.
#'
#' @return A tsibble
#' @export
#'
#' @examples
#' df_macro <- get_data(quiet = TRUE)
#' df_macro_augmented <- ts_transform(df_macro, "ipca")
ts_transform <- function (
  data,
  y,
  stationary       = TRUE,
  lags             = 1:4,
  fill_down_na     = TRUE,
  seasonal_dummies = TRUE
  ) {

  my_df <- data

  if (!tsibble::is_tsibble(my_df)) {
    stop("data must be a tsibble.", call. = FALSE)
  }

  if (stationary) {

    # Aplicar testes de estacionariedade p/ identificar nº de diferenças
    # para séries serem estacionárias
    vars_ndiffs <- my_df %>%
      dplyr::select(-tsibble::index(my_df)) %>%
      report_ndiffs()

    # Diferenciar séries que são não estacionárias
    my_df <- my_df %>%
      dplyr::mutate(
        dplyr::across(
          .cols = vars_ndiffs$variable[vars_ndiffs$ndiffs > 0 & vars_ndiffs$variable != y],
          .fns  = ~tsibble::difference(
            x           = .x,
            differences = vars_ndiffs$ndiffs[vars_ndiffs$variable == dplyr::cur_column()]
          )
        )
      )
  }

  # Epandir base criando defasagens e dummies, preencher valores NA (de baixo),
  # filtrar amostra e selecionar variáveis de interesse
  if (fill_down_na) {

    data_lagged <- my_df %>%
      timetk::tk_augment_lags(.value = !tsibble::index(my_df), .lags = lags) %>%
      tidyr::fill(!dplyr::any_of(c(tsibble::index_var(my_df), y)), .direction = "down") %>%
      tidyr::drop_na() %>%
      dplyr::select(
        !dplyr::any_of(
          stringr::str_subset(
            string  = names(my_df),
            pattern = paste0(tsibble::index(my_df), "|", y, "|expectativa"),
            negate  = TRUE
            )
          )
        ) %>%
      tsibble::as_tsibble(index = tsibble::index_var(my_df))

    } else {

      data_lagged <- my_df %>%
        timetk::tk_augment_lags(.value = !tsibble::index(my_df), .lags = lags) %>%
        tidyr::drop_na() %>%
        dplyr::select(
          !dplyr::any_of(
            stringr::str_subset(
              string  = names(my_df),
              pattern = paste0(tsibble::index_var(my_df), "|", y, "|expectativa"),
              negate  = TRUE
              )
            )
          ) %>%
        tsibble::as_tsibble(index = tsibble::index_var(my_df))

      }

  if (seasonal_dummies) {

    yy_ts <- stats::ts(
      data = dplyr::pull(data_lagged, y),
      start = c(
        lubridate::year(min(dplyr::pull(data_lagged, tsibble::index(data_lagged)))),
        lubridate::month(min(dplyr::pull(data_lagged, tsibble::index(data_lagged))))
        ),
      frequency = tsibble::guess_frequency(dplyr::pull(data_lagged, tsibble::index(data_lagged)))
      )
    seasonal_dummies <- forecast::seasonaldummy(yy_ts)

    data_lagged <- dplyr::bind_cols(data_lagged, seasonal_dummies)

    }

  return(data_lagged)

  }
