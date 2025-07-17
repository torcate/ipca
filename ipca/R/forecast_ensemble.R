#' Generate an out-of-sample Ensemble forecast from the CSR and Bagging models
#'
#' @param data A tsibble object
#' @param y Column name of the variable of interest
#' @param horizon Forecast horizon
#' @param K Number of variables to be selected after the pre-testing. If K=ncol(x) the pre-testing is redundant (see `HDeconometrics::csr`).
#' @param k Number of variables in each subset. Must be smaller than K (see `HDeconometrics::csr`).
#' @param R Number of bootstrap replucations (see `HDeconometrics::bagging`).
#' @param pre_testing The type of pre-testing (see `HDeconometrics::bagging`).
#' @param seasonal_dummies If `TRUE`, seasonal dummies are created for model estimation and out-of-sample forecasting.
#' @param auto_scenario If `TRUE`, the `forecast::auto.arima` function is used to generate scenarios for the independent variables in `horizon` out-of-sample periods. If `FALSE`, the user must supply a `xreg_scenario` matrix with the scenarios, containing a number of lines equal to `horizon`.
#' @param xreg_scenario Scenario matrix for independent variables if `auto scenario` is `FALSE`.
#' @param ... Other arguments passed to `HDeconometrics::bagging`
#'
#' @return A tibble
#' @export
#'
#' @examples
#' df_macro <- ts_transform(get_data(), "ipca", seasonal_dummies = FALSE)
#' df_fcsts <- forecast_ensemble(df_macro, "ipca")
forecast_ensemble <- function (
  data,
  y,
  horizon = 12,
  K = 20,
  k = 15,
  R = 500,
  pre_testing = "group-joint",
  seasonal_dummies = TRUE,
  auto_scenario = TRUE,
  xreg_scenario,
  ...
  ) {

  my_df <- data
  my_df_tbl <- dplyr::as_tibble(my_df)

  if (!tsibble::is_tsibble(my_df)) {
    stop("data must be a tsibble.", call. = FALSE)
  } else date_col <- tsibble::index_var(my_df)

  if (seasonal_dummies) {

    yy_ts <- stats::ts(
      data = dplyr::pull(my_df_tbl, y),
      start = c(
        lubridate::year(min(dplyr::pull(my_df_tbl, date_col))),
        lubridate::month(min(dplyr::pull(my_df_tbl, date_col)))
        ),
      frequency = tsibble::guess_frequency(dplyr::pull(my_df, date_col))
      )
    seas_dummies <- forecast::seasonaldummy(yy_ts)
    seas_dummies_oos <- forecast::seasonaldummy(x = yy_ts, h = horizon)

    my_df_tbl <- dplyr::bind_cols(my_df_tbl, seas_dummies)

  }

  yy <- dplyr::pull(my_df_tbl, y)
  xx <- my_df_tbl %>%
    dplyr::select(!dplyr::any_of(c(date_col, y))) %>%
    as.matrix()


  if (seasonal_dummies) {

    fit_csr <- HDeconometrics::csr(
      x              = xx,
      y              = yy,
      K              = K,
      k              = k,
      fixed.controls = colnames(seas_dummies)
      )

    fit_bagging <- HDeconometrics::bagging(
      x              = xx,
      y              = yy,
      R              = R,
      pre.testing    = pre_testing,
      fixed.controls = colnames(seas_dummies),
      ...
      )

  } else {

    fit_csr <- HDeconometrics::csr(x = xx, y = yy, K = K, k = k)

    fit_bagging <- HDeconometrics::bagging(
      x           = xx,
      y           = yy,
      R           = R,
      pre.testing = pre_testing,
      ...
      )

  }


  if (auto_scenario & seasonal_dummies) {

    xreg_arima <- purrr::map_df(
      .x = dplyr::select(
        my_df_tbl,
        !dplyr::any_of(c(date_col, y, colnames(seas_dummies)))
        ),
      .f = ~{
        forecast::auto.arima(.x) %>%
          forecast::forecast(h = horizon) %>%
          magrittr::extract2("mean") %>%
          as.numeric()
        }
      )

    xx_oos <- as.matrix(dplyr::bind_cols(xreg_arima, seas_dummies_oos))

  } else if (auto_scenario & !seasonal_dummies) {

    xreg_arima <- purrr::map_df(
      .x = dplyr::select(
        my_df_tbl,
        !dplyr::any_of(c(date_col, y))
        ),
      .f = ~{
        forecast::auto.arima(.x) %>%
          forecast::forecast(h = horizon) %>%
          magrittr::extract2("mean") %>%
          as.numeric()
        }
      )

    xx_oos <- as.matrix(xreg_arima)

  } else if (!auto_scenario & seasonal_dummies) {

    xx_oos <- as.matrix(dplyr::bind_cols(xreg_scenario, seas_dummies_oos))

  } else if (!auto_scenario & !seasonal_dummies) {

    xx_oos <- xreg_scenario

    }


  fcst_csr <- predict(object = fit_csr, newdata = xx_oos)
  fcst_bagging <- predict(object = fit_bagging, newdata = xx_oos)
  fcst_ensemble <- dplyr::tibble(csr = fcst_csr, bagging = fcst_bagging) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      ensemble = mean(dplyr::c_across(c("csr", "bagging")), na.rm = TRUE)
      ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      date = seq.Date(
        from       = max(lubridate::as_date(dplyr::pull(my_df_tbl, date_col))) + months(1),
        by         = "month",
        length.out = length(fcst_csr)
        ) %>% tsibble::yearmonth(),
      .before = 1
      ) %>%
    tsibble::as_tsibble(index = "date")

  return(fcst_ensemble)

}
