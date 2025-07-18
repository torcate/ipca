#' Report number of differences to make time series stationary (vectorized)
#'
#' @param x List-like object with vectors of the series to be tested
#' @param test Type of unit root test to use, see forecast::ndiffs
#' @param term Specification of the deterministic component in the regression, see forecast::ndiffs
#' @param alpha Level of the test, possible values range from 0.01 to 0.1
#' @param na_rm Remove NAs from x?
#'
#' @return Tibble with variable name from x and the number of differences found
#' @export
#'
#' @examples
#' ts1 <- as.vector(arima.sim(list(order = c(1, 1, 0), ar = 0.7), n = 19))
#' ts2 <- rnorm(20)
#' report_ndiffs(data.frame(ts1 = ts1, ts2 = ts2))
report_ndiffs <- function (
    x,
    test  = c("kpss", "adf", "pp"),
    term  = c("level", "trend"),
    alpha = 0.05,
    na_rm = TRUE
  ) {

  # All possible tests and terms
  ndiffs_tests <- purrr::cross(list(test = test, type = term))
  ndiffs_tests <- purrr::set_names(
    x  = ndiffs_tests,
    nm = paste(
      purrr::map_chr(ndiffs_tests, 1),
      purrr::map_chr(ndiffs_tests, 2),
      sep = "_"
      )
    )

  # Nested for-loop
  purrr::map(
    .x = if (na_rm) {stats::na.omit(x)} else x,
    .f = ~purrr::map(
      .x = ndiffs_tests,
      .f = function (y) {
        forecast::ndiffs(
          x     = .x,
          alpha = alpha,
          test  = y[[1]],
          type  = y[[2]]
          )
        }
      )
    ) %>%
    purrr::map_df(dplyr::bind_rows, .id = "variable") %>%
    # Create column with most frequent value to differentiate
    dplyr::rowwise() %>%
    dplyr::mutate(
      ndiffs = dplyr::c_across(!dplyr::any_of("variable")) %>%
        table() %>%
        sort(decreasing = TRUE) %>%
        names() %>%
        purrr::chuck(1) %>%
        as.numeric()
    ) %>%
    dplyr::ungroup()

}



#' Format x-axislabels (dates) as year/month in two lines
#'
#' @param x Date vector, usually takes as input the output of `breaks` in `ggplot2::scale_x_date`
#'
#' @return Character vetor
#' @export
#'
#' @examples
#' dates <- seq.Date(Sys.Date(), Sys.Date() + 120, by = "month")
#' ym_label(dates)
ym_label <- function(x) {

  x <- lubridate::as_date(x)

  dplyr::if_else(
    is.na(dplyr::lag(x)) | tsibble::yearmonth(dplyr::lag(x)) != tsibble::yearmonth(x),
    paste(lubridate::month(x, label = TRUE), "\n", lubridate::year(x)),
    paste(lubridate::month(x, label = TRUE))
  )

}
