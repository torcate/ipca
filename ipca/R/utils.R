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
