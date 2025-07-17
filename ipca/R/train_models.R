#' Train models: CSR, LASSO, Bagging, Ensemble, RW and Benchmark
#'
#' This function implements the cross-validation method with the models mentioned above. The Ensemble model is a simple average of the CSR and Bagging model predictions. The Benchmark model comes from columns in `data`, used to calculate the accuracy (RMSE) in relation to the observed `y`. Usually, these columns are external predictions (Focus) used in the model and as a benchmark.
#'
#' @param data A tsibble object
#' @param y Column name of the variable of interest
#' @param init_window Number of initial observations to be used in the first cross-validation subsample
#' @param horizon Forecast horizon
#' @param K Number of variables to be selected after the pre-testing. If K=ncol(x) the pre-testing is redundant (see `HDeconometrics::csr`).
#' @param k Number of variables in each subset. Must be smaller than K (see `HDeconometrics::csr`).
#' @param R Number of bootstrap replucations (see `HDeconometrics::bagging`).
#' @param pre_testing The type of pre-testing (see `HDeconometrics::bagging`).
#' @param quiet The default (`FALSE`) prints log messages to the Console.
#' @param benchmark_col_regex Regex expression for internal selection of columns to take predictions from a "benchmark model". The number of columns must be at least 1 and at most equal to `horizon`, corresponding to the forecast horizons.
#' @param benchmark_name Name to be given to the benchmark model.
#' @param ... Arguments passed to the HDeconometrics package
#'
#' @return Tibble
#' @export
#' @importFrom rlang :=
#' @importFrom stats predict
#'
#' @examples
#' df_macro <- get_data(quiet = TRUE)
#' df_macro_augmented <- ts_transform(df_macro, "ipca")
#' seasonal_dummies <- names(df_macro_augmented[, names(df_macro_augmented) %in% month.abb])
#' acc <- train_models(df_macro_augmented, "ipca", fixed.controls = seasonal_dummies)
train_models <- function(
  data,
  y,
  init_window         = 150,
  horizon             = 12,
  K                   = 20,
  k                   = 15,
  R                   = 500,
  pre_testing         = "group-joint",
  quiet               = FALSE,
  benchmark_col_regex = "expectativa_ipca_h_\\d{1,2}$",
  benchmark_name      = "Focus",
  ...
  ) {

  my_df <- data
  my_df_tbl <- dplyr::as_tibble(my_df)

  if (!tsibble::is_tsibble(my_df)) {
    stop("data must be a tsibble.", call. = FALSE)
  } else date_col <- tsibble::index_var(my_df)

  rlang::check_installed("glmnet", reason = "to use `train_models()`")

  # |-- Modelo Lasso ----

  # Criar esquema de validação cruzada
  cv_config <- caret::trainControl(
    method          = "timeslice",
    initialWindow   = init_window,
    horizon         = horizon,
    fixedWindow     = FALSE,
    verboseIter     = !quiet,
    savePredictions = "all"
    )


  # Treino do modelo
  fit_lasso <- caret::train(
    form       = stats::as.formula(paste0(y, " ~ .")),
    data       = my_df_tbl[-1],
    method     = "glmnet",
    trControl  = cv_config,
    metric     = "RMSE"
    )


  # Acurácia por horizonte preditivo
  acc_lasso <- fit_lasso %>%
    purrr::pluck("pred") %>%
    dplyr::filter(
      alpha == fit_lasso$bestTune$alpha,
      lambda == fit_lasso$bestTune$lambda
      ) %>%
    dplyr::group_by(Resample) %>%
    dplyr::mutate(h = dplyr::row_number(), model = "lasso") %>%
    dplyr::group_by(h, model) %>%
    dplyr::summarise(
      rmse    = sqrt(mean((obs - pred)^2, na.rm = TRUE)),
      .groups = "drop"
      )



  # |-- Modelo CSR ----

  # Aplica função criada para reportar RMSE por horizonte preditivo e pontos de previsão
  acc_csr <- get_cv_rmse_hdecon(
    model       = "csr",
    data        = my_df,
    y           = y,
    init_window = init_window,
    horizon     = horizon,
    K           = K,
    k           = k,
    quiet       = quiet,
    ...
    )



  # |-- Modelo Bagging ----

  # Aplica função criada para reportar RMSE por horizonte preditivo e pontos de previsão
  acc_bagging <- get_cv_rmse_hdecon(
    model       = "bagging",
    data        = my_df,
    y           = y,
    init_window = init_window,
    horizon     = horizon,
    R           = R,
    pre.testing = pre_testing,
    quiet       = quiet,
    ...
    )



  # |-- Modelo Ensemble e RW ----

  # Tratar dados para obter previsões por amostra de validação cruzada dos 3 modelos
  df_ensemble <- fit_lasso %>%
    purrr::pluck("pred") %>%
    dplyr::filter(
      alpha == fit_lasso$bestTune$alpha,
      lambda == fit_lasso$bestTune$lambda
      ) %>%
    dplyr::left_join(
      y = my_df_tbl %>%
        dplyr::select(dplyr::all_of(date_col)) %>%
        dplyr::mutate(index = dplyr::row_number(), model = "lasso"),
      by = c("rowIndex" = "index")
      ) %>%
    dplyr::select(
      dplyr::all_of(
        c(
          ".id" = "Resample",
          date_col,
          "fcst" = "pred",
          "model"
          )
        )
      ) %>%
    dplyr::group_by(.id) %>%
    dplyr::mutate(.id = as.character(dplyr::cur_group_id())) %>%
    dplyr::ungroup() %>%
    dplyr::bind_rows(acc_csr$forecasts, acc_bagging$forecasts) %>%
    tidyr::pivot_wider(
      id_cols     = c(".id", date_col),
      names_from  = "model",
      values_from = "fcst"
      )


  # Calcular acurácia do modelo Ensemble (previsão média dos modelos CSR e Bagging) e RW
  acc_ensemble_rw <- df_ensemble %>%
    dplyr::left_join(
      y  = my_df_tbl %>%
        dplyr::select(dplyr::all_of(c(date_col, y))) %>%
        dplyr::mutate(y_lag1 = dplyr::lag(.data[[y]], n = 1)),
      by = "date"
      ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      ensemble = mean(dplyr::c_across(c("csr", "bagging")), na.rm = TRUE)
      ) %>%
    dplyr::group_by(.id) %>%
    dplyr::mutate(h = dplyr::row_number()) %>%
    dplyr::group_by(h) %>%
    dplyr::summarise(
      ensemble = sqrt(mean((!!rlang::sym(y) - ensemble)^2, na.rm = TRUE)),
      rw       = sqrt(mean((!!rlang::sym(y) - y_lag1)^2, na.rm = TRUE)),
      .groups  = "drop"
      ) %>%
    tidyr::pivot_longer(cols = -"h", names_to = "model", values_to = "rmse")



  # |-- Acurácia do Benchmark ----

  # Calcular acurácia do Benchmark por horizonte preditivo
  acc_benchmark <- my_df_tbl %>%
    dplyr::select(
      dplyr::all_of(c(date_col, y)),
      dplyr::matches(benchmark_col_regex)
      ) %>%
    tidyr::pivot_longer(
      cols      = -c(date_col, y),
      names_to  = "h",
      values_to = benchmark_name
      ) %>%
    dplyr::mutate(
      h     = as.integer(stringr::str_remove(h, "\\D+")),
      model = benchmark_name
      ) %>%
    dplyr::left_join(
      y = df_ensemble %>%
        dplyr::group_by(.id) %>%
        dplyr::mutate(h = dplyr::row_number()) %>%
        dplyr::select(-c("lasso", "csr", "bagging")),
      by = c(date_col, "h")
      ) %>%
    tidyr::drop_na() %>%
    dplyr::group_by(h, model) %>%
    dplyr::summarise(
      rmse = sqrt(mean((!!rlang::sym(y) - !!rlang::sym(benchmark_name))^2, na.rm = TRUE)),
      .groups  = "drop"
      )


  # Juntar dados de acurácia de todos os modelos
  acc_rmse <- dplyr::bind_rows(
    acc_lasso,
    acc_csr$rmse,
    acc_bagging$rmse,
    acc_ensemble_rw,
    acc_benchmark
    ) %>%
    dplyr::mutate(
      model = dplyr::recode(
        model,
        "lasso"    = "LASSO",
        "csr"      = "Complete Subset Regression",
        "bagging"  = "Bagging",
        "ensemble" = "Ensemble",
        "rw"       = "Random Walk"
        )
      ) %>%
    dplyr::arrange(h, rmse, model) %>%
    dplyr::select("horizon" = "h", "model", "rmse")

  return(acc_rmse)

}



#' CSR and Bagging: estimates model with cross-validation and reports accuracy by forecast horizon
#'
#' @param model Model to be estimated, possible values are `csr` or `bagging`, see HDeconometrics functions
#' @param data A tsibble
#' @param y Column name of the variable of interest (used to report accuracy)
#' @param init_window Number of initial observations to be used in the first cross-validation subsample
#' @param horizon Forecast horizon
#' @param quiet The default (`FALSE`) prints log messages to the Console.
#' @param ... Additional arguments to `HDeconometrics::csr` or `HDeconometrics::bagging`
#'
#' @return List with 2 tibbles: RMSE per forecast horizon and point forecast.
get_cv_rmse_hdecon <- function (
  model,
  data,
  y,
  init_window = 150,
  horizon     = 12,
  quiet       = FALSE,
  ...
  ) {

  my_df <- data

  if (!tsibble::is_tsibble(my_df)) {
    stop("data must be a tsibble.", call. = FALSE)
  } else date_col <- tsibble::index_var(my_df)

  n <- nrow(my_df)
  idx <- seq(init_window, (n - horizon), by = 1)
  add <- rep(1, length(idx))

  cv_train_index <- purrr::map2(
    .x = add,
    .y = idx,
    .f = seq
    )

  n_fcst <- length(cv_train_index)

  point_fcst <- list()

  for (i in seq_len(n_fcst)) {

    if (!quiet) { cat(paste0("\nIteration: ", i, "/", n_fcst)) }

    curr_index <- cv_train_index[[i]]
    data_train <- data[curr_index, ]

    yy_in <- dplyr::pull(data_train, dplyr::all_of(y))

    xx_in <- data_train %>%
      dplyr::as_tibble() %>%
      dplyr::select(!dplyr::any_of(c(date_col, y))) %>%
      as.matrix()
    xx_out <- as.matrix(data[-curr_index, colnames(xx_in)][1:horizon, ])

    if (model == "csr") {

      fit_csr <- HDeconometrics::csr(
        x = xx_in,
        y = yy_in,
        ...
      )


      fcsts <- predict(object = fit_csr, newdata = xx_out)

    } else if (model == "bagging") {

      fit_bagging <- HDeconometrics::bagging(
        x = xx_in,
        y = yy_in,
        ...
      )

      fcsts <- predict(object = fit_bagging, newdata = xx_out)

    } else stop("model must be 'csr' or 'bagging'.")

    point_fcst[[i]] <- dplyr::tibble(
      {{ date_col }} := seq.Date(
        from       = lubridate::as_date(max(dplyr::pull(data_train, date_col))) + months(1),
        by         = "month",
        length.out = length(fcsts)
        ),
      fcst = fcsts
      )

  }

  fc <- point_fcst %>%
    dplyr::bind_rows(.id = ".id") %>%
    dplyr::mutate(model = dplyr::if_else(model == "csr", "csr", "bagging"))

  rmse_tbl <- dplyr::left_join(
    x  = fc,
    y  = dplyr::select(data, dplyr::all_of(c(date_col, y))),
    by = {{ date_col }}
    ) %>%
    dplyr::group_by(.id) %>%
    dplyr::mutate(h = dplyr::row_number()) %>%
    dplyr::group_by(h, model) %>%
    dplyr::summarise(
      rmse = sqrt(mean((!!rlang::sym(y) - fcst)^2, na.rm = TRUE)),
      .groups = "drop"
      )

  return(
    list("rmse" = rmse_tbl, "forecasts" = fc)
    )

}
