#' Collect and pre-process macroeconomic data
#'
#' @description It collects the IPCA time series and other economic variables of the Brazilian economy for use in forecasting models.
#'
#' @param quiet Collect data silently? The default (`FALSE`) prints messages to the Console.
#'
#' @return A tibble
#' @export
#'
#' @examples
#' df_macro <- get_data(quiet = TRUE)
get_data <- function (quiet = FALSE) {

  # Ler metadados/códigos de coleta (CSV)
  metadata <- get0("metadata", envir = asNamespace("ipca"))

  # Data inicial da coleta de dados
  init_date <- lubridate::ymd("2002-12-01")


  # |-- BCB ----
  # Códigos de coleta de dados do SGS/BCB
  codes_bcb <- metadata %>%
    dplyr::filter(fonte == "BCB") %>%
    dplyr::summarise(
      purrr::set_names(x = as.numeric(codigo), nm = acronimo)
      ) %>%
    dplyr::pull()


  # Coleta de dados do SGS/BCB
  raw_bcb <- GetBCBData::gbcbd_get_series(
    id          = codes_bcb,
    first.date  = init_date,
    last.date   = lubridate::today(),
    use.memoise = FALSE,
    be.quiet    = quiet
    )


  # |-- IPEADATA ----
  # Códigos de coleta de dados do IPEADATA
  codes_ipeadata <- metadata %>%
    dplyr::filter(fonte == "IPEADATA") %>%
    dplyr::summarise(
      purrr::set_names(x = codigo, nm = acronimo)
      ) %>%
    dplyr::pull()


  # Coleta de dados do IPEADATA
  raw_ipeadata <- ipeadatar::ipeadata(code = codes_ipeadata, quiet = quiet)


  # |-- IBGE ----
  # Códigos de coleta de dados do IBGE
  codes_ibge <- metadata %>%
    dplyr::filter(fonte == "IBGE") %>%
    dplyr::summarise(
      purrr::set_names(x = codigo, nm = acronimo)
      ) %>%
    dplyr::pull()


  # Coleta de dados do IBGE
  if (quiet) {
    get_sidra_quiet <- purrr::quietly(sidrar::get_sidra)
    raw_ibge <- purrr::map(
      .x = codes_ibge,
      .f = ~get_sidra_quiet(api = .x)$result
      )
    } else raw_ibge <- purrr::map(
      .x = codes_ibge,
      .f = ~sidrar::get_sidra(api = .x)
      )


  # |-- Google Trends ----
  # Códigos de coleta de dados do Google Trends
  codes_google <- metadata %>%
    dplyr::filter(fonte == "Google Trends") %>%
    dplyr::summarise(
      purrr::set_names(x = codigo, nm = acronimo)
      ) %>%
    dplyr::pull()


  # Coleta de dados do Google Trends
  raw_google <- gtrendsR::gtrends(
    keyword      = codes_google,
    geo          = "BR",
    time         = "all",
    onlyInterest = TRUE
    )


  # |-- Focus/BCB ----
  # Códigos de coleta de dados do Focus/BCB
  codes_focus <- metadata %>%
    dplyr::filter(fonte == "Focus/BCB") %>%
    dplyr::summarise(
      purrr::set_names(x = codigo, nm = acronimo)
      ) %>%
    dplyr::pull()


  # Coleta de dados do Focus/BCB
  raw_focus <- rbcb::get_market_expectations(
    type       = "monthly",
    indic      = codes_focus,
    start_date = init_date,
    end_date   = lubridate::today()
    )



  # Tratamento de dados -----------------------------------------------------


  # Dados do SGS/BCB
  df_bcb <- raw_bcb %>%
    dplyr::select(
      "date"     = "ref.date",
      "variable" = "series.name",
      "value"
      ) %>%
    tidyr::pivot_wider(
      id_cols     = "date",
      names_from  = "variable",
      values_from = "value"
      )


  # Dados do IPEADATA
  df_ipeadata <- raw_ipeadata %>%
    dplyr::select("date", "variable" = "code", "value") %>%
    dplyr::left_join(
      y  = dplyr::select(metadata, "codigo", "acronimo"),
      by = c("variable" = "codigo")
      ) %>%
    tidyr::pivot_wider(
      id_cols     = "date",
      names_from  = "acronimo",
      values_from = "value"
      ) %>%
    # Obter média mensal (para séries com freq. diária)
    dplyr::group_by(date = tsibble::yearmonth(date)) %>%
    dplyr::summarise(
      dplyr::across(where(is.numeric), ~mean(.x, na.rm = TRUE))
      ) %>%
    dplyr::mutate(date = lubridate::as_date(date))


  # Dados do IBGE
  df_ibge <- raw_ibge %>%
    purrr::map_dfr(
      .f  = ~dplyr::select(.x, "date" = "M\u00eas (C\u00f3digo)", "value" = "Valor"),
      .id = "variable"
      ) %>%
    tidyr::pivot_wider(
      id_cols     = "date",
      names_from  = "variable",
      values_from = "value"
      ) %>%
    dplyr::mutate(date = lubridate::ym(date)) %>%
    dplyr::filter(date >= init_date) %>%
    dplyr::relocate("date", "ipca")


  # Dados do Google
  df_google <- raw_google %>%
    purrr::pluck("interest_over_time") %>%
    dplyr::mutate(
      date    = lubridate::as_date(date),
      keyword = stringr::str_replace(keyword, " ", "_")
      ) %>%
    tidyr::pivot_wider(
      id_cols     = "date",
      names_from  = "keyword",
      values_from = "hits"
      )


  # Dados do Focus/BCB
  df_focus <- raw_focus %>%
    dplyr::arrange(Data) %>%
    # Calcula horizonte em meses da expectativa
    dplyr::mutate(
      monthyear = tsibble::yearmonth(Data),
      horizon   = tsibble::yearmonth(DataReferencia, format = "%m/%Y") - monthyear
      ) %>%
    # Agrupar por mês de estatísticas registradas no Focus
    dplyr::group_by(monthyear) %>%
    # Filtra estatísticas tipo 0 (últimos 30 dias) no horizonte de 1 ano e
    # de datas próxima ao dia 15
    dplyr::filter(
      baseCalculo == 0,
      horizon > 0 & horizon < 13,
      lubridate::day(Data) < 16
      ) %>%
    dplyr::filter(
      abs(lubridate::day(Data) - 15) == min(abs(lubridate::day(Data) - 15))
      ) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(
      "date" = lubridate::floor_date(x = Data, unit = "month"),
      horizon,
      .keep_all = TRUE
      ) %>%
    tidyr::pivot_wider(
      id_cols      = date,
      names_from   = horizon,
      values_from  = Mediana,
      names_prefix = "expectativa_ipca_h_"
      )


  # Cruzamento de dados
  df_ipca <- purrr::reduce(
    .x = list(df_ibge, df_bcb, df_ipeadata, df_google, df_focus),
    .f = dplyr::left_join,
    by = "date"
    ) %>%
    dplyr::mutate(date = tsibble::yearmonth(date)) %>%
    tsibble::as_tsibble(index = "date")

  return(df_ipca)

}
