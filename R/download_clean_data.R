#' Download data from Eurostat and Google and clean them
#'
#' @description
#' `download_clean_data` returns two tibbles containing monthly time series from
#' Eurostat and the Google Mobility Index (GMI) for a chosen country. Data are first
#' downloaded from the official APIs and then merged and cleaned so to make all
#' the series stationary. The first tibble contains the high-frequency GMI, while the
#' second tibble reports the indexes aggregated to have monthly frequency.
#'
#'
#'
#' @param geo_code A string containing the country code, for example "IT" for Italy or "FR" for France
#'
#' @return A list of two tibbles, the first with the daily GMI,
#' the second with an aggregated monthly mobility index
#'
#' @export
#'
#' @examples
#' #country_code <- "IT"
#' #data <- download_clean_data(country_code)

download_clean_data <- function(geo_code){

  AIM <- BS_CSMCI_BAL <- BS_ESI_I <- BS_ICI_BAL <- EXP <- FC_IND_C0100 <- FC_IND_C0200 <- GID_CAL_C0100 <- GID_CAL_C0200 <- GID_CAL_O4652 <- GID_CAL_O4671 <- GID_CAL_O4680 <- IC_OBS <- IMP_O4100_TOT <- TI_EHG_MAP <- gas_supply <- gdp_SCA <- geo <- gross_capital_formation_SCA <- indic <- is_holiday <- month <- nrg_bal <- petroleum_supply <- siec <- solid_fossil_supply <- time <- unit <- values <- year <- NULL

  ## GDP ----
  cat("\n-------------Download GDP---------------------")
  namq_10_gdp <- eurostat::get_eurostat("namq_10_gdp",
                              filters = list(na_item = "B1GQ",
                                             s_adj = "SCA",
                                             unit = "CLV10_MEUR",
                                             geo = geo_code))

  eurostat_gdp <- namq_10_gdp %>%
    dplyr::mutate(time = lubridate::yq(time)) %>%
    dplyr::select(date = time, gdp_SCA = values)


  ## Consumption ----
  cat("\n-------------Download consumption---------------------")
  eurostat_consumption <- eurostat::get_eurostat("namq_10_gdp",
                                       filters = list(na_item = "P3",
                                                      s_adj = "SCA",
                                                      unit = "CLV10_MEUR",
                                                      geo = geo_code)) %>%
    dplyr::mutate(time = lubridate::yq(time)) %>%
    dplyr::select(date = time, consumption_SCA = values)

  ## Gross capital ----
  cat("\n-------------Download gross capital formation---------------------")
  eurostat_gross_capital_formation <- eurostat::get_eurostat("namq_10_gdp",
                                                   filters = list(na_item = "P5G",
                                                                  s_adj = "SCA",
                                                                  unit = "CLV10_MEUR",
                                                                  geo = geo_code)) %>%
    dplyr::mutate(time = lubridate::yq(time)) %>%
    dplyr::select(date = time, gross_capital_formation_SCA = values)


  ## Exports ----
  cat("\n-------------Download exports---------------------")
  eurostat_exports <- eurostat::get_eurostat("namq_10_gdp",
                                   filters = list(na_item = "P6",
                                                  s_adj = "SCA",
                                                  unit = "CLV10_MEUR",
                                                  geo = geo_code)) %>%
    dplyr::mutate(time = lubridate::yq(time)) %>%
    dplyr::select(date = time, exports_SCA = values)

  ## Imports ----
  cat("\n-------------Download imports---------------------")
  eurostat_imports <- eurostat::get_eurostat("namq_10_gdp",
                                   filters = list(na_item = "P7",
                                                  s_adj = "SCA",
                                                  unit = "CLV10_MEUR",
                                                  geo = geo_code)) %>%
    dplyr::mutate(time = lubridate::yq(time)) %>%
    dplyr::select(date = time, imports_SCA = values)


  ## Indpro ----
  cat("\n-------------Download industrial production---------------------")
  eurostat_indpro <- eurostat::get_eurostat("sts_inpr_m",
                                  filters = list(geo = geo_code,
                                                 s_adj = "SCA",
                                                 unit = "I15",
                                                 nace_r2 = "C")) %>%
    dplyr::mutate(time = lubridate::ym(time)) %>%
    dplyr::select(date = time, indpro_SCA = values)

  ## Inflation ----
  cat("\n-------------Download HICP---------------------")
  eurostat_hicp <- eurostat::get_eurostat("prc_hicp_midx",
                                filters = list(geo = geo_code,
                                               unit = "I15",
                                               coicop = "CP00")) %>%
    dplyr::mutate(time = lubridate::ym(time)) %>%
    dplyr::select(date = time, hicp = values)

  eurostat_import_price <- eurostat::get_eurostat("sts_inpi_m",
                                        filters = list(geo = geo_code,
                                                       unit = "I15",
                                                       indic_bt = "IMPR",
                                                       cpa2_1 = "CPA_B-D")) %>%
                                                       # nace_r2 = "B-D")) %>%
    dplyr::mutate(time = lubridate::ym(time)) %>%
    dplyr::select(date = time, import_price_industry = values)


  ## EMU ----
  cat("\n-------------Download EMU---------------------")
  eurostat_emu <- eurostat::get_eurostat("irt_lt_mcby_m",
                               filters = list(geo = geo_code)) %>%
    dplyr::mutate(time = lubridate::ym(time)) %>%
    dplyr::select(date = time, emu = values)

  # eurostat_emu_daily <- eurostat::get_eurostat("irt_lt_mcby_d",
  #                                    filters = list(geo = geo_code)) %>%
  #   dplyr::select(date = time, emu = values)


  ## Fossils
  cat("\n-------------Download Supply solid fossil---------------------")
  nrg_cb_sffm <- eurostat::get_eurostat("nrg_cb_sffm",
                              filters = list(geo = geo_code))#)#,
  # nrg_bal = c("GID_CAL", "FC_IND"),
  # siec = c("C0100", "C0200")))
  eurostat_supply_solid_fossil<- nrg_cb_sffm %>%
    dplyr::mutate(time = lubridate::ym(time)) %>%
    dplyr::select(date = time, solid_fossil_supply = values, siec, nrg_bal) %>%
    tidyr::pivot_wider(names_from = nrg_bal, values_from = solid_fossil_supply) %>%
    tidyr::pivot_wider(names_from = siec, values_from = EXP:TI_EHG_MAP) %>%
    # dplyr::select(date, GID_CAL_C0100, FC_IND_C0100, GID_CAL_C0200, FC_IND_C0200) %>%
    dplyr::select(date, GID_CAL_C0100, FC_IND, GID_CAL_C0200) %>%
    dplyr::select(where(function(x){mean(is.na(x), na.rm = T)<0.1})) %>%
    dplyr::select(where(function(x){mean(x==0, na.rm = T) != 1}))


  ## Oil supply ----
  cat("\n-------------Download Oil supply---------------------")
  nrg_cb_oilm <- eurostat::get_eurostat("nrg_cb_oilm",
                                        filters = list(geo=geo_code))

  eurostat_supply_oil <- nrg_cb_oilm %>%
    dplyr::mutate(time = lubridate::ym(time)) %>%
    dplyr::select(date = time, petroleum_supply = values, siec, nrg_bal) %>%
    tidyr::pivot_wider(names_from = nrg_bal, values_from = petroleum_supply) %>%
    tidyr::pivot_wider(names_from = siec, values_from = -c(date, siec)) %>%
    dplyr::select(where(function(x){mean(is.na(x), na.rm = T)<0.1})) %>%
    dplyr::select(date, IMP_O4100_TOT, GID_CAL_O4652, GID_CAL_O4671, GID_CAL_O4680) %>%
    dplyr::arrange(date)


  ## Gas supply ----
  cat("\n-------------Download Gas supply---------------------")
  nrg_cb_gasm <- eurostat::get_eurostat("nrg_cb_gasm",
                              filters = list(geo = geo_code))
  eurostat_supply_gas <- nrg_cb_gasm %>%
    dplyr::mutate(time = lubridate::ym(time)) %>%
    dplyr::filter(unit == "MIO_M3") %>%  # milion cubic meters
    dplyr::select(date = time, gas_supply = values, nrg_bal) %>%
    tidyr::pivot_wider(names_from = nrg_bal, values_from = gas_supply) %>%
    dplyr::filter(dplyr::if_any(-date, ~ !is.na(.))) %>%
    dplyr::select(where(function(x){mean(is.na(x), na.rm = T)<0.1})) %>%
    dplyr::select(date, IC_OBS_G3000 = IC_OBS)


  ## Electricity ----
  cat("\n-------------Download electricity---------------------")
  nrg_cb_em <- eurostat::get_eurostat("nrg_cb_em",
                            filters = list(geo = geo_code))
  eurostat_supply_electricity<- nrg_cb_em %>%
    dplyr::mutate(time = lubridate::ym(time)) %>%
    dplyr::filter(unit == "GWH") %>%  # milion cubic meters
    dplyr::select(date = time, gas_supply = values, nrg_bal) %>%
    tidyr::pivot_wider(names_from = nrg_bal, values_from = gas_supply) %>%
    dplyr::filter(dplyr::if_any(-date, ~ !is.na(.))) %>%
    dplyr::select(where(function(x){mean(is.na(x), na.rm = T)<0.1})) %>%
    dplyr::select(date, AIM_E7000 = AIM)


  ## Confidence ----
  cat("\n-------------Download confidence (2 indicators)---------------------")
  ei_bssi_m_r2 <- eurostat::get_eurostat("ei_bssi_m_r2",
                               filters = list(geo = geo_code,
                                              s_adj = "SA"))
  eurostat_confidence <- ei_bssi_m_r2 %>%
    dplyr::mutate(time = lubridate::ym(time)) %>%
    dplyr::select(date = time, values, indic) %>%
    tidyr::pivot_wider(names_from = "indic", values_from = "values") %>%
    dplyr::rename_with(~stringr::str_replace_all(., "-", "_")) %>%
    dplyr::select(date, BS_ESI_I, BS_ICI_BAL, BS_CSMCI_BAL)

  ei_bsrt_m_r2 <- eurostat::get_eurostat("ei_bsrt_m_r2",
                               filters = list(geo = geo_code,
                                              s_adj = "SA"))

  eurostat_confidence <- ei_bsrt_m_r2 %>%
    dplyr::mutate(time = lubridate::ym(time)) %>%
    dplyr::select(date = time, values, indic) %>%
    tidyr::pivot_wider(names_from = "indic", values_from = "values") %>%
    dplyr::rename_with(~stringr::str_replace_all(., "-", "_")) %>%
    dplyr::right_join(eurostat_confidence)

  # ei_bsci_m_r2 <- eurostat::get_eurostat("ei_bsci_m_r2")
  # eurostat_confidence <- ei_bsci_m_r2 %>%
  #   dplyr::select(date = time, eurozone_BCI = values) %>%
  #   dplyr::right_join(eurostat_confidence)


  ## Balance of payment
  cat("\n-------------Download BoP---------------------")
  ei_bpm6fa_m <- eurostat::get_eurostat("ei_bpm6fa_m",
                              filters = list(geo = geo_code,
                                             s_adj = "NSA",
                                             bop_item = "FA",
                                             stk_flow = "NET",
                                             partner = "WRL_REST"
                              ))

  eurostat_financial_account <- ei_bpm6fa_m %>%
    dplyr::mutate(time = lubridate::ym(time)) %>%
    dplyr::select(date = time, financial_account_net = values)

  ei_bpm6ca_m <- eurostat::get_eurostat("ei_bpm6ca_m",
                              filters = list(geo = geo_code,
                                             s_adj = "NSA",
                                             bop_item = "CA",
                                             stk_flow = "BAL",
                                             partner = "WRL_REST"
                              ))

  eurostat_current_account <- ei_bpm6ca_m %>%
    dplyr::mutate(time = lubridate::ym(time)) %>%
    dplyr::select(date = time, current_account_balance = values)


  ## Turnover ----
  cat("\n-------------Download Turnovers---------------------")
  sts_intv_m <- eurostat::get_eurostat("sts_intv_m",
                             filters = list(geo = geo_code,
                                            s_adj = "SCA",
                                            unit = "I15",
                                            nace_r2 = "B_C"
                             ))

  eurostat_turnover_industry <- sts_intv_m %>%
    dplyr::mutate(time = lubridate::ym(time)) %>%
    dplyr::select(date = time, turnover_industry = values)

  # sts_setu_m <- eurostat::get_eurostat("sts_setu_m",
  #                            filters = list(geo = geo_code,
  #                                           s_adj = "SCA",
  #                                           unit = "I15",
  #                                           nace_r2 = "H-N_X_K",
  #                                           indic_bt = "TOVV"
  #                            ))
  #
  # eurostat_turnover_services <- sts_setu_m %>%
  #   dplyr::select(date = time, turnover_services = values)


  ## Unemployment ----
  cat("\n-------------Download Unemployment---------------------")
  une_rt_m <- eurostat::get_eurostat("une_rt_m",
                           filters = list(geo = geo_code,
                                          s_adj = "SA",
                                          sex = "T",
                                          unit = "PC_ACT",
                                          age = "TOTAL"
                           ))

  eurostat_unemployment <- une_rt_m %>%
    dplyr::mutate(time = lubridate::ym(time)) %>%
    dplyr::select(date = time, unemployment = values)


  ## Tourism ----
  cat("\n-------------Download Tourism---------------------")
  tour_occ_nim <- eurostat::get_eurostat("tour_occ_nim",
                               filters = list(geo = geo_code,
                                              c_resid = "TOTAL",
                                              nace_r2 = "I551-I553",
                                              unit = "NR"
                               ))
  eurostat_tourism <- tour_occ_nim %>%
    dplyr::mutate(time = lubridate::ym(time)) %>%
    dplyr::select(date = time, tourism_nights = values)



  cat("\n-------------Download Google Mobility---------------------")
  google_link <- paste0("https://storage.googleapis.com/covid19-open-data/v3/location/", geo_code, ".csv")
  google_covid_opendata <- readr::read_csv(google_link)

  google_mobility <- google_covid_opendata %>%
    dplyr::select(date, tidyselect::starts_with("mobility"))

  google_mobility_ca <- google_mobility %>%
    dplyr::mutate(is_holiday = timeDate::isHoliday(timeDate::as.timeDate(date))*1) %>%
    dplyr::mutate(dplyr::across(tidyselect::starts_with("mobility_"), function(x){ifelse(is_holiday == 1, NA, x)})) %>%
    tidyr::fill(tidyselect::starts_with("mobility_"), .direction = "downup") # impute holidays with previous value


  google_mobility_monthly <-  google_mobility_ca %>%
    dplyr::mutate(year = lubridate::year(date), month = lubridate::month(date)) %>%
    dplyr::relocate(year, month) %>%
    dplyr::group_by(year, month) %>%
    dplyr::summarise(dplyr::across(tidyselect::starts_with("mobility"), mean)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(date = lubridate::ym(paste(year, month, sep = "-"))) %>%
    dplyr::relocate(date) %>%
    dplyr::select(date, tidyselect::starts_with("mobility"))


  google_mobility_wide <- google_mobility_ca %>%
    dplyr::mutate(day = lubridate::day(date)) %>%
    dplyr::mutate(dplyr::across(date, function(x){lubridate::day(x)<-1; x})) %>%
    dplyr::group_by(date) %>%
    tidyr::pivot_wider(id_cols = 1, names_from = "day", values_from = tidyselect::starts_with("mobility"), names_prefix = "gmi_d")

  # google_mobility_scaled <- cbind(google_mobility_wide[,1], scale(google_mobility_wide[,-1])) %>% tibble::as_tibble()

  ## data_gmi_stationary_scaled ----
  google_mobility_stationary <- cbind(google_mobility_wide[-1,1],
                                      lapply(google_mobility_wide[,-1], diff)) %>%
    tibble::as_tibble() %>%
    dplyr::select(date,
                  tidyselect::starts_with("mobility_retail_and_recreation"),
                  tidyselect::starts_with("mobility_transit_stations"),
                  tidyselect::starts_with("mobility_workplaces")) %>%
    dplyr::select(-tidyselect::ends_with("gmi_d31"))

  google_mobility_monthly_stationary <- cbind(google_mobility_monthly[-1,1],
                                              lapply(google_mobility_monthly[,-1], diff)) %>%
    tibble::as_tibble() %>%
    dplyr::select(date,
                  tidyselect::starts_with("mobility_retail_and_recreation"),
                  tidyselect::starts_with("mobility_transit_stations"),
                  tidyselect::starts_with("mobility_workplaces"))


  # Merging ----
  cat("\n------------------------------------------------")
  cat("\n------------------------------------------------")
  cat("\n-------------Merge data---------------------")
  dataset <- eurostat_indpro %>%
    dplyr::left_join(eurostat_gdp) %>%
    dplyr::left_join(eurostat_consumption) %>%
    dplyr::left_join(eurostat_exports) %>%
    dplyr::left_join(eurostat_imports) %>%
    dplyr::left_join(eurostat_gross_capital_formation) %>%
    dplyr::left_join(eurostat_confidence) %>%
    dplyr::left_join(eurostat_emu) %>%
    dplyr::left_join(eurostat_hicp) %>%
    dplyr::left_join(eurostat_supply_electricity) %>%
    dplyr::left_join(eurostat_supply_gas) %>%
    dplyr::left_join(eurostat_supply_oil) %>%
    # dplyr::left_join(eurostat_supply_solid_fossil) %>%
    dplyr::left_join(eurostat_financial_account) %>%
    dplyr::left_join(eurostat_current_account) %>%
    dplyr::left_join(eurostat_turnover_industry) %>%
    dplyr::left_join(eurostat_unemployment) %>%
    dplyr::left_join(eurostat_import_price) %>%
    # dplyr::left_join(eurostat_turnover_services) %>%
    dplyr::left_join(eurostat_tourism)


  cat("\n-------------Clean data---------------------")
  dataset1 <- dataset %>%
    dplyr::filter(date >= lubridate::ymd("1990-01-01"))

  # Transformation
  na_idx_gdp <- is.na(dataset1$gdp_SCA)    # consider the QoQ GDP growth rate
  na_idx_other_q <- is.na(dataset1$consumption_SCA)
  dataset2 <- dataset1 %>%
    tidyr::fill(gdp_SCA:gross_capital_formation_SCA, .direction = "down")

  ## First difference
  dataset2 <- lapply(dataset2[,-1], diff) %>%
    tibble::as_tibble() %>%
    dplyr::bind_cols(date = dataset1$date[2:nrow(dataset1)]) %>%
    dplyr::relocate(date)
  dataset2$gdp_SCA[na_idx_gdp[2:nrow(dataset1)]] <- NA
  dataset2$consumption_SCA[na_idx_other_q[2:nrow(dataset1)]] <- NA
  dataset2$exports_SCA[na_idx_other_q[2:nrow(dataset1)]] <- NA
  dataset2$imports_SCA[na_idx_other_q[2:nrow(dataset1)]] <- NA
  dataset2$gross_capital_formation_SCA[na_idx_other_q[2:nrow(dataset1)]] <- NA

  dataset3 <- dataset2 %>%
    tibble::as_tibble() %>%
    dplyr::left_join(google_mobility_stationary)

  dataset4 <- dataset2 %>%
    tibble::as_tibble() %>%
    dplyr::left_join(google_mobility_monthly_stationary)

  return(list(data_high_freq = dataset3,
              data_low_freq = dataset4))

}
