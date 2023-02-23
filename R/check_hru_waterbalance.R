#' Check average annual water balance simulations at the HRU level
#'
#' This function applies check rules which were proposed by White et al. (2014)
#' and which are implemented in SWATCheck for average annual water balance
#' output variables. Different to SWATCheck the checks here are performed for
#' all or only selected HRUs
#'
#'
#' @param sim_verify Simulation output of the function \code{run_swat_verification()}.
#'   To plot the heat units at least the output option \code{outputs = 'mgt'} must
#'   be set in  \code{run_swat_verification()}
#' @param check Character vector to indicate which water balance components should
#'   be checked. Options are 'precip' to check precipitation, 'et' to check
#'   evapotranspiration and components of ET, 'runoff' to check runoff components
#'   such as 'wyld' (water yield), surq (surface runoff), or 'perc' (percolation),
#'   'sw' to check the soil water content, and 'cn' to check curve number values.
#' @param ignore_lum Character vector to define land uses which should be ignored
#'   in the water balance checks.
#' @param add_values Boolean, to define wether to only show the checks (FALSE),
#'   or to also include the respective average annual water balance values.
#'
#' @return Returns a table of HRUs for which at least one of the selected checks
#'   identified a potential issue.
#'
#' @importFrom case_when left_join mutate rename select %>%
#' @importFrom stringr str_detect
#' @importFrom tibble tibble
#' @importFrom tidyselect ends_with everything starts_with
#'
#' @export
#'
check_hru_waterbalance <- function(sim_verify,
                                   check = c('precip', 'et', 'runoff', 'sw', 'cn'),
                                   ignore_lum = NULL,
                                   add_values = TRUE) {
  hru_wb  <- sim_verify$hru_wb_aa %>%
    select(unit, precip, surq_gen, wateryld, perc, et, eplant, esoil, cn, sw_final) %>%
    rename(id = unit)
  lum_mgt <- select(sim_verify$lum_mgt, id, lu_mgt)

  if (!is.null(ignore_lum)) {
    ignore_lum <- ignore_lum %>%
      paste(., collapse = '|') %>%
      str_detect(lum_mgt$lu_mgt, .)

    lum_mgt <- lum_mgt[!ignore_lum, ]
  }

  check_tbl <- left_join(lum_mgt, hru_wb, by = "id") %>%
    mutate(esr = 0.26*precip*(0.0129*cn - 0.2857),
           surq_wyld = surq_gen / wateryld,
           perc_wyld = perc / wateryld,
    ) %>%
    mutate(precip_check = case_when(precip < 65.0 ~ 'precip < 65mm',
                                    precip > 3400.0 ~ 'precip > 3400mm',
                                    TRUE ~ NA_character_) %>% as.factor(),
           et_check = case_when(et < 0.30 * precip ~ 'et < 30% precip',
                                et > 0.98 * precip ~ 'et < 98% precip',
                                TRUE ~ NA_character_) %>% as.factor(),
           eplant_check = ifelse(eplant < esoil,
                                 'eplant < esoil',
                                 NA_character_) %>% as.factor(),
           surq_wyld_check = case_when(surq_wyld < 0.31 ~ 'surq/wyld < 31%',
                                       surq_wyld > 0.80 ~ 'surq/wyld > 78%',
                                       TRUE ~ NA_character_) %>% as.factor(),
           perc_wyld_check = case_when(perc_wyld < 0.22 ~ 'perc/wyld < 22%',
                                       perc_wyld > 0.69 ~ 'perc/wyld > 69%',
                                       TRUE ~ NA_character_) %>% as.factor(),
           surq_check = case_when(surq_gen < 0.5 * esr ~ 'surq < 50% exp. surq',
                                  surq_gen > 1.5 * esr ~ 'surq > 150% exp. surq',
                                  TRUE ~ NA_character_) %>% as.factor(),
           sw_check = case_when(sw_final < 40.0 ~ '< 40mm',
                                sw_final > 600 ~ '> 600mm',
                                TRUE ~ NA_character_) %>% as.factor(),
           cn_check = case_when(cn < 35 ~ 'cn < 35',
                                cn > 95 ~ 'cn > 95',
                                TRUE ~ NA_character_) %>% as.factor()
    ) %>%
    select(-esr)

  if (!'precip' %in% check) {
    check_tbl <-  select(check_tbl, - starts_with('precip'))
  }
  if (!'et' %in% check) {
    check_tbl <- select(check_tbl, - starts_with(c('et', 'eplant', 'esoil')))
  }
  if (!'runoff' %in% check) {
    check_tbl <- select(check_tbl, - starts_with(c('surq', 'wateryld', 'perc')))
  }
  if (!'sw' %in% check) {
    check_tbl <- select(check_tbl, - starts_with('sw'))
  }
  if (!'cn' %in% check) {
    check_tbl <- select(check_tbl, - starts_with('cn'))
  }

  if (add_values) {
    check_tbl <- select(check_tbl, id, lu_mgt, ends_with('_check'), everything())
  } else {
    check_tbl <- select(check_tbl, id, lu_mgt, ends_with('_check'))
  }

  no_flag <- check_tbl %>%
    select(., ends_with('_check')) %>%
    apply(., 1, is.na) %>%
    apply(., 2, all)

  check_tbl <- check_tbl[!no_flag, ]

  return(check_tbl)
}
