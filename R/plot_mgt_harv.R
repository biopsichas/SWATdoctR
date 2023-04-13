#' Boxplot for relevant variables at harvest-kill
#'
#' plot_variable_at_harvkill plots boxplots of one of the variables crop heat unit
#' fractions ('phu'), crop yields ('yield'), or plant biomass ('bioms') for crops
#' at harvest-kill of a crop separated for all identified crops.
#'
#' @param sim_verify Simulation output of the function \code{run_swat_verification()}
#' as a single object or as a list of objects (representing \code{run_swat_verification()}
#' function runs with different settings).
#'   To plot the heat units at least the output option \code{outputs = 'mgt'}
#'   must be set in \code{run_swat_verification()}.
#' @param variable Selected variable to be plotted. Must be one of: 'phu',
#'   'yield', 'bioms'
#' @param years Simulated years which are aggregated in the boxplot
#'
#' @return ggplot boxplot the selected variable at harvest-kill.
#'
#' @importFrom dplyr filter group_by mutate n rename select ungroup %>%
#' @importFrom ggplot2 aes ggplot geom_boxplot geom_hline labs theme_bw
#' @importFrom purrr set_names
#'
#' @export
#' @examples
#' \dontrun{
#' ##As single object
#' plot_variable_at_harvkill(sim_nostress, 'yield')
#' ##As a list
#' sim_list<- list(
#' no_stress = sim_nostress,
#' only_nutrient = sim_nutrientstress,
#' all_stress = sim_allstress)
#' plot_variable_at_harvkill(sim_list, 'yield')
#' }
#'
plot_variable_at_harvkill <- function(sim_verify, variable, years = 1900:2100) {
  ##Identifying if it is single object of a list and generating df for plotting
  is_single_sim <- "mgt_out" %in% names(sim_verify)
  is_list_sim <- "mgt_out" %in% names(sim_verify[[1]])
  if(is_single_sim){
    years <- years[years >= min(sim_verify$basin_wb_day$yr)]
    tbl_harv <- prepare_var_at_harvkill(sim_verify$mgt_out, years, variable)
  } else if (is_list_sim){
    years <- years[years >= min(sim_verify[[1]]$basin_wb_day$yr)]
    df <- NULL
    nn <- c()
    for(n in names(sim_verify)){
      mgt_out <- sim_verify[[n]][["mgt_out"]]
      tbl_harv <- prepare_var_at_harvkill(mgt_out, years, variable)
      tbl_harv$simulation <- n
      nn <- c(nn, n)
      if(is.null(df)){df <- tbl_harv} else {df <- bind_rows(df, tbl_harv)}
    }
    tbl_harv <- df %>%
      mutate(simulation = factor(simulation, levels = nn))
  } else {
    stop('Incorrect input for sim_verify')
  }

  y_lbl <- case_when(variable == 'phu'    ~ 'Crop HU fraction before kill',
                     variable == 'yield'  ~ 'Crop yield per growing period',
                     variable == 'bioms'  ~ 'Biomass at harvest/kill',
                     variable == 'stress' ~ 'Plant stress in growing period')

  gg <- ggplot(data = tbl_harv) +
    labs(x = 'Crops', y = y_lbl) +
    theme_bw()

  if (variable == 'stress') {
    gg <- gg +
      geom_boxplot(aes(x = crop, y = var, fill = stress),
                   color = "grey30", outlier.size=1, outlier.colour = "grey50") +
      labs(fill = 'Stress factor') +
      scale_fill_manual(values = c('deepskyblue4', 'lightcyan3', 'indianred3',
                                   'palegreen4', 'orange3')) +
      theme(legend.position = 'bottom')

    if (is_list_sim) {
      gg <- gg + facet_wrap(~simulation)
    }

  } else {
    if (is_single_sim) {
      gg <- gg +
        geom_boxplot(aes(x = crop, y = var),
                     color = "grey30", outlier.size=1, outlier.colour = "grey50")
    } else {
      gg <- gg + geom_boxplot(aes(x = crop, y = var, fill = simulation),
                              color = "grey30", outlier.size=1,
                              outlier.colour = "grey50") +
        scale_fill_manual(values = c('white', 'palegreen4', 'orange3',
                                     'lightcyan3', 'indianred3'))
    }
  }

  if(variable == 'phu') {
    gg <- gg + geom_hline(yintercept = 1, lty = 2)
  }

  return(gg)
}

#' Prepare dataframe for plot_variable_at_harvkill function
#'
#' @param mgt_out Data frame with management output data
#' @param years Integer vector of years for which values should be calculated
#' @param variable String to define which variable is selected for plotting
#'
#' @return data frame for plotting
#' @importFrom dplyr %>% filter group_by mutate ungroup
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' var_at_harv(sim_nostress$mgt_out, 1900:2100)
#' }

prepare_var_at_harvkill <- function(mgt_out, years, variable){
  if(variable == 'phu') {
    tbl_var <- prepare_phu(mgt_out, years)

    # y_lbl <-  'Crop HU fraction before kill'
  }else if(variable == 'yield') {
    tbl_var <- prepare_yield(mgt_out, years)

    # y_lbl <-  'Crop yield per growing period'
  } else if(variable == 'bioms') {
    tbl_var <- prepare_biomass(mgt_out, years)

    # y_lbl <-  'Biomass at harvest/kill'
  } else if(variable == 'stress') {
    tbl_var <- prepare_stress(mgt_out, years)

    # y_lbl <-  'Plant stress at harvest/kill'
  } else {
    stop("Variable must be one of: 'phu', 'yield', 'bioms', 'stress'")
  }

  return(tbl_var)
}

#' Prepare mgt_out data for plotting yields
#'
#' For static land uses (no plant and kill operations) annual yields are calculated.
#' For crops with defined growing periods the yield sums of that period is calculated.
#'
#' @param mgt_out Data frame with management output data
#' @param years Integer vector of years for which values should be calculated
#'
#' @return data frame with crop names and yields
#'
#' @importFrom dplyr bind_rows filter group_by mutate select summarise ungroup %>%
#' @importFrom purrr set_names
#'
#' @keywords internal
#'
prepare_yield <- function(mgt_out, years) {
  yield_tbl <- mgt_out %>%
    filter(operation %in% c('PLANT','HARVEST', 'KILL')) %>%
    select(hru, year, operation, op_typ, op_var) %>%
    group_by(hru, op_typ) %>%
    mutate(grw_group = ifelse(operation == 'KILL', 1, 0),
           grw_group = cumsum(grw_group)) %>%
    ungroup() %>%
    group_by(hru) %>%
    mutate(no_plntkill = max(grw_group) == 0)

  yield_mono <- yield_tbl %>%
    filter(no_plntkill) %>%
    filter(year %in% years) %>%
    group_by(hru, year, op_typ) %>%
    summarise(op_var = sum(op_var), .groups = 'drop') %>%
    select(-hru, -year)

  yield_rota <- yield_tbl %>%
    filter(!no_plntkill) %>%
    filter(operation == 'HARVEST') %>%
    group_by(hru, op_typ, grw_group) %>%
    summarise(op_var = sum(op_var), year = max(year), .groups = 'drop') %>%
    filter(year %in% years) %>%
    select(-hru, -grw_group, -year)

  yield <- bind_rows(yield_mono, yield_rota) %>%
    set_names(c('crop', 'var'))

  return(yield)
}

#' Prepare mgt_out data for plotting biomass
#'
#' Biomass is only extracted for harvest/kill operations.
#'
#' @param mgt_out Data frame with management output data
#' @param years Integer vector of years for which values should be calculated
#'
#' @return data frame with crop names and bio mass
#'
#' @importFrom dplyr filter group_by mutate n select summarise ungroup %>%
#' @importFrom purrr set_names
#'
#' @keywords internal
#'
prepare_biomass <- function(mgt_out, years) {
  bioms <- mgt_out %>%
    filter(year %in% years) %>%
    filter(operation %in% c('HARVEST', 'KILL')) %>%
    group_by(hru, year, mon, day) %>%
    mutate(n_op = n()) %>%
    ungroup()

  crop_rmv <- filter(bioms, n_op != 2) %>% .$op_typ %>% unique()

  if(length(crop_rmv) > 0) {
    message('Data for the following crops were removed before plotting:\n',
            paste(crop_rmv, collapse = ', '), '\n')
  }

  bioms <- bioms %>%
    filter(n_op == 2, operation == 'HARVEST') %>%
    select(op_typ, plant_bioms) %>%
    set_names(c('crop', 'var'))

  return(bioms)
}

#' Prepare mgt_out data for plotting PHU fractions
#'
#' PHU fractions are extracted for the last operation before a kill operation is set.
#'
#' @param mgt_out Data frame with management output data
#' @param years Integer vector of years for which values should be calculated
#'
#' @return data frame with crop names and phu fractions
#'
#' @importFrom dplyr arrange filter group_by lead mutate n select summarise ungroup %>%
#' @importFrom purrr set_names
#'
#' @keywords internal
#'
prepare_phu <- function(mgt_out, years) {
  phu_tbl <- mgt_out %>%
    mutate(date = 1e4*year + 100*mon + day) %>%
    filter(operation %in% c('PLANT','HARVEST', 'KILL')) %>%
    select(hru, date, year, operation, op_typ, phuplant) %>%
    group_by(hru, op_typ) %>%
    mutate(grw_group = ifelse(operation == 'PLANT', 1, 0),
           grw_group = cumsum(grw_group)) %>% #filter(hru == 4557) %>% View()
    ungroup() %>%
    filter(operation != 'PLANT') %>%
    group_by(hru, op_typ, grw_group) %>%
    mutate(n_op = n()) %>%
    ungroup()

  crop_only_kill  <- filter(phu_tbl, n_op == 1 & operation == 'KILL') %>% .$op_typ %>% unique()
  crop_multi_harv <- filter(phu_tbl, n_op > 2) %>% .$op_typ %>% unique()

  if(length(crop_only_kill) > 0) {
    message('For the following crops no harvest was found before the kill operation:\n',
            paste(crop_only_kill, collapse = ', '), '\n',
            'These data were removed before plotting.\n')
  }

  if(length(crop_only_kill) > 0) {
    message('For the following crops multiple harvests were detected before the kill operation:\n',
            paste(crop_multi_harv, collapse = ', '), '\n',
            'These data were removed before plotting.\n')
  }

  phu_tbl <- phu_tbl %>%
    filter(n_op == 2) %>%
    group_by(hru, year, op_typ) %>%
  # ungroup() %>%
    # filter(n_op > 1) %>%
    mutate(date_diff = diff(date)) %>%
    ungroup() %>%
    filter(year %in% years)

  if(any(phu_tbl$date_diff > 1)) {
    crop_diff <- phu_tbl %>%
      filter(operation == 'KILL') %>%
      filter(date_diff > 1) %>%
      .$op_typ %>%
      unique(.)

    message('For the following crops the difference between the last harvest and\n',
            'the final kill operation was more than 1 day:\n',
            paste(crop_diff, collapse = ', '), '\n',
            'The plotted PHU fractions may therefore not be correct.\n')
  }

  phu <- phu_tbl %>%
    filter(operation != 'KILL') %>%
    select(op_typ, phuplant) %>%
    set_names(c('crop', 'var'))

  return(phu)
}

#' Prepare mgt_out data for plotting stress factors
#'
#' For static land uses (no plant and kill operations) annual changes in stress
#' values are calculated (annual increase of a stress factor).
#' For crops with defined growing periods the stress values for the last harvest
#' operation is extracted.
#'
#' @param mgt_out Data frame with management output data
#' @param years Integer vector of years for which values should be calculated
#'
#' @return data frame with crop names, stress names and values
#'
#' @importFrom dplyr filter group_by mutate select summarise ungroup %>%
#' @importFrom purrr set_names
#' @importFrom tidyr pivot_longer
#'
#' @keywords internal
#'
prepare_stress <- function(mgt_out, years) {
  stress_tbl <- mgt_out %>%
    filter(operation %in% c('PLANT','HARVEST', 'KILL')) %>%
    select(hru, year, operation, op_typ, var4, var5, var3, var1, var2) %>%
    group_by(hru, op_typ) %>%
    mutate(grw_group = ifelse(operation == 'KILL', 1, 0),
           grw_group = cumsum(grw_group)) %>%
    ungroup() %>%
    group_by(hru) %>%
    mutate(no_plntkill = max(grw_group) == 0) %>%
    ungroup()

  stress_mono <- stress_tbl %>%
    filter(no_plntkill) %>%
    group_by(hru, year, op_typ) %>%
    summarise(var4 = max(var4), var5 = max(var5), var3 = max(var3),
              var2 = max(var2), var1 = max(var1), .groups = 'drop') %>%
    group_by(hru, op_typ) %>%
    mutate(var4 = diff1(var4), var5 = diff1(var5), var3 = diff1(var3),
           var2 = diff1(var2), var1 = diff1(var1)) %>%
    ungroup() %>%
    filter(year %in% years) %>%
    select(-hru, -year)

  stress_rota <- stress_tbl %>%
    filter(year %in% years) %>%
    filter(!no_plntkill) %>%
    filter(operation == 'HARVEST') %>%
    group_by(hru, op_typ, grw_group) %>%
    summarise(var4 = max(var4), var5 = max(var5), var3 = max(var3),
              var2 = max(var2), var1 = max(var1), .groups = 'drop') %>%
    ungroup(.) %>%
    select(-hru, -grw_group)

  stress <- bind_rows(stress_mono, stress_rota) %>%
    set_names(c('crop', 'water', 'aeration',
                'temperature', 'nitrogen', 'phosphorus')) %>%
    pivot_longer(., cols = - crop, names_to = 'stress', values_to = 'var') %>%
    mutate(., stress = factor(stress,
                              levels = c('water', 'aeration', 'temperature',
                                         'nitrogen', 'phosphorus')))

  return(stress)
}

#' Concatenate the first value of a vector and the differences of following values.
#'
#' @param x numeric vector
#'
#' @return numeric vector
#'
#' @keywords internal
#'
diff1 <- function(x) {
  c(x[1], diff(x))
}

#' Filter HRUs by their variable values at harvest/kill
#'
#' The function can be complementary to \code{plot_variable_at_harvkill()} plots.
#' If any issues are identified in the boxplots, the results can be filtered for
#' further analysis with \code{filter_hru_at_harvkill()}.
#'
#' @param sim_verify Simulation output of the function \code{run_swat_verification()}
#' as a single object or as a list of objects (representing \code{run_swat_verification()}
#' function runs with different settings).
#'   To plot the heat units at least the output option \code{outputs = 'mgt'}
#'   must be set in \code{run_swat_verification()}.
#' @param ... Boolean operations to be applied in the filter operation of the
#'   management simulation outputs. Possible variables are \code{crop, phu, plant_bioms,
#'   yield, water_stress, aero_stress, temp_stress, n_stress} and \code{p_stress}.
#'
#' @importFrom dplyr filter mutate select %>%
#' @importFrom lubridate ymd
#' @importFrom purrr set_names
#'
#' @export
#' @examples
#' \dontrun{
#' # Filter all HRUs with unusually high PHU values at harvest kill
#' filter_hru_at_harvkill(sim_verify, phu > 5)
#'
#' # Filter all HRUs with crop 'wbar' or 'wira' planted but no yield
#' filter_hru_at_harvkill(sim_verify, crop %in% c('wbar', 'wira'), yield == 0)
#' }
filter_hru_at_harvkill <- function(sim_verify, ...) {
  sim_verify$mgt_out %>%
    mutate(date = ymd(paste(year, mon, day, sep = '-'))) %>%
    filter(operation == 'HARVEST') %>%
    select(hru, year, date, op_typ, phuplant, plant_bioms, op_var, var4, var5, var3, var1, var2) %>%
    set_names(c('hru','year' , 'date', 'crop', 'phu', 'plant_bioms', 'yield', 'water_stress', 'aero_stress',
                'temp_stress', 'n_stress', 'p_stress')) %>%
    filter(., ...) %>%
    select(-year)
}
