
#' Boxplot for relevant variables at harvest-kill
#'
#' plot_variable_at_harvkill plots boxplots of one of the variables crop heat unit
#' fractions ('phu'), crop yields ('yield'), or plant biomass ('bioms') for crops
#' at harvest-kill of a crop separated for all identified crops.
#'
#' @param sim_verify Simulation output of the function \code{run_swat_verification()}.
#'   To plot the heat units at least the output option \code{outputs = 'mgt'} must
#'   be set in  \code{run_swat_verification()}
#' @param variable Selected variable to be plotted. Must be one of: 'phu', 'yield', 'bioms'
#' @param years Simulated years which are aggregated in the boxplot
#'
#' @return ggplot boxplot the selected variable at harvest-kill.
#'
#' @importFrom dplyr filter group_by mutate n rename select ungroup %>%
#' @importFrom ggplot2 aes ggplot geom_boxplot geom_hline labs theme_bw
#' @importFrom purrr set_names
#'
#' @export
#'
plot_variable_at_harvkill <- function(sim_verify, variable, years = 1900:2100) {
  tbl_harv <- sim_verify$mgt_out %>%
    filter(year %in% years) %>%
    filter(operation %in% c('HARVEST', 'KILL')) %>%
    group_by(hru,year, mon, day) %>%
    mutate(n = n()) %>%
    ungroup() %>%
    filter(n == 2, operation == 'HARVEST')

  if(variable == 'phu') {
    tbl_harv <- tbl_harv %>%
      select(op_typ, phuplant) %>%
      set_names(c('crop', 'var'))

    y_lbl <-  'Crop HUs at harvest/kill'
  }else if(variable == 'yield') {
    tbl_harv <- tbl_harv %>%
      select(op_typ, op_var) %>%
      set_names(c('crop', 'var'))

    y_lbl <-  'Crop yield at harvest/kill'
  } else if(variable == 'bioms') {
    tbl_harv <- tbl_harv %>%
      select(op_typ, plant_bioms) %>%
      set_names(c('crop', 'var'))

    y_lbl <-  'Biomass at harvest/kill'
  } else if(variable == 'stress') {
    tbl_harv <- tbl_harv %>%
      select(op_typ, var4, var5, var3, var1, var2) %>%
      set_names(c('crop', 'water', 'aeration',
                  'temperature', 'nitrogen', 'phosphorus')) %>%
      pivot_longer(., cols = -crop) %>%
      mutate(., name = factor(name,
                              levels = c('water', 'aeration', 'temperature',
                                         'nitrogen', 'phosphorus')))

    y_lbl <-  'Plant stress at harvest/kill'
  } else {
    stop("Variable must be one of: 'phu', 'yield', 'bioms'")
  }

  gg <- ggplot(data = tbl_harv) +
    labs(x = 'Crops', y = y_lbl) +
    theme_bw()

  if (variable == 'stress') {
    gg <- gg +
      geom_boxplot(aes(x = crop, y = value, fill = name)) +
      labs(fill = 'Stress factor') +
      scale_fill_manual(values = c('deepskyblue4', 'lightcyan3',
                                   'indianred3', 'palegreen4', 'orange3')) +
      theme(legend.position = 'bottom')

  } else {
    gg <- gg + geom_boxplot(aes(x = crop, y = var))
  }

  if(variable == 'phu') {
    gg <- gg + geom_hline(yintercept = 1, lty = 2)
  }

  return(gg)
}
