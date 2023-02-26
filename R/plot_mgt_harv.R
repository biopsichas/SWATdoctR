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
  if("mgt_out" %in% names(sim_verify)){
    tbl_harv <- var_at_harv(sim_verify$mgt_out, years)
    tbl_harv$name <- deparse(substitute(sim_verify))
  } else if ("mgt_out" %in% names(sim_verify[[1]])){
    df <- NULL
    nn <- c()
    for(n in names(sim_verify)){
      mgt_out <- sim_verify[[n]][["mgt_out"]]
      tbl_harv <- var_at_harv(mgt_out, years)
      tbl_harv$name <- n
      nn <- c(nn, n)
      if(is.null(df)){df <- tbl_harv} else {df <- bind_rows(df, tbl_harv)}
    }
    tbl_harv <- df %>%
      mutate(name = factor(name, levels = nn))
  }


  if(variable == 'phu') {
    tbl_harv <- tbl_harv %>%
      select(op_typ, phuplant, name) %>%
      set_names(c('crop', 'var', 'simulation'))

    y_lbl <-  'Crop HUs at harvest/kill'
  }else if(variable == 'yield') {
    tbl_harv <- tbl_harv %>%
      select(op_typ, op_var, name) %>%
      set_names(c('crop', 'var', 'simulation'))

    y_lbl <-  'Crop yield at harvest/kill'
  } else if(variable == 'bioms') {
    tbl_harv <- tbl_harv %>%
      select(op_typ, plant_bioms, name) %>%
      set_names(c('crop', 'var', 'simulation'))

    y_lbl <-  'Biomass at harvest/kill'
  } else if(variable == 'stress') {
    tbl_harv <- tbl_harv %>%
      select(op_typ, var4, var5, var3, var1, var2, name) %>%
      set_names(c('crop', 'water', 'aeration',
                  'temperature', 'nitrogen', 'phosphorus', 'simulation')) %>%
      pivot_longer(., cols = -c(crop, simulation)) %>%
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
      geom_boxplot(aes(x = crop, y = value, fill = name),
                   color = "grey30", outlier.size=1, outlier.colour = "grey50") +
      labs(fill = 'Stress factor') +
      scale_fill_manual(values = c('deepskyblue4', 'lightcyan3',
                                                 'indianred3', 'palegreen4', 'orange3')) +
                                                   theme(legend.position = 'bottom')+
      facet_wrap(~simulation)+
      theme(axis.text.x=element_text(angle=45, hjust=1))

  } else {
    gg <- gg + geom_boxplot(aes(x = crop, y = var, fill = simulation),
                            color = "grey30", outlier.size=1, outlier.colour = "grey50")+
      scale_fill_manual(values = c('white', 'palegreen4', 'orange3', 'lightcyan3', 'indianred3'))
  }
  if(variable == 'phu') {
    gg <- gg + geom_hline(yintercept = 1, lty = 2)
  }
  return(gg)
}

#' Prepare dataframe for plot_variable_at_harvkill function
#'
#' @param mgt_out dataframe with model output data
#' @param years integer vector to filter dataframe
#'
#' @return dataframe for plotting
#' @importFrom dplyr %>% filter group_by mutate ungroup
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' var_at_harv(sim_nostress$mgt_out, 1900:2100)
#' }

var_at_harv <- function(mgt_out, years){
  tbl_harv_b <- mgt_out %>%
    filter(year %in% years) %>%
    filter(operation %in% c('HARVEST', 'KILL'))

  tbl_harv <- tbl_harv_b %>%
    group_by(hru, year, mon, day) %>%
    mutate(n = n()) %>%
    ungroup() %>%
    filter(n == 2, operation == 'HARVEST')

  ##Just in case dates of harvest and kill differ a bit
  if(dim(tbl_harv)[1] == 0){
    print("harv and kill are not on the same day. Switching to closest on same month.")
    tbl_harv <- tbl_harv_b %>%
      group_by(hru, year, mon) %>%
      mutate(n = n()) %>%
      ungroup() %>%
      filter(n == 2, operation == 'HARVEST')
  }
  return(tbl_harv)
}
