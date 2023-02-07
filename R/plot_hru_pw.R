#' Get HRU ids by filtering for HRU attributes
#'
#' get_hru_id_by_attribute Uses the hru-data and landuse_lum saved after running
#' \code{run_swat_verification()} and filters the attributes by the defined
#' filter attributes.
#'
#' @param sim_verify Simulation output of the function \code{run_swat_verification()}.
#'   To plot the heat units at least the output option \code{outputs = 'mgt'} must
#'   be set in  \code{run_swat_verification()}
#' @param lum Optional character vector with landuse.lum labels
#' @param mgt Optional character vector with management labels as defined in management.sch.
#' @param soil Optional character vector with soil type labels as defined in the soil data.
#'
#' @return Returns a table with hru ids and attributes.
#'
#' @export
#'
get_hru_id_by_attribute <- function(sim_verify, lum = NULL, mgt = NULL, soil = NULL) {
  hru_attr <- sim_verify$lum_mgt[,c('id', 'lu_mgt', 'mgt', 'soil')]
  if(!is.null(lum)) {
    hru_attr <- hru_attr[hru_attr$lu_mgt %in% lum,]
  } else {
    hru_attr$lu_mgt <- NULL
  }
  if(!is.null(mgt)) {
    hru_attr <- hru_attr[hru_attr$mgt %in% mgt,]
  } else {
    hru_attr$mgt <- NULL
  }
  if(!is.null(soil)) {
    hru_attr <- hru_attr[hru_attr$soil %in% soil,]
  } else {
    hru_attr$soil <- NULL
  }
  return(hru_attr)
}

#' Plot daily simulated variables which are saved in hru_pw_day
#'
#' plot_hru_pw_day plots daily time series of variables from the output file hru_pw_day
#'
#' @param sim_verify Simulation output of the function \code{run_swat_verification()}.
#'   To plot the heat units at least the output option \code{outputs = 'mgt'} must
#'   be set in  \code{run_swat_verification()}
#' @param hru_id Numeric vector with HRU ids for which variables should be plotted
#' @param var Character vector that defines the variable names that are plotted
#' @param title Character for title to be put in the figure.
#' @param years Years of the simulated data for which varaibles are plotted.
#'
#' @importFrom dplyr filter mutate select %>%
#' @importFrom lubridate ymd
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect all_of
#' @import ggplot2
#'
#' @return Returns a table with hru ids and attributes.
#'
#' @export
#'
plot_hru_pw_day <- function(sim_verify, hru_id, var, title = "", years = 1900:2100) {
  plot_data <- sim_verify$hru_pw_day %>%
    filter(unit %in% hru_id) %>%
    filter(yr %in% years) %>%
    mutate(date = ymd(paste(yr, mon, day, sep = '-')),
           unit = paste('hru:', unit)) %>%
    select(date, unit, all_of(var)) %>%
    pivot_longer(., cols = - c(date, unit), names_to = 'var', values_to = 'value')

  ggplot(plot_data) +
    geom_line(aes(x = date, y = value, color = unit, lty = unit)) +
    labs(x = 'Date', color = 'HRU', lty = 'HRU', title=title) +
    scale_x_date(date_labels = '%Y-%m-%d') +
    facet_grid(rows = vars(all_of(var)), scales = 'free_y', switch = 'y') +
    theme_bw() +
    theme(legend.position = 'bottom',
          strip.background = element_blank(),
          strip.placement = 'outside',
          strip.text = element_text(face = 'bold'),
          axis.title.y = element_blank())
}

#' Print the average annual qtile for HRUs
#'
#' print_avannual_qtile prints a table with the average annual qtile in mm
#' for HRUs that used a tile flow parametrization in landuse.lum
#'
#' @param sim_verify Simulation output of the function \code{run_swat_verification()}.
#'   To plot the heat units at least the output option \code{outputs = 'wb'} must
#'   be set in  \code{run_swat_verification()}
#' @param exclude_lum Character vector to define land uses which are excluded
#'   in the printed table.
#'
#' @importFrom dplyr arrange filter left_join rename select %>%
#'
#' @return Returns a table with hru ids average annual qtile and attributes.
#'
#' @export
#'
print_avannual_qtile <- function(sim_verify,
                                 exclude_lum = c(
                                   "urhd_lum", "urmd_lum", "urml_lum",
                                   "urld_lum", "ucom_lum", "uidu_lum",
                                   "utrn_lum", "uins_lum", "urbn_lum"
                                 )) {

  sim_verify$hru_wb_aa %>%
    rename(id = unit) %>%
    left_join(., sim_verify$lum_mgt, by = "id") %>%
    filter(tile != 'null') %>%
    filter(!lu_mgt %in% exclude_lum) %>%
    select(id, qtile, lu_mgt, mgt, soil) %>%
    arrange(qtile, id)
}

#' Aggregate and plot simulated variables saved in hru_pw_day
#'
#' @param sim_verify Simulation output of the function \code{run_swat_verification()}.
#'   To plot the heat units at least the output option \code{outputs = 'mgt'} must
#'   be set in  \code{run_swat_verification()}.
#' @param hru_id Numeric vector with HRU ids for which variables should be plotted.
#' @param var Character vector that defines the variable names that are plotted.
#' @param period (optional) character describing, which time interval to display (default is "day",
#' other examples are "week", "month", etc). \code{Default = "day"}
#' @param fn_summarize (optional) function to recalculate to time interval (default is "mean", other examples
#' are "median", "sum", etc). \code{Default = "mean"}
#' @return plotly figure object
#' @importFrom lubridate floor_date
#' @importFrom plotly plot_ly layout
#' @importFrom dplyr %>% rename summarize mutate group_by arrange
#' @export
#'
#' @examples
#' \dontrun{
#' id <- get_hru_id_by_attribute(sim_nostress, "wwht_lum")
#' plot_hru_var(sim_nostress, sample(id$id, 10), "lai")
#' }

plot_hru_var <- function(sim_verify, hru_id, var, period = "day", fn_summarize = "mean"){
  options(dplyr.summarise.inform = FALSE)
  df <- sim_verify$hru_pw_day[sim_verify$hru_pw_day$unit %in% hru_id, c("unit", "yr", "mon", "day", var)]
  ##Aggregating data by time step
  df$Date<- floor_date(ISOdate(df$yr, df$mon, df$day), period)
  df <- df[c("Date", "unit", var)] %>%
    rename(Values = 3) %>%
    mutate(unit = paste('hru:', unit)) %>%
    group_by(unit, Date) %>%
    summarize(Values = get(fn_summarize)(Values))

  ##Plotting
  plot_ly(df %>% arrange(Date), x=~Date, y=~Values,  color = ~factor(unit), colors = "Set2", type = 'scatter', mode = 'lines',
          connectgaps = FALSE) %>% layout(showlegend = TRUE) %>%
    layout(title = paste(var, "parameter"), xaxis = list(title = "Date"), yaxis = list(title = "Values")) %>%
    hide_show()
}

#' Plot simulated variables of filtered HRUs saved in hru_wb_aa with boxplots
#'
#' @param sim_verify Simulation output of the function \code{run_swat_verification()}.
#'   To plot the heat units at least the output option \code{outputs = 'wb'} must
#'   be set in  \code{run_swat_verification()}
#' @param lum Optional character vector with landuse.lum labels
#' @param mgt Optional character vector with management labels as defined in management.sch.
#' @param soil Optional character vector with soil type labels as defined in the soil data.
#' @return plot_ly multi boxplot figure for all available variables, which are not 0.
#' @importFrom dplyr %>% mutate group_by group_map
#' @importFrom tidyr pivot_longer
#' @importFrom plotly plot_ly layout subplot
#' @export
#'
#' @examples
#' \dontrun{
#' plot_hru_var_aa(sim_nostress, "wwht_lum")
#' }

plot_hru_var_aa <- function(sim_verify, lum = NULL, mgt = NULL, soil = NULL){
  p <- paste(lum, mgt, soil)
  id <- get_hru_id_by_attribute(sim_verify, lum, mgt, soil)
  fig <- sim_verify$hru_wb_aa[sim_verify$hru_wb_aa$unit %in% id$id, -c(1:7)] %>%
    .[, colSums(.!= 0) > 0] %>%
    mutate(p = p) %>%
    pivot_longer(-p,names_to = 'var', values_to = 'Values') %>%
    group_by(p, var) %>%
    group_map(~plot_ly(., y=~Values, color = ~var, colors = "cyan4", type = 'box'), keep = TRUE) %>%
    subplot(nrows = 5) %>%
    layout(title = paste("HRUs selected by", p))
  return(fig)
}

