#' Plot annual overview of simulated climate variables of a SWAT+ simulation
#'
#' plot_climate_annual uses the simulated basin averages of pet, the different
#' et fractions, precip, snofall, temperatures, and solar radiation to plot
#' annual sums and averages.
#' The aim of the function is to provide an overview of the simulated variables
#' and help to identify potential issues with the model weather input data.
#'
#' @param sim_verify Simulation output of the function \code{run_swat_verification()}.
#'   To plot the climate outputs at least the output option \code{outputs = 'wb'} must
#'   be set in  \code{run_swat_verification()}.
#'
#' @return Returns a patchwork of ggplots for the annual values of the simulated
#'   climate variables.
#'
#' @importFrom dplyr filter group_by left_join mutate rename select summarise %>%
#' @importFrom lubridate ymd
#' @importFrom tidyr pivot_longer
#' @import ggplot2
#' @import patchwork
#' @export
#'
plot_climate_annual <- function(sim_verify) {
  basin_wb <- sim_verify$basin_wb_day %>%
    mutate(date = ymd(paste(yr, mon, day, sep = '-')),
           rainfall = precip - snofall) %>%
    select(date, yr, mon, day, jday, precip, rainfall, snofall, et, ecanopy, eplant, esoil, pet)
  basin_pw <- sim_verify$basin_pw_day %>%
    mutate(date = ymd(paste(yr, mon, day, sep = '-'))) %>%
    select(date, yr, mon, day, jday, tmx, tmn, tmpav, solarad, wndspd, rhum)

  clim_data <- left_join(basin_wb, basin_pw, by = c("jday", "mon", "day", "yr", "date")) %>%
    pivot_longer(., cols = -c(date, yr, mon, day, jday))

  clim_data_annual <- clim_data %>%
    group_by(name, yr) %>%
    summarise(value_sum = sum(value), value_mean = mean(value), .groups = 'drop')

  y_lim <- clim_data_annual %>%
    filter(name %in% c('pet', 'precip')) %>%
    .$value_sum %>%
    max(.)

  et_tbl <- clim_data_annual %>%
    filter(name %in% c('ecanopy', 'eplant', 'esoil')) %>%
    select(name, yr, value_sum)

  pet_tbl <- clim_data_annual %>%
    filter(name == 'pet') %>%
    select(name, yr, value_sum)

  gg_et <- ggplot() +
    geom_col(data = et_tbl, aes(x = yr, y = value_sum, fill = name)) +
    geom_errorbar(data = pet_tbl, aes(xmax = yr + 0.5, xmin = yr - 0.5, y = value_sum, col = name), lwd = 1) +
    scale_color_manual(values = 'black') +
    scale_fill_manual(values = c('skyblue3', 'springgreen3', 'wheat3')) +
    ylim(c(0, 1.1*y_lim)) +
    # geom_step(data = pet_tbl, aes(x = yr, y = value_sum), direction = 'mid') +
    labs(y = 'PET, ETa (mm)') +
    theme_bw() +
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          legend.title = element_blank(),
          legend.box = "horizontal",
          legend.direction = 'horizontal',
          legend.justification=c(1,1),
          legend.position=c(0.995,0.99),
          legend.key = element_blank(),
          legend.background = element_blank(),
          legend.box.background = element_rect(colour = "black", fill =alpha('white', 0.6)))

  pcp_tbl <- clim_data_annual %>%
    filter(name %in% c('rainfall', 'snofall')) %>%
    select(name, yr, value_sum)

  gg_pcp <- ggplot() +
    geom_col(data = pcp_tbl, aes(x = yr, y = value_sum, fill = name)) +
    scale_fill_manual(values = c('dodgerblue4','slategray2')) +
    # geom_step(data = pet_tbl, aes(x = yr, y = value_sum), direction = 'mid') +
    labs(y = 'Precipitation (mm)') +
    ylim(c(0, 1.1*y_lim)) +
    theme_bw() +
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          legend.title = element_blank(),
          legend.box = "horizontal",
          legend.direction = 'horizontal',
          legend.justification=c(1,1),
          legend.position=c(0.995,0.99),
          legend.key = element_blank(),
          legend.background = element_blank(),
          legend.box.background = element_rect(colour = "black", fill =alpha('white', 0.6)))

  tmp_tbl <- basin_pw %>%
    select(yr, tmn, tmx, tmpav) %>%
    group_by(yr) %>%
    summarise(tmn_d = min(tmn),
              tmx_d = max(tmx),
              # tmn_a = mean(tmn),
              # tmx_a = mean(tmx),
              tmpav = mean(tmpav)) %>%
    pivot_longer(cols = -yr)

  gg_tmp <- ggplot() +
    geom_errorbar(data = tmp_tbl, aes(xmax = yr + 0.5, xmin = yr - 0.5,
                                      y = value, col = name),
                  linewidth = 1) +
    scale_color_manual(values = c('dodgerblue4', 'black', 'tomato3')) +
    # geom_step(data = pet_tbl, aes(x = yr, y = value_sum), direction = 'mid') +
    labs(y = 'Temperature (\u00b0C)') +
    theme_bw() +
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          legend.title = element_blank(),
          legend.box = "horizontal",
          legend.direction = 'horizontal',
          legend.justification=c(1,1),
          legend.position=c(0.995,0.99),
          legend.key = element_blank(),
          legend.background = element_blank(),
          legend.box.background = element_rect(colour = "black", fill =alpha('white', 0.6)))

  rhum_tbl <- basin_pw %>%
    select(yr, rhum) %>%
    group_by(yr) %>%
    summarise(rhum_min  = min(rhum),
              rhum_max  = max(rhum),
              rhum_mean = mean(rhum)) %>%
    pivot_longer(cols = -yr)

  rhum_check <- unique(rhum_tbl$value[!is.na(rhum_tbl$value)])
  if(length(rhum_check) == 1 && rhum_check == 0.99){
    warning("Relative humidity model output is consistently 0.99. Please check the input data.
         It is likely that your model input is in the wrong units. Relative humidity
         input should be in fractional units (0-1), not percentage!")
  }

  gg_rhum <- ggplot() +
    geom_errorbar(data = rhum_tbl, aes(xmax = yr + 0.5, xmin = yr - 0.5,
                                       y = value, col = name),
                  linewidth = 1) +
    scale_color_manual(values = c('dodgerblue4', 'black', 'tomato3')) +
    # geom_step(data = pet_tbl, aes(x = yr, y = value_sum), direction = 'mid') +
    labs(y = 'Relative humidity (-)') +
    theme_bw() +
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          legend.title = element_blank(),
          legend.box = "horizontal",
          legend.direction = 'horizontal',
          legend.justification=c(1,1),
          legend.position=c(0.995,0.99),
          legend.key = element_blank(),
          legend.background = element_blank(),
          legend.box.background = element_rect(colour = "black", fill =alpha('white', 0.6)))

  wnd_tbl <- basin_pw %>%
    select(yr, wndspd) %>%
    group_by(yr) %>%
    summarise(wnd_min  = min(wndspd),
              wnd_max  = max(wndspd),
              wnd_mean = mean(wndspd)) %>%
    pivot_longer(cols = -yr)

  gg_wnd <- ggplot() +
    geom_errorbar(data = wnd_tbl, aes(xmax = yr + 0.5, xmin = yr - 0.5,
                                      y = value,col = name),
                  linewidth = 1) +
    scale_color_manual(values = c('dodgerblue4', 'black', 'tomato3')) +
    # geom_step(data = pet_tbl, aes(x = yr, y = value_sum), direction = 'mid') +
    labs(y = expression(Wind~speed~(m~s^{-1}))) +
    theme_bw() +
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          legend.title = element_blank(),
          legend.box = "horizontal",
          legend.direction = 'horizontal',
          legend.justification=c(1,1),
          legend.position=c(0.995,0.99),
          legend.key = element_blank(),
          legend.background = element_blank(),
          legend.box.background = element_rect(colour = "black", fill =alpha('white', 0.6)))

  slr_tbl <- clim_data_annual %>%
    filter(name %in% c('solarad')) %>%
    select(name, yr, value_sum)

  gg_slr <- ggplot() +
    geom_col(data = slr_tbl, aes(x = yr, y = value_sum, fill = name)) +
    scale_fill_manual(values = 'tan3') +
    # labs(y = expression("Another really really really really long y label here "~cm^3)) +
    labs(y = expression(Solar~radiation~(MJ~m^{-2})),
         x = 'Year') +
    theme_bw() +
    theme(legend.title = element_blank(),
          legend.box = "horizontal",
          legend.direction = 'horizontal',
          legend.justification=c(1,1),
          legend.position=c(0.995,0.99),
          legend.key = element_blank(),
          legend.background = element_blank(),
          legend.box.background = element_rect(colour = "black", fill =alpha('white', 0.6)))

  gg_et_stat  <- plot_stat_text(tbl = clim_data_annual,
                                vars = c('pet', 'et', 'ecanopy', 'eplant', 'esoil'),
                                pos = c(2,1,0,3,4), unit_add = 'mm', add_title = T)
  gg_pcp_stat <- plot_stat_text(tbl = clim_data_annual,
                                vars = c('precip', 'rainfall', 'snofall'),
                                pos = c(5,4,3), unit_add = 'mm', add_title = F)
  tmp_stat <- clim_data %>%
    filter(name %in% c('tmn', 'tmx', 'tmpav')) %>%
    group_by(name) %>%
    summarise(val_min = min(value),
              val_max = max(value),
              val_mean = mean(value)) %>%
    mutate(name = c('tmn day', 'tmpav', 'tmx day'),
           value_sum = c(val_min[1], val_mean[2], val_max[3])) %>%
    select(name, value_sum)
  gg_tmp_stat <- plot_stat_text(tbl = tmp_stat,
                                vars = c('tmn day', 'tmpav', 'tmx day'),
                                pos = c(5,4,3), unit_add = '\u00b0C', add_title = F,
                                digit = 1)
  gg_rhum_stat <- plot_stat_text(tbl = clim_data_annual, vars = c('rhum'),
                                pos = c(5), unit_add = '', add_title = F,
                                digit = 2, type = 'value_mean')
  gg_wnd_stat <- plot_stat_text(tbl = clim_data_annual, vars = c('wndspd'),
                                pos = c(5), unit_add = 'm/s', add_title = F,
                                digit = 2, type = 'value_mean')
  gg_slr_stat <- plot_stat_text(tbl = clim_data_annual, vars = c('solarad'),
                                pos = c(5), unit_add = 'MJ m^-2', add_title = F)

  gg_et + gg_et_stat + gg_pcp + gg_pcp_stat + gg_tmp + gg_tmp_stat +
    gg_rhum + gg_rhum_stat + gg_wnd + gg_wnd_stat +
    gg_slr + gg_slr_stat + plot_layout(ncol = 2, widths = c(0.85, 0.15))
}

#' Plot annual average values in tabular text form as a ggplot
#' @param tbl Processed climate variable table
#' @param vars Character vector which defines the variables to be printed
#' @param pos Print positions of the variables and their values
#' @param unit_add Character string which defines unit to be added to values
#' @param add_title Boolean to define if title should be added to table
#' @param type Either plot annual sums (\code{type = 'value_sum'}), or annual
#'   means (\code{type = 'value_mean'})
#'
#' @importFrom dplyr filter group_by mutate select summarise %>%
#' @importFrom ggplot2 aes ggplot lims geom_text theme_void
#' @importFrom tidyselect all_of
#'
#' @keywords internal
#'
plot_stat_text <- function(tbl, vars, pos, unit_add, add_title,
                           digit = 0, type = 'value_sum') {
  tbl_mean <- tbl %>%
    filter(name %in% vars) %>%
    select(name, all_of(type)) %>%
    set_names(c('name', 'value')) %>%
    group_by(name) %>%
    summarise(value = round(mean(value), digits = digit)) %>%
    mutate(value = paste(value, unit_add),
           y = pos)

  gg <- ggplot() +
    theme_void() +
    lims(x = c(0, 5), y = c(0,5)) +
    geom_text(aes(x = 0, y = tbl_mean$y, label = paste0(tbl_mean$name, ':')), hjust = 0) +
    geom_text(aes(x = 5, y = tbl_mean$y, label = tbl_mean$value), hjust = 1)

  if(add_title) {
    gg <- gg + geom_text(aes(x = 0, y = 5, label = 'Average annual:'), hjust = 0, size = 6)
  }

  return(gg)
}

#' Plot average monthly precipitation components rainfall and snofall and the snow melt
#'
#' plot_snow_monthly uses the simulated basin averages of precip, and snofall,
#' and plots average monthly sums of rainfall, snofall and snomlt.
#' This is plot can be helpful to verify the dominant snow processes and
#' can be useful in catchments where large spring runoff through snowmelt
#' plays a relevant role.
#'
#' @param sim_verify Simulation output of the function \code{run_swat_verification()}.
#'   To plot the climate outputs at least the output option \code{outputs = 'wb'} must
#'   be set in  \code{run_swat_verification()}.
#'
#' @return Returns a ggplots for the monthly average values of the simulated
#'   precip, snofall and snomlt.
#'
#' @importFrom dplyr group_by mutate select summarise %>%
#' @importFrom lubridate month
#' @importFrom tidyr pivot_longer
#' @import ggplot2
#' @export
#'
plot_monthly_snow <- function(sim_verify) {
  sno_wb <- sim_verify$basin_wb_day %>%
    mutate(rainfall = precip - snofall) %>%
    select(yr, mon, rainfall, snofall, snomlt) %>%
    pivot_longer(., cols = - c(yr, mon)) %>%
    group_by(name, mon, yr) %>%
    summarise(value = sum(value), .groups = 'drop_last') %>%
    group_by(name, mon) %>%
    summarise(., value = mean(value), .groups = 'drop') %>%
    mutate(process = ifelse(name == 'snomlt', 'Snow melt', 'Precipitation'),
           mon = month(mon, label = T))

  ggplot(data = sno_wb) +
    geom_col(aes(x = mon, y = value, fill = name)) +
    labs(x = 'Month', y = 'Average monthly sum (mm)', fill = 'Process') +
    scale_fill_manual(values = c('dodgerblue4','slategray2', 'skyblue4')) +
    facet_wrap(.~process, ncol = 1) +
    theme_bw()
}

#' Plot selected basin variable aggregated by defined time step
#'
#' @param sim_verify Simulation output of the function \code{run_swat_verification()}.
#'   To plot the climate outputs at least the output option \code{outputs = 'wb'} must
#'   be set in  \code{run_swat_verification()}.
#' @param var Character which defines the variable to be printed
#' @param period (optional) character describing, which time interval to display (default is "day",
#' other examples are "week", "month", etc). \code{Default = "day"}
#' @param fn_summarize (optional) function to recalculate to time interval (default is "mean", other examples
#' are "median", "sum", etc). \code{Default = "mean"}
#' @importFrom lubridate floor_date
#' @importFrom plotly plot_ly layout
#' @importFrom dplyr %>% rename summarise mutate group_by arrange
#' @return plotly figure object
#' @export
#'
#' @examples
#' \dontrun{
#' plot_basin_var(sim_nostress, "percn")
#' }

plot_basin_var <- function(sim_verify, var, period = "day", fn_summarize = "mean"){
  if(var %in% names(sim_verify$basin_wb_day)){
    df <- sim_verify$basin_wb_day[,c("yr","mon","day",var)]
  } else if (var %in% names(sim_verify$basin_pw_day)){
    df <- sim_verify$basin_pw_day[,c("yr","mon","day",var)]
  }
  ##Aggregating data by time step
  df$Date<- floor_date(ISOdate(df$yr, df$mon, df$day), period)
  df <- df[c("Date", var)] %>%
    rename(Values = 2) %>%
    group_by(Date) %>%
    summarise(Values = get(fn_summarize)(Values))
  ##Plotting
  plot_ly(df %>% arrange(Date), x=~Date, y=~Values, name = var, type = 'scatter', mode = 'lines',
          connectgaps = FALSE) %>% layout(showlegend = FALSE) %>%
    layout(title = paste(var, "variable"), yaxis = list(title = "Values"))
}


