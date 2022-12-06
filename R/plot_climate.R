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
    geom_errorbarh(data = pet_tbl, aes(xmax = yr + 0.5, xmin = yr - 0.5, y = value_sum, height = 0, col = name), lwd = 1) +
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
              tmn_a = mean(tmn),
              tmx_a = mean(tmx),
              tmpav = mean(tmpav)) %>%
    pivot_longer(cols = -yr)

  gg_tmp <- ggplot() +
    geom_errorbarh(data = tmp_tbl, aes(xmax = yr + 0.5, xmin = yr - 0.5, y = value, height = 0, col = name), lwd = 1) +
    scale_color_manual(values = c('dodgerblue2', 'dodgerblue4', 'black', 'tomato1', 'tomato3')) +
    # geom_step(data = pet_tbl, aes(x = yr, y = value_sum), direction = 'mid') +
    labs(y = 'Temperature (°C)') +
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
    labs(y = expression(Solar~Radiation~(MJ~m^{-2})),
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

  gg_et_stat  <- plot_stat_text(clim_data_annual,
                                vars = c('pet', 'et', 'ecanopy', 'eplant', 'esoil'),
                                c(2,1,0,3,4), 'mm', T)
  gg_pcp_stat <- plot_stat_text(clim_data_annual,
                                vars = c('precip', 'rainfall', 'snofall'),
                                c(5,4,3), 'mm', F)
  gg_tmp_stat <- plot_stat_text(rename(clim_data, value_sum = value),
                                vars = c('tmn', 'tmx', 'tmpav'),
                                c(5,4,3), '°C', F)
  gg_slr_stat <- plot_stat_text(clim_data_annual, vars = c('solarad'),
                                c(5), 'MJ m^-2', F)

  gg_et + gg_et_stat + gg_pcp + gg_pcp_stat + gg_tmp + gg_tmp_stat +
    gg_slr + gg_slr_stat + plot_layout(ncol = 2, widths = c(0.85, 0.15))
}

#' Plot annual average values in tabular text form as a ggplot
#' @param tbl Processed climate variable table
#' @param vars Character vector which defines the variables to be printed
#' @param pos Print positions of the variables and their values
#' @param unit_add Character string which defines unit to be added to values
#' @param add_title Boolean to define if title should be added to table
#'
#' @importFrom dplyr filter group_by mutate select summarise %>%
#' @importFrom ggplot2 aes ggplot lims geom_text theme_void
#'
#' @keywords internal
#'
plot_stat_text <- function(tbl, vars, pos, unit_add, add_title) {
  tbl_mean <- tbl %>%
    filter(name %in% vars) %>%
    select(name, yr, value_sum) %>%
    group_by(name) %>%
    summarise(value_sum = round(mean(value_sum))) %>%
    mutate(value_sum = paste(value_sum, unit_add),
           y = pos)

  gg <- ggplot() +
    theme_void() +
    lims(x = c(0, 5), y = c(0,5)) +
    geom_text(aes(x = 0, y = tbl_mean$y, label = paste0(tbl_mean$name, ':')), hjust = 0) +
    geom_text(aes(x = 5, y = tbl_mean$y, label = tbl_mean$value_sum), hjust = 1)

  if(add_title) {
    gg <- gg + geom_text(aes(x = 0, y = 5, label = 'Average annual values:'), hjust = 0, size = 6)
  }

  return(gg)
}
