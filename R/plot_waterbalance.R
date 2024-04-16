#' Plot the average annual water balance
#'
#' This function uses average annual outputs from basin_wb_aa and
#' basin_aqu_aa and plot the simulated values of water balance components
#' in a flow chart.
#'
#' @param sim_verify Simulation output of the function \code{run_swat_verification()}
#'   as a single object.
#'   To plot the water balance at least the output option \code{outputs = 'wb'}
#'   must be set in \code{run_swat_verification()}.
#' @param simplified Boolean, TRUE to plot a simplified version of the flow chart.
#'  Default is FALSE.
#'
#' @return Returns a ggplot flow chart with the simulated average annual water
#'   balance.
#'
#' @importFrom dplyr %>% distinct
#' @importFrom png readPNG
#' @import ggplot2
#' @export

plot_waterbalance <- function(sim_verify, simplified = FALSE) {
  if (simplified) {
    img <- readPNG(paste0(system.file(package = "SWATdoctR"), '/extdata/swatplus_wb_simple.png'))
    wb_aa  <- round(unlist(sim_verify$basin_wb_aa[,c(8:12, 14:18, 20:23, 27:32, 34:39, 43)]), 1)
    aqu_aa <- round(unlist(sim_verify$basin_aqu_aa[,c(8:10, 12:13, 22:24)]), 1)
  } else {
    img <- readPNG(paste0(system.file(package = "SWATdoctR"), '/extdata/swatplus_wb.png'))
    wb_aa  <- round(unlist(sim_verify$basin_wb_aa[,c(8:12, 14:18, 20:23, 27:32, 34:39, 43)]), 2)
    aqu_aa <- round(unlist(sim_verify$basin_aqu_aa[,c(8:10, 12:13, 22:24)]), 2)
  }

  if (aqu_aa['flo_cha'] == 0 & aqu_aa['flo_res'] == 0 & aqu_aa['flo'] > 0) {
    aqu_flo <- aqu_aa['flo']
  } else {
    aqu_flo <- aqu_aa['flo_cha'] + aqu_aa['flo_res']
  }

  surq <- wb_aa['surq_cha'] + wb_aa['surq_res']

  base <- wb_aa['latq_cha'] + wb_aa['latq_res'] +
          aqu_flo + wb_aa['qtile']

  wyld <- surq + base

  ratios <- round(c(wb_aa['et'] / wb_aa['precip'], wyld / wb_aa['precip'],
                    surq / wyld, base / wyld, surq, base, wyld), 2)
  if(simplified) {
    latq <- wb_aa['latq_cha'] + wb_aa['latq_res']

    val <- c('', '', surq, latq, wb_aa['qtile'], aqu_flo, wb_aa['et'], wb_aa['precip'],
             wb_aa['snofall'], wb_aa['precip'] - wb_aa['snofall'],
             wb_aa['sw_init'],   wb_aa['sw_final'], wb_aa['sw_ave'], paste(aqu_aa['dep_wt'], 'm'),
             aqu_aa['revap'], aqu_aa['seep'], wb_aa['perc'], wyld, wb_aa['pet'])

    x <- c(0, 11.81, 7.7, 7.7, 7.7, 7.7, 5.6, 3,
           3.8, 1.2, 1.8, 1.8, 1.8, 1.8, 4.58, 5.73, 6.91, 9.5, 5.6)
    y <- c(0, 10.00, 4.75, 3.8, 2.85, 1.9, 6.96,
           9.5, 6, 6.5, 3.7, 3.2, 2.7, 1.85, 1.92, 0.6, 2.02, 0.5, 7.4)
    a <- c(0, 0, 0, 0, 0, 0, 0, 0,
           0, 0, 0, 0, 0, 0,
           90, 90, 90, 0, 0)
    l <- c('','', 'surq: ','latq: ', 'qtile: ', 'flo: ', 'et: ', 'precip: ',
           'snofall: \n', 'rain: ', 'sw_init: ', 'sw_final: ', 'sw_ave: ', 'dep_wt: ',
           'revap: ', 'seep: ', 'perc: ', 'wyld: ', 'pet: ')
  } else {
    val <- c('', '', wb_aa, aqu_aa, '', '', '', '', '', '', '', '', ratios)

    x <- c(0, 11.81,
           2.25, 2.78, 4.69, 7.16, 7.16,
           6.73, 0.90, 1.30, 0.90, 0.52,
           3.80, 3.30, 3.30, 3.30, 2.60,
           3.80, 7.16, 4.69, 0.12, 0.12,
           9.90, 9.90, 9.90, 9.90, 9.90,
           9.90, 3.30,
           7.16, 0.35, 0.35, 4.44, 2.15,
           9.90, 9.90, 9.90,
           7.16, 7.16, 7.16, 9.40, 9.40,
           7.16, 7.16, 7.16,
           8.60, 8.60, 10.75, 10.75, 10.75,
           10.75, 10.75)
    y <- c(0, 10.00,
           8.34, 5.80, 5.12, 5.12, 3.48,
           1.87, 7.80, 6.52, 5.68, 4.85,
           8.37, 4.30, 3.80, 3.30, 5.12,
           8.87, 2.85, 6.35, 9.70, 2.83,
           5.68, 5.15, 4.61, 3.98, 3.45,
           2.95, 2.80,
           1.58, 2.08, 1.58, 0.43, 2.22,
           2.12, 1.60, 1.07,
           9.70, 9.20, 8.70, 9.20, 8.70,
           7.80, 6.70, 6.20,
           9.20, 8.70, 9.20, 8.70, 7.80,
           6.70, 6.20)
    a <- c(0, 0,
           0,    90,    0,    0,    0,
           90,   90,   90,   90,   90,
           0,     0,    0,    0,    0,
           0,     0,    0,    0,    0,
           0,     0,    0,    0,    0,
           0,     0,
           0,     0,    0,   90,   90,
           0,     0,    0,
           0,     0,    0,    0,    0,
           0,     0,    0,
           0,     0,    0,    0,    0,
           0, 0)
    l <- c('','',
           'precip:\n', 'sno_fall: ', 'sno_melt: ', 'surq_gen: ', 'latq: ',
           'perc: ', 'et: ', 'ecanopy: ', 'eplant: ', 'esoil: ',
           'cn: ', 'sw_init: ', 'sw_final: ', 'sw_ave: ', 'sno_pack: ',
           'pet: ', 'qtile: ', 'irr: ', 'surq_ro: ', 'latq_ro: ',
           'surq_cha: ', 'surq_res: ', 'surq_ls: ', 'latq_cha: ', 'latq_res: ',
           'latq_ls: ', 'sw_change: ',
           'flo: ', 'dep_wt: ', 'stor: ', 'seep: ', 'revap: ',
           'flo_cha: ', 'flo_res: ', 'flo_ls: ',
           'Water balance ratios: ', 'et / precip: ', 'wyld / precip:', 'surq / wyld:', 'base / wyld: ',
           # 'wyld = surq_gen + latq + qtile + flo :',
           paste0('surq = surq_cha + surq_res:'),
           paste0('base = latq_cha + latq_res +\n',
                  '             flo_cha  + flo_res  +\n',
                  '             qtile:'),
           paste0('wyld = surq + base:'),
           '', '', '', '', '', ''
    )
  }

  l <- paste0(l, val)

  ggplot() +
    annotation_raster(img, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, interpolate = FALSE) +
    geom_text(aes(x = x,
                  y = y,
                  label = l,
                  angle = a), vjust = 0, hjust = 0) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    coord_equal() +
    geom_point() +
    theme_void()
}
