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
#'
#' @return Returns a ggplot flow chart with the simulated average annual water
#'   balance.
#'
#' @importFrom dplyr %>% distinct
#' @importFrom png readPNG
#' @import ggplot2
#' @export
#'
plot_waterbalance <- function(sim_verify) {
  img <- readPNG(paste0(system.file(package = "SWATdoctR"), '/extdata/swatplus_wb.png'))

  wb_aa  <- round(unlist(sim_verify$basin_wb_aa[,c(8:12, 14:18, 20:23, 27:32, 34:39, 43)]), 2)
  aqu_aa <- round(unlist(sim_verify$basin_aqu_aa[,c(8:10, 12:13, 22:24)]), 2)

  ratios <- round(c(wb_aa['et'] / wb_aa['precip'],
                    (wb_aa['surq_gen'] + wb_aa['latq'] + wb_aa['qtile'] + aqu_aa['flo']) / wb_aa['precip'],
                    (wb_aa['surq_cha'] + wb_aa['latq_cha'] + wb_aa['qtile'] + aqu_aa['flo_cha']) / wb_aa['precip'],
                    wb_aa['surq_gen'] + wb_aa['latq'] + wb_aa['qtile'] + aqu_aa['flo'],
                    wb_aa['surq_cha'] + wb_aa['latq_cha'] + wb_aa['qtile'] + aqu_aa['flo_cha'],
                    aqu_aa['flo'] / (wb_aa['surq_gen'] + wb_aa['latq'] + wb_aa['qtile'] + aqu_aa['flo'])
  ),
  2)

  val <- c('', '', wb_aa, aqu_aa, '', '', '', '', '', '', '', ratios)

  x <- c(0, 11.81,
         2.25, 2.78, 4.69, 7.16, 7.16,
         6.73, 0.90, 1.30, 0.90, 0.52,
         3.80, 3.30, 3.30, 3.30, 2.60,
         3.80, 7.16, 4.69, 0.12, 0.12,
         9.90, 9.90, 9.90, 9.90, 9.90,
         9.90, 3.30,
         7.16, 0.35, 0.35, 4.44, 2.15,
         9.90, 9.90, 9.90,
         7.16, 7.16, 7.16, 7.16, 7.16,
         7.16, 7.16,
         9.90, 9.90, 9.90, 10.75, 10.75,
         9.90)
  y <- c(   0, 10.00,
            8.34, 5.80, 5.12, 5.12, 3.48,
            1.87, 7.80, 6.52, 5.68, 4.85,
            8.37, 4.30, 3.80, 3.30, 5.12,
            8.87, 2.85, 6.35, 9.70, 2.83,
            5.68, 5.15, 4.61, 3.98, 3.45,
            2.95, 2.80,
            1.58, 2.08, 1.58, 0.43, 2.22,
            2.12, 1.60, 1.07,
            9.70, 9.20, 8.70, 8.20, 7.70,
            6.90, 6.20,
            9.20, 8.70, 8.20, 6.90, 6.20,
            7.70)
  a <- c( 0, 0,
          0,   90,    0,    0,    0,
          90,   90,   90,   90,   90,
          0,    0,    0,    0,    0,
          0,    0,    0,    0,    0,
          0,    0,    0,    0,    0,
          0,    0,
          0,    0,    0,   90,   90,
          0,    0,    0,
          0,    0,    0,    0,    0,
          0,    0,
          0,    0,    0,    0,    0,
          0)
  l <- c('','',
         'precip:\n', 'sno_fall: ', 'sno_melt: ', 'surq_gen: ', 'latq: ',
         'perc: ', 'et: ', 'ecanopy: ', 'eplant: ', 'esoil: ',
         'cn: ', 'sw_init: ', 'sw_final: ', 'sw_ave: ', 'sno_pack: ',
         'pet: ', 'qtile: ', 'irr: ', 'surq_ro: ', 'latq_ro: ',
         'surq_cha: ', 'surq_res: ', 'surq_ls: ', 'latq_cha: ', 'latq_res: ',
         'latq_ls: ', 'sw_change: ',
         'flo: ', 'dep_wt: ', 'stor: ', 'seep: ', 'revap: ',
         'flo_cha: ', 'flo_res: ', 'flo_ls: ',
         'Water balance ratios: ', 'et / precip: ', 'wyld / precip :', 'wyld_cha / precip :', 'flo / wyld: ',
         'wyld = surq_gen + latq + qtile + flo :',
         'wyld_cha = surq_cha + latq_cha +\n                      qtile + flo_cha :',
         '', '', '', '', '',
         ''
  )

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
