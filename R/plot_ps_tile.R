#' Plot yearly simulated point source values (as yearly loads or average concentrations)
#'
#' @param sim_verify Simulation output of the function \code{run_swat_verification()}.
#'   To plot the heat units at least the output option \code{outputs = 'wb'} must
#'   be set in  \code{run_swat_verification()}
#' @param conc Boolean, TRUE to provide figure for point source average yearly pollutant concentrations,
#' FALSE - for average yearly pollutant loads.
#' @return ggplot object for point source yearly simulated data
#' @importFrom dplyr %>% mutate group_by group_map select bind_rows case_when
#' @importFrom ggplot2 ggplot geom_line facet_wrap labs theme_bw theme aes
#' @importFrom tidyr pivot_longer
#' @export
#'
#' @examples
#' \dontrun{
#' plot_ps(sim_nostress, TRUE)
#' }

plot_ps <- function(sim_verify, conc = FALSE){
  if(!is.null(sim_verify$recall_yr)){
    df <- sim_verify$recall_yr[, -c(1:3,6)] %>%
      .[, colSums(.!= 0) > 0] %>%
      mutate(yr = as.factor(yr),
             name = gsub("hru00", "ps", name))
    if(conc){
      df <- df %>%
        pivot_longer(-c(yr, name, flo), names_to = 'var', values_to = 'Values') %>%
        mutate(Values = ifelse(var == "sed", (Values/flo)*(1000000/(24*69*60*365.25)),
                               (Values/flo)*(1000/(24*69*60*365.25)))) %>%
        select(yr, name, var, Values) %>%
        bind_rows(df[c("yr", "name", "flo")] %>% mutate(var = "flo") %>% rename(Values = flo)) %>%
        mutate(var = case_when(var == 'flo' ~ "flo m3/s",
                               var %in% c("orgn", "no3", "nh3", "no2") ~ paste(var, "N mg/y"),
                               var %in% c("sedp", "solp") ~ paste(var, "P mg/y"),
                               TRUE ~  paste(var, "mg/l")))
    } else {
      df <- df %>%
        pivot_longer(-c(yr, name), names_to = 'var', values_to = 'Values') %>%
        mutate(var = case_when(var == 'flo' ~ "flo m3/s",
                               var == 'sed' ~ "sed t/y",
                               var %in% c("orgn", "no3", "nh3", "no2") ~ paste(var, "N kg/y"),
                               var %in% c("sedp", "solp") ~ paste(var, "P kg/y"),
                               TRUE ~  paste(var, "kg/y")))
    }
    fig <- ggplot(df, aes(x=yr, y=Values,  group=name, colour=name))+
      geom_line(size=1.5)+
      facet_wrap(~var, scales = "free_y")+
      labs(color='Point sources', x = 'Year') +
      theme_bw()+
      theme(strip.background = element_rect(fill = "deepskyblue3", colour = "azure3"),
            strip.text = element_text(color = "white", face="bold"),
            panel.border = element_rect(colour = "azure3"),
            axis.text.x = element_text(angle = 25, hjust=1))

    return(fig)
  } else {
    print("No point sources exists in this model setup!!!")
  }
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

#' Plot tile drain flow histogram and distribution curve
#'
#' @param sim_verify Simulation output of the function \code{run_swat_verification()}.
#'   To plot the heat units at least the output option \code{outputs = 'wb'} must
#'   be set in  \code{run_swat_verification()}
#' @param exclude_lum Character vector to define land uses which are excluded
#'   in the printed table.
#'
#' @importFrom ggplot2 ggplot geom_histogram labs theme_bw theme geom_density aes
#' @return Returns ggplot object with histogram and density curve for tile drain from distribution.
#' @export
#'
#' @examples
#' \dontrun{
#' plot_qtile(sim_nostress)
#' }

plot_qtile <- function(sim_verify, exclude_lum = c("urhd_lum", "urmd_lum", "urml_lum",
                                                   "urld_lum", "ucom_lum", "uidu_lum",
                                                   "utrn_lum", "uins_lum", "urbn_lum")){
  df <- print_avannual_qtile(sim_verify, exclude_lum)

  fig <- ggplot(df, aes(x=qtile)) +
    geom_histogram(aes(y=..density..), color="black", fill="blue", breaks = seq(min(df$qtile), max(df$qtile), 10))+
    geom_density(alpha=.3, fill="white", linewidth = 1, color = "grey25", linetype = "twodash")+
    labs(title = "Tile drain flow density mm/year")+
    theme_bw()+
    theme(panel.border = element_blank(),
          axis.line = element_line(color='black'),
          axis.title.x=element_blank())
  return(fig)
}
