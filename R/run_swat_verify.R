#' Run simulations for model verification
#'
#' This function allows to run a SWAT2012 project in R.
#' Basic settings for the SWAT run such as the simulation period or the time
#' interval for the outputs can be done directly. SWAT simulation outputs can be
#' defined that are returned in a 'tidy' format in R. Functionality such as model
#' parametrization, parallel execution of simulations, or incremental saving of
#' simulation runs is provided.
#'
#' @param project_path Path to the SWAT+ project folder (i.e. TxtInOut).
#' @param outputs Define the outputs that should be read after the simulation
#'   run. The outputs that are defined here depend on the verification steps
#'   that should be performed on the outputs.
#' @param start_date (optional) Start date of the SWAT simulation. Provided as
#'   character string in any ymd format (e.g. 'yyyy-mm-dd'), numeric value
#'   in the form yyyymmdd, or in Date format.
#' @param end_date (optional) End date of the SWAT simulation. Provided as
#'   character string in any ymd format (e.g. 'yyyy-mm-dd'), numeric value
#'   in the form yyyymmdd, or in Date format.
#' @param years_skip (optional) Integer value to define the number of simulation
#'   years that are skipped before writing SWAT model outputs.
#' @param nostress nostress parameter in the 'codes.bsn' file to activate/deactivate
#'   plant stresses for plant growth. Set \code{nostress = 0} to activate all stress
#'   factors, \code{nostress = 1} to deactivate all stress factors, and \code{nostress = 2}
#'   to only activate nutrient plant stress.
#' @param keep_folder (optional) If \code{keep_folder = TRUE}
#'   '.model_run/verification' is kept and not deleted after finishing model runs.
#'   In this case '.model_run' is reused in a new model run if \code{refresh = FALSE}.
#'   \code{Default = FALSE}
#'
#' @return Returns the simulation results for the defined output variables as a
#'   list of tibbles.
#'
#' @importFrom dplyr %>% distinct
#' @importFrom processx run
#' @importFrom stringr str_split
#' @importFrom tibble tibble
#' @export
#'
run_swat_verification <- function(project_path, outputs = c('wb', 'mgt', 'plt'),
                                  start_date = NULL, end_date = NULL,
                                  years_skip = NULL, nostress = 1,
                                  keep_folder = FALSE) {

  # Check settings before starting to set up '.model_run'
  ## General function input checks
  stopifnot(is.character(project_path))
  stopifnot(all(outputs %in% c('wb', 'mgt', 'plt')))
  stopifnot(is.logical(keep_folder))

  run_path <- build_model_run(project_path, '/.run_verify')

  set_print_prt(project_path, run_path, outputs, years_skip)
  set_time_sim(project_path, run_path, start_date, end_date)
  set_codes_bsn(run_path, nostress)

  ## Get operating system and find SWAT executable file
  os <- get_os()
  swat_exe <- find_swat_exe(project_path, os)

  msg <- run(run_os(swat_exe, os), wd = run_path,
             error_on_status = FALSE)

  if(nchar(msg$stderr) > 0) {
    out_msg <- str_split(msg$stdout, '\r\n|\r|\n', simplify = TRUE) %>%
      .[max(1, length(.) - 10):length(.)]
    err_msg <- str_split(msg$stderr, '\r\n|\r|\n', simplify = TRUE)
    err_msg <- c('Last output:', out_msg, 'Error:', err_msg)
    model_output <- err_msg
  } else if(nchar(msg$stderr) == 0) {
    model_output <- list()
    if ('plt' %in% outputs) {
      model_output$hru_pw_day <- read_tbl('hru_pw_day.txt', run_path, 3) %>% lwr
    }
    if ('wb' %in% outputs) {
      model_output$basin_wb_day  <- read_tbl('basin_wb_day.txt', run_path, 3) %>% lwr
      model_output$basin_pw_day  <- read_tbl('basin_pw_day.txt', run_path, 3) %>% lwr
      model_output$basin_wb_aa   <- read_wb_aa(run_path) %>% lwr
      model_output$basin_aqu_aa  <- read_tbl('basin_aqu_aa.txt',  run_path, 3) %>% lwr
      # model_output$basin_cha_aa  <- read_tbl('basin_sd_cha_aa.txt',  run_path, 3) %>% lwr
      model_output$hru_wb_aa <- read_tbl('hru_wb_aa.txt', run_path, 3) %>% lwr
      tryCatch({
        model_output$recall_yr <- read_tbl('recall_yr.txt', run_path, 3) %>% lwr
      },
      error = function(e) {
        model_output$recall_yr <- NULL
      })
      tryCatch({
        model_output$exco_om <- read_tbl('exco_om.exc', run_path, 2) %>% lwr
      },
      error = function(e) {
        model_output$exco_om <- NULL
      })
    }
    if ('mgt' %in% outputs) {
      model_output$mgt_out <- read_mgt(run_path) %>% lwr
      model_output$mgt_sch <- read_sch(run_path) %>% lwr

      hru_data <- read_tbl('hru-data.hru', run_path, 2) %>% lwr
      landuse_lum <- read_tbl('landuse.lum', run_path, 2) %>% lwr
      model_output$lum_mgt <- left_join(hru_data,
                                        landuse_lum,
                                        by = c("lu_mgt" = 'name')) %>%
        distinct() %>%
        select(id, topo, hydro, soil, lu_mgt, plnt_com, mgt, tile)
    }
  }

  if(!keep_folder) unlink(run_path, recursive = TRUE, force = TRUE)

  return(model_output)
}

#' Read SWAT+ output that is arranged in a tabular format (most outputs)
#' and return the read output table in a tibble format
#'
#' @param out_file Name of the output file that should be read.
#' @param run_path Path to the folder where simulations are performed
#' @param n_skip Integer number, how many lines to skip when reading table
#'
#' @importFrom data.table fread
#' @importFrom purrr set_names
#' @importFrom readr read_lines
#' @importFrom stringr str_trim str_split
#' @importFrom tibble tibble
#'
#' @keywords internal
#'
read_tbl <- function(file, run_path, n_skip) {
  file_path <- paste0(run_path, '/', file)

  col_names <- read_lines(file = file_path, skip = 1, n_max = 1, lazy = FALSE) %>%
    str_trim(.) %>%
    str_split(., '[:space:]+') %>%
    unlist()

  name_duplicate <- table(col_names) %>%
    .[. > 1]
  if(length(name_duplicate) > 0) {
    for (i in 1:length(name_duplicate)) {
      col_names[col_names == names(name_duplicate[i])] <-
        paste0(names(name_duplicate[i]), 1:name_duplicate[i])
    }
  }

  fread(file_path, skip = n_skip, header = FALSE) %>%
    set_names(., col_names) %>%
    tibble(.)
}

#' Read SWAT+ management output file and return the read output in a tibble
#'
#' @param run_path Path to the folder where simulations are performed
#'
#' @importFrom dplyr %>%
#' @importFrom purrr map map_df set_names
#' @importFrom readr read_lines
#' @importFrom stringr str_trim str_split
#' @importFrom tibble as_tibble
#'
#' @keywords internal
#'
read_mgt <- function(run_path) {
  file_path <- paste0(run_path, '/mgt_out.txt')

  mgt <- read_lines(file_path, skip = 3, lazy = FALSE) %>%
    unlist() %>%
    str_trim(.) %>%
    str_split(., '\t[:space:]+|[:space:]+') %>%
    map(., ~ .x[1:21]) %>%
    unlist() %>%
    matrix(., nrow = 21) %>%
    t() %>%
    as_tibble(., .name_repair = 'minimal') %>%
    set_names(., c('hru', 'year', 'mon', 'day', 'op_typ', 'operation',
                   'phubase', 'phuplant', 'soil_water', 'plant_bioms',
                   'surf_rsd', 'soil_no3', 'soil_solp', 'op_var',
                   paste0('var', 1:7)))

  mgt[,c(1:4, 7:21)] <- map_df(mgt[,c(1:4, 7:21)], as.numeric)

  return(mgt)
}

#' Read SWAT+ management schedule file and return the read file in a tibble
#'
#' @param run_path Path to the folder where simulations are performed
#'
#' @importFrom dplyr mutate %>%
#' @importFrom purrr map map_int map_chr map2 map2_df map_df set_names
#' @importFrom readr read_lines
#' @importFrom stringr str_replace_all str_trim str_split
#' @importFrom tibble as_tibble
#'
#' @keywords internal
#'
read_sch <- function(run_path) {
  schdl_path <- paste0(run_path, '/management.sch')

  schdl <- read_lines(schdl_path, skip = 2, lazy = FALSE) %>%
    str_trim(.) %>%
    str_replace_all(., '\t', ' ') %>%
    str_split(., '[:space:]+')

  n_elem <- map_int(schdl, length)
  schdl <- schdl[n_elem != 1]
  n_elem <- map_int(schdl, length)
  schdl_def_pos <- which(n_elem == 3)

  schdl_name <- map_chr(schdl_def_pos, ~ schdl[[.x]][1])

  schdl_start <- schdl_def_pos + 1
  schdl_end <- c(schdl_def_pos[2:length(schdl_def_pos)] - 1, length(schdl))
  no_entry <- schdl_start > schdl_end
  schdl_start[no_entry] <- length(schdl) + 1
  schdl_end[no_entry] <- length(schdl) + 1
  schdl_start[no_entry] <- 1e9
  schdl_end[no_entry] <- 1e9

  schdl_mgt <- map2(schdl_start, schdl_end, ~ schdl[.x:.y]) %>%
    map(., unlist) %>%
    map(., as_mtx_null) %>%
    map(., ~ as_tibble(.x, .name_repair = 'minimal')) %>%
    map(., ~ set_names(.x, c('op_typ', 'mon', 'day', 'hu_sch', paste0('op_data', 1:3)))) %>%
    map2_df(., schdl_name, ~ mutate(.x, schedule = .y, .before = 1))

  schdl_mgt[,c(3:5, 8)] <- map_df(schdl_mgt[,c(3:5, 8)], as.numeric)

  return(schdl_mgt)
}
