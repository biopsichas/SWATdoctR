#' Generate folder structure for SWAT execution
#'
#' @param project_path Path to the SWAT project folder (i.e. TxtInOut)
#' @param folder_name  Name of the folder in which simulations are performed
#'
#' @keywords internal
#'
build_model_run <- function(project_path, folder_name){
  run_path <- paste0(project_path, folder_name)
  swat_files <- dir(project_path, full.names = TRUE)
  exclude <- ".txt$|.csv$|.db$"
  swat_files <- swat_files[!grepl(exclude,swat_files)]

  dir.create(run_path, recursive = TRUE)
  file.copy(swat_files, run_path)

  return(run_path)
}

#' Read and set SWAT+ print.prt file write the updated print.prt and the original file
#' as a backup 'print_backup.prt'
#'
#' @param project_path Path to the SWAT project folder (i.e. TxtInOut)
#' @param run_path Path to the folder where simulations are performed
#' @param outputs Define the outputs that should be read after the simulation
#'   run. The outputs that are defined here depend on the verification steps
#'   that should be performed on the outputs.
#' @param years_skip (optional) Integer value to define the number of simulation
#'   years that are skipped before writing SWAT model outputs.
#'
#' @importFrom readr read_lines write_lines
#' @importFrom stringr str_replace str_sub
#'
#' @keywords internal
#'
set_print_prt <- function(project_path, run_path, outputs, years_skip) {
  print_prt <- read_lines(paste0(project_path,'/print.prt'), lazy = FALSE)

  print_prt[7] <- "n             n             n             "
  print_prt[7] <- "n             n             n             n             "
  print_prt[11:length(print_prt)] <-
    paste0(str_sub(print_prt[11:length(print_prt)], 1, 29),
           'n             n             n             n  ')

  if(!is.null(years_skip)){
    print_prt[3] <- str_replace(print_prt[3], '[:digit:]+(?=[:space:])', as.character(years_skip))
  }

  if ('wb' %in% outputs) {
    print_prt[11] <- "basin_wb                     y             n             n             n  "
    print_prt[14] <- "basin_pw                     y             n             n             n  "
    print_prt[33] <- "hru_wb                       n             n             n             y  "
    print_prt[45] <- "recall                       n             n             y             y  "
  }
  if ('mgt' %in% outputs) {
    print_prt[9]  <- "n             y             n             n             "
  }
  if ('plt' %in% outputs) {
    print_prt[36] <- "hru_pw                       y             n             n             n  "
  }
  if ('wb_sft' %in% outputs) {
    print_prt[11] <- "basin_wb                     n             n             n             y  "
  }

  write_lines(print_prt, paste0(run_path,'/print.prt'))
}

#' Read and set SWAT+ time.sim file write the updated time.sim and the original file
#' as a backup 'time_backup.sim'
#'
#' @param project_path Path to the SWAT project folder (i.e. TxtInOut)
#' @param run_path Path to the folder where simulations are performed
#' @param start_date (optional) Start date of the SWAT simulation. Provided as
#'   character string in any ymd format (e.g. 'yyyy-mm-dd'), numeric value
#'   in the form yyyymmdd, or in Date format.
#' @param end_date (optional) End date of the SWAT simulation. Provided as
#'   character string in any ymd format (e.g. 'yyyy-mm-dd'), numeric value
#'   in the form yyyymmdd, or in Date format.
#' @importFrom lubridate interval int_end int_start yday year ymd
#' @importFrom readr read_lines write_lines
#' @importFrom stringr str_trim str_split
#'
#' @keywords internal
#'
set_time_sim <- function(project_path, run_path, start_date, end_date) {
  time_sim <- read_lines(paste0(project_path,'/time.sim'), lazy = FALSE)

  if(xor(is.null(start_date), is.null(end_date))) {
    stop("'start_date' and 'end_date' must be provided together!")
  } else if (!is.null(start_date)) {
    ## Determine required date indices for writing to time.sim
    start_date <- ymd(start_date)
    end_date   <- ymd(end_date)
    time_interval <- interval(start_date, end_date)
    start_year    <- year(int_start(time_interval))
    start_jdn     <- yday(int_start(time_interval))
    end_year      <- year(int_end(time_interval))
    end_jdn       <- yday(int_end(time_interval))

    time_sim[3] <- c(start_jdn, start_year, end_jdn, end_year, 0) %>%
      sprintf("%10d",.) %>%
      paste(., collapse = "")
  }

  write_lines(time_sim, paste0(run_path,'/time.sim'))
}

#' Set the nostress value in the code.bsn file
#'
#' @param run_path Path to the folder where simulations are performed
#' @param nostress nostress parameter in the 'codes.bsn' file to activate/deactivate
#'   plant stresses for plant growth. Set \code{nostress = 1} to activate all stress
#'   factors, \code{nostress = 1} to deactivate all stress factors, and \code{nostress = 1}
#'   to only activate nutrient plant stress.
#'
#' @importFrom dplyr %>%
#' @importFrom purrr map2_chr
#' @importFrom readr read_lines write_lines
#' @importFrom stringr str_trim str_split
#'
#' @keywords internal
#'
set_codes_bsn <- function(run_path, nostress) {
  bsn_path <- paste0(run_path, '/codes.bsn')
  bsn <- read_lines(bsn_path)
  bsn_val <- bsn[3] %>%
    str_trim(.) %>%
    str_split(., '[:space:]+') %>%
    unlist()
  bsn_val[11] <- nostress
  bsn[3] <- bsn_val %>%
    map2_chr(., c('%16s', '%18s', rep('%10s', 19), '%18s', rep('%10s', 2)),
             ~sprintf(.y, .x)) %>%
    paste(., collapse = '')

  write_lines(bsn, bsn_path)
}
