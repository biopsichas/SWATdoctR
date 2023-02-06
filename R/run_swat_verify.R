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
#' @importFrom dplyr %>%
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

  if(msg$timeout) {
    out_msg <- str_split(msg$stdout, '\r\n|\r|\n', simplify = TRUE) %>%
      .[max(1, length(.) - 10):length(.)]
    err_msg <- c(paste0('Simulation timed out after ', time_out, ' sec'),
                 'Simulation run:', out_msg)
    model_output <- err_msg
  } else if(nchar(msg$stderr) == 0) {
    model_output <- list()

    if ('plt' %in% outputs) {
      model_output$hru_pw_day <- read_tbl('hru_pw_day.txt', run_path, 3)
    }
    if ('wb' %in% outputs) {
      model_output$basin_wb_day <- read_tbl('basin_wb_day.txt', run_path, 3)
      model_output$basin_pw_day <- read_tbl('basin_pw_day.txt', run_path, 3)
      model_output$hru_wb_aa <- read_tbl('hru_wb_aa.txt', run_path, 3)
    }
    if ('mgt' %in% outputs) {
      model_output$mgt_out <- read_mgt(run_path)
      model_output$mgt_sch <- read_sch(run_path)

      hru_data <- read_tbl('hru-data.hru', run_path, 2)
      landuse_lum <- read_tbl('landuse.lum', run_path, 2)
      model_output$lum_mgt <- left_join(hru_data, landuse_lum,
                                        by = c("lu_mgt" = 'name')) %>%
        select(id, topo, hydro, soil, lu_mgt, plnt_com, mgt, tile)
    }
  }

  if(!keep_folder) unlink(run_path, recursive = TRUE, force = TRUE)

  return(model_output)
}

#' Identify the operating system.
#' The function was written by Will Lowe and was copied from here:
#' https://conjugateprior.org/2015/06/identifying-the-os-from-r/
#'
#' @keywords internal
#'
get_os <- function() {
  sysinf <- Sys.info()

  if (!is.null(sysinf)) {
    os <- sysinf["sysname"]
    if (os == "Darwin") {
      os <- "osx"
    }

    # If rare case occurs that Sys.info() is NULL
  } else {
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os)) {
      os <- "osx"
    }
    if (grepl("linux-gnu", R.version$os)) {
      os <- "linux"
    }
  }
  tolower(os)
}

#' Add './' to run the exe on unix systems
#' @param exe Text string that defines the name of the executable file
#' @param os Text string that defines the operating system
#' @keywords internal
#'
run_os <- function(exe, os) {
  if(os == 'unix') exe <- '.'%//%exe
  return(exe)
}

#' Find the SWAT+ executable file and trigger error if 0 or more than 1
#' file are found.
#'
#' @param project_path Path to the SWAT+ project folder (i.e. TxtInOut).
#' @param os Text string that defines the operating system
#'
#' @keywords internal
#'
find_swat_exe <- function(project_path, os) {
  if(os == "windows") {
    swat_exe <- list.files(project_path) %>%
      .[grepl(".exe$",.)]

  } else if(os == "linux") {
    swat_exe <- system("find"%&&%project_path%&&%"-executable -type f",
                       intern = T) %>%
      basename(.)
  } else if (os == 'osx') {
    stop('Functionality not tested for Mac. Therefore run aborted')
  }

  # Make sure that there is exactly one executable in the SWAT project folder
  if(length(swat_exe) == 0) stop("No SWAT executable found in the project folder!")
  if(length(swat_exe) > 1) stop("Project folder contains more than one executable!")

  return(swat_exe)
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

  col_names <- read_lines(file = file_path, skip = 1, n_max = 1) %>%
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
