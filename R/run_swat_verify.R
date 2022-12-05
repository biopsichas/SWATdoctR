#' Run simulations for model verification
#'
#' This function allows to run a SWAT2012 project in R.
#' Basic settings for the SWAT run such as the simulation period or the time
#' interval for the outputs can be done directly. SWAT simulation outputs can be
#' defined that are returned in a 'tidy' format in R. Functionality such as model
#' parametrization, parallel execution of simulations, or incremental saving of
#' simulation runs is provided.
#'
#' @param project_path Character string, path to the SWAT+ project folder
#'   (i.e. TxtInOut).
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
                                  years_skip = NULL, keep_folder = FALSE) {

  # Check settings before starting to set up '.model_run'
  ## General function input checks
  stopifnot(is.character(project_path))
  stopifnot(all(outputs %in% c('wb', 'mgt', 'plt')))
  stopifnot(is.logical(keep_folder))

  run_path <- build_model_run(project_path)

  set_print_prt(project_path, run_path, outputs, years_skip)
  set_time_sim(project_path, run_path, start_date, end_date)

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
      model_output$hru_pw_day <- read_tbl('hru_pw_day', run_path)
    }
    if ('wb' %in% outputs) {
      model_output$basin_wb_day <- read_tbl('basin_wb_day', run_path)
      model_output$basin_pw_day <- read_tbl('basin_pw_day', run_path)
    }
    if ('mgt' %in% outputs) {
      model_output$mgt_out <- read_mgt(run_path)
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
#'
#' @importFrom data.table fread
#' @importFrom purrr set_names
#' @importFrom readr read_lines
#' @importFrom stringr str_trim str_split
#' @importFrom tibble tibble
#'
#' @keywords internal
#'
read_tbl <- function(out_file, run_path) {
  file_path <- paste0(run_path, '/', out_file, '.txt')

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

  fread(file_path, skip = 3) %>%
    set_names(., col_names) %>%
    tibble(.)
}

#' Read SWAT+ management output file and return the read output in a tibble
#'
#' @param run_path Path to the folder where simulations are performed
#'
#' @importFrom purrr map set_names
#' @importFrom stringr str_trim str_split
#' @importFrom tibble as_tibble
#' @importFrom vroom vroom_lines
#'
#' @keywords internal
#'
read_mgt <- function(run_path) {
  file_path <- paste0(run_path, '/mgt_out.txt')

  vroom_lines(file_path, skip = 3) %>%
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
}


#' Generate folder structure for parallel SWAT execution
#'
#' @param project_path Path to the SWAT project folder (i.e. TxtInOut)
#'
build_model_run <- function(project_path){
  run_path <- paste0(project_path, '/.run_verify')
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
  }
  if ('mgt' %in% outputs) {
    print_prt[9]  <- "n             y             n             n             "
  }
  if ('plt' %in% outputs) {
    print_prt[36] <- "hru_pw                       y             n             n             n  "
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
