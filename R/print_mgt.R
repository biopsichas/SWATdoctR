#' Print the triggered management operations from the mgt outputs for a certain HRU
#'
#' print_triggered_mgt extracts the triggered management operation which are written
#' into mgt_out.txt and reformats them to resemble the scheduled operations in
#' managment.sch to ease a comparison between them.
#'
#' @param sim_verify Simulation output of the function \code{run_swat_verification()}.
#'   To print the management at least the output option \code{outputs = 'mgt'} must
#'   be set in  \code{run_swat_verification()}
#' @param hru_id id of the HRU for which the triggered management operations should
#'   be printed
#' @param years Integer vector to define the years to be printed.
#'
#' @return Prints a tibble with triggered operations to the R console.
#'
#' @importFrom dplyr filter mutate rename select %>%
#'
#' @export
#'

print_triggered_mgt <- function(sim_verify, hru_id, years = 1900:2100) {
  cat('Triggered managament for\n', ' hru:       ', hru_id, '\n',
      ' management:', sim_verify$lum_mgt$mgt[sim_verify$lum_mgt$id == hru_id], '\n\n')
  sim_verify$mgt_out %>%
    filter(hru == hru_id) %>%
    filter(year %in% years) %>%
    rename(op_data1 = op_typ,
           op_data3 = var1) %>%
    mutate(op_data3 = ifelse(operation != 'FERT', 0, op_data3)) %>%
    # mutate()
    # mutate(date = ymd(paste(year, mon, day, sep = '-'))) %>%
    select(., year, mon, day, phuplant, operation, op_data1, op_data3) %>%
    print(., n = Inf)
}

#' Generate a report table that compares the scheduled and triggered managements
#'
#' report_mgt compares the scheduled management operations for all schedules
#' with the triggered management operations. Therefore HRUs are randomly
#' selected for each schedule where one of the schedules is implemented.
#'
#' @param sim_verify Simulation output of the function \code{run_swat_verification()}.
#'   To print the management at least the output option \code{outputs = 'mgt'} must
#'   be set in  \code{run_swat_verification()}
#' @param write_report (optional) Boolean TRUE for writing output to 'schedule_report.txt' file,
#' FALSE - not preparing this file. Default \code{write_report = FALSE}.
#'
#' @return Returns a tibble that summarises all management schedules for
#'   which operations where scheduled, that were either not triggered of
#'   for which operation properties differ..
#'
#' @importFrom dplyr filter group_by group_split lead left_join mutate rename select slice_sample summarise %>% rename_with full_join ends_with
#' @importFrom purrr map
#' @importFrom stringr str_sub str_remove
#' @importFrom readr write_delim write_lines
#'
#' @export
#'
report_mgt <- function(sim_verify, write_report = FALSE) {
  yr_start <- min(sim_verify$mgt_out$year)
  mgt_lbl <- unique(sim_verify$lum_mgt$mgt)
  mgt_lbl <- mgt_lbl[!is.na(mgt_lbl)]

  schdl_mgt <- sim_verify$mgt_sch %>%
    filter(schedule %in% mgt_lbl) %>%
    filter(!is.na(op_typ)) %>%
    group_by(schedule) %>%
    mutate(rm_skp = lead(op_typ, 1),
           rm_skp = ifelse(op_typ == 'skip' &
                             rm_skp  != 'skip' |
                             is.na(rm_skp), TRUE, FALSE)) %>%
    filter(!rm_skp) %>%
    select(-rm_skp) %>%
    mutate(year = c(NA, diff(mon)),
           year = ifelse(is.na(year) | year >= 0, 0, 1),
           year = cumsum(year) + yr_start) %>%
    ungroup(.)

  mgt_lbl <- unique(schdl_mgt$schedule)

  hru_sel <- sim_verify$lum_mgt %>%
    filter(mgt %in% mgt_lbl) %>%
    group_by(mgt) %>%
    slice_sample(., n = 1)

  mgt_i <- sim_verify$mgt_out %>%
    filter(hru %in% hru_sel$id) %>%
    left_join(., hru_sel, by = c('hru' = 'id')) %>%
    rename(schedule = mgt,
           op_typ = operation,
           op_data1_trig = op_typ) %>%
    select(., schedule, year, mon, day, op_typ, op_data1_trig) %>%
    mutate(op_typ = str_sub(op_typ, 1, 4) %>%  tolower(.),
           op_typ = ifelse(op_typ == 'plan', 'plnt', op_typ))

  schdl_join <- full_join(schdl_mgt, mgt_i,
                          by = c("schedule", "year",  "mon", "day", "op_typ", "op_data1" = "op_data1_trig"), keep = TRUE) %>%
    select(-ends_with(".y")) %>%
    rename_with(~str_remove(., '.x')) %>%
    select(schedule, year, mon, day, op_typ, op_data1_trig, starts_with('op_data')) %>%
    mutate(op_issue = is.na(op_data1_trig) | op_data1_trig != op_data1,
           year = year - yr_start + 1) %>%
    filter(op_issue & year <= max(sim_verify$mgt_out$year) - yr_start)

  schdl_report <- schdl_join %>%
    select(schedule, op_issue) %>%
    group_by(schedule) %>%
    summarise(op_issue = sum(op_issue),
              .groups = 'drop')

  ops_detail <- schdl_join %>%
    group_by(schedule) %>%
    group_split() %>%
    map(., ~ filter(.x, op_issue)) %>%
    map(., ~ select(.x, year, mon, day, op_typ, op_data1_trig, starts_with('op_data')))

  schdl_report <- schdl_report %>%
    mutate(schedule_report = ops_detail)

  if(write_report){
    print("Writing schedule_report.txt")
    write(paste("File was written with SWATdoctR at", Sys.time( )), file = "schedule_report.txt")
    for (i in seq(1, length(schdl_report$schedule_report))){
      mgt <- schdl_report$schedule[[i]]
      id <- get_hru_id_by_attribute(sim_verify, mgt = mgt)$id[1]
      write_lines(" ", "schedule_report.txt", append = TRUE)
      write_lines(paste("HRU number -", id, "- management name:", mgt), "schedule_report.txt", append = TRUE)
      write_lines(" ", "schedule_report.txt", append = TRUE)
      write_delim(schdl_report$schedule_report[[i]], "schedule_report.txt", delim = "\t", append = TRUE, col_names = TRUE)
    }
    print(paste("The file schedule_report.txt was written in", getwd( ), "directory."))
  }

  return(schdl_report)
}


#' Transform x to a matrix with 7 columns and fill up with NA values
#'
#' @param x character vector or NULL
#'
#' @keywords internal
#'
as_mtx_null <- function(x) {
  if(is.null(x)) {
    matrix(rep(NA_character_, 7), ncol = 7)
  } else {
    matrix(x, nrow = 7) %>%
      t(.)
  }
}




