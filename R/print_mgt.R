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
           op_data3 = op_var) %>%
    mutate(op_data3 = ifelse(operation != 'FERT', 0, op_data3)) %>%
    # mutate()
    # mutate(date = ymd(paste(year, mon, day, sep = '-'))) %>%
    select(., year, mon, day, phuplant, operation, op_data1, op_data3)
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
#' @importFrom dplyr filter group_by group_split lead left_join mutate rename select slice_sample summarise %>% rename_with full_join ends_with inner_join reframe
#' @importFrom purrr map
#' @importFrom stringr str_sub str_remove
#' @importFrom readr write_delim write_lines
#' @importFrom stats lag
#'
#' @export
#'
report_mgt <- function(sim_verify, write_report = FALSE) {
  yr_start <- min(sim_verify$mgt_out$year)
  yr_end   <- max(sim_verify$mgt_out$year)
  mgt_lbl <- unique(sim_verify$lum_mgt$mgt)
  mgt_lbl <- mgt_lbl[!is.na(mgt_lbl)]

  schdl_template <- sim_verify$mgt_sch %>%
    filter(schedule %in% mgt_lbl) %>%
    group_by(schedule) %>%
    mutate(
      # Create a 'relative year' index based on 'skip'
      # We use lag() so the operations AFTER a skip belong to the next year
      is_skip = ifelse(op_typ == "skip", 1, 0),
      rel_year = cumsum(lag(is_skip, default = 0))
    ) %>%
    # Now we can safely remove the 'skip' rows
    filter(op_typ != "skip") %>%
    mutate(rot_length = max(rel_year) + 1) %>%
    ungroup()

  schdl_mgt <- schdl_template %>%
    group_by(schedule) %>%
    reframe(year = yr_start:yr_end)%>%
    # Join back to the template using the Modulo Operator
    inner_join(schdl_template, by = "schedule", relationship = "many-to-many") %>%
    mutate(
      # This determines which year of the template fits into the current sim year
      # e.g., for a 2-yr rotation: 2015->0, 2016->1, 2017->0, 2018->1...
      match_year = (year - yr_start) %% rot_length
    ) %>%
    # Keep only the operations that belong to that specific year in the cycle
    filter(rel_year == match_year) %>%
    # Cleanup and sort
    select(schedule, op_typ, mon, day, hu_sch, starts_with("op_data"), year) %>%
    arrange(schedule, year, mon, day)

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

  ##Case for PHU scheduling
  if(any(schdl_mgt$hu_sch>0)){
    schdl_mgt_in <- schdl_mgt %>%
      group_by(schedule, year) %>%
      mutate(year = year - yr_start + 1) %>%
      mutate(id = row_number())

    df <- NULL
    for(sch in unique(schdl_mgt_in$schedule)){ ##For each schedule in schdl_mgt
      sch1 <- schdl_mgt_in[schdl_mgt_in$schedule == sch,] ##data separated
      counter_max <- max(sch1$year) ##counter set
      counter <- counter_min <- 1
      for (n in seq(yr_start,max(sim_verify$mgt_out$year),1)){ ##For each modeling year
        d_copy <- sch1[sch1$year == counter,]
        d_copy$year <- n
        if(!is.null(df)){df <- bind_rows(df, d_copy)}else{df <- d_copy}
        if(counter>=counter_max){counter<-counter_min}else{counter<-counter+1} ##Counter for schedules with more than 1 year
      }
    }
    schdl_join <- full_join(df, mgt_i %>%
                              group_by(schedule, year) %>%
                              mutate(id = row_number()),
                            by = c("schedule", "year", "id", "op_typ", "op_data1" = "op_data1_trig"), keep = TRUE)
  } else {
    schdl_join <- full_join(schdl_mgt, mgt_i,
                            by = c("schedule", "year",  "mon", "day", "op_typ", "op_data1" = "op_data1_trig"), keep = TRUE)
  }

  schdl_join <- schdl_join %>%
    select(-ends_with(".y")) %>%
    rename_with(~str_remove(., '.x')) %>%
    select(schedule, year, mon, day, op_typ, op_data1_trig, starts_with('op_data')) %>%
    mutate(op_issue = is.na(op_data1_trig) | op_data1_trig != op_data1,
           year = year - yr_start + 1) %>%
    filter(op_issue & year <= max(sim_verify$mgt_out$year) - yr_start)

  if(nrow(schdl_join) > 0) {
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

    # if(write_report){
    #   print("Writing schedule_report.txt")
    #   write(paste("File was written with SWATdoctR at", Sys.time( )), file = "schedule_report.txt")
    #   for (i in seq(1, length(schdl_report$schedule_report))){
    #     mgt <- schdl_report$schedule[[i]]
    #     id <- get_hru_id_by_attribute(sim_verify, mgt = mgt)$id[1]
    #     write_lines(" ", "schedule_report.txt", append = TRUE)
    #     write_lines(paste("HRU number -", id, "- management name:", mgt), "schedule_report.txt", append = TRUE)
    #     write_lines(" ", "schedule_report.txt", append = TRUE)
    #     write_delim(schdl_report$schedule_report[[i]], "schedule_report.txt", delim = "\t", append = TRUE, col_names = TRUE)
    #   }
    #   print(paste("The file schedule_report.txt was written in", getwd( ), "directory."))
    # }

    return(schdl_report)

  } else {
    cat('Management OK! No differences between scheduled and triggered managments identified.')
  }
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




