#' Update the land use labels which were written by SWATfarmR
#'
#' The maximum length for labels in input files which point to entries in other
#' input files are limited to 16 characters. SWATfarmR writes labels for
#' landuses, plant communities, and managements by adding index values. They
#' can result in labels longer than 16 characters. This routine fixes this issue.
#'
#' @param project_path Path to the SWAT+ project folder (i.e. TxtInOut).
#'
#' @return Rewrites the input files hru-data.hru, landuse.lum, management.sch,
#'   and plant.ini with shorter labels.
#'
#' @importFrom dplyr distinct left_join group_by group_split mutate select %>%
#' @importFrom purrr list_c list_rbind map map_lgl map2 map2_df
#' @importFrom readr read_lines write_lines
#' @importFrom stringr str_detect str_remove str_remove_all str_replace
#' @importFrom tibble tibble
#'
#' @export
#'
update_landuse_labels <- function(project_path) {
  mgt_sch <- read_sch(project_path)
  mgt_sch_head <- read_lines(paste0(project_path, '/management.sch'),
                             n_max = 2, lazy = FALSE)
  hru_data <- read_tbl('hru-data.hru', project_path, 2)
  hru_data_head <- read_lines(paste0(project_path, '/hru-data.hru'),
                              n_max = 2, lazy = FALSE)
  landuse_lum <- read_tbl('landuse.lum', project_path, 2)
  landuse_lum_head <- read_lines(paste0(project_path, '/landuse.lum'),
                                 n_max = 2, lazy = FALSE)
  plant_ini <- read_plant_ini(project_path)
  plant_ini_head <- read_lines(paste0(project_path, '/plant.ini'),
                               n_max = 2, lazy = FALSE)

  lu_lbl <- hru_data %>%
    select(lu_mgt) %>%
    mutate(lu_mgt_ini = str_remove(lu_mgt, '\\_[:digit:]+\\_[:digit:]+$')) %>%
    group_by(lu_mgt_ini) %>%
    mutate(n = n()) %>%
    mutate(has_suffix = str_detect(lu_mgt, '\\_[:digit:]+\\_[:digit:]+$')) %>%
    mutate(., suffix_upd = ifelse(n > 1 & has_suffix, 1, NA),
           suffix_upd = cumsum(suffix_upd)) %>%
    mutate(suffix_upd = ifelse(!is.na(suffix_upd), paste0('_', suffix_upd), '')) %>%
    mutate(lu_mgt_upd = str_remove(lu_mgt_ini, '\\_lum')) %>%
    mutate(n_chr = max(nchar(suffix_upd) + max(nchar(lu_mgt_upd)))) %>%
    mutate(lu_mgt_upd = ifelse(n_chr > 12,
                               str_remove_all(lu_mgt_upd, 'a|e|i|o|u'),
                               lu_mgt_upd)) %>%
    mutate(n_chr = max(nchar(suffix_upd) + max(nchar(lu_mgt_upd)))) %>%
    group_split() %>%
    map(., ~ remove_consonants(.x)) %>%
    list_rbind(.) %>%
    mutate(lu_mgt_upd = paste0(lu_mgt_upd, suffix_upd))

  lu_lbl <- lu_lbl %>%
    select(lu_mgt, lu_mgt_upd) %>%
    mutate(lu_mgt_upd = paste0(lu_mgt_upd, '_lum')) %>%
    distinct(.)

  mgt_lbl <- lu_lbl %>%
    mutate(schedule = str_replace(lu_mgt, 'lum', 'mgt'),
           schedule_upd = str_replace(lu_mgt_upd, 'lum', 'mgt')) %>%
    select(schedule, schedule_upd)

  pcm_lbl <- lu_lbl %>%
    mutate(plnt_com = str_replace(lu_mgt, 'lum', 'comm'),
           plnt_com_upd = str_replace(lu_mgt_upd, 'lum', 'com')) %>%
    select(plnt_com, plnt_com_upd)

  hru_data <- hru_data %>%
    left_join(., lu_lbl, by = 'lu_mgt') %>%
    mutate(lu_mgt = lu_mgt_upd) %>%
    select(-lu_mgt_upd)

  write_tbl_file(hru_data,
                 hru_data_head,
                 c('%8d', '%-16s', rep('%16s', 8)),
                 paste0(project_path, '/hru-data.hru'))



  landuse_lum <- landuse_lum %>%
    left_join(., lu_lbl, by = c('name' = 'lu_mgt')) %>%
    mutate(name = lu_mgt_upd) %>%
    select(-lu_mgt_upd) %>%
    mutate(plnt_com = str_replace(name, 'lum', 'com'),
           mgt      = str_replace(name, 'lum', 'mgt'))

  write_tbl_file(landuse_lum,
                 landuse_lum_head,
                 c('%-20s', rep('%16s', 13)),
                 paste0(project_path, '/landuse.lum'))

  mgt_sch <- mgt_sch %>%
    left_join(., mgt_lbl, by = 'schedule') %>%
    mutate(schedule = schedule_upd) %>%
    select(-schedule_upd)

  mgt_list <- mgt_sch %>%
    group_by(schedule) %>%
    group_split()

  is_no_mgt <- map_lgl(mgt_list, ~ find_no_mgt(.x))

  mgt_init <- map2(mgt_list, is_no_mgt,
                   ~ paste(sprintf('%-26s', .x[1,1]),
                           ifelse(.y, sprintf('%8d', 0), sprintf('%8d', nrow(.x))),
                           sprintf('%10d', 0)))

  mgt_list <- map2(mgt_list, is_no_mgt, ~ if(.y) {.x[0,]} else {.x})

  mgt_fmt <- c('%46s', '%16s', '%8d', '%8d', '%12.3f', '%16s', '%16s', '%12.2f')

  mgt_lines <- mgt_list %>%
    map(., ~ mutate(.x, schedule = '')) %>%
    map(., ~ map2_df(.x, mgt_fmt, ~ sprintf(.y, .x))) %>%
    map(., ~ apply(.x, 1, paste, collapse = '  ')) %>%
    map2(., mgt_init, ~ c(.y, .x)) %>%
    list_c(.) %>%
    c(mgt_sch_head, .)

  write_lines(mgt_lines, paste0(project_path, '/management.sch'))

  ini_tbl <- tibble(plnt_com = plant_ini$ini_lbl) %>%
    left_join(pcm_lbl, by = "plnt_com") %>%
    mutate(plnt_com_upd = ifelse(is.na(plnt_com_upd), NA, sprintf('%-27s', plnt_com_upd)))

  plant_ini <- map2_chr(plant_ini$ini, ini_tbl$plnt_com_upd,
                        ~ ifelse(!is.na(.y), replace_ini_lbl(.x, .y), .x)) %>%
    c(plant_ini_head, .)

  write_lines(plant_ini, paste0(project_path, '/plant.ini'))
}

#' Remove single consonants of words if they are longer than 12 characters
#'
#' @param tbl Tibble with labels and label lengths
#'
#' @importFrom stringr str_detect str_split
#'
#' @keywords internal
#'
remove_consonants <- function(tbl) {
  if (tbl$n_chr[1] > 12) {
    n <- tbl$n_chr[1] - 12
    txt_split <- str_split(tbl$lu_mgt_upd[1], '', simplify = TRUE)
    alph_pos <- which(str_detect(txt_split, '[:alpha:]'))
    pos_rmv <- sample(alph_pos[alph_pos > 1], n)
    txt <- paste(txt_split[-pos_rmv], collapse = '')
    tbl$lu_mgt_upd <- txt
  }
  return(tbl)
}

#' Read function for the plant.ini input file
#'
#' @param path Path to the SWAT+ project folder (i.e. TxtInOut).
#'
#' @importFrom dplyr %>%
#' @importFrom readr read_lines
#' @importFrom stringr str_split str_trim
#' @importFrom purrr map_lgl map2_chr
#'
#' @keywords internal
#'
read_plant_ini <- function(path) {
  ini <- read_lines(paste0(path, '/plant.ini'), skip = 2, lazy = FALSE)
  ini_lbl <- ini %>%
    str_trim(.) %>%
    str_split(., '[:space:]+')
  is_ini_lbl <- map_lgl(ini_lbl, ~ length(.x) == 3)
  ini_lbl <- map2_chr(ini_lbl, is_ini_lbl, ~ ifelse(.y, .x[1], ''))
  return(list(ini = ini,
              ini_lbl = ini_lbl))
}

#' Replace the plant ini label in a line of the plant.ini input file
#'
#' @param ini_line Text line from the plant.ini input file
#' @param ini_lbl_upd New plant.ini label for replacement of the old label
#'
#' @importFrom dplyr %>%
#' @importFrom readr read_lines
#' @importFrom stringr str_split str_trim
#' @importFrom purrr map_lgl map2_chr
#'
#' @keywords internal
#'
replace_ini_lbl <- function(ini_line, ini_lbl_upd) {
  ini_line %>%
    str_trim(.) %>%
    str_split(., '[:space:]+') %>%
    .[[1]] %>%
    .[2:3] %>%
    c(ini_lbl_upd, .) %>%
    sprintf(c('%-25s', '%3s', '%12s'), .) %>%
    paste(., collapse = '')
}

#' Find the managments with 0 management operations
#'
#' @param mgt Management schedule input table in tibble format
#'
#' @importFrom dplyr select %>%
#'
#' @keywords internal
#'
find_no_mgt <- function(mgt) {
  is_all_na <- mgt %>%
    select(-schedule) %>%
    apply(., 1, is.na) %>%
    apply(., 2, all)

  all(is_all_na)
}

#' Write tabular input file in SWAT+ table text fiel
#'
#' @param tbl Parameter input table
#' @param head header lines of input file
#' @param fmt vector of formats in which the table columns should be written
#' @param path Path of the input file in the project folder
#'
#' @importFrom dplyr %>%
#' @importFrom purrr map2_df
#' @importFrom readr write_lines
#'
#' @keywords internal
#'
write_tbl_file <- function(tbl, head, fmt, path) {
  file <- tbl %>%
    map2_df(., fmt, ~ sprintf(.y, .x)) %>%
    apply(., 1, paste, collapse = '  ') %>%
    c(head, .)

  write_lines(file, path)
}

