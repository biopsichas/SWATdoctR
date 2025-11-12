#' Replace hvkl operations with harv + kill
#'
#' @param project_path Character string, path to the SWAT+ project folder
#'   (i.e. TxtInOut).
#' @return updated management.sch file
#' @importFrom readr read_lines write_lines
#' @importFrom stringr str_replace str_count
#' @export
#'
#' @examples
#' \dontrun{
#' add_kill_op('path_to_your_model')
#' }

add_kill_op <- function(project_path){
  mgt_sch <- read_lines(paste0(project_path,'/management.sch'), lazy = FALSE)
  l <- 0
  while(length(grep("hvkl", mgt_sch)) != 0){
    i <- grep("hvkl", mgt_sch)[1]
    if(as.numeric(strsplit(mgt_sch[i], " +")[[1]][3])>0 | as.numeric(strsplit(mgt_sch[i], " +")[[1]][4])>0 ){
      kill_line <- str_replace(mgt_sch[i], "hvkl", "kill") %>%
        str_replace(c("forest_cut|grain1|grain|grass_bag|grass_mulch|hay_cut_high|hay_cut_low|orchard|peanuts|
              silage|stover_high|stover_los|stover_med|tuber|vegetables"), "null")
      mgt_sch[i] <- str_replace(mgt_sch[i], "hvkl", "harv")
      mgt_sch <- insert_line_at(mgt_sch, kill_line, insert_after=i)
      l <- l+1
    } else {
      ##Changing hvkl to harv and kill operations
      kill_line <- str_replace(mgt_sch[i], "hvkl", "kill") %>%
        str_replace(c("forest_cut|grain1|grain|grass_bag|grass_mulch|hay_cut_high|hay_cut_low|orchard|peanuts|
              silage|stover_high|stover_los|stover_med|tuber|vegetables"), "null") %>%
        str_replace_all("[:digit:]", "0") %>%
        str_replace("0.00000", "0.00001") ## Kill operation at 0.00001 HU, next day after harvest.
      mgt_sch[i] <- str_replace(mgt_sch[i], "hvkl", "harv")
      mgt_sch <- insert_line_at(mgt_sch, kill_line, insert_after=i)
      l <- l+1
    }
    ## Adding increased counter
    ii <- i - 1
    ##Fixing counter
    while (str_count(mgt_sch[ii], "\\S+") != 3 & ii != 0) {
      ii <- ii - 1
    }
    ##Adding one additional operation
    c <- strsplit(mgt_sch[ii], " +")[[1]]
    mgt_sch[ii] <-  paste0(c[1], "                           ", as.numeric(c[2])+1, "          ", c[3], "  ")
  }
  if(l > 0){
    file.copy(paste0(project_path,'/management.sch'), paste0(project_path,'/management_bak.sch'))
    write_lines(mgt_sch, paste0(project_path,'/management.sch'))
    return(paste0(l, " lines were updated (`hvkl` changed to `harv`) and same number added (with `kill` operations) in 'management.sch'. Original file is backed up in 'management_bak.sch'."))
  } else {
    return("No `hvkl` operations exist in 'management.sch'. File was not changed.")
  }
}

#' Insert line into character with multiple lines
#'
#' @param dat character with multiple lines
#' @param txt character line to be inserted
#' @param insert_after numeric for line number after, which to insert txt
#'
#' @return character with multiple lines with inserted new lines
#' @keywords internal

insert_line_at <- function(dat, txt, insert_after){
  pre <- dat[1:insert_after]
  if(length(dat) > insert_after){
    post <- dat[(insert_after+1):length(dat)]
    return(c(pre, txt, post))
  } else {
    return(c(pre, txt))
  }
}

#' Function to put option remove hide or show all lines in chart and this function also print chart.
#'
#' @param graph plotly graph object
#' @importFrom plotly plotly_build layout
#' @return plotly graph object with option to remove or show all lines
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' hide_show(fig)
#' }

hide_show <- function(graph){
  plotly_build(graph) %>%
    layout(updatemenus = list(
      list(type = "buttons", direction = "right", xanchor = "center", yanchor = "top",
           showactive = FALSE, x = 0.3, y = 1.0,
           buttons = list(
             list(method = "restyle",
                  args = list("visible", "all"),
                  label = "show all"),
             list(method = "restyle",
                  args = list("visible", "legendonly"),
                  label = "hide all")))))
}

#' Remove tail endings for management, plant community, land use classes
#'
#' This function is required for SWATfarmR generated management files
#' as management.sch, plant.ini, landuse.lum and hru-data.hru can get too
#' long names for swat executable.
#'
#' @param f multiline character
#' @param pattern pattern after which to remove tail
#' @return corrected multiline character
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' library(readr)
#' landuse <- read_lines(paste0(project_path,'/landuse.lum'), lazy = FALSE)
#' landuse  <- remove_tail(landuse, "lum")
#' landuse  <- remove_tail(landuse, "comm")
#' landuse  <- remove_tail(landuse, "mgt")
#' write_lines(landuse, paste0(project_path,'/landuse_new.lum'))
#' }

remove_tail <- function(f, pattern){
  ind <- grep(paste0("_", pattern,"_"), f)
  for(i in ind){
    f[i] <- gsub(paste0("(_", pattern, ").*?\\s"), "\\1 ", f[i])
  }
  return(f)
}

#' Get the Highest Version Number of a File
#'
#' This function searches for files in a specified directory that match a given
#' pattern and extracts the highest version number from those files.
#'
#' @param parent_dir A string representing the path to the directory where the
#' files are located.
#' @param file_name A string representing the setup name.
#' @param add A string to be added to the file name pattern to match.
#' @return The highest version number found among the matching files as a
#' numeric value. Returns 0 if no matching files are found.
#' @export
#' @examples
#' \dontrun{
#' file_version <- get_file_version(parent_dir, save_name, '_nostr_') + 1
#' }

get_file_version <- function(parent_dir, file_name, add) {
  files <- list.files(path = parent_dir,
                      pattern = paste0(file_name, add, '\\d{1,2}', '\\.rds'))
  vers_val <- as.numeric(gsub(paste(file_name, add, '\\.rds', sep = '|'),
                              '', files))
  return(max(vers_val, 0))
}

#' @keywords internal

lwr <- function(df){
  names(df) <- tolower(names(df))
  return(df)
}
