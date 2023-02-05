#' Replace hvkl operations with harv + kill
#'
#' @param project_path Character string, path to the SWAT+ project folder
#'   (i.e. TxtInOut).
#' @return updated management.sch file
#' @importFrom readr read_lines write_lines
#' @importFrom stringr str_replace
#' @export
#'
#' @examples
#' \dontrun{
#' add_kill_op('path_to_your_model')
#' }

add_kill_op <- function(project_path){
  mgt_sch <- read_lines(paste0(project_path,'/management.sch'), lazy = FALSE)
  write_lines(mgt_sch, paste0(project_path,'/management_bak.sch'))
  l <- 0
  while(length(grep("hvkl", mgt_sch)) != 0){
    i <- grep("hvkl", mgt_sch)[1]
    kill_line <- str_replace(mgt_sch[i], "hvkl", "kill") %>%
      str_replace(c("forest_cut|grain1|grain|grass_bag|grass_mulch|hay_cut_high|hay_cut_low|orchard|peanuts|
                silage|stover_high|stover_los|stover_med|tuber|vegetables"), "null")
    mgt_sch[i] <- str_replace(mgt_sch[i], "hvkl", "harv")
    mgt_sch <- insert_line_at(mgt_sch, kill_line, insert_after=i)
    l <- l+1
  }
  if(l > 0){
    write_lines(mgt_sch, paste0(project_path,'/management.sch'))
    return(paste0(l, " lines were updated in 'management.sch'. Original file is backed up in 'management_bak.sch'."))
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
  post <- dat[(insert_after+1):length(dat)]
  return(c(pre, txt, post))
}
