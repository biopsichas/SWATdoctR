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
  post <- dat[(insert_after+1):length(dat)]
  return(c(pre, txt, post))
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
