

# to be done with the @ stuff
library(data.table) # fread
library(dplyr) # filter, %>%, slice, ranking/row_number
library(tidyr) # unite


# this will need to be determined / passed
path = "C:/Users/NIBIO/Documents/GitLab/optain-swat/SWAT_softcal/swatplus_rev60_demo/"



# The following is only required due to the poor formatting of the SWAT output
read_wb_aa <- function(path){
  # read the wb_aa file from its PATH
  basin_wb_aa = fread(paste0(path, "basin_wb_aa.txt"), fill = TRUE, )
  
  
  # change the column names to those of the second row in the text file. 
  colnames(basin_wb_aa) <- basin_wb_aa %>% slice(2) %>% unlist(., use.names = F)
  
  # add placeholder names for the last three columns which did not get a name
  colnames(basin_wb_aa)[c(47:49)] <- c("x", "y", "z")
  
  # remove rows 1 to 3, as they don't contain any real data
  basin_wb_aa <- basin_wb_aa %>% filter(!row_number() %in% c(1:3))
  
  # Unite the last three rows into one string (as they were intended to be)
  # the new column will be named description and describes what the softcal
  # algorithm did in that step. (So its probably important).
  # the united columns are separated by a space (trailing space exists now)
  basin_wb_aa = tidyr::unite(data = basin_wb_aa,
                             col = description,
                             c(47:49),
                             sep = " ") 
  
  
  # figure out which columns should be a double and not a string
  # (everything except for name and decription columns)
  dbl_cols = basin_wb_aa %>% colnames()
  dbl_cols= dbl_cols[! dbl_cols %in% c("name","description")]
  # and convert them 
  basin_wb_aa = basin_wb_aa %>% mutate_at(dbl_cols, as.numeric) %>% tibble()
  
  return(basin_wb_aa)
}



df = read_wb_aa(path)
library(ggplot2)
ggplot(df)+geom_col(mapping = aes(x = description, y = wateryld ))

df %>% tibble()
