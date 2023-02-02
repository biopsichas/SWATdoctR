

# I'll make it package compatible later, once its working.
library(data.table) # fread
library(dplyr) # filter, %>%, slice, ranking/row_number, tibble
library(tidyr) # unite


# note: for now this is only water balance.
# once i finish water balance, ill add the crop routine.


# this will need to be determined / passed
path = "C:/Users/NIBIO/Documents/GitLab/optain-swat/SWAT_softcal/swatplus_rev60_demo/"

# downloads the required .sft files from some source
# printing just for diagnostics, maybe remove.
download_sft_files <- function(path) {

  # this could be passed as a parameter
  required_files = c("codes.sft", "wb_parms.sft")

  # "water_balance.sft" currently does not exist on bitbucket but needs to be downloaded!!

  # I hope this remains constant for all the files???
  # or can we host this on our own gitlab?
  # this could be passed as a parameter
  source__online_directory = "https://bitbucket.org/blacklandgrasslandmodels/modular_swatplus/raw/3d2126fb115077b51be90526238c83cd79e3ef4c/database_files/"

  # find out which sft files are missing and download them.
  for (file in required_files) {
    if (file %in% list.files(path) == FALSE) {
      paste(file, "not in your SWAT+ directory. downloading now...") %>% print()

      try(download.file(
        url = paste0(source__online_directory, file),
        destfile = paste0(path, file)
      ))

      if (file %in% list.files(path)) {
        paste(file, "downloaded successfully") %>% print()
      } else{
        paste(file, "NOT DOWNLOADED!") %>% print()
      }
    } else{
      paste(file, "already exists in directory, not downloading again") %>% print()
    }
  }
}

# toggles the soft calibration routine either ON or OFF
toggle_sft <- function(path, switch){
  # file.cio modifications

  # read the file.cio in
  file.cio = readLines(con = paste0(path, "file.cio"))
  # grab the 22nd line
  file_cio_line_22 = file.cio[22]

  # split that line based of white space
  line22 = file_cio_line_22 %>% strsplit("\\s+") %>% unlist()

  if(switch == "on"){
    # replace the the column values 4,5,6 with the sft file names
    line22[4] = "codes.sft"
    line22[5] = "wb_parms.sf"
    line22[6] = "water_balanbce.sft"
  }

  if(switch == "off"){
    # replace the the column values 4,5,6 with the sft file names
    line22[4] = "null"
    line22[5] = "null"
    line22[6] = "null"
  }


  # merge them back together
  new_line22 = paste(line22, collapse = "   ")

  # replace the old line with the new line
  file.cio[22] = new_line22

  # write the modified file.cio
  writeLines(text = file.cio, con = paste0(path, "file.cio"))

  # codes.sft modifications:

  # read in the codes.sft file (warnings turned off because of "incomplete final line)
  codes.sft = readLines(con = paste0(path, "codes.sft"), warn = F)
  # split that line based of white space

  # find out what the column index is of HYD_HRU
  line2 = codes.sft[2] %>% strsplit("\\s+") %>% unlist()
  hyd_hru_col_index = which(line2=="HYD_HRU")

  line3 = codes.sft[3] %>% strsplit("\\s+") %>% unlist()

  if (switch == "on") {
    # Edit ‘codes.sft’ file by changing the “n” to “y” in the first column.
    line3[hyd_hru_col_index] = "y"
  }

  if (switch == "off") {
    # Edit ‘codes.sft’ file by changing the “y” to “n” in the first column.
    line3[hyd_hru_col_index] = "n"
  }

  # merge line 3 back together
  line3 = paste(line3, collapse = "   ")

  # and apply it to the file
  codes.sft[3] = line3

  # write the modified codes.sft file
  writeLines(text = codes.sft, con = paste0(path, "codes.sft"))

  if (switch == "on") {
    print("soft cal enabled")
  }
  if (switch == "off") {
    print("soft cal disabled")
  }
}

# The following is only required due to the poor formatting of the SWAT output
read_wb_aa <- function(path){
  # read the wb_aa file from its PATH
  basin_wb_aa = fread(paste0(path, "basin_wb_aa.txt"), fill = TRUE, )

  # change the column names to those of the second row in the text file.
  colnames(basin_wb_aa) <- basin_wb_aa %>% slice(2) %>% unlist(., use.names = F)

  # some columns are not given a name (thanks..) need to figure out which ones
  # they are...
  no_name_columns = which(colnames(basin_wb_aa) == "")

  # .. and add placeholder names for the last n columns which did not get a name
  # if we don't do this, dplyr gets angry in the next line
  colnames(basin_wb_aa)[no_name_columns] <- letters[1:length(no_name_columns)]

  # remove rows 1 to 3, as they don't contain any real data
  basin_wb_aa <- basin_wb_aa %>% filter(!row_number() %in% c(1:3))

  # Unite the last three rows into one string (as they were intended to be)
  # the new column will be named description and describes what the softcal
  # algorithm did in that step. (So its probably important).
  # the united columns are separated by a space (trailing space exists now)
  basin_wb_aa = tidyr::unite(data = basin_wb_aa,
                             col = description,
                             no_name_columns,
                             sep = " ")

  # figure out which columns should be a double and not a string
  # (everything except for name and description columns)
  dbl_cols = basin_wb_aa %>% colnames()
  dbl_cols= dbl_cols[! dbl_cols %in% c("name","description")]
  # and convert them to numeric
  basin_wb_aa = basin_wb_aa %>% mutate_at(dbl_cols, as.numeric)

  return(basin_wb_aa %>% tibble())
}

# Modify ‘water_balance.sft’ with your values for fractions. Only the values under the WYR and
# BFR columns need to be modified (see Soft data). Make sure to count the columns, as the file
# is a free-format. Save the edits.
modify_wb_parms <- function(path) {

  # ask user for parameter values (could be changed to whatever method)
  WYLD_PCP_Ratio = readline(prompt = "Enter your value for WYLD_PCP_Ratio: ") %>% as.numeric()
  Subsurface_WYLD_Ratio = readline(prompt = "Enter your value for Subsurface_WYLD_Ratio: ") %>% as.numeric()


  # read the water balance file
  water_balance_sft = readLines(paste0(path, "water_balance.sft"))

  # extract the line with the column names
  line5 = water_balance_sft[5] %>% strsplit("\\s+") %>% unlist()

  # locate the desired parameters
  WYLD_PCP_Ratio_INDEX = which(line5 == "WYLD_PCP_Ratio")
  Subsurface_WYLD_Ratio_INDEX = which(line5 == "Subsurface_WYLD_Ratio")

  # extract the line with the parameter values
  line6 = water_balance_sft[6] %>% strsplit("\\s+") %>% unlist()

  # change the values with the user given values
  line6[WYLD_PCP_Ratio_INDEX] = WYLD_PCP_Ratio
  line6[Subsurface_WYLD_Ratio_INDEX] = Subsurface_WYLD_Ratio

  # merge them back together
  line6 = paste(line6, collapse = "   ")

  # add the modified parameter line back to the text file
  water_balance_sft[6] = line6

  # and write it
  writeLines(text = water_balance_sft, con = paste0(path, "water_balance.sft"))

  paste("water balance parameters updated with values:",WYLD_PCP_Ratio, Subsurface_WYLD_Ratio ) %>% print()

}


# enables the soft calibration routine
toggle_sft(path, switch = "on")
# downloads any missing sft file
download_sft_files(path)
# modify the wb parms
modify_wb_parms(path)
# reads the results of the wb soft calibration
df = read_wb_aa(path)
# disables the soft-calibration routine
toggle_sft(path, switch = "off")







library(ggplot2)
ggplot(df)+geom_col(mapping = aes(x = description, y = wateryld ))

df %>% tibble()
