# loading libraries
lib <- c("magrittr", "tidyverse")
lapply(lib, require, character.only = TRUE)
rm(lib)


#### importing models ####
# import files
path <- "/Users/francesco/Dropbox/tirocinio\ notts/hearing\ study/Analyses/models_20_05_18"
filelist <-  path %>%
  list.files(recursive = T)

# create filenames
filenames <- (filelist %>%
                str_match("Vocab-by-stage-(.*).txt"))[,2] %>%
  tolower()

# create list of models dataframes
mods_raw <- filelist %>%
  # import all dataframes
  lapply(function(x) {
    path %>%
      paste(x, sep = "/") %>%
      read_tsv(col_names = F)
  }) %>% 
  (function(x) {
    # rename list dataframes
    names(x) <- filenames
    x
  }) 

