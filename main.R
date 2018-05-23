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

#### tidy models ####
mods <- mods_raw

mods %<>%
  lapply(function(df) {
    mod_imp_raw <- df
    mod_imp_raw %<>%
      filter(grepl("vocab-learnt-this|correct-vocab|original-word", X1))
    
    filter_match <- function(match) {
      x <- mod_imp_raw %>%
        filter(grepl(match, X1)) 
      x$X1
    }
    
    var_names <- c("phon_corr","phon_inc","phon_inc_or",
                   "phon_inc_cum","phon_inc_cum_freq",
                   "phon_inc_or_cum", "phon_inc_or_cum_freq")
    matches <- c(" vocab-learnt-this-stage","incorrect-vocab-learnt-this-stage",
                 "original-word-for-incorrect-vocab","all-incorrect-vocab-so-far ",
                 "all-incorrect-vocab-so-far-freq-of-use", "all-original-words-so-far ",
                 "all-original-words-so-far-freq-of-use")
    
    # split variables in different coloums
    mod_imp <- tibble(V1 = rep(NA,240)) # 240 = n(babies)*n(stages)
    for (i in seq_along(var_names)) {
      mod_imp %<>%
        mutate(!!var_names[i] := filter_match(matches[i])) %>%
        select(-one_of("V1"))
    }
    
    # create baby and section vars and clean remaining variables
    mod_imp %<>%
      separate(phon_corr, c("baby_section", "phon_corr"), sep = " vocab-learnt-this-stage ") %>%
      separate(baby_section, c("baby", "section"), sep=" ") %>%
      mutate(baby = tolower(baby), section = as.numeric(section)) %>%
      mutate_at(vars(phon_corr:phon_inc_or_cum_freq), 
                funs(str_replace_all(., "^[0-9]* |^[A-Za-z]* [0-9]* [A-Za-z-]* [0-9]* ", "") %>%
                       str_split(" ") %>%
                       rapply(function(x) ifelse(x=="",NA,x), how = "replace"))) %>%
      mutate_at(vars(phon_inc_cum_freq, phon_inc_or_cum_freq),
                funs(lapply(., as.numeric)))
    
    mod_imp
  })

