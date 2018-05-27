# loading libraries
lib <- c("magrittr", "tidyverse", "Publish")
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

filter_match <- function(df, match) {
  x <- df %>%
    filter(grepl(match, X1)) 
  x$X1
}

mods %<>%
  lapply(function(df) {
    mod_imp_raw <- df
    mod_imp_raw %<>%
      filter(grepl("vocab-learnt-this|correct-vocab|original-word", X1))
    
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
        mutate(!!var_names[i] := filter_match(mod_imp_raw, matches[i])) %>%
        select(-one_of("V1"))
    }
    
    # create baby and section vars and clean remaining variables
    mod_imp %<>%
      separate(phon_corr, c("baby_section", "phon_corr"), sep = " vocab-learnt-this-stage ") %>%
      separate(baby_section, c("baby", "section"), sep=" ") %>%
      mutate(baby = tolower(baby), section = as.numeric(section)) %>%
      mutate_at(vars(phon_corr:phon_inc_cum, phon_inc_or_cum, phon_inc_or_cum_freq), 
                funs(str_replace_all(., "^[0-9]* |^0|^[A-Za-z]* [0-9]* [A-Za-z-]* [0-9]* |^[A-Za-z]* [0-9]* [A-Za-z-]* [0-9]*$|^[A-Za-z]* [0-9]* [A-Za-z-]*$", "") %>%
                       str_split(" ") %>%
                       rapply(function(x) ifelse(x %in% c("", "NA"),NA,x), how = "replace"))) %>%
      mutate_at(vars(phon_inc_cum_freq),
                funs(str_replace_all(., "^[0-9]* |^0|^[A-Za-z]* [0-9]* [A-Za-z-]* |^[A-Za-z]* [0-9]* [A-Za-z-]*$", "") %>%
                       str_split(" ") %>%
                       rapply(function(x) ifelse(x %in% c("", "NA"),NA,x), how = "replace"))) %>%
      mutate_at(vars(phon_inc_cum_freq, phon_inc_or_cum_freq),
                funs(lapply(., as.numeric)))
    
    mod_imp
  })

#### mods summary (n corr-inc words & prop inc words) ####
length_inlist <- function(x) {
  # unlist the list of stages and return total number of words across participants
  x %>%
    unlist() %>%
    na.omit() %>%
    length()
}

# load mot_uni 
load("mot_uni.RData")

n_phon_mot <- mot_uni %>%
  select(phon) %>%
  unlist() %>%
  na.omit() %>%
  length()

# calculate number of correct and incorrect words, and proportion of incorrect words
mods_summary <- filenames %>%
  lapply(function(x) {
    phon_corr_tokens <- mods[[x]]$phon_corr %>%
      length_inlist()
    phon_inc_tokens <- mods[[x]]$phon_inc %>%
      length_inlist()
    prop_phon_inc <- round(phon_inc_tokens/(phon_corr_tokens+phon_inc_tokens),2) 
    
    tibble(mod = x,
           phon_corr_tokens,
           phon_inc_tokens,
           prop_phon_inc)
  }) %>%
  do.call(what = rbind) %>%
  mutate(`prop_phon_inc (mot)` = round(phon_inc_tokens/n_phon_mot, 2))

# prop inc words (dividing by mot tot phonemes)
mods_summary_stage <- mods %>%
  sapply(function(x) {
    x %>%
      group_by(section) %>%
      summarise(phon_inc_prop = phon_inc %>%
                  unlist() %>%
                  na.omit() %>%
                  (function(x) {
                    length(x)/n_phon_mot
                  })) %>%
      mutate(phon_inc_prop_cum = cumsum(phon_inc_prop) %>%
               round(2)) %>%
      select(phon_inc_prop_cum)
  }) %>%
  unlist() %>%
  matrix(ncol = 20, byrow = T) %>%
  as_tibble() %>%
  (function(x) {
    colnames(x) <- 1:20
    x
  }) %>%
  mutate(models = filenames) %>%
  select(models, `1`:`20`)

#### pn & pp for original inc words ####
# import iphod for mot-chi-mod and old models
online_imp <- read_tsv("online_imp.txt") %>%
  arrange(phon)

# function to check if all the words are in online (iphod)
iphod_check <- function(var, save = F) {
  words_imp <- mods %>%
    lapply(function(x) {
      x %>%
        select(var) %>% # as a string
        unlist()
    }) %>%
    unlist() %>%
    na.omit() %>%
    unique()
  
  words_missing <- sort(words_imp[!words_imp %in% online_imp$phon])
  
  if (save == F) {
    words_missing
  } else {
    write.table(words_missing,
                "words_imp_missing.txt",
                sep = "\t",
                quote = F,
                row.names = F,
                col.names = F)
  }
} # unused

# import plural nouns
plurals <- "plurals.txt" %>%
  read_tsv(col_names = F) %>%
  unlist()

# import online and create quartiles
online_qu <- "online_qu.txt" %>%
  read_tsv()

qu_pn <- quantile(online_qu$nei)[c(2,3,4)]
qu_pp <- quantile(online_qu$phon_prob, na.rm = T)[c(2,3,4)]

# select only phon_inc_or that is of interest
mods_iph <- mods %>%
  lapply(function(x) {
    x %>%
      select(baby, section, phon_inc_or) %>%
      # delete plurals
      mutate(phon_inc_or = phon_inc_or %>%
               lapply(function(y) {
                 y[which(!y %in% plurals)] %>%
                   sort() %>% # sort so that pp and pn will have the same order
                   na.omit() # delete NAs to not lose order in pp and pn
               })) %>%
      # assign pp and pn variables
      mutate(phon_inc_or_pn = phon_inc_or %>%
               sapply(function(y) {
                 ifelse(length(na.omit(y)) > 0, 
                        inner_join(tibble(phon = y), online_imp, by = "phon") %>% 
                          select(nei), NA)
               }),
             phon_inc_or_pp = phon_inc_or %>%
               sapply(function(y) {
                 ifelse(length(na.omit(y)) > 0, 
                         inner_join(tibble(phon = y), online_imp, by = "phon") %>% 
                           select(phon_prob), NA)
               })) %>%
      # cumulative percentage of words in each quartile
      mutate(pn_qu1 = sapply(phon_inc_or_pn, function(x) {length(x[which(x < qu_pn[1])])}),
             pn_qu2 = sapply(phon_inc_or_pn, function(x) {length(x[which(x >= qu_pn[1] & x < qu_pn[2])])}),
             pn_qu3 = sapply(phon_inc_or_pn, function(x) {length(x[which(x >= qu_pn[2] & x < qu_pn[3])])}),
             pn_qu4 = sapply(phon_inc_or_pn, function(x) {length(x[which(x >= qu_pn[3])])}),
             pp_qu1 = sapply(phon_inc_or_pp, function(x) {sum(x<qu_pp[1], na.rm = T)}),
             pp_qu2 = sapply(phon_inc_or_pp, function(x) {sum(x>=qu_pp[1] & x<qu_pp[2], na.rm = T)}),
             pp_qu3 = sapply(phon_inc_or_pp, function(x) {sum(x>=qu_pp[2] & x<qu_pp[3], na.rm = T)}),
             pp_qu4 = sapply(phon_inc_or_pp, function(x) {sum(x>=qu_pp[3], na.rm = T)})) %>%
      group_by(baby) %>%
      mutate(pn_qu1cum = c(cumsum(pn_qu1)),
             pn_qu2cum = c(cumsum(pn_qu2)),
             pn_qu3cum = c(cumsum(pn_qu3)),
             pn_qu4cum = c(cumsum(pn_qu4)),
             pp_qu1cum = c(cumsum(pp_qu1)),
             pp_qu2cum = c(cumsum(pp_qu2)),
             pp_qu3cum = c(cumsum(pp_qu3)),
             pp_qu4cum = c(cumsum(pp_qu4))) %>%
      group_by(baby, section) %>%
      mutate(pn_qu1perc = round(c(pn_qu1cum/sum(pn_qu1cum+pn_qu2cum+pn_qu3cum+pn_qu4cum)*100),1),
             pn_qu2perc = round(c(pn_qu2cum/sum(pn_qu1cum+pn_qu2cum+pn_qu3cum+pn_qu4cum)*100),1),
             pn_qu3perc = round(c(pn_qu3cum/sum(pn_qu1cum+pn_qu2cum+pn_qu3cum+pn_qu4cum)*100),1),
             pn_qu4perc = round(c(pn_qu4cum/sum(pn_qu1cum+pn_qu2cum+pn_qu3cum+pn_qu4cum)*100),1),
             pp_qu1perc = round(c(pp_qu1cum/sum(pp_qu1cum+pp_qu2cum+pp_qu3cum+pp_qu4cum)*100),1),
             pp_qu2perc = round(c(pp_qu2cum/sum(pp_qu1cum+pp_qu2cum+pp_qu3cum+pp_qu4cum)*100),1),
             pp_qu3perc = round(c(pp_qu3cum/sum(pp_qu1cum+pp_qu2cum+pp_qu3cum+pp_qu4cum)*100),1),
             pp_qu4perc = round(c(pp_qu4cum/sum(pp_qu1cum+pp_qu2cum+pp_qu3cum+pp_qu4cum)*100),1))
  })

#### cumulative frequency by stage ####
CI_df <- function(df, group_var, var_names, 
                  var1, var2, var3, var4, var5) {
  group_var <- enquo(group_var)
  var1 <- enquo(var1)
  var2 <- enquo(var2)
  var3 <- enquo(var3)
  var4 <- enquo(var4)
  var5 <- enquo(var5)
  
  df_mean <- df %>%
    group_by(!!group_var) %>%
    summarise(!!var_names[1] := mean(!!var1, na.rm = T),
              !!var_names[2] := mean(!!var2, na.rm = T)) %>%
    gather(word_type, !!var3, c(!!var4, !!var5))
  
  df_lower <- df %>%
    group_by(!!group_var) %>%
    summarise(!!var_names[1] := ci.mean(!!var1)$lower,
              !!var_names[2] := ci.mean(!!var2)$lower) %>%
    gather(word_type, CI_lower, c(!!var4, !!var5)) %>%
    select(-!!group_var)
  
  df_upper <- df %>%
    group_by(!!group_var) %>%
    summarise(!!var_names[1] := ci.mean(!!var1)$upper,
              !!var_names[2] := ci.mean(!!var2)$upper) %>%
    gather(word_type, CI_upper, c(!!var4, !!var5)) %>%
    select(-!!group_var)
  
  cbind(df_mean, `CI95%_lower` = df_lower$CI_lower, `CI95%_upper` = df_upper$CI_upper) %>%
    as_tibble()
}

mods_freq <- mods %>%
  lapply(function(x) {
    x %>%
      select(baby, section, phon_inc_cum_freq, phon_inc_or_cum_freq) %>%
      mutate(inc_freq_avg = phon_inc_cum_freq %>%
               sapply(function(y) {
                 mean(y, na.rm = T)
               }),
             inc_or_freq_avg = phon_inc_or_cum_freq %>%
               sapply(function(y) {
                 mean(y, na.rm = T)
               })) %>%
      CI_df(section,
            c("inc_freq_avg_stage","inc_or_freq_avg_stage"),
            inc_freq_avg, inc_or_freq_avg, freq_avg_stage,
            inc_freq_avg_stage, inc_or_freq_avg_stage)
  })

#### incorrect word replacement ####
mods_freq_repl <- mods %>%
  lapply(function(x) {
    x %>%
      select(baby, section, phon_inc_cum_freq, phon_inc_or_cum_freq) %>%
      rowwise() %>%
      mutate(inc_higher = sum((phon_inc_cum_freq - phon_inc_or_cum_freq) > 0),
             inc_or_higher = sum((phon_inc_cum_freq - phon_inc_or_cum_freq) < 0)) %>%
      ungroup() %>%
      CI_df(section, 
            c("inc_higher_avg_stage", "inc_or_higher_avg_stage"), 
            inc_higher, inc_or_higher, higher_avg_stage, 
            inc_higher_avg_stage, inc_or_higher_avg_stage)
  })