# prepare github

# library(usethis)
# ?use_github
# edit_r_environ()
# use_github(protocol = 'https', auth_token = Sys.getenv("GITHUB_PAT"))
# ##

library(here)
library(readxl)
library(tidyverse)
library(skimr)


rowdata <- read_excel("input/rohdata.xls") %>% as_tibble() %>%
  janitor::clean_names() %>% 
  rename(ID = 1, Narkoseform = narkoseform) %>% mutate(ID = as.factor(ID)) 


data <- rowdata %>% 
  select(-c(auf_neo2,ve)) %>% 
  separate(ssw, into = c("week","day")) %>% 
  mutate(
    across(.cols = c(week,day), ~ as.integer(.x)),
    across(.cols = c(week,day), ~ replace_na(.x, 0)),
    pregnancy_days = week + day
  ) %>% 
  select(-c(week,day)) %>% 
  mutate(
    hb = round(as.double(ifelse(hb == "12,2,","12.2",hb)),1),
    stillen =  replace_na(as.integer(na_if(stillen, "?")),1)
  ) %>% 
  rename(reifegrad = reifeegrad1_0_reif_1_unreif_2_uberreif) %>% 
  mutate_at(vars(reifegrad),
            ~ case_when(. == "1" ~ -1,
                        . == "unreif" ~ -1,
                        . == "überreif" ~ 1,
                        . == "2" ~ 1,
                        TRUE ~ 0
            ) %>% as.numeric()
  ) %>% 
  mutate_at(vars(Narkoseform),
          ~ case_when(
            is.na(.) ~ "spontan",
            . == "Spa" ~ "SPA",
            . == "SPA" ~ "SPA",
            . == "PDA" ~ "PDA",
            . == "ITN" ~ "ITN",
            . == "PDA/SPA" ~ "SPA",
            TRUE ~ "combined")
  ) %>% 
  mutate(
    Narkoseform = as.factor(Narkoseform),
    blutverlust = na_if(as.integer(parse_number(blutverlust)),7)
  ) %>% 
  select(ID, Narkoseform, everything())

# filter out variables with too many missing values or small variance

replace_func <- function(df){
  df %>% mutate(across(where(is.numeric), ~ replace_na(.x, median(.x, na.rm = TRUE)) ))
}

filter_function <- function(df, group, min_complete, min_uniques){
  
  too_many_missings <- df %>% group_by({{group}}) %>% 
    summarise_all(~ 1-mean(is.na(.x))) %>% # the complete rate per group
    summarise_if(is.numeric, ~ min(.x)) %>% # the minimum complete rate per group
    pivot_longer(everything()) %>% filter(value <= min_complete) %>% # the variables with less then 75% completeness in >= 1 group
    pull(name)
  
  low_variance_cols <- df %>% group_by({{group}}) %>% 
    summarise_all(~ length(unique(na.omit(.x)))) %>% 
    summarise_if(is.numeric, ~ min(.x)) %>% 
    pivot_longer(everything()) %>% filter(value <= min_uniques) %>% 
    pull(name)
  
  ans <- df %>% select(-any_of(c(too_many_missings,low_variance_cols)))
  
  return(ans)
  
}

set.seed(111)
data_ira <- data %>% filter(Narkoseform %in% c("ITN","PDA","SPA")) %>% 
  filter_function(group = Narkoseform, min_complete = 0.75, min_uniques = 1) %>% 
  group_split(Narkoseform) %>%
    map(~ replace_func(.x)) %>% map(~ sample_frac(.x)) %>%
  map_dfr(~ head(.x, 200)) %>% 
  select(!where(is.character)) %>% 
  select(-ID) %>% as.data.frame()


library(Boruta)
Boruta_search <- Boruta::Boruta(Narkoseform ~ . ,
                                data = data_ira %>% mutate(Narkoseform = as.factor(Narkoseform)),
                                maxRuns = 250, doTrace = 1)
plot_Boruta(Boruta_search, predictors = names(data_ira),text_size = 8)
harvest_trees(Boruta_search)


CB_itn <- CrossBoruta( filter(data_ira,Narkoseform == "ITN")[-1], maxRuns = 200, doTrace = 1,
                       withTentative = FALSE,withNormImp = TRUE,withSpearman = TRUE)


nodes_to_remove <- c("atropin_ja_1_nein_0","infusionsvolum","allergie","gemini","prim_sectio",
                     "atropin","akrinor_ja_1_nein_0","hb","akrinor","anzahl_der_kinder",
                     "sa_o2", "sa_o2_vor_massnahmen", "sa_os_max" ,"sa_o2_min",
                     "nsu","ais","alter","geburten_0_1kind_1_2kinder_2_3kinder","stillen",
                     "nikotin","nikotin_anzahl","geschlecht1_1_mannlich_2_weiblich")

CB_itn %>% plot_symCB(remove_nodes = nodes_to_remove, maxwidth = 1.5, min_metric = 0, max_metric = 50) 

CB_pda <- CrossBoruta( filter(data_ira,Narkoseform == "PDA")[-1],
                       maxRuns = 200, doTrace = 1,
                       withTentative = FALSE,withNormImp = TRUE,withSpearman = TRUE)

CB_pda %>% plot_symCB(maxwidth = 1.5)


CB_spa <- CrossBoruta( filter(data_ira,Narkoseform == "SPA")[-1],
                       maxRuns = 200, doTrace = 1,
                       withTentative = FALSE,withNormImp = TRUE,withSpearman = TRUE)







