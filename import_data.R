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

# list of dropped variables:
# "NW","Einleitung","Bemerkung","Indikation","Vorerkrankung","VE","auf Neo2"
# "SaO2 , Austreibungsphase 2[min] , BZ2 , Temp2 , pH2 ven , Reifegrad2 , Rea2 ,
# Apgar1min2 , Apgar5min2 , Apgar 10min2 , pH2 art , BE2 , Gew2 , Größe2 , Geschlecht2 ,
# O2;2 , Maske2 , Intub2 , Volu2 , Pufferung2"


rowdata <- read_excel("input/rohdata.xls") %>% as_tibble() %>%
  rename(ID = 1) %>% mutate(ID = as.factor(ID)) 

vars_2ndchild <- enframe(map_dbl(rowdata, ~ mean(is.na(.x)))) %>%
  arrange(desc(value)) %>% slice(1:20) %>% pull(name)

data <- rowdata %>% 
  select(-any_of(c("NW","Einleitung","Bemerkung","Indikation","Vorerkrankung","VE","auf Neo2",vars_2ndchild))) %>% 
  separate(SSW, into = c("week","day")) %>% 
  mutate(
    across(.cols = c(week,day), ~ as.integer(.x)),
    across(.cols = c(week,day), ~ replace_na(.x, 0)),
    pregnancy_days = week + day
  ) %>% 
  select(-c(week,day)) 

rm(vars_2ndchild)

data <- data %>% 
  mutate(
    Hb = round(as.double(ifelse(Hb == "12,2,","12.2",Hb)),1),
    stillen =  replace_na(as.integer(na_if(stillen, "?")),1)
  ) %>% 
  rename(Reifegrad = `Reifeegrad1 (0=reif/1=unreif/2=überreif`) %>% 
  mutate_at(vars(Reifegrad),
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
    Blutverlust = na_if(as.integer(parse_number(Blutverlust)),7),
    Trapanal = ifelse(is.na(Trapanal),0,1)
  )
    

missing_values <- list(
rename(enframe(map_dbl(filter(data,Narkoseform=="spontan"),~mean(is.na(.x)))),spontan=value),
rename(enframe(map_dbl(filter(data,Narkoseform=="ITN"),~mean(is.na(.x)))),itn=value),
rename(enframe(map_dbl(filter(data,Narkoseform=="combined"),~mean(is.na(.x)))),combined=value),
rename(enframe(map_dbl(filter(data,Narkoseform=="SPA"),~mean(is.na(.x)))),spa=value),
rename(enframe(map_dbl(filter(data,Narkoseform=="PDA"),~mean(is.na(.x)))),pda=value)
) %>% 
reduce(full_join, by = "name") %>% 
mutate(pmax = pmax(spontan,itn,combined,spa,pda), pmin = pmin(spontan,itn,combined,spa,pda)) %>% 
filter(pmax > 0) %>% 
mutate(across(.cols = spontan:last_col(), ~ round(.x, digits = 2))) %>% 
arrange(desc(pmin)) 

data <- select(data, -any_of(filter(missing_values,pmin>0.5)$name))

missing_values %>% select(-c(pmax:pmin)) %>% 
  gt(rowname_col = "name") %>% 
  fmt_percent(columns = 2:6, decimals = 0)







select(-temp2, - reifegrad2, -nw, -vorerkrankung, -trapanal, -ve, -indikation, -bemerkung, -einleitung) %>% 
  mutate(stillen = as.integer(stillen), blutverlust = as.integer(blutverlust)) %>% 
  rename(reifegrad = reifeegrad1_0_reif_1_unreif_2_uberreif) %>% 
  replace_na(list(einleitung = "keine", narkoseform = "keine", reifegrad = "0")) %>% 
  mutate_at(vars(reifegrad),
            ~ case_when(. == "1" ~ -1,
                      . == "unreif" ~ -1,
                      . == "überreif" ~ 1,
                      . == "2" ~ 1,
                      TRUE ~ 0
                      ) %>% as.numeric()
            ) %>% 
  mutate_at(vars(narkoseform),
            ~ case_when(. == "Spa" ~ "SPA",
                        . == "keine" ~ "natural",
                        . == "PDA" ~ "PDA",
                        . == "ITN" ~ "general_anae",
                        TRUE ~ "combined")
            ) %>% 
  separate(ssw, c("week", "day"), remove = TRUE) %>% 
    mutate_at(vars(week,day), as.integer) %>% 
    mutate(pregnancy_duration = 7 * week + day) %>%
    select(-c(week,day)) %>% 
  mutate(bmi = round(gewicht/(grosse^2),1)) %>% 
  mutate_at(vars(austreibungsphase_1_min, p_h1_ven , be , hb , zunahme_in_ss , temp1 , grosse1 , gravida , gewicht, bmi, para ,
                 nikotin_anzahl, pregnancy_duration, stillen, blutverlust), 
            ~ replace_na(., median(., na.rm = TRUE))) 



data <- rowdata %>% select(-one_of(
                              rowdata %>% check_struct() %>% filter(NAs > 50) %>% pull(names))
                           ) 

#data %>% skimr::skim() %>% as_tibble() %>% filter(n_missing > 0) %>% pull(skim_variable) %>% as.character() %>% str_c(collapse = ",") # columns with small nr of missing values

data %>% plot_missing()

library(openxlsx)
data %>% openxlsx::write.xlsx(file = here("input/process_data.xlsx"))


# manual edit
data <- read_excel("input/process_data.xlsx") %>% 
  mutate(hb = as.integer(hb)) %>% 
  mutate_if(is.numeric, list(~ replace(., is.na(.), median(., na.rm = TRUE)))) %>% 
  select(-c(akrinor_ja_1_nein_0,atropin_ja_1_nein_0))


corr_RF <- function(df, iter) {
  set.seed(111)
  require(Boruta)
  require(dplyr)
  require(furrr)
  
  plan(multiprocess)
  
  df <- stats::na.omit(df) %>% as_tibble() %>% ## remove zero variance columns
    select(
      -one_of(
        summarise_all(df, list(~n_distinct(.))) %>%
          pivot_longer(everything()) %>%
          filter(value <= 1) %>% pull(name)
      )
    )
  # use calibration1 and calibration2 to give a high value of correlation to scale upon, this is not performed in the boruta loop
  df <- dplyr::mutate(df, calibration1 = 1, calibration2 = 1, calibration3 = runif(nrow(df),min = 0, max = 1), calibration4 = runif(nrow(df),min = 0, max = 1))
  # Boruta specific
  df <- df %>% 
    names() %>% 
    future_map_dfr(
      ~ attStats(
        Boruta::Boruta(
          formula(
            paste0('`', ., '` ~ ', paste(names(df), collapse = " + "))
          ),
          data = df,
          mcAdj = TRUE, doTrace = 0, holdHistory = TRUE, 
          pValue = 0.01, # min(1/(nrow(df))^2 , 0.01)
          maxRuns = iter
        )
      ) %>% 
        TentativeRoughFix() %>% 
        rownames_to_column() %>% 
        mutate(Score = ifelse(decision == "Rejected", 0, medianImp * normHits)) %>%
        dplyr::select(rowname, Score) %>% 
        pivot_wider(names_from = rowname, values_from = Score),
      .progress = TRUE
    ) %>%
    # postprocessing 
    as_tibble() %>% 
    dplyr::mutate(target = colnames(df)) %>% 
    pivot_longer(cols = -target, names_to = "feature") %>% 
    dplyr::mutate(
      value =  as.integer(100*value/max(value, na.rm = TRUE)), 
      ident = case_when(
        target == feature ~ "x",
        target %in% c("calibration1","calibration2","calibration3","calibration4") ~ "x", # remove the calibration columns
        feature %in%  c("calibration1","calibration2","calibration3","calibration4") ~ "x",
        TRUE ~ "v" # preserve the rest
      )
    ) %>%
    dplyr::filter(ident == "v") %>% # the scaling is from 0-100
    dplyr::select(-ident) %>%  # remove the columns that are identical (feature == target)
    mutate(pair = paste(pmin(target,feature), pmax(target,feature), sep = " ~ ")) 
  
  df <- dplyr::full_join(df,
                         group_by(df, pair) %>% 
                           summarise(value_min = min(value),
                                     value_mean = mean(value),
                                     value_max = max(value)) %>% 
                           ungroup(),
                         by = "pair" 
  ) %>% 
    filter(value_max >0) %>% 
    arrange(pair)
  
  return(df)
}



