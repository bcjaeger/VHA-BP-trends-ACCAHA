

library(tidyverse)

inverse_logit <- function(x) exp(x) / (1 + exp(x))

relu <- function(x, thresh = 0){
  x[x<thresh] <- 0
  x
}

cohort_make <- function(cohort_n, cohort_year){

  data_init <- tibble(
    bp_sys_preindex_mmhg = rnorm(n = cohort_n, mean = 150, sd = 15),
    bp_sys_postindex_mmhg = rnorm(n = cohort_n, mean = 140, sd = 15),
    bp_dia_preindex_mmhg = rnorm(n = cohort_n, mean = 90, sd = 10),
    bp_dia_postindex_mmhg = rnorm(n = cohort_n, mean = 85, sd = 10),
    age_years = rnorm(n = cohort_n, mean = 65, sd = 10),
    pdc = sample(x = c('low',
                       'medium',
                       'high'),
                 size = cohort_n,
                 replace = TRUE),
    sex = sample(c('male', 'female'), 
                 size = cohort_n,
                 prob = c(0.90, 0.10), 
                 replace = TRUE),
    race = sample(c('white', 'black', 'other'),
                  size = cohort_n,
                  prob = c(0.80, 0.10, 0.10),
                  replace = TRUE),
    date_preindex = sample(
      seq(as.Date(paste0(cohort_year, '/11/13')), 
          as.Date(paste0(as.numeric(cohort_year)+1, '/11/12')), 
          by="day"),
      size = cohort_n,
      replace = TRUE
    )
  ) %>% 
    mutate(
      date_postindex = date_preindex + 
        round(runif(n = cohort_n, min = 180, max = 365)),
      postindex_days_after_pandemic = as.numeric(
        date_preindex + 180 - as.Date('2020-03-12')
      ),
      postindex_days_after_pandemic = pmax(0, postindex_days_after_pandemic)
    )
  
  data_miss <- data_init %>% 
    mutate(
      miss_prob = inverse_logit(
        -2 + # intercept
          relu(age_years - 55) * 0.08 + 
          postindex_days_after_pandemic * 0.01 + 
          (sex == 'female') * (-0.5) + 
          (race == 'white') * (-0.25)
      ),
      miss_status = rbinom(n = cohort_n,
                           prob = miss_prob,
                           size = 1),
      bp_sys_postindex_mmhg = if_else(condition = miss_status == 1,
                              true = NA_real_,
                              false = bp_sys_postindex_mmhg),
      bp_dia_postindex_mmhg = if_else(condition = is.na(bp_sys_postindex_mmhg),
                              true = NA_real_,
                              false = bp_dia_postindex_mmhg)
    ) %>% 
    select(-miss_prob, -miss_status)
  
  list(data_complete = data_init,
       data_missing = data_miss)
  
}

cohort_bind <- function(...){
  
  bind_rows(list(...), .id = 'cohort') %>% 
    mutate(
      cohort = as.numeric(cohort),
      # cohort effect
      bp_sys_preindex_mmhg = bp_sys_preindex_mmhg - cohort/4,
      bp_sys_postindex_mmhg = bp_sys_postindex_mmhg + cohort/4,
      bp_dia_preindex_mmhg = bp_dia_preindex_mmhg - cohort/8,
      bp_dia_postindex_mmhg = bp_dia_postindex_mmhg + cohort/8,
      
      # interaction b/t PDC and ACC AHA BP guideline for BP control
      bp_sys_postindex_mmhg = case_when(
        cohort_grp == 'pre_accaha' & pdc == 'high' ~ bp_sys_postindex_mmhg - 12,
        cohort_grp == 'post_accaha' & pdc == 'high' ~ bp_sys_postindex_mmhg - 6,
        cohort_grp == 'pre_accaha' & pdc == 'medium' ~ bp_sys_postindex_mmhg - 8,
        cohort_grp == 'post_accaha' & pdc == 'medium' ~ bp_sys_postindex_mmhg - 4,
        TRUE ~ bp_sys_postindex_mmhg
      ),
      bp_sys_postindex_mmhg = case_when(
        cohort_grp == 'pre_accaha' ~ bp_sys_postindex_mmhg - 6,
        cohort_grp == 'post_accaha' ~ bp_sys_postindex_mmhg - 2,
        TRUE ~ bp_sys_postindex_mmhg
      ),
      bp_sys_postindex_mmhg = if_else(
        race == 'black',
        bp_sys_postindex_mmhg + 4,
        bp_sys_postindex_mmhg
      ),
      bp_dia_postindex_mmhg = case_when(
        cohort_grp == 'pre_accaha' & pdc == 'high' ~ bp_dia_postindex_mmhg - 6,
        cohort_grp == 'post_accaha' & pdc == 'high' ~ bp_dia_postindex_mmhg - 3,
        cohort_grp == 'pre_accaha' & pdc == 'medium' ~ bp_dia_postindex_mmhg - 4,
        cohort_grp == 'post_accaha' & pdc == 'medium' ~ bp_dia_postindex_mmhg - 2,
        TRUE ~ bp_dia_postindex_mmhg
      ),
      bp_dia_postindex_mmhg = case_when(
        cohort_grp == 'pre_accaha' ~ bp_dia_postindex_mmhg - 3,
        cohort_grp == 'post_accaha' ~ bp_dia_postindex_mmhg - 1,
        TRUE ~ bp_dia_postindex_mmhg
      ),
      bp_dia_postindex_mmhg = if_else(
        race == 'black',
        bp_dia_postindex_mmhg + 2,
        bp_dia_postindex_mmhg
      ),
      bp_control_postindex_jnc = as.numeric(
        bp_sys_postindex_mmhg < 140 & bp_dia_postindex_mmhg < 90
      ),
      bp_control_postindex_acc  = as.numeric(
        bp_sys_postindex_mmhg < 130 & bp_dia_postindex_mmhg < 80
      )
    ) %>% 
    relocate(cohort_grp, .after = cohort) %>% 
    mutate(cohort = as.character(cohort))
  
}

cohorts <- map(
  .x = as.character(seq(2014, 2019)),
  .f = ~cohort_make(cohort_n = 5000, cohort_year = .x)
) 

vha_complete <- cohort_bind(map(cohorts, 'data_complete'))
vha_missing <- cohort_bind(map(cohorts, 'data_missing'))

write_csv(vha_complete, 'data/vha_sim_complete.csv')
write_csv(vha_missing, 'data/vha_sim_missing.csv')




