

# # create generic BP cohort data with dropout ----
# 
# library(tidyverse)
# 
# results <- matrix(nrow = 10, ncol = 3)
# colnames(results) <- c('cc', 'ipw', 'mi')
# 
# # mimics the sample size for cohort 6
# cohort_n <- 12474
# 
# set.seed(1)
# 
# for(i in seq(nrow(results))){
#   
#   data_generic <- tibble(
#     sbp_preindex = rnorm(n = cohort_n, mean = 150, sd = 15),
#     sbp_postindex = rnorm(n = cohort_n, mean = 140, sd = 15),
#     dbp_preindex = rnorm(n = cohort_n, mean = 90, sd = 10),
#     dbp_postindex = rnorm(n = cohort_n, mean = 85, sd = 10),
#     age_preindex = rnorm(n = cohort_n, mean = 65, sd = 10),
#     sex = sample(c('male', 'female'), 
#                  size = cohort_n,
#                  prob = c(0.90, 0.10), 
#                  replace = TRUE),
#     race = sample(c('white', 'black', 'other'),
#                   size = cohort_n,
#                   prob = c(0.80, 0.10, 0.10),
#                   replace = TRUE),
#     date_preindex = sample(
#       seq(as.Date('2019/11/13'), as.Date('2020/11/12'), by="day"),
#       size = cohort_n,
#       replace = TRUE
#     ),
#     date_postindex = date_preindex + 
#       round(runif(n = cohort_n, min = 180, max = 365)),
#     postindex_begins_wrt_pandemic = as.numeric(
#       date_preindex + 180 - as.Date('2020-03-12')
#     )
#   )
#   
#   # induce missing data for people who are older and who have 
#   # later pre-index dates.
#   
#   inverse_logit <- function(x) exp(x) / (1 + exp(x))
#   
#   relu <- function(x, thresh = 0){
#     x[x<thresh] <- 0
#     x
#   }
#   
#   data_generic_miss <- data_generic %>% 
#     mutate(
#       miss_prob = inverse_logit(
#         -2 + # intercept
#           relu(age_preindex - 55) * 0.08 + 
#           postindex_begins_wrt_pandemic * 0.01 + 
#           (sex == 'female') * (-0.5) + 
#           (race == 'white') * (-0.25) #+ 
#           # relu(sbp_preindex - 120) * (-0.005)
#       ),
#       miss_status = rbinom(n = cohort_n,
#                            prob = miss_prob,
#                            size = 1),
#       sbp_postindex = if_else(condition = miss_status == 1,
#                               true = NA_real_,
#                               false = sbp_postindex),
#       dbp_postindex = if_else(condition = is.na(sbp_postindex),
#                               true = NA_real_,
#                               false = dbp_postindex)
#     )
#   
#   ggplot(data_generic_miss) +
#     aes(x = date_preindex, y = as.numeric(is.na(sbp_postindex))) +
#     geom_smooth()
#   # 
#   # ggplot(data_generic_miss) +
#   #   aes(x = sbp_preindex, y = as.numeric(is.na(sbp_postindex))) +
#   #   geom_smooth()
#   
#   # hist(data_generic_miss$miss_prob)
#   
#   n_junk_variables <- 20
#   
#   for(j in seq(n_junk_variables)){
#     
#     data_generic_miss[[paste("junk", j, sep = '_')]] <- rnorm(n = cohort_n)
#     
#   }
#   
#   # IPW (inverse probability of dropout weights) ----
#   
#   # STEP 1; fit a model to predict missing data 
#   library(glmnet)
#   
#   y <- if_else(condition = is.na(data_generic_miss$sbp_postindex),
#                true = 1,
#                false = 0)
#   
#   mean(y)
#   
#   x <- data_generic_miss %>%
#     mutate(
#       male = as.numeric(sex == 'male'),
#       race_black = as.numeric(race == 'black'),
#       race_other = as.numeric(race == 'other')
#     ) %>%
#     select(-race, -sex, 
#            -date_preindex,
#            -contains('miss'), 
#            -ends_with("postindex")) %>%
#     as.matrix()
#   
#   fit_cv <- cv.glmnet(x = x,
#                       y = y,
#                       nfolds = 10,
#                       family = 'binomial',
#                       alpha = 1)
#   
#   # plot(fit_cv)
#   
#   fit_final <- glmnet(x = x,
#                       y = y,
#                       family = 'binomial',
#                       lambda = fit_cv$lambda.1se)
#   
#   coef(fit_final) %>%
#     as.matrix() %>%
#     as_tibble(rownames = 'variable') %>%
#     filter(s0 != 0)
#   
#   # STEP 2; create, trim, and standardize weights 
#   
#   data_miss_ipw <- data_generic_miss %>% 
#     mutate(prob_ipw = predict(fit_final, 
#                               newx = x, 
#                               type = 'response'),
#            prob_ipw = as.numeric(prob_ipw),
#            weight_ipw = 1 / (1-prob_ipw)) %>% 
#     mutate(weight_ipw = pmax(weight_ipw, quantile(weight_ipw, 0.005)),
#            weight_ipw = pmin(weight_ipw, quantile(weight_ipw, 0.995)),
#            weight_ipw = weight_ipw / mean(weight_ipw)) %>% 
#     select(-starts_with('junk'))
#   
#   # weights should sum to the original sample size
#   sum(data_miss_ipw$weight_ipw)
#   
#   # true mean of postindex BP
#   truth <- mean(data_generic$sbp_postindex)
#   
#   # mean if we ignore missing data
#   estimate_cc <- mean(data_generic_miss$sbp_postindex, na.rm = TRUE)
#   
#   # mean using IPCW 
#   estimate_ipw <- weighted.mean(x = data_miss_ipw$sbp_postindex, 
#                                 w = data_miss_ipw$weight_ipw,
#                                 na.rm = TRUE)
#   
#   
#   # MI (multiple imputation) ----
#   
#   library(mice)
#   
#   data_mi <- data_generic_miss %>% 
#     select(-miss_status, -miss_status, -date_preindex) %>%
#     mutate(sex = factor(sex), race = factor(race)) %>% 
#     mice(method = 'pmm', printFlag = FALSE)
#   
#   estimate_mi <- data_mi %>% 
#     complete(action = 'all') %>% 
#     map_dbl(~ mean(.x$sbp_postindex)) %>% 
#     mean()
#   
#   # BIAS ----
#   
#   bias_cc <- truth - estimate_cc
#   bias_ipw <- truth - estimate_ipw
#   bias_mi <- truth - estimate_mi
#   
#   results[i, ] <- c(bias_cc,
#                     bias_ipw,
#                     bias_mi)
#   
#   print(results[i, ])
#   
# }
# 
# apply(results, 2, mean)
# 
# apply(results, 2, function(x) mean(abs(x)))
# 
