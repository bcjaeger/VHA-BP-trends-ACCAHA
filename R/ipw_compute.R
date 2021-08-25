#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param formula
#' @param data
ipw_compute <- function(formula, data){
  
  # drop the date variables (they will break glmnet)
  data <- data %>% 
    select(-where(is.Date))
  
  .formula <- stats::formula(x = stats::terms(formula, data = data))
  
  .y_missing <- is_missing(data[[formula_get_outcome_vars(.formula)]]) 
  
  .x_data <- data[, formula_get_predictor_vars(.formula)]
  
  .x_preprocessor <- recipe(~., data = .x_data) %>% 
    step_indicate_na(contains("preindex"),
                     age_years, sex, race) %>% 
    step_impute_mean(all_numeric()) %>% 
    step_impute_mode(all_nominal()) %>% 
    step_dummy(all_nominal()) %>% 
    prep()
  
  .x_preprocessed <- juice(.x_preprocessor, composition = 'matrix')
  
  message("candidate predictors:\n - ", 
          paste(colnames(.x_preprocessed), collapse = '\n - '))
  
  glmnet_cv <- cv.glmnet(x = .x_preprocessed, 
                                 y = .y_missing, 
                                 nfolds = 10,
                                 family = 'binomial')
  
  glmnet_fit <- glmnet(x = .x_preprocessed, 
                               y = .y_missing,
                               family = 'binomial',
                               lambda = glmnet_cv$lambda.1se)
  
  .x_vars_selected <- as.matrix(stats::coef(glmnet_fit)) %>%
    tibble::as_tibble(rownames = 'variable') %>%
    dplyr::filter(s0 != 0, variable != '(Intercept)') %>% 
    dplyr::pull(variable)
  
  message("selected predictors:\n - ",
          paste(.x_vars_selected, collapse = '\n - '))
  
  inverse_prob_miss <- 1 - stats::predict(glmnet_fit, 
                                          newx = .x_preprocessed, 
                                          type = 'response')
  
  inverse_weight_miss <- 1 / inverse_prob_miss
  
  # trim
  
  inverse_weight_miss <- 
    pmax(inverse_weight_miss, quantile(inverse_weight_miss, 0.005))
  
  inverse_weight_miss <- 
    pmin(inverse_weight_miss, quantile(inverse_weight_miss, 0.995))
  
  # normalize 
  
  inverse_weight_miss <- 
    inverse_weight_miss / mean(inverse_weight_miss)
  
  as.numeric(inverse_weight_miss)
  
}

formula_get_outcome_vars <- function(formula){
  all.vars(stats::update(formula, . ~ 1))
}

formula_get_predictor_vars <- function(formula){
  all.vars(stats::update(formula, 1 ~ .))
}

is_missing <- function(x) as.numeric(is.na(x))


