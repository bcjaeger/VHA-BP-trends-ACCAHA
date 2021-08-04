#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param data
#' @param y
#' @param x_main
#' @param x_group
#' @param x_control


make_pr_estimates <- function(data, y, x_main, x_group, x_control, w){
  
  grid <- expand_grid(outcome = y, exposure = x_main)
  
  data$weight <- w
  data$ones <- 1
  
  prev_estimates <- grid %>% 
    mutate(
      prev_main = map2(
        .x = exposure, 
        .y = outcome,
        .f = ~ data %>% 
          split(list(data[[.x]], data$cohort_grp)) %>% 
          map(df_prop_test, status_col = .y, weight_col = 'ones')
      ),
      prev_wtd = map2(
        .x = exposure, 
        .y = outcome,
        .f = ~ data %>% 
          split(list(data[[.x]], data$cohort_grp)) %>% 
          map(df_prop_test, status_col = .y, weight_col = 'weight')
      )
    ) %>% 
    pivot_longer(cols = starts_with('prev'),
                 names_prefix = 'prev_',
                 names_to = 'analysis') %>% 
    unnest_longer(value) %>% 
    separate(value_id, 
             into = c("level", "cohort_grp"), 
             sep = '\\.') %>% 
    pivot_wider(names_from = cohort_grp, 
                values_from = value,
                names_prefix = 'prev_')
  
  mdl_estimates <- grid %>% 
    mutate(
      pr_estimates = map2(
        .x = exposure, 
        .y = outcome,
        .f = ~pr_estimate(data = data,
                          y = .y, 
                          x_main = .x, 
                          x_group = x_group,
                          x_control = x_control,
                          w = w)
      )
    ) %>% 
    unnest_wider(pr_estimates) %>% 
    pivot_longer(cols = c(main, wtd),
                 names_to = 'analysis') %>% 
    unnest(value)
  
  full_join(mdl_estimates, prev_estimates)
  
}

pr_estimate <- function(data, y, x_main, x_group, x_control, w){
  
  list(
    main = pr_estimate_(data, y, x_main, x_group, x_control, w = NULL),
    wtd = pr_estimate_(data, y, x_main, x_group, x_control, w)
  )
  
}

pr_estimate_ <- function(data, y, x_main, x_group, x_control, w=NULL){
  
  x_cc <- x_control %>% 
    setdiff(x_main) %>% 
    paste(collapse = ' + ')
  
  main_ref = levels(data[[x_main]])[1]
  
  formula <- as.formula(glue("{y} ~ {x_group} * {x_main} + {x_cc}"))
  
  if(is.null(w)) w <- rep(1, nrow(data))
  
  fit <- geeglm(formula = formula,
                data = data,
                family = 'poisson',
                weights = w,
                id = seq(nrow(data)))
  
  fit_inference <- 
    suppressWarnings(tidy(anova(fit))) %>% 
    filter(
      str_detect(
        string = term, 
        pattern = paste0(x_main, "|", 
                         x_group, ":", x_main)
      )
    ) %>% 
    mutate(
      term = factor(
        term, 
        levels = c(x_main, paste0(x_group, ":", x_main)),
        labels = c("main", "interaction")
      )
    ) %>% 
    select(term, p.value) %>% 
    pivot_wider(names_from = term, 
                values_from = p.value,
                names_prefix = 'pval_') %>% 
    mutate(across(everything(), table_pvalue)) %>% 
    mutate(variable = x_main, .before = 1)
  
  fit_coef <- coef(fit)
  
  K_data <- expand.grid(
    main = paste0(
      x_main,
      levels(data[[x_main]])[-1]
    ),
    grp = paste0(x_group, levels(data[[x_group]])),
    stringsAsFactors = FALSE
  ) %>% 
    mutate(interaction = paste(grp, main, sep = ":")) %>% 
    mutate(
      index = map2(
        .x = main, 
        .y = interaction, 
        .f = ~{
          grep(make_patterns(.x, .y), names(fit_coef))
        }
      ),
      k = map(index, make_ones, size = length(fit_coef))
    ) 
  
  K_mat <- K_data %>% 
    pull(k) %>% 
    Reduce(rbind, x = .)
  
  rspec <- round_spec() %>% 
    round_using_decimal(digits = 2)
  
  fit_estimates <- glht(model = fit, linfct = K_mat) %>% 
    confint() %>% 
    getElement('confint') %>% 
    as.data.frame() %>% 
    cbind(K_data[, c('main', 'grp')], .) %>% 
    rbind(mutate(., 
                 main = paste0(x_main, main_ref), 
                 Estimate = 0, 
                 lwr = 0, 
                 upr = 0)) %>% 
    mutate(
      across(
        .cols = c(Estimate, lwr, upr),
        .fns = exp
      )
    ) %>% 
    transmute(
      variable = x_main,
      level = str_remove(main, paste0("^", x_main)),
      grp = str_remove(grp, paste0("^", x_group)),
      tbl_value = table_glue("{Estimate}\n({lwr}, {upr})", rspec = rspec),
      tbl_value = recode(tbl_value, '1.00\n(1.00, 1.00)' = "1 (reference)"),
      level = factor(level, levels = levels(data[[x_main]]))
    ) %>% 
    arrange(level) %>% 
    distinct() %>% 
    pivot_wider(names_from = grp, 
                values_from = tbl_value,
                names_prefix = 'pratio_')
  
  left_join(fit_estimates, fit_inference, by = 'variable') %>% 
    select(-variable)
  
}

make_patterns <- function(...){
  
  paste0("^",c(...),"$", collapse = '|')
  
}

make_ones <- function(index, size){
  
  out <- matrix(rep(0, size), nrow = 1)
  out[, index] <- 1
  out
  
}