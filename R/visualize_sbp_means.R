#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

visualize_sbp_means <- function() {

  key <- tibble(sheet = c('sbp_preindex','sbp_postindex')) %>% 
    mutate(
      data = map(
        sheet, 
        ~read_xlsx('data/bp_table_values.xlsx', sheet = .x) %>% 
          mutate(across(starts_with('ptrend'), as.character))
      )
    ) %>% 
    unnest(data) %>% 
    mutate(
      across(
        .cols = matches("^overall$|^cohort_\\d$"),
        .fns = list(lwr = ~map_chr(.x, bracket_lower_bound),
                    upr = ~map_chr(.x, bracket_upper_bound),
                    est = ~map_chr(.x, bracket_drop)),
        .names = "{.col}..{.fn}"
      ),
      across(
        starts_with('ptrend'),
        ~as.numeric(str_remove(.x, '<'))
      )
    ) %>% 
    select(-matches("^overall$|^cohort_\\d$"))
  
  
  gg_trend <- key %>% 
    select(-starts_with('overall')) %>% 
    pivot_longer(starts_with('cohort'),
                 names_prefix = 'cohort_',
                 names_to = c('cohort', 'stat'),
                 names_sep = '\\.\\.') %>% 
    mutate(value = as.numeric(value)) %>% 
    pivot_wider(names_from = stat, 
                values_from = value) %>% 
    mutate(
      date_preindex = recode(
        cohort,
        '1' = "2015-05-13",
        '2' = "2016-05-13",
        '3' = "2017-05-13",
        '4' = "2018-05-13",
        '5' = "2019-05-13",
        '6' = "2020-05-13"
      ),
      date_postindex = recode(
        cohort,
        '1' = "2016-05-13",
        '2' = "2017-05-13",
        '3' = "2018-05-13",
        '4' = "2019-05-13",
        '5' = "2020-05-13",
        '6' = "2021-05-13"
      ),
      date_preindex = as.Date(date_preindex),
      date_postindex = as.Date(date_postindex)
    )
  
  gg_trend_ascvd <- filter(gg_trend, 
                           str_detect(variable, '^High ASCVD'),
                           group == 'Yes')
  
  
  
  gg_trend_bracket <- gg_trend_ascvd %>% 
    group_by(sheet) %>% 
    mutate(min = min(lwr), max = max(upr)) %>% 
    select(sheet, min, max, starts_with('ptrend')) %>% 
    distinct() %>%
    mutate(y.position = case_when(sheet == 'sbp_preindex' ~ max+1/2,
                                  sheet == 'sbp_postindex' ~ min-1/2)) %>% 
    select(-min, -max) %>% 
    pivot_longer(starts_with('ptrend'),
                 names_prefix = 'ptrend_',
                 names_to = 'ptrend',
                 values_to = 'label') %>% 
    mutate(
      xmin = case_when(
        str_detect(sheet, 'pre') & ptrend == 1 ~ "2015-05-13",
        str_detect(sheet, 'post') & ptrend == 1 ~ "2016-05-13",
        str_detect(sheet, 'pre') & ptrend == 2 ~ "2018-05-13",
        str_detect(sheet, 'post') & ptrend == 2 ~ "2019-05-13"
      ),
      xmax = case_when(
        str_detect(sheet, 'pre') & ptrend == 1 ~ "2017-05-13",
        str_detect(sheet, 'post') & ptrend == 1 ~ "2018-05-13",
        str_detect(sheet, 'pre') & ptrend == 2 ~ "2020-05-13",
        str_detect(sheet, 'post') & ptrend == 2 ~ "2021-05-13"
      ),
      across(c(xmax, xmin), as.Date),
      label = as.character(label),
      label = recode(label, '0.001' = '<0.001'),
      label = paste("P-trend:", label),
      tip.length = case_when(sheet == 'sbp_preindex' ~ .02,
                             sheet == 'sbp_postindex' ~ -.02),
      vjust = case_when(sheet == 'sbp_preindex' ~ 0,
                        sheet == 'sbp_postindex' ~ 3),
    )
  
  # browser()
  
  gg_data <- gg_trend_ascvd %>% 
    mutate(sheet = recode(sheet,
                          'sbp_preindex' = 'Pre-index period',
                          'sbp_postindex' = 'Post-index period'),
           sheet = factor(sheet, levels = c('Pre-index period',
                                            'Post-index period'))) %>% 
    mutate(
      date = case_when(
        str_detect(sheet, 'Pre') ~ date_preindex,
        str_detect(sheet, 'Post') ~ date_postindex
      )
    )
  
  gg_arrow <- gg_data %>% 
    mutate(
      date = if_else(str_detect(sheet, 'Post'), date - 21, date),
      est = if_else(str_detect(sheet, 'Post'), est + 0.15, est)
    )
  
  gg_text <- gg_data %>% 
    mutate(est = if_else(str_detect(sheet, 'Post'), est + 0.15, est)) %>% 
    group_by(cohort) %>% 
    select(cohort, sheet, est, date) %>% 
    summarize(
      delta_x = as.numeric(diff(date))/365,
      delta_y = diff(est),
      est = mean(est),
      date = mean(date) + 56
    ) %>% 
    mutate(cohort = paste("Cohort", cohort),
           angle = atan(delta_y/delta_x)*(165/pi))
  
  gg_obj <- ggplot(gg_data) + 
    aes(x = date, 
        y = est, 
        ymin = lwr, 
        ymax = upr,
        color = sheet) + 
    geom_line(data = gg_arrow, 
              aes(group = cohort), 
              color = 'grey', 
              linetype = 2,
              arrow = arrow(length = unit(0.15, "cm"), 
                            type = 'closed')) +
    geom_pointrange() + 
    geom_text(data = gg_text, 
              inherit.aes = FALSE,
              alpha = 1/2,
              color = 'black',
              mapping = aes(x = date,
                            y = est, 
                            label = cohort,
                            angle = angle)) + 
    labs(color = 'BP measured during') + 
    scale_color_manual(values = c("purple", "orange")) + 
    theme_bw() + 
    theme(panel.grid = element_blank()) + 
    labs(y = 'Systolic blood pressure, mm Hg',
         x = 'Calendar year') + 
    scale_x_date(date_breaks = '1 year',
                 date_labels = "%Y",
                 expand = c(0,0)) + 
    scale_y_continuous(limits = c(129.5, 139.5),
                       breaks = 130:140,
                       expand = c(0,0))
  
  for(i in seq(nrow(gg_trend_bracket))){
    
    gg_obj <- gg_obj + 
      geom_bracket(
        inherit.aes = FALSE,
        xmin = gg_trend_bracket$xmin[i], 
        xmax = gg_trend_bracket$xmax[i], 
        y.position = gg_trend_bracket$y.position[i], 
        label = gg_trend_bracket$label[i],
        tip.length = gg_trend_bracket$tip.length[i],
        vjust = gg_trend_bracket$vjust[i]
      )
    
  }
  
  gg_obj + geom_rect(
    inherit.aes = FALSE,
    aes(xmin = as.Date("2015-01-13"),
        xmax = as.Date('2017-11-12'),
        ymin = 138.5, 
        ymax = 139.5),
    alpha = 0.02,
    color = 'black',
    fill = 'grey'
  ) + geom_rect(
    inherit.aes = FALSE,
    aes(xmin = as.Date('2017-11-14'),
        xmax = as.Date('2022-01-01'),
        ymin = 138.5, 
        ymax = 139.5),
    alpha = 0.02,
    color = 'black',
    fill = 'grey'
  ) + 
    geom_text(
      inherit.aes = FALSE,
      aes(x = mean(as.Date(c("2015-01-13", '2017-11-12'))),
          y = 139,
          label = 'Before ACC/AHA BP guideline') 
    ) + 
    geom_text(
      inherit.aes = FALSE,
      aes(x = mean(as.Date(c('2017-11-14', '2022-01-01'))),
          y = 139,
          label = 'After ACC/AHA BP guideline') 
    )
  
  
  
  # ggsave('sbp_high_ascvd_risk.png', 
  #        device = 'png', 
  #        width = 8, 
  #        height = 6, 
  #        dpi = 600)

}
