#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param data_pr
tabulate_pr_estimates <- function(data_pr) {

  data_pr %>% 
    split(f = .$analysis, .$outcome) %>% 
    map(
      .f = ~ .x %>% 
        select(exposure,
               level,
               prev_pre_accaha,
               pratio_pre_accaha,
               prev_post_accaha,
               pratio_post_accaha,
               pval_main,
               pval_interaction) %>% 
        as_grouped_data(, groups = c('exposure')) %>% 
        as_flextable(hide_grouplabel = TRUE) %>% 
        # merge_v(j = 'exposure', part = 'body') %>% 
        add_header_row(values = c("Group",
                                  "Pre ACC/AHA BP guideline",
                                  "Post ACC/AHA BP guideline",
                                  "P-value"),
                       colwidths = c(1, 2, 2, 2)) %>% 
        theme_box() %>% 
        width(width = 1.15) %>% 
        width(j = c("pval_main", "pval_interaction"), width = 0.85) %>% 
        align(align = 'center', part = 'all') %>% 
        align(j = 1, align = 'left', part = 'all') %>% 
        padding(i = ~is.na(exposure), 
                j = 1, 
                part = 'body', 
                padding.left = 10) %>% 
        set_header_labels(
          # exposure = "Group",
          level = "Group",
          prev_pre_accaha = "Prevalence\n(95% CI)",
          prev_post_accaha = "Prevalence\n(95% CI)",
          pratio_pre_accaha = "Prevalence ratio\n(95% CI)",
          pratio_post_accaha = "Prevalence ratio\n(95% CI)",
          pval_main = "Main effect",
          pval_interaction = "Interaction effect"
        ) %>% 
        merge_v(j = 1, part = 'header') %>% 
        merge_v(j = 'pval_main', part = 'body') %>% 
        merge_v(j = 'pval_interaction', part = 'body')
    )
    

}
