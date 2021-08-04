## Load your packages, e.g. library(targets).
source("./packages.R")

## Load your R files
lapply(list.files("./R", full.names = TRUE), source)

## tar_plan supports drake-style targets and also tar_target()
tar_plan(
  
  data_vha = load_vha(fpath = 'data/vha_sim_missing.csv'),
  
  weights = ipw_compute(formula = bp_sys_postindex_mmhg ~ ., 
                        data = data_vha),
  
  data_pr = make_pr_estimates(
    data = data_vha,
    y = c("bp_control_postindex_jnc",
          "bp_control_postindex_acc"),
    x_main = c("race", "sex", "pdc"),
    x_control = c("age_years", "sex", "race", "pdc"),
    x_group = "cohort_grp",
    w = weights
  ),
  
  tbl_pr = tabulate_pr_estimates(data_pr),
  
  plt_sbp = visualize_sbp_means(),
  
  tar_render(VHA_BP_trends_ACCAHA_results, 
             "doc/VHA_BP_trends_ACCAHA_results.Rmd")
  
  
)