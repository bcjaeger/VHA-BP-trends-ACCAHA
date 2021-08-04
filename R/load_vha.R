#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param fpath
load_vha <- function(fpath) {

  read_csv(fpath) %>% 
    mutate(
      pdc = factor(
        pdc,
        levels = c("low", "medium", "high")
      ),
      race = factor(
        race, 
        levels = c("white", "black")
      ),
      sex = factor(
        sex,
        levels = c("male", "female")
      ),
      cohort_grp = factor(
        cohort %in% c(1,2,3), 
        levels = c(TRUE, FALSE), 
        labels = c('pre_accaha', 'post_accaha')
      ),
      across(where(is.character), as.factor)
    )

}
