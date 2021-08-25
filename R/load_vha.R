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
      across(where(is.character), as.factor)
    )

}
