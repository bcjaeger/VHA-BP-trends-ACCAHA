#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param x
#' @param w
prop_test <- function(x, w=NULL){
  
  if(is.null(w)) w <- rep(1, length(x))
  
  cc <- !is.na(x)
  .w <- w[cc]
  .x <- x[cc]
  
  xx <- sum(.w[.x==1])
  nn <- sum(.w)
  
  p <- prop.test(xx, nn)
  
  rspec <- round_spec() %>% 
    round_using_decimal(digits = 1)
  
  table_glue("{100*p$estimate}\n({100*p$conf.int[1]}, {100*p$conf.int[2]})",
             rspec = rspec)
  
}

df_prop_test <- function(data, status_col, weight_col){
  prop_test(x=data[[status_col]], w=data[[weight_col]])
}
