

#' filter data with cor,miss, nzv
#'
#' @param formula formula of data
#' @param data data set, a dataframe
#' @param corthreshold threshold of corr
#' @param method method of corr
#' @param misthreshold threshold of miss value proportion
#' @return A recipe object
#' @example data(credit_data, package = "modeldata")
#' @example result <- filterFun(Status ~ ., data = credit_data,data = credit_data)
#' @example bake(result,credit_data)

filterFun <- function(formula = formula(NULL),
                      data,corthreshold=0.9,
                      method="pearson",misthreshold=0.1){
  require(recipes)
  ad_rec <- recipe(as.formula(formula), data = data) %>%
    step_corr(all_numeric_predictors(),threshold=corthreshold) %>%
    step_filter_missing(all_predictors(),threshold = misthreshold) %>%
    step_nzv(all_predictors()) %>%
    prep()
  return(ad_rec)

}






