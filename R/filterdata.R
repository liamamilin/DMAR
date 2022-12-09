

#' filter data with cor,miss, nzv
#'
#' @param formula formula of data
#' @param data data set, a dataframe
#' @param corthreshold threshold of corr
#' @param method method of corr
#' @param misthreshold threshold of miss value proportion
#' @return A recipe object
#' @examples
#' re <- filterFun(Species~.,data = iris)
#' result <- bake(re,iris)




filterFun <- function(formula = formula(NULL),
                      data,corthreshold=0.9,
                      method="pearson",misthreshold=0.1){
  require(recipes)
  ad_rec <- recipe(as.formula(formula), data = data) %>%
    step_corr(all_numeric_predictors(),threshold=corthreshold) %>%
    step_filter_missing(all_predictors(),threshold = misthreshold) %>%
    step_nzv(all_numeric_predictors()) %>%
    prep()
  return(ad_rec)

}


#' split data
#'
#' @param  data a dataframe of dataset
#' @param  p split probability
#' @return a list of split dataset
#' @examples
#' splitData(iris)


splitData <- function(data,p=c(0.8,0.2)){
  require(scorecard)
  data_list <- split_df(data, ratio = p)
  return(data_list)

}












