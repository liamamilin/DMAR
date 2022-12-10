

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





#' Classificatio model evaluation function
#'
#' @param truth lable of data
#' @param estimate prediction of model
#' @return a list of result
#' @examples
#' data("two_class_example")
#' classificationModelEva(two_class_example$truth,two_class_example$predicted)

classificationModelEva <- function(truth,estimate){
  require(yardstick)
  sens.result <- sensitivity_vec(truth = truth,estimate = estimate)
  spec.result <- specificity_vec(truth = truth,estimate = estimate)
  accuracy.result <- accuracy_vec(truth = truth,estimate = estimate)

  return(list("sensitive"=sens.result,"specificity"=spec.result,
              "accuracy"=accuracy.result))
}



#' Regression model evaluation function
#'
#' @param truth lable of data
#' @param estimate prediction of model
#' @return a list of result
#' @examples
#' data("solubility_test")
#' regModelEva(solubility_test$solubility,solubility_test$prediction)



regModelEva <- function(truth,estimate){
  require(yardstick)
  rmse.result <- rmse_vec(truth,estimate)
  mase.result <- mase_vec(truth,estimate)

  return(list("Root mean squared error"=rmse.result,
              "Mean absolute scaled error"=mase.result))
}



#' Scale data
#'
#' @param data dataset
#' @param col column of dataset
#' @param all if all=F, remove original column, otherwise keep
#' @param center logical value or numeric vector of length equal to the number of columns
#' @param scale logical value or numeric vector of length equal to the number of columns
#' @return scale dataset
#' @examples
#' # scale data to 0-1
#' maxs <- apply(iris[,c(1,2)], 2, max)
#' mins <- apply(iris[,c(1,2)], 2, min)
#' #scaleData(iris,col = c(1,2),all = T,center = mins,scale = maxs-mins)

scaleData <- function(data,col,all=F,center=T,scale=T){
  require(dplyr)
  scaled_data <- data[,col] %>%
    scale(center = center,scale = scale)
  if(all==F){
    data <- data[,-col]
    data <- data %>% cbind(data.frame(scaled_data))
    return(data)
  }else{
    scaled_data <- data.frame(scaled_data)
    n <- names(scaled_data)
    names(scaled_data) <- paste(n,"_scale",sep = "")
    return(data %>% cbind((scaled_data)))
  }
}

