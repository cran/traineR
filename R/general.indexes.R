
#' general.indexes
#'
#' @description Calculates the confusion matrix, overall accuracy, overall error and the category accuracy
#'
#' @param newdata matrix or data frame of test data.
#' @param prediction a prmdt prediction object.
#' @param mc (optional) a matrix for calculating the indices. If mc is entered as parameter newdata and prediction are not necessary.
#'
#' @return A list with the confusion matrix, overall accuracy, overall error and the category accuracy. The class of this list is indexes.prmdt
#'
#' @export
#'
#' @examples
#'
#' data("iris")
#'
#' n <- seq_len(nrow(iris))
#' .sample <- sample(n, length(n) * 0.75)
#' data.train <- iris[.sample,]
#' data.test <- iris[-.sample,]
#'
#' modelo.knn <- train.knn(Species~., data.train)
#' modelo.knn
#' prob <- predict(modelo.knn, data.test, type = "prob")
#' prob
#' prediccion <- predict(modelo.knn, data.test, type = "class")
#' prediccion
#' general.indexes(data.test, prediccion)
#'
general.indexes <- function(newdata, prediction, mc = NULL){
  indexes <- list()

  if( !missing(newdata) && !missing(prediction) ){
    mc <- confusion.matrix(newdata, prediction)
  }

  indexes[["confusion.matrix"]] <- mc
  indexes["overall.accuracy"] <- sum(diag(mc))/sum(mc)
  indexes["overall.error"] <- 1 - indexes[["overall.accuracy"]]
  indexes[["category.accuracy"]] <- diag(mc)/rowSums(mc)

  class(indexes) <- c("indexes.prmdt", "list")

  return(indexes)
}
