
#' create.prediction
#'
#' @keywords internal
#'
create.prediction  <- function(model, prediction){
  prediction <- list(prediction = prediction, "var.pred" =  model$prmdt$var.pred)
  class(prediction) <- c("prediction.prmdt", "list")
  return(prediction)
}

#' predict.ada.prmdt
#'
#' @keywords internal
#' @references Mark Culp, Kjell Johnson and George Michailidis (2016). ada: The R Package Ada for Stochastic Boosting.
#' R package version 2.0-5. https://CRAN.R-project.org/package=ada
#'
predict.ada.prmdt <- function(object, newdata, type = "class", n.iter = NULL, ...){
  type <- ifelse(type == "class", "vector", type)
  ans <- predict(original_model(object), get_test_less_predict(newdata, object$prmdt$var.pred), type, n.iter, ...)
  if(type == "prob"){
    colnames(ans) <- object$prmdt$levels
  }else{
    ans <- type_correction(object, ans, type == "vector")
  }
  return(create.prediction(object, ans))
}

#' predict.bayes.prmdt
#'
#' @keywords internal
#' @references David Meyer, Evgenia Dimitriadou, Kurt Hornik, Andreas Weingessel and Friedrich Leisch (2020).
#' e1071: Misc Functions of the Department of Statistics, Probability Theory Group (Formerly: E1071), TU Wien. R package version 1.7-4.
#' https://CRAN.R-project.org/package=e1071
#'
predict.bayes.prmdt <- function(object, newdata, type = "class", threshold = 0.001, eps = 0, ...){
  type <- ifelse(type == "prob", "raw", type)
  ans <- predict(original_model(object), get_test_less_predict(newdata, object$prmdt$var.pred), type, threshold, eps, ...)
  ans <- type_correction(object, ans, type == "class")
  return(create.prediction(object, ans))
}

#' predict.knn.prmdt
#'
#' @keywords internal
#' @references Klaus Schliep and Klaus Hechenbichler (2016). kknn: Weighted k-Nearest Neighbors.
#' R package version 1.3.1. https://CRAN.R-project.org/package=kknn
#'
predict.knn.prmdt <- function(object, newdata, type = "class", ...){
  type <- ifelse(type == "class", "raw", type)
  ans <- predict(original_model(object), type = type, get_test_less_predict(newdata, object$prmdt$var.pred), ...)
  ans <- type_correction(object, ans, type == "raw")
  return(create.prediction(object, ans))
}

#' predict.nnet.prmdt
#'
#' @keywords internal
#' @references Venables, W. N. & Ripley, B. D. (2002) Modern Applied Statistics with S. Fourth Edition. Springer, New York. ISBN 0-387-95457-0
#'
predict.nnet.prmdt <- function(object, newdata, type = "class", ...){
  type <- ifelse(type == "prob", "raw", type)
  ans <- predict(original_model(object),  get_test_less_predict(newdata, object$prmdt$var.pred), type, ...)

  num.class <- length(object$prmdt$levels)

  if(type == "raw"){
    if(num.class == 2){
      ans <- cbind(1 - ans, ans)
      colnames(ans) <- object$prmdt$levels
    }
  }
  ans <- type_correction(object, ans, type == "class")
  return(create.prediction(object, ans))
}

#' predict.neuralnet.prmdt
#'
#' @keywords internal
#' @references Stefan Fritsch, Frauke Guenther and Marvin N. Wright (2019). neuralnet: Training of Neural Networks.
#' R package version 1.44.2. https://CRAN.R-project.org/package=neuralnet
#'
predict.neuralnet.prmdt <- function(object, newdata, type = "class", ...){
  selector <- unlist(lapply(newdata, is.ordered))

  if(any(selector)){
    newdata[,selector] <- lapply(newdata[,selector, drop = FALSE], function(x) factor(x, ordered = FALSE, levels = levels(x)) )
  }

  var.predict <- object$prmdt$var.pred
  selector <- which(colnames(newdata) == var.predict)
  suppressWarnings(newdata <- cbind(dummy.data.frame(newdata[, -selector, drop = FALSE], drop = FALSE,
                                                     dummy.classes = c("factor","character")), newdata[selector]))

  selector <- which(colnames(newdata) == var.predict)

  ans <- neuralnet::compute(original_model(object), newdata[, -selector])

  if(type == "all"){
    return(create.prediction(object, ans))
  }

  ans <- ans$net.result
  colnames(ans) <- object$prmdt$levels

  if(type == "class"){
    ans <- max_col(ans)
    ans <- numeric_to_predict(newdata[, selector], ans)
    ans <- type_correction(object, ans, type == "class")
  }

  return(create.prediction(object, ans))
}

#' predict.randomForest.prmdt
#'
#' @keywords internal
#' @references A. Liaw and M. Wiener (2002). Classification and Regression by randomForest. R News 2(3), 18--22.
#'
predict.randomForest.prmdt <- function(object, newdata, type = "class", norm.votes = TRUE, predict.all = FALSE, proximity = FALSE, nodes = FALSE, cutoff, ...){
  type <- ifelse(type == "class", "response", type)
  ans <- predict(original_model(object), get_test_less_predict(newdata, object$prmdt$var.pred), type, norm.votes, predict.all, proximity, nodes, cutoff, ...)
  if(type == "prob"){
    class(ans) <- "matrix"
  }else{
    ans <- type_correction(object, ans, type == "response")
  }

  return(create.prediction(object, ans))
}

#' predict.rpart.prmdt
#'
#' @keywords internal
#' @references Terry Therneau and Beth Atkinson (2019). rpart: Recursive Partitioning and Regression Trees.
#' R package version 4.1-15. https://CRAN.R-project.org/package=rpart
#'
predict.rpart.prmdt <- function(object, newdata, type = "class", na.action = na.pass, ...){
  ans <- predict(original_model(object), newdata, type, na.action, ...)
  ans <- type_correction(object, ans, type == "class")
  return(create.prediction(object, ans))
}

#' predict.svm.prmdt
#'
#' @keywords internal
#' @references David Meyer, Evgenia Dimitriadou, Kurt Hornik, Andreas Weingessel and Friedrich Leisch (2020).
#' e1071: Misc Functions of the Department of Statistics, Probability Theory Group (Formerly: E1071), TU Wien. R package version 1.7-4.
#' https://CRAN.R-project.org/package=e1071
#'
predict.svm.prmdt <- function(object, newdata, type = "class", decision.values = FALSE, ..., na.action = na.omit){
  ans <- predict(original_model(object), newdata, decision.values, probability = type == "prob", ..., na.action = na.action)
  if(type == "prob"){
    ans <- attr(ans, "probabilities")
    ans <- ans[,object$prmdt$levels]
  }else{
    ans <- type_correction(object, ans,  type == "class")
  }
  return(create.prediction(object, ans))
}

#' predict.xgb.Booster
#'
#' @keywords internal
#' @references Tianqi Chen, Tong He, Michael Benesty, Vadim Khotilovich, Yuan Tang, Hyunsu Cho,
#' Kailong Chen, Rory Mitchell, Ignacio Cano, Tianyi Zhou, Mu Li, Junyuan Xie, Min Lin, Yifeng Geng and Yutian Li (2020).
#' xgboost: Extreme Gradient Boosting. R package version 1.2.0.1. https://CRAN.R-project.org/package=xgboost
#'
predict.xgb.Booster.prmdt <- function(object, newdata, type = "class", missing = NA, outputmargin = FALSE, ntreelimit = NULL, predleaf = FALSE, predcontrib = FALSE,
                                approxcontrib = FALSE, predinteraction = FALSE, reshape = FALSE, ...){

  .colnames <- all.vars(object$prmdt$vars)
  var.pred <-  object$prmdt$var.pred
  selector <- which(colnames(newdata) == var.pred)

  if(length(.colnames) == 1 && .colnames == "."){
    .colnames <- colnames(newdata[,-selector, drop = FALSE])
  }

  test_aux <- newdata %>% select(c(.colnames,var.pred))  %>% select_on_class(c("numeric","integer", "factor"))
  test_aux[] <- lapply(test_aux, as.numeric)

  if(min(test_aux[,var.pred]) != 0){
    test_aux[,var.pred]  <- test_aux[,var.pred]  - 1
  }

  selector <- which(colnames(test_aux) == var.pred)
  test_aux  <- xgb.DMatrix(data = data.matrix(test_aux[,-selector]), label = data.matrix(test_aux[,selector]))

  ans <- predict(original_model(object), test_aux, missing, outputmargin, ntreelimit, predleaf, predcontrib, approxcontrib, predinteraction, reshape, ...)

  num.class <- length(object$prmdt$levels)

  if(type == "class"){
    if(num.class > 2){
      ans <- max.col(matrix(ans, ncol = num.class, byrow = TRUE))
    }else{
      ans <- ifelse(ans > 0.5, 2, 1)
    }
    ans <- numeric_to_predict(newdata[,var.pred], ans)
  }

  if(type == "prob"){
    if(num.class > 2){
      ans <- matrix(ans, ncol = num.class, byrow = TRUE)
    }else{
      ans <- matrix(ans, ncol = 1, byrow = TRUE)
      ans <- cbind(1 - ans, ans)
    }
    colnames(ans) <- object$prmdt$levels
  }

  ans <- type_correction(object, ans, type == "class")

  return(create.prediction(object, ans))
}


#' predict.glm.prmdt
#'
#' @keywords internal
#' @references R Core Team (2020). R: A language and environment for statistical computing.
#' R Foundation for Statistical Computing, Vienna, Austria. URL https://www.R-project.org/.
#'
predict.glm.prmdt <- function(object, newdata, type = "class", se.fit = FALSE, dispersion = NULL, terms = NULL, na.action = na.pass, ...){
  ans <- predict(original_model(object), get_test_less_predict(newdata, object$prmdt$var.pred), "response",  se.fit = se.fit, dispersion = dispersion, terms = terms, na.action = na.action, ... = ...)
  levels.class <- object$prmdt$levels

  if(type == "prob"){
    ans <- matrix(as.numeric(ans), ncol = 1, byrow = TRUE)
    ans <- cbind(1 - ans, ans)
    colnames(ans) <- levels.class
  }else{
    ans <- ifelse(ans > 0.5, levels.class[2], levels.class[1])
    ans <- type_correction(object, ans, type == "class")
  }
  return(create.prediction(object, ans))
}
