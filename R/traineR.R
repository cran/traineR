#' @name traineR
#' @aliases traineR
#' @title Predictive (Classification and Regression) Models Homologator
#' @author
#' Maintainer: Oldemar Rodriguez Rojas <oldemar.rodriguez@ucr.ac.cr>\cr
#' \itemize{
#'   \item Oldemar Rodriguez Rojas <oldemar.rodriguez@ucr.ac.cr>
#'   \item Andres Navarro D
#'   \item Ariel Arroyo S
#'   \item Diego Jiménez
#' }
#' @description
#' Methods to unify the different ways of creating predictive models and their different predictive formats for classification and regression. It includes
#' methods such as K-Nearest Neighbors Schliep, K. P. (2004) <doi:10.5282/ubm/epub.1769>, Decision Trees Leo Breiman, Jerome H. Friedman, Richard A. Olshen, Charles J. Stone (2017) <doi:10.1201/9781315139470>,
#' ADA Boosting Esteban Alfaro, Matias Gamez, Noelia García (2013) <doi:10.18637/jss.v054.i02>, Extreme Gradient Boosting Chen & Guestrin (2016) <doi:10.1145/2939672.2939785>,
#' Random Forest Breiman (2001) <doi:10.1023/A:1010933404324>, Neural Networks Venables, W. N., & Ripley, B. D. (2002) <ISBN:0-387-95457-0>,
#' Support Vector Machines Bennett, K. P. & Campbell, C. (2000) <doi:10.1145/380995.380999>, Bayesian Methods Gelman, A., Carlin, J. B., Stern, H. S., & Rubin, D. B. (1995) <doi:10.1201/9780429258411>,
#' Linear Discriminant Analysis Venables, W. N., & Ripley, B. D. (2002) <ISBN:0-387-95457-0>, Quadratic Discriminant Analysis Venables, W. N., & Ripley, B. D. (2002) <ISBN:0-387-95457-0>,
#' Logistic Regression Dobson, A. J., & Barnett, A. G. (2018) <doi:10.1201/9781315182780> and Penalized Logistic Regression Friedman, J. H., Hastie, T., & Tibshirani, R. (2010) <doi:10.18637/jss.v033.i01>.
#' @details
#' \tabular{ll}{
#' Package: \tab traineR\cr
#' Type: \tab Package\cr
#' Version: \tab 2.2.2\cr
#' Date: \tab 2025-05-23\cr
#' License: \tab GPL (>=2)\cr
#' }
#' @keywords package
"_PACKAGE"

NULL
utils::globalVariables(c(
  "prop", "dummy", "x", "y"
))
