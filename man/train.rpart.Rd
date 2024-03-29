% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/train.R
\name{train.rpart}
\alias{train.rpart}
\title{train.rpart}
\usage{
train.rpart(
  formula,
  data,
  weights,
  subset,
  na.action = na.rpart,
  method,
  model = TRUE,
  x = FALSE,
  y = TRUE,
  parms,
  control,
  cost,
  ...
)
}
\arguments{
\item{formula}{a formula, with a response but no interaction terms. If this a a data frame, that is taken as the model frame.}

\item{data}{an optional data frame in which to interpret the variables named in the formula.}

\item{weights}{optional case weights.}

\item{subset}{optional expression saying that only a subset of the rows of the data should be used in the fit.}

\item{na.action}{the default action deletes all observations for which y is missing, but keeps those in which one or more predictors are missing.}

\item{method}{one of "anova", "poisson", "class" or "exp". If method is missing then the routine tries to make an intelligent guess. If y is a survival object, then method = "exp" is assumed, if y has 2 columns then method = "poisson" is assumed, if y is a factor then method = "class" is assumed, otherwise method = "anova" is assumed. It is wisest to specify the method directly, especially as more criteria may added to the function in future.
Alternatively, method can be a list of functions named init, split and eval. Examples are given in the file ‘tests/usersplits.R’ in the sources, and in the vignettes ‘User Written Split Functions’.}

\item{model}{if logical: keep a copy of the model frame in the result? If the input value for model is a model frame (likely from an earlier call to the rpart function), then this frame is used rather than constructing new data.}

\item{x}{keep a copy of the x matrix in the result.}

\item{y}{keep a copy of the dependent variable in the result. If missing and model is supplied this defaults to FALSE.}

\item{parms}{optional parameters for the splitting function.
Anova splitting has no parameters.
Poisson splitting has a single parameter, the coefficient of variation of the prior distribution on the rates. The default value is 1.
Exponential splitting has the same parameter as Poisson.
For classification splitting, the list can contain any of: the vector of prior probabilities (component prior), the loss matrix (component loss) or the splitting index (component split). The priors must be positive and sum to 1. The loss matrix must have zeros on the diagonal and positive off-diagonal elements. The splitting index can be gini or information. The default priors are proportional to the data counts, the losses default to 1, and the split defaults to gini.}

\item{control}{a list of options that control details of the rpart algorithm. See \code{\link[rpart]{rpart.control}}.}

\item{cost}{a vector of non-negative costs, one for each variable in the model. Defaults to one for all variables. These are scalings to be applied when considering splits, so the improvement on splitting on a variable is divided by its cost in deciding which split to choose.}

\item{...}{arguments to \code{\link[rpart]{rpart.control}} may also be specified in the call to rpart. They are checked against the list of valid arguments.}
}
\value{
A object rpart.prmdt with additional information to the model that allows to homogenize the results.
}
\description{
Provides a wrapping function for the \code{\link[rpart]{rpart}}.
}
\note{
the parameter information was taken from the original function \code{\link[rpart]{rpart}}.
}
\examples{

# Classification
data("iris")

n <- seq_len(nrow(iris))
.sample <- sample(n, length(n) * 0.75)
data.train <- iris[.sample,]
data.test <- iris[-.sample,]

modelo.rpart <- train.rpart(Species~., data.train)
modelo.rpart
prob <- predict(modelo.rpart, data.test, type = "prob")
prob
prediccion <- predict(modelo.rpart, data.test, type = "class")
prediccion

# Regression
len <- nrow(swiss)
sampl <- sample(x = 1:len,size = len*0.20,replace = FALSE)
ttesting <- swiss[sampl,]
ttraining <- swiss[-sampl,]
model.rpart <- train.rpart(Infant.Mortality~.,ttraining)
prediction <- predict(model.rpart,ttesting)
prediction

}
\seealso{
The internal function is from package \code{\link[rpart]{rpart}}.
}
