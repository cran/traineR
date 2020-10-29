## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = FALSE,
  comment = "#>",
  fig.align = "center",
  fig.width = 7,
  fig.height = 5
)
library(traineR)

## ---- echo=FALSE--------------------------------------------------------------
knitr::kable(head(Puromycin, 10))

## -----------------------------------------------------------------------------
n <- seq_len(nrow(Puromycin))
.sample <- sample(n, length(n) * 0.7)
data.train <- Puromycin[.sample,]
data.test  <- Puromycin[-.sample,]

## -----------------------------------------------------------------------------
model <- train.glm(state~., data.train)
model

## -----------------------------------------------------------------------------
prediction <- predict(model, data.test , type = "prob")
prediction

## -----------------------------------------------------------------------------
prediction <- predict(model, data.test , type = "class")
prediction

## -----------------------------------------------------------------------------
mc <- confusion.matrix(data.test, prediction)
mc

## -----------------------------------------------------------------------------
general.indexes(mc = mc)

## -----------------------------------------------------------------------------
model <- train.ada(state~., data.train, iter = 200)
model

## -----------------------------------------------------------------------------
prediction <- predict(model, data.test , type = "prob")
prediction

## -----------------------------------------------------------------------------
prediction <- predict(model, data.test , type = "class")
prediction

## -----------------------------------------------------------------------------
mc <- confusion.matrix(data.test, prediction)
mc

## -----------------------------------------------------------------------------
general.indexes(mc = mc)

## ---- echo = FALSE------------------------------------------------------------
knitr::kable(head(iris, 10))

## -----------------------------------------------------------------------------
data("iris")
n <- seq_len(nrow(iris))
.sample <- sample(n, length(n) * 0.75)
data.train <- iris[.sample,]
data.test <- iris[-.sample,]

## -----------------------------------------------------------------------------
model <- train.rpart(Species~., data.train)
model

## -----------------------------------------------------------------------------
prediction <- predict(model, data.test , type = "prob")
prediction

## -----------------------------------------------------------------------------
prediction <- predict(model, data.test , type = "class")
prediction

## -----------------------------------------------------------------------------
mc <- confusion.matrix(data.test, prediction)
mc

## -----------------------------------------------------------------------------
general.indexes(mc = mc)

## ----message=FALSE, warning=FALSE---------------------------------------------
library(rpart.plot)
prp(model, extra = 104, branch.type = 2, 
    box.col = c("pink", "palegreen3", "cyan")[model$frame$yval])

## -----------------------------------------------------------------------------
model <- train.bayes(Species~., data.train)
model

## -----------------------------------------------------------------------------
prediction <- predict(model, data.test , type = "prob")
prediction

## -----------------------------------------------------------------------------
prediction <- predict(model, data.test , type = "class")
prediction

## -----------------------------------------------------------------------------
mc <- confusion.matrix(data.test, prediction)
mc

## -----------------------------------------------------------------------------
general.indexes(mc = mc)

## -----------------------------------------------------------------------------
model <- train.randomForest(Species~., data.train)
model

## -----------------------------------------------------------------------------
prediction <- predict(model, data.test , type = "prob")
prediction

## -----------------------------------------------------------------------------
prediction <- predict(model, data.test , type = "class")
prediction

## -----------------------------------------------------------------------------
mc <- confusion.matrix(data.test, prediction)
mc

## -----------------------------------------------------------------------------
general.indexes(mc = mc)

## ---- message=FALSE, warning=FALSE--------------------------------------------
library(randomForest)
varImpPlot(model)

## -----------------------------------------------------------------------------
model <- train.knn(Species~., data.train)
model

## -----------------------------------------------------------------------------
prediction <- predict(model, data.test , type = "prob")
prediction

## -----------------------------------------------------------------------------
prediction <- predict(model, data.test , type = "class")
prediction

## -----------------------------------------------------------------------------
mc <- confusion.matrix(data.test, prediction)
mc

## -----------------------------------------------------------------------------
general.indexes(mc = mc)

## -----------------------------------------------------------------------------
model <- train.nnet(Species~., data.train, size = 20)
model

## -----------------------------------------------------------------------------
prediction <- predict(model, data.test , type = "prob")
prediction

## -----------------------------------------------------------------------------
prediction <- predict(model, data.test , type = "class")
prediction

## -----------------------------------------------------------------------------
mc <- confusion.matrix(data.test, prediction)
mc

## -----------------------------------------------------------------------------
general.indexes(mc = mc)

## -----------------------------------------------------------------------------
model <- train.neuralnet(Species~., data.train, hidden = c(5, 7, 6),
                         linear.output = FALSE, threshold = 0.01, stepmax = 1e+06)
summary(model)

## -----------------------------------------------------------------------------
prediction <- predict(model, data.test , type = "prob")
prediction

## -----------------------------------------------------------------------------
prediction <- predict(model, data.test , type = "class")
prediction

## -----------------------------------------------------------------------------
mc <- confusion.matrix(data.test, prediction)
mc

## -----------------------------------------------------------------------------
general.indexes(mc = mc)

## -----------------------------------------------------------------------------
model <- train.svm(Species~., data.train)
model

## -----------------------------------------------------------------------------
prediction <- predict(model, data.test , type = "prob")
prediction

## -----------------------------------------------------------------------------
prediction <- predict(model, data.test , type = "class")
prediction

## -----------------------------------------------------------------------------
mc <- confusion.matrix(data.test, prediction)
mc

## -----------------------------------------------------------------------------
general.indexes(mc = mc)

## -----------------------------------------------------------------------------
model <- train.xgboost(Species~., data.train, nrounds = 79, maximize = FALSE, verbose = 0)
model

## -----------------------------------------------------------------------------
prediction <- predict(model, data.test , type = "prob")
prediction

## -----------------------------------------------------------------------------
prediction <- predict(model, data.test , type = "class")
prediction

## -----------------------------------------------------------------------------
mc <- confusion.matrix(data.test, prediction)
mc

## -----------------------------------------------------------------------------
general.indexes(mc = mc)

