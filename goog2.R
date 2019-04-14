#Data Mining Project
#Connor Rouan and Yadukrishnan Sethumadhavan

#Read in data sets
library(readxl)
train_data_full <- read_excel("prices-split-adjusted.xlsx", sheet = "GOOG-train")
val_data_full <- read_excel("prices-split-adjusted.xlsx", sheet = "GOOG-val")
test_data_full <- read_excel("prices-split-adjusted.xlsx", sheet = "GOOG-model")

#normalization functions
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

date_normalize<-function(y)
{ #making it a dataframe
  y<-as.data.frame(y)
  #converting to required format
  date_convert <- function(x) {
    return (format(x, format="%Y%m%d"))}
  y<- as.data.frame(lapply(y, date_convert))
  #converting to numeric value
  y<- as.numeric(unlist(y))
  #normalizing the data
  y<-normalize(y)
  return(y)
}

#normalize data sets
train_data_full$date <- date_normalize(train_data_full$date)
#train_data_full$date <- NULL
train_data <- as.data.frame(lapply(train_data_full, normalize))


val_data_full$date <- date_normalize(val_data_full$date)
#val_data_full$date <- NULL
val_data <- as.data.frame(lapply(val_data_full, normalize))


test_data_full$date <- date_normalize(test_data_full$date)
#test_data_full$date <- NULL
test_data <- as.data.frame(lapply(test_data_full, normalize))


#Build NN
library(neuralnet)
model <- neuralnet(
  formula = open~date+close+low+high+volume, 
  train_data,
  hidden = 1,
  threshold = 0.01,
  stepmax = 1e+06, 
  rep = 1,
  startweights = NULL,
  learningrate.limit = NULL,
  learningrate.factor = list(minus = 0.5, plus = 1.2),
  learningrate=NULL,
  lifesign = "minimal",
  lifesign.step = 1000,
  algorithm = "rprop+",
  err.fct = "sse",
  act.fct = "logistic",
  linear.output = TRUE,
  exclude = NULL,
  constant.weights = NULL,
  likelihood = FALSE)

#---Training Data Prediction---
#prediction
train_pred_data <- train_data
#train_pred_data$open <- NULL
train_result <- compute(model, train_pred_data)

#denormalize predictions
train_minvec <- min(train_data_full$open)
train_maxvec <- max(train_data_full$open)
denormalize <- function(x,minval,maxval) {
  x*(maxval-minval) + minval
}
train_actual <- t(as.data.frame(Map(denormalize,train_data$open,train_minvec,train_maxvec)))
train_pred <- t(as.data.frame(Map(denormalize,train_result$net.result,train_minvec,train_maxvec)))

#error evaluation
library(ModelMetrics)
rmse(train_actual, train_pred)
head(data.frame(actual = train_actual, prediction = train_pred))

#---Validation Data Prediction---
#prediction
val_pred_data <- val_data
val_pred_data$open <- NULL
val_result <- compute(model, val_pred_data)

#denormalize predictions
val_minvec <- min(val_data_full$open)
val_maxvec <- max(val_data_full$open)
denormalize <- function(x,minval,maxval) {
  x*(maxval-minval) + minval
}
val_actual <- t(as.data.frame(Map(denormalize,val_data$open,val_minvec,val_maxvec)))
val_pred <- t(as.data.frame(Map(denormalize,val_result$net.result,val_minvec,val_maxvec)))

#error evaluation
library(ModelMetrics)
rmse(val_actual, val_pred)
head(data.frame(actual = val_actual, prediction = val_pred))
