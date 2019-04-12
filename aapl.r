#Data Mining Project
#Connor Rouan and Yadukrishnan Sethumadhavan

#Read in data sets
library(readxl)
train_data_full <- read_excel("prices-split-adjusted.xlsx", sheet = "AAPL-train")
val_data_full <- read_excel("prices-split-adjusted.xlsx", sheet = "AAPL-val")
test_data_full <- read_excel("prices-split-adjusted.xlsx", sheet = "AAPL-model")

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
traindate <- date_normalize(train_data_full$date)
train_data_full$date <- NULL
train_data <- as.data.frame(lapply(train_data_full, normalize))
train_data$date <- traindate

valdate <- date_normalize(val_data_full$date)
val_data_full$date <- NULL
val_data <- as.data.frame(lapply(val_data_full, normalize))
val_data$date <- valdate

testdate <- date_normalize(test_data_full$date)
test_data_full$date <- NULL
test_data <- as.data.frame(lapply(test_data_full, normalize))
test_data$date <- testdate


#Build NN
library(neuralnet)
model <- neuralnet(
  formula = open~date+close+low+high+volume, 
  train_data,
  hidden = 1,
  threshold = 0.005,
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
pred_data <- train_data
pred_data$open <- NULL
result <- compute(model, pred_data)

#denormalize predictions
minvec <- min(train_data_full$open)
maxvec <- max(train_data_full$open)
denormalize <- function(x,minval,maxval) {
  x*(maxval-minval) + minval
}
actual <- t(as.data.frame(Map(denormalize,train_data$open,minvec,maxvec)))
pred <- t(as.data.frame(Map(denormalize,result$net.result,minvec,maxvec)))

#error evaluation
library(ModelMetrics)
rmse(actual, pred)
#data.frame(actual = actual, prediction = pred)

#---Validation Data Prediction---
#prediction
pred_data <- val_data
pred_data$open <- NULL
result <- compute(model, pred_data)

#denormalize predictions
minvec <- min(val_data_full$open)
maxvec <- max(val_data_full$open)
denormalize <- function(x,minval,maxval) {
  x*(maxval-minval) + minval
}
actual <- t(as.data.frame(Map(denormalize,pred_data$open,minvec,maxvec)))
pred <- t(as.data.frame(Map(denormalize,result$net.result,minvec,maxvec)))

#error evaluation
library(ModelMetrics)
rmse(actual, pred)
#data.frame(actual = actual, prediction = pred)