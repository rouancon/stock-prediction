#Stock price prediction: Apple Inc.

#import data
library(readxl)
train_data <- read_excel("Documents/GitHub/stock-prediction/prices-split-adjusted.xlsx", sheet = "AAPL-train")
val_data <- read_excel("Documents/GitHub/stock-prediction/prices-split-adjusted.xlsx", sheet = "AAPL-val")
model_data <- read_excel("Documents/GitHub/stock-prediction/prices-split-adjusted.xlsx", sheet = "AAPL-model")

#normalize data
library(BBmisc)

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

X1 <- date_normalize(train_data$date)
X2 <- normalize(train_data$close)
X3 <- normalize(train_data$low)
X4 <- normalize(train_data$high)
X5 <- normalize(train_data$volume)
Y <- normalize(train_data$open)

X <- array( c(X1,X2,X3,X4,X5), dim=c(1006,1,5))
Y <- array( Y, dim=c(1006,1,1))

#train the model
library(rnn)
set.seed(1)
model <- trainr(Y, X, learningrate = 0.01, hidden_dim = 15, network_type = "lstm", batch_size = 25, numepochs = 100)

#see evolution of error rate from training
plot(colMeans(model$error),type='l', xlab='epoch', ylab='errors')

#normalize validation data
val_X1 <- val_data$date
val_X2 <- normalize(val_data$close)
val_X3 <- normalize(val_data$low)
val_X4 <- normalize(val_data$high)
val_X5 <- normalize(val_data$volume)
val_Y <- normalize(val_data$open)

val_X <- array( c(val_X1,val_X2,val_X3,val_X4,val_X5), dim=c(504,1,5))

#predict validation data
predict_Y  <- predictr(model,val_X)

#denormalize predictions
minvec <- min(val_data$open)
maxvec <- max(val_data$open)
denormalize <- function(x,minval,maxval) {
  x*(maxval-minval) + minval
}
predict_Y <- t(as.data.frame(Map(denormalize,predict_Y,minvec,maxvec)))

#plot differences, should ideally be flat and close to 0
hist( predict_Y - val_data$open )

