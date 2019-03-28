#Stock price prediction: Apple Inc.

#import data
library(readxl)
train_data <- read_excel("Documents/GitHub/stock-prediction/prices-split-adjusted.xlsx", sheet = "AAPL-train")
val_data <- read_excel("Documents/GitHub/stock-prediction/prices-split-adjusted.xlsx", sheet = "AAPL-val")
model_data <- read_excel("Documents/GitHub/stock-prediction/prices-split-adjusted.xlsx", sheet = "AAPL-model")

#normalize the data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

X1 <- train_data$date
X2 <- as.data.frame(lapply(train_data$close, normalize))
X3 <- as.data.frame(lapply(train_data$low, normalize))
X4 <- as.data.frame(lapply(train_data$high, normalize))
X5 <- as.data.frame(lapply(train_data$volume, normalize))
Y <- as.data.frame(lapply(train_data$open, normalize))

X <- array( c(X1,X2,X3,X4,X5), dim=c(dim(X1),5) )
Y <- array( Y, dim=c(dim(Y),1) )

#train the model
library(rnn)
set.seed(1)
model <- trainr(Y=Y[,dim(Y)[1]:1,,drop=F], X=X[,dim(X)[5]:1,,drop=F], learningrate = 0.1, hidden_dim = 10, batch_size = 25, numepochs = 10)

#see evolution of error rate from training
plot(colMeans(model$error),type='l', xlab='epoch', ylab='errors')

#https://cran.r-project.org/web/packages/rnn/vignettes/rnn.html