#Stock price prediction: Google Inc.

#import data
goog_train <- read_excel("Documents/GitHub/stock-prediction/prices-split-adjusted.xlsx", sheet = "GOOG-train")
goog_val <- read_excel("Documents/GitHub/stock-prediction/prices-split-adjusted.xlsx", sheet = "GOOG-val")
goog_model <- read_excel("Documents/GitHub/stock-prediction/prices-split-adjusted.xlsx", sheet = "GOOG-model")

#normalize the data


#set up the model
