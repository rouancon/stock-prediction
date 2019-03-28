#Stock price prediction

#import data
apple_train <- read_excel("Documents/GitHub/stock-prediction/prices-split-adjusted.xlsx", sheet = "AAPL-train")
apple_val <- read_excel("Documents/GitHub/stock-prediction/prices-split-adjusted.xlsx", sheet = "AAPL-val")
apple_model <- read_excel("Documents/GitHub/stock-prediction/prices-split-adjusted.xlsx", sheet = "AAPL-model")
goog_train <- read_excel("Documents/GitHub/stock-prediction/prices-split-adjusted.xlsx", sheet = "GOOG-train")
goog_val <- read_excel("Documents/GitHub/stock-prediction/prices-split-adjusted.xlsx", sheet = "GOOG-val")
goog_model <- read_excel("Documents/GitHub/stock-prediction/prices-split-adjusted.xlsx", sheet = "GOOG-model")
xom_train <- read_excel("Documents/GitHub/stock-prediction/prices-split-adjusted.xlsx", sheet = "XOM-train")
xom_val <- read_excel("Documents/GitHub/stock-prediction/prices-split-adjusted.xlsx", sheet = "XOM-val")
xom_model <- read_excel("Documents/GitHub/stock-prediction/prices-split-adjusted.xlsx", sheet = "XOM-model")


