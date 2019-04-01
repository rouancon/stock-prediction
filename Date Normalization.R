#call the function to normalize the date in the format "yyyy-mm-dd"(like in prices-split-adjusted.xslx)
date_normalize<-function(y)
  { #making it a dataframe
    y<-as.data.frame(y)
    #converting to required format
    date_convert <- function(x) {
    return (format(x, format="%Y%m%d"))}
    y<- as.data.frame(lapply(y, date_convert))
    #converting to numeric value
    y<-data.frame(lapply(y,as.numeric))
    #normalizing the data
    y<-normalize(y)
    return(y)
  }


