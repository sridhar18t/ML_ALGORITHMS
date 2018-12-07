#MLP Algorithm for R

library(caTools)
library(neuralnet)  
library(plyr) 
library(plotly)


workset <- foo

# Initialize errors to null values 
test_workset_error <- NULL
train_workset_error <- NULL

index <- 0.50

# Applying min-max scaling…
dmaximum <- apply(workset, 2, max)
dminimum <- apply(workset, 2, min)
scaled_workset <- as.workset.frame(scale(workset, center = dminimum, scale = dmaximum - dminimum))
i <- sample(1:nrow(scaled_workset),round(index*nrow(scaled_workset)))    
scaled_workset <- scaled_workset[i,]

crossvalidate <- function(workset,hid=c(3))
{
  dmaximum <- apply(workset, 2, max)
  dminimum <- apply(workset, 2, min)
  scaled_workset <- as.workset.frame(scale(workset, center = dminimum, scale = dmaximum - dminimum))
  error_cross_validate <- NULL
  k <- 10

# Cross validation loop…
  for(j in 1:k)
  {
    i <- sample(1:nrow(workset),round(index*nrow(workset)))
    train.cv <- scaled_workset[i,]
    test.cv <- scaled_workset[-i,]
    neural <- neuralnet(apple, mango, banana,  workset=train.cv,hidden=hid,linear.output=T)
    neural_net_predicted <- compute(neural,test.cv[, 2:7])
  
    neural_net_predicted <- neural_net_predicted$net.result*(max(workset$Param)-min(workset$Param))+min(workset$Param)
   
    res <- (test.cv$Param)*(max(workset$Param)-min(workset$Param))+min(workset$Param)
   
    error_cross_validate[j] <- sum((res - neural_net_predicted)^2)/nrow(test.cv)
  }
 
  return(mean(error_cross_validate))
}

set.seed(30)
for(i in 1:4)
{
  neural <- neuralnet(Param ~ apple, mango, banana,workset=scaled_workset, hidden=c(i),linear.output=T)
  train_workset_error[i] <- (sum(((as.workset.frame(neural$net.result)*(50-5)+5) - (scaled_workset$Param*(50-5)+5))^2)/nrow(scaled_workset))/100
  test_workset_error[i] <- crossvalidate(workset,hid=c(i))
}

test_workset_error
train_workset_error

which(min(test_workset_error) == test_workset_error)
which(min(train_workset_error) == train_workset_error)

RMSE.Neural_Net <- min(test_workset_error)
