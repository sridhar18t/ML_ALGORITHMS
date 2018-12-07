#Set the current working directory
library(caTools) 
library(neuralnet)  
library(plyr) 
library(readxl)

input = read.csv("input_file.csv", header = T)
input <- subset(input, select = c(#required_features))


#Splitting training and testing dataset

sample = sample.split(input,SplitRatio = 0.75) #75% training data
set.seed(25)
train =subset(input,sample ==TRUE)
test=subset(input, sample==FALSE)

#Main Regression Algorithm

polynomial <- input


polynomial$p1 <- with(polynomial , (Feature1)^2)
polynomial$p2 <- with(polynomial , (Feature1)^3)


#scale
max = apply(polynomial, 2, maximum)
min = apply(polynomial, 2, minimim)
scaled_polynomial = as.data.frame(scale(polynomial, center = minimum, scale = maximum - minimum))
sample = sample.split(scaled_data,SplitRatio = 0.75)
set.seed(500)
train =subset(scaled_polynomial,sample ==TRUE)
test_polynomial=subset(scaled_polynomial, sample==FALSE)

fit_polynomial <- lm(features, data = scaled_polynomial)

summary(fit_polynomial)
confint(fit_polynomial) 



