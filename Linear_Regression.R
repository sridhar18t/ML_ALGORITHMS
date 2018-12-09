#Set the current working directory
library(caTools) 
library(neuralnet)  
library(plyr) 
library(readxl)

input = read.csv("input_file.csv", header = T)
input <- subset(input, select = c(#required_features))

#scale
max = apply(polynomial, 2, maximum)
min = apply(polynomial, 2, minimim)
scaled_polynomial = as.data.frame(scale(polynomial, center = minimum, scale = maximum - minimum))
sample = sample.split(scaled_data,SplitRatio = 0.75)
set.seed(500)
train =subset(scaled_polynomial,sample ==TRUE)
test_polynomial=subset(scaled_polynomial, sample==FALSE)

#Splitting training and testing dataset

sample = sample.split(input,SplitRatio = 0.75) #75% training data
set.seed(25)
train =subset(input,sample ==TRUE)
test=subset(input, sample==FALSE)


#Linear Regression

fit_linear <- lm(Fruits ~ apple+mango, data = train)

summary(fit_linear)

pred1 <- predict(fit_linear, newdata = test)