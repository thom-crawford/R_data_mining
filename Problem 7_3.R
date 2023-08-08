#Problem 7.3
#Loading Data and Partitioning into training + validatation

housing.df = read.csv("BostonHousing.csv")
housing.df = housing.df[,-14] #remove CATMEDV cat. variable

set.seed(2)
index = sample(nrow(housing.df), nrow(housing.df)*0.60)
train.df = housing.df[index,]
valid.df = housing.df[-index,]

#Part A - kNN prediction
library(FNN)
library(class)
library(caret)
#normalize the data
train.df.norm = train.df
valid.df.norm = valid.df
norm.values = preProcess(train.df[,-13], method=c("center", "scale"))
train.df.norm[,-13] = predict(norm.values, train.df[,-13])
valid.df.norm[,-13] = predict(norm.values, valid.df[,-13])

#testing value of K from 1 to 5

accuracy.df = data.frame(k=seq(1,5,1), RMSE = rep(0,5))
for (i in 1:5) {
  knn.model = class::knn(train = train.df.norm[,-13], 
                         test = valid.df.norm[,-13],
                         cl = train.df$MEDV, k = i)
  accuracy.df[i,2] = RMSE(as.numeric(as.character(knn.model)), valid.df$MEDV)
}
accuracy.df

#Part B
#Create new dataframe based on provided table
newdata = data.frame(0.20,0,7,0,0.538,6,62,4.7,4,307,21,10)
names(newdata) = names(train.df.norm)[-13]
#Normalize newly added data
newdata.norm = predict(norm.values, newdata)
#kNN model on new data with new K value
knn.new = class::knn(train = train.df.norm[,-13], 
                     test = newdata.norm,
                     cl = train.df$MEDV, k=3) #Best k from previous model
knn.new

#Part C
new.accuracy.df = RMSE(as.numeric(as.character(knn.new)), valid.df[,13])
new.accuracy.df