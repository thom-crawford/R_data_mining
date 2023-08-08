#Problem 13.4
#load packages and data
library(class)
library(rpart)
library(caret)
library(gains)
library(adabag)
library(randomForest)
library(uplift)
library(dummies)

hair.df = read.csv("Hair-Care-Product.csv")
#View(hair.df)

#Part 13.4.A
ftable(hair.df[c("Purchase", "Promotion_ord")])
#Purchase propensity of DID receive
80/(4896+80)
#Purchase propensity of DID NOT receive
32/(4992+32)

#Part 13.4.B
#Partition Data (60/40)
hair.df$Purchase = ifelse(hair.df$Purchase=="1", 1,0)
set.seed(2)
index = sample(nrow(hair.df), nrow(hair.df)*0.6)
train.df = hair.df[index,]
validate.df = hair.df[-index,]

#Create Random Forest using Uplift package
hair.RF = upliftRF(Purchase ~ Age + Hair.Color + U.S..Region + Validation +
                     trt(Promotion_ord) + Gender_ord + Residence_ord,
                   data = train.df, mtry = 3, ntree = 100, split_method = "KL",
                   minsplit = 200, verbose = TRUE)
hair.pred = predict(hair.RF, newdata = validate.df)
#View results in dataframe
hair.uplift = data.frame(hair.pred, "uplift" = hair.pred[,1] - hair.pred[,2])
head(hair.uplift)

#Create KNN using Uplift package
predictors = c(2,3,4,5,7,8) #Remove outcome and treatment from train/validation
hair.knn = upliftKNN(train.df[,predictors], validate.df[,predictors], train.df$Purchase, train.df$Promotion_ord, k=1,
                     dist.method = "euclidean", p=2, ties.meth = "min", agg.method = "majority")
head(hair.knn)
