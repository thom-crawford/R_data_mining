#Problem 9.1
#Load packages and csv
library(rpart)
library(rpart.plot)
library(randomForest)
library(xgboost)
library(Matrix)
library(caret)
library(gains)

ebay.df = read.csv("eBayAuctions.csv")
View(ebay.df)
lapply(ebay.df, class)

#Convert Duration variable into categorical variable
ebay.df$Duration = as.factor(ebay.df$Duration)
ebay.df$Competitive. = as.factor(ebay.df$Competitive.)

#Split Data into Training (60%) and Validation (40%)
set.seed(1)
t.index = sample(nrow(ebay.df), nrow(ebay.df)*0.6)
train.df = ebay.df[t.index, ]
validate.df = ebay.df[-t.index, ]

#9.1a
#Creating a classification tree
tree1 = rpart(Competitive. ~ ., data = train.df, minbucket = 50,
              maxdepth = 7)
p.tree1 = prune(tree1, cp = tree1$cptable[which.min(tree1$cptable[,"xerror"]), "CP"])
prp(p.tree1)

#Most Important Variables
t(t(p.tree1$variable.importance))
#Tree Rules
p.tree1

#9.1.d
#Fit new tree to new predictors to better predict outcome of auction
#Removing variables End Day & Close Price, only known after auction closes
selected.var = c(1,2,3,4,7,8)
tree2 = rpart(Competitive. ~ ., data = train.df[,selected.var],
              minbucket = 50, maxdepth = 7)
p.tree2 = prune(tree2, cp = tree2$cptable[which.min(tree2$cptable[,"xerror"]), "CP"])
prp(p.tree2)

#9.1.e
#Scatter Plot of our 2 best variables (Seller Rating & Open Price)
x = train.df$sellerRating
y = train.df$OpenPrice

plot(x, y, xlab = "Seller Rating", ylab = "Open Price", main = "Open Price vs. Seller Rating",
     col = ifelse(train.df$Competitive. == "1", "red", "blue"))
#Scatter plot data too dense, converting to log scale
logx = log(train.df$sellerRating)
logy = log(train.df$OpenPrice)

plot(logx, logy, xlab = "Seller Rating", ylab = "Open Price", main = "log(Open Price) vs. log(Seller Rating)",
     col = ifelse(train.df$Competitive. == "1", "red", "blue"))

#9.1.f
#Create Lift Chart & Confusion Matrix
#Create predicts of model using validation set
pred = predict(p.tree2, validate.df[,selected.var])
head(pred[,2])
#Confusion matrix
confusionMatrix(factor(1*(pred[,2]>0.5)), validate.df$Competitive., positive = "1")

#Lift Chart
lift = gains(as.numeric(factor(validate.df$Competitive.)),
             as.numeric(factor(1*(pred[,2]>0.5))))
lift
plot(c(0, lift$cume.pct.of.total*sum(as.numeric(validate.df$Competitive.)))~c(0, lift$cume.obs),
     xlab = "# cases", ylab = "Cumulative", main = "Lift Chart for validattion data", type = "l")
lines(c(0, sum(as.numeric(validate.df$Competitive.))) ~ c(0,dim(validate.df)[1]), lty = 1)
#Decile-Wise Chart
heights = lift$mean.resp/mean(as.numeric(validate.df$Competitive.))
midpoints = barplot(heights, names.arg = lift$depth, ylim = c(0,9),
                    xlab = "Percentile", ylab = "Mean Response",
                    main = "Decile-Wise Chart for validation data")
