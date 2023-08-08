#Problem 6.1
library(leaps)
library(gains)
library(forecast)
library(corrgram)

housing.df = read.csv("BostonHousing.csv")
housing.df = housing.df[,-14] #remove CATMEDV cat. variable

#Part B

reg.model = lm(MEDV ~ CRIM + CHAS + RM, data = housing.df)
summary(reg.model)

#Part C
#Predict MEDV when CRIM = 0.1, CHAS = 0, and RM = 6
medv.eq = predict(reg.model, newdata = list(CRIM = 0.1, CHAS = 0, RM = 6),
                  se.fit = TRUE)

#Part D.ii
cor(housing.df[,-4]) #remove CHAS column, categorical 

#Part D.iii
set.seed(1)
train.index = sample(nrow(housing.df), nrow(housing.df)*0.60)
train = housing.df[train.index,]
valid = housing.df[-train.index,]

#Step Wise Regression
#Forward
reg.model2 = lm(MEDV~., data = train)
step.forward = step(reg.model2, direction = "forward")
summary(step.forward)

lm.forward = lm(MEDV ~ CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + 
                  TAX + PTRATIO + LSTAT, data = train)
pred.forward = predict(lm.forward, data = valid)
accuracy(pred.forward, valid$MEDV)


#Backward
step.backward = step(reg.model2, direction = "backward")
summary(step.backward)

lm.backward = lm(MEDV ~ CRIM + ZN + CHAS + NOX + RM + DIS + RAD + TAX + PTRATIO + 
                   LSTAT, data = train)
pred.backward = predict(lm.backward, data = valid)
accuracy(pred.backward, valid$MEDV)

#Both
step.both = step(reg.model2, direction = "both")
summary(step.both)

lm.both = lm(MEDV ~ CRIM + ZN + CHAS + NOX + RM + DIS + RAD + TAX + PTRATIO + 
               LSTAT, data = train)
pred.both = predict(lm.both, data = valid)
accuracy(pred.both, valid$MEDV)

