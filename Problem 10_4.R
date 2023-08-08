#Problem 10.4
#Load packages and data
library(caret)
library(MASS)
library(glmulti)

ebay.df = read.csv("eBayAuctions.csv", stringsAsFactors = FALSE)
View(ebay.df)

#10.4.a
#Create pivot table for mean of Competitive based on the Categorical variables
data.frame(tapply(ebay.df$Competitive., ebay.df$Category, mean))
data.frame(tapply(ebay.df$Competitive., ebay.df$currency, mean))
data.frame(tapply(ebay.df$Competitive., ebay.df$endDay, mean))
data.frame(tapply(ebay.df$Competitive., ebay.df$Duration, mean))

names(ebay.df)
#Category - combining Computers & Business/Industrial (have same mean)
ebay.df$Category[ebay.df$Category == "Business/Industrial"] = "Computer"
#Category - combining Antique/Art/Craft & Collectibles
ebay.df$Category[ebay.df$Category == "Antique/Art/Craft"] = "Collectibles"
#Category - combining Automotive & Pottery/Glass
ebay.df$Category[ebay.df$Category == "Pottery/Glass"] = "Automotive"
#Category - combining Books & Clothing/Accessories
ebay.df$Category[ebay.df$Category == "Clothing/Accessories"] = "Books"

#Nothing to Combine Together w/ Currency

#EndDay - combining Wed & Sun (closer than Fri/Sun)
ebay.df$endDay[ebay.df$endDay == "Sun"] = "Wed_Sun"
ebay.df$endDay[ebay.df$endDay == "wed"] = "Wed_Sun"

#No close means to combine together w/ Duration

#10.4.b
#Partition data
set.seed(111)
index = sample(nrow(ebay.df), nrow(ebay.df)*0.6)
train.df = ebay.df[index, ]
validate.df = ebay.df[-index, ]
options(scipen = 999, digits = 4)

#Logistic Model
logreg = glm(Competitive. ~., data = train.df, family="binomial")
summary(logreg)

#Predict logreg on validation data
log.pred = predict(logreg, newdata = validate.df, type = "response")
head(log.pred)
confusionMatrix(factor(ifelse(log.pred>0.5, 1, 0)),
                factor(validate.df$Competitive.), positive = "1")

#10.4.c
#Removing Closing Price from Regression Model
new.var = c(1,2,3,4,5,7,8)
new.logreg = glm(Competitive. ~., data = train.df[,-6], family = "binomial")
summary(new.logreg)

#Predict on new logreg w/ validation data
new.pred = predict(new.logreg, newdata = validate.df[,-6], type = "response")
head(new.pred)
confusionMatrix(factor(ifelse(new.pred > 0.5, 1, 0)),
                factor(validate.df$Competitive.), positive = "1")

#10.4.e
#Stepwise regression on new.logreg (no ClosePrice variable)
t.forward = stepAIC(new.logreg, direction = "forward")
summary(t.forward)

t.backward = stepAIC(new.logreg, direction = "backward")
summary(t.backward)

t.both = stepAIC(new.logreg, direction = "both")
summary(t.both)

#Exhaustive Search
pred.var = c("Category", "currency", "sellerRating", "Duration", "endDay", "OpenPrice")
t.exhaust = glmulti("Competitive.", xr = pred.var, data = train.df, level = 1)
summary(t.exhaust)

#10.4.f
# regression on validation data (no Closing Price)
v.logreg = glm(Competitive. ~., data = validate.df[,-6], family = "binomial")

v.forward = stepAIC(v.logreg, direction = "forward")
summary(v.forward)

v.backward = stepAIC(v.logreg, direction = "backward")
summary(v.backward)

v.both = stepAIC(v.logreg, direction = "both")
summary(v.both)

#Exhaustive Search on validation data
pred.var = c("Category", "currency", "sellerRating", "Duration", "endDay", "OpenPrice")
v.exhaust = glmulti("Competitive.", xr = pred.var, data = validate.df, level = 1)
summary(v.exhaust)
