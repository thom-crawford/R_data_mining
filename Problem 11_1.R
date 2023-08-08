#Problem 11.1
library(caret)
library(neuralnet)
library(forecast)
library(gains)

data.df = data.frame(Years = c(4,18,1,3,15,6),
                     Salary = c(43,65,53,95,88,112),
                     Used_Credit = c(0,1,0,0,1,1))
View(data.df)
data.df$Used_CreditY = ifelse(data.df$Used_Credit == 1,1,0)
data.df$Used_CreditN = ifelse(data.df$Used_Credit == 0,1,0)
head(data.df)

#Create Neural Net
nn1 = neuralnet(Used_CreditY + Used_CreditN ~ Years + Salary,
                data = data.df, linear.output = F, hidden = 3)
plot(nn1)
