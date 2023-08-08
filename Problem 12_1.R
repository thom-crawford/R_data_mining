#Problem 12.1

#Install packages and load data
library(DiscriMiner)
library(caret)
library(gains)
library(MASS)

bank.df = read.csv("UniversalBank.csv")
#View(bank.df)

#Pre-process data, convert education into dummy variables
bank.df$Education_level2 = bank.df$Education==2
bank.df$Education_level3 = bank.df$Education==3

#Partition data - 60% train, 40% validation
set.seed(111)
index = sample(nrow(bank.df), nrow(bank.df)*0.6)
train = bank.df[index, ]
validate = bank.df[-index, ]

#Part 12.1.a
#Compute summary statistics for predictors 
pred.vars = c("Age", "Experience", "Income", "Family", "CCAvg",
              "Mortgage", "Securities.Account", "CD.Account", "Online",
              "CreditCard", "Education_level2", "Education_level3")
data.frame(sapply(bank.df[bank.df$Personal.Loan == 1, pred.vars], mean),
           sapply(bank.df[bank.df$Personal.Loan == 0, pred.vars], mean),
           sapply(bank.df[bank.df$Personal.Loan == 1, pred.vars], sd),
           sapply(bank.df[bank.df$Personal.Loan == 0, pred.vars], sd))

#Part 12.1.b
#validate model performance of discrim analysis
discrim = linDA(train[,pred.vars], train$Personal.Loan)
cls = classify(discrim, newdata = validate[,pred.vars])
confusionMatrix(factor(cls$pred_class), factor(validate$Personal.Loan),
                positive = "1")

#Part 12.1.b.iii
#Calc probability of success from cls
denom = exp(cls$scores[,1])+exp(cls$scores[,2])
numer = exp(cls$scores[,2])
success = numer/denom
#Create dataframe to hold all prob values
actualANDpred = data.frame(ID = validate$ID, Actual_Class = validate$Personal.Loan, 
                           Predicted_Class = cls$pred_class, success)
#Acceptors misclassified as Non-Acceptors
mis.acceptors = actualANDpred[which(actualANDpred$Actual_Class==1 &
                                      actualANDpred$Predicted_Class==0),]
head(mis.acceptors,3)

#Non-Acceptors misclassified as Acceptors
mis.nonaccept = actualANDpred[which(actualANDpred$Actual_Class==0 &
                                      actualANDpred$Predicted_Class==1),]
head(mis.nonaccept,3)

#Part 12.1.c
#Create Lift Chart
lift = gains(validate$Personal.Loan, success, groups = 10)
length(success)
plot(c(0, lift$cume.pct.of.total*sum(validate$Personal.Loan))~c(0, lift$cume.obs),
     xlab = "# of cases", ylab = "Cumulative", main = "", type = "l")
lines(c(0, sum(validate$Personal.Loan)) ~ c(0, nrow(validate)), lty=2)

#Decile-Wise lift Chart
heights = lift$mean.resp/mean(validate$Personal.Loan)
midpoints = barplot(heights, names.arg = lift$depth, ylim = c(0,9),
                    xlab = "Depth of File",
                    ylab = "Mean Response",
                    main = "Decile-Wise Lift Chart")
text(midpoints, heights+0.5, labels = round(heights,1), cex = 0.8)

#Part 12.1.d
#Logistic Regression
bank.log = glm(Personal.Loan ~ Age + Experience + Income + Family + CCAvg + Mortgage +
                 Securities.Account + CD.Account + Online + CreditCard + Education_level2 + Education_level3,
               data = train, family = "binomial")
summary(bank.log)
#Check w/ validation data
bank.pred = predict(bank.log, newdata = validate, type = "response")
head(bank.pred)
confusionMatrix(factor(ifelse(bank.pred>0.5, 1, 0)),
                factor(validate$Personal.Loan), positive = "1")
#Create Lift Chart
log.lift = gains(validate$Personal.Loan, bank.pred, groups = 10)
plot(c(0, log.lift$cume.pct.of.total*sum(validate$Personal.Loan))~c(0, log.lift$cume.obs),
     xlab = "# of cases", ylab = "Cumulative", main = "", type = "l")
lines(c(0, sum(validate$Personal.Loan))~c(0, nrow(validate)), lty=2)
#Decile Wise Lift Chart
log.heights = log.lift$mean.resp/mean(validate$Personal.Loan)
log.mid = barplot(log.heights, names.arg = log.lift$depth, ylim = c(0,9),
                  xlab = "Depth of File", 
                  ylab = "Mean Response",
                  main = "Decile-Wise Lift Chart")
text(log.mid, log.heights+0.5, labels = round(log.heights,1), cex = 0.8)

#Part 12.1.e
MaxProfit = 1000 * 50
MaxProfit
Cost = 1000 * 1
Cost
TotalProfit = MaxProfit-Cost
TotalProfit
summary(success) #Use calculated mean response from discriminant analysis
ExpectedProfit = TotalProfit*0.0896361
ExpectedProfit
