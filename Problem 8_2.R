#Problem 8.2
#Load Data & create INJURY dummy variable
library(e1071)
library(caret)

accidents.df = read.csv("accidentsFull.csv")
View(accidents.df)

accidents.df$INJURY = ifelse(accidents.df$MAX_SEV_IR>0, "yes", "no")
head(accidents.df)

#Part A
inj.table = table(accidents.df$INJURY)
inj.table
#Calculate probability of Injury happening
inj.prob = (inj.table["yes"])/(inj.table["yes"]+inj.table["no"])
inj.prob

#Part B
#Convert and Verify all variables are categorical
for (i in c(1:dim(accidents.df)[2])){
  accidents.df[,i] = as.factor(accidents.df[,i])
}
str(accidents.df)
pred.vars = c("INJURY", "WEATHER_R", "TRAF_CON_R")
head(accidents.df[ ,pred.vars], 12)

#B.i - Create pivot table examining INJURY
#ftable(accidents.df[1:12,pred.vars])
#More detailed pivot table
library(pivottabler)
pivot.acci = qhpvt(accidents.df[1:12,], c("INJURY", "WEATHER_R"), "TRAF_CON_R", "n()")

#B.ii
#P(INJURY = yes | WEATHER_R = 1, TRAF_CON_R = 0)
num1 = (2/3)*(3/12)
denom1 = 3/12
prob1 = num1/denom1
prob1
#P(INJURY = yes | WEATHER_R = 1, TRAF_CON_R = 1)
num2 = 0 * (3/12)
denom2 = 1/12
prob2 = num2/denom2
prob2
#P(INJURY = yes | WEATHER_R = 1, TRAF_CON_R = 2)
num3 = 0 * (3/12)
denom3 = 1/12
prob3 = num3/denom3
prob3
#P(INJURY = yes | WEATHER_R = 2, TRAF_CON_R = 0)
num4 = (1/3) * (3/12)
denom4 = 6/12
prob4 = num4/denom4
prob4
#P(INJURY = yes | WEATHER_R = 2, TRAF_CON_R = 1)
num5 = 0 * (3/12)
denom5 = 1/12
prob5 = num5/denom5
prob5
#P(INJURY = yes | WEATHER_R = 2, TRAF_CON_R = 2)
num6 = 0 * (3/12)
denom6 = 1/12
prob6 = num6/denom6
prob6

#B.iii
#Create new separate df to handle the probabilities calculated above
prob.df = accidents.df[1:12, pred.vars]
head(prob.df)
#Create list of corresponding probabilities for 12 accidents
prob.inj = c(0.667, 0.167, 0, 0, 0.667, 0.167, 0.167, 0.667, 0.167, 0.167, 0.167, 0)
#Add new list as column to df
prob.df$prob.inj = prob.inj
prob.df
#Create new column predicting injury w/ cutoff of 0.5
prob.df$predict.inj = ifelse(prob.df$prob.inj>0.5, "Yes", "No")
prob.df

#B.v
new.df = accidents.df[1:12, pred.vars]
nb = naiveBayes(INJURY ~ ., data = new.df)
predict(nb, newdata = new.df, type = "raw")

#check accuracy of model using caret package
#caret library already installed
nb2 = train(INJURY ~ WEATHER_R + TRAF_CON_R, data = accidents.df[1:12], method = "nb")
predict(nb2, newdata = accidents.df[1:12, pred.vars])
predict(nb2, newdata = accidents.df[1:12, pred.vars], type = "raw")
#Returns NO for all rows, doesn't seem correct

#Tried different method found in ?train help, labelled default S3 Method
#x is out dataframe being used, removing outcome variable
x = new.df[,-1]
#y is the predicted outcome variable for the data
y = new.df$INJURY
#Default S3 method, specifying data frame, outcome variable, 
#and using naive-Bayes 
model = train(x,y,method = 'nb')
model
#Caret Model has very low accuracy at classifying Injury or Not 
#based on our 2 predictors
#Run prediction outcome with model and dataframe
model.pred = predict(model, x)
model.pred
#same results as nb2 model from above, return No for all rows

#Part C
#Partition entire dataset
set.seed(5)
train.index = sample(nrow(accidents.df), nrow(accidents.df)*0.6)
train.df = accidents.df[train.index,]
validate.df = accidents.df[-train.index,]

#C.ii
#Create vector of important variables
imp.vars = c("INJURY", "ï..HOUR_I_R",  "ALIGN_I" ,"WRK_ZONE",  "WKDY_I_R",
             "INT_HWY",  "LGTCON_I_R", "PROFIL_I_R", "SPD_LIM", "SUR_COND",
             "TRAF_CON_R",   "TRAF_WAY",   "WEATHER_R")
nb3 = naiveBayes(INJURY ~ ., data = train.df[,imp.vars])
nb3
#Create confusion matrix
confusionMatrix(train.df$INJURY, predict(nb3, train.df[,imp.vars]),
                positive = "yes")

#C.iii
#Overall Error Rate
confusionMatrix(validate.df$INJURY, predict(nb3, validate.df[,imp.vars]),
                positive = "yes")
#1-accuracy from Confusion Matrix for validate.df
error = 1 - 0.5347
error
error.rate = error*100
error.rate

#C.v
#Reducing number of digits
options(digits = 3)
nb3
