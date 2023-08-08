#Problem 14.2

#load packages and data
library(arules)
library(recommenderlab)

courses.df = read.csv("Coursetopics.csv")
View(courses.df)

#Convert dataframe into matrix to be converted
#into transactions list next

courses.mat = as.matrix(courses.df)
courses.mat

courses.trans = as(courses.mat, "transactions")
inspect(courses.trans)
itemFrequencyPlot(courses.trans)

#Generate association rules based on highest lift
rules = apriori(courses.trans, parameter = list(supp = 0.01, conf = 0.5, 
                                                target = "rules"))
inspect(head(sort(rules, by = "lift"),10))
options(digits = 3)
