####################################
#problem 15.1 University Rankings
####################################

#load packages and data
library(caret)

uni.df = read.csv("Universities.csv")
View(uni.df)

#Part A
#Remove missing data points from data frame
missing.data = rowSums(is.na(uni.df))>0
uni.df.complete = uni.df[!missing.data,]
View(uni.df.complete)

#Part B
#Cluster Analysis
#First have to normalize input variables
uni.df.complete.norm = uni.df.complete[,-c(1:3)] #Removing non-continuous variables
norm.values = preProcess(uni.df.complete[,-c(1:3)], method = c("center", "scale"))
uni.df.complete.norm = predict(norm.values, uni.df.complete[,-c(1:3)])

#Compute Euclidean Distance
d.norm = dist(uni.df.complete.norm, method = "euclidean")

#Create Clusters
hc1 = hclust(d.norm, method = "complete")
plot(hc1, hang = -1, ann = FALSE)

#Prune Tree - too dense to read
hc1.pruned = cutree(hc1, k=2)
table(hc1.pruned)

#Part C
#Compute summary statistics for each cluster (2)
clusters = aggregate(.~hc1.pruned, data = uni.df.complete, FUN = mean)
clusters.norm = aggregate(.~hc1.pruned, data = uni.df.complete.norm, FUN = mean)

clusters
clusters.norm

#Part D
#Use categorical data to characterize Clusters
row.names(uni.df.complete) <- paste(hc1.pruned, ": ", row.names(uni.df.complete), sep = "")

#Part F
#Tufts University is missing values, need new original copy of data set
tufts.df = uni.df[uni.df['College.Name']=='Tufts University']
#Convert NAs into 0 to allow for computing distance
tufts.df[is.na(tufts.df)] = 0
#Remove categorical data
tufts.cont = as.integer(tufts.df[-c(1:3)])
#Define euclidean function to use
Euclidean = function(x1,x2)sqrt(sum((x1-x2)^2))

for (i in 1:nrow(clusters.norm)){
  tufts = clusters.norm[i,][-1]
  print(paste('Distance from cluster', i,'is', Euclidean(tufts.cont,tufts) ))
}

#Compute missing values for Tufts based on averages from Cluster 1
#Data missing is for X..PT.Undergrad
uni.df$X..PT.undergrad[uni.df$College.Name == 'Tufts University'] = clusters.norm$X..PT.undergrad[clusters.norm$hc1.pruned==2]

uni.df[uni.df['College.Name']=='Tufts University']
