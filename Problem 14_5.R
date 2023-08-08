#######################################

#Problem 14.5 Course Ratings

#######################################

#Load Packages
library(recommenderlab)

#Part A
#Calculating correlations
#Average Rating by Student
LN = (4+3+2+4+2)/5
LN #3
EN = (4+4+4+3)/4
EN #3.75
DS = (4+2+4)/3
DS #3.33
Class1_LNEN = (4-3)*(4-3.75) #0.25
Class2_LNEN = (4-3)*(4-3.75) #0.25
Class3_LNEN = (2-3)*(3-3.75) #0.75
Numer_LNEN = Class1_LNEN+Class2_LNEN+Class3_LNEN
Numer_LNEN #1.25

Class1_DSEN = (4-3.33)*(4-3.75) #0.167
Class2_DSEN = (2-3.33)*(4-3.75) #-0.333
Class3_DSEN = (4-3.33)*(4-3.75) #0.167
Numer_DSEN = Class1_DSEN+Class2_DSEN+Class3_DSEN
Numer_DSEN #0.0025

LN_sqrt = sqrt(((4-3)^2)+((4-3)^2)+((2-3)^2))
EN_sqrt1 = sqrt(((4-3.75)^2)+((4-3.75)^2)+((3-3.75)^2))
EN_sqrt2 = sqrt(((4-3.75)^2)+((4-3.75)^2)+((4-3.75)^2))
DS_sqrt = sqrt(((4-3.33)^2)+((2-3.33)^2)+((4-3.33)^2))
Denom_LNEN = LN_sqrt + EN_sqrt1
Denom_LNEN
Denom_DSEN = DS_sqrt + EN_sqrt2
Denom_DSEN

Corr_LNEN = Numer_LNEN/Denom_LNEN
Corr_LNEN

Corr_DSEN = Numer_DSEN/Denom_DSEN
Corr_DSEN

#Part C - Cosine Similarity
rating.df = read.csv("courserating.csv")
View(rating.df)
row.names(rating.df) = rating.df[,1]
rating.df = rating.df[,-1]
head(rating.df)

#create matrix and turn ratings/na's into binary values
m1 = as.matrix(rating.df)
m1[!is.na(m1)] = 1
m1[is.na(m1)] = 0
#Create sub-matrix of only LN, EN, and DS
msub = m1[c(1,4,15),]
r1 = as(msub, "realRatingMatrix")
#Calculate cosine similarity between LN, EN, and DS
similarity(r1, method = "cosine")

#Part F.ii
#SQL-Spatial pair
#Calculate numerator based on co-ratings
Numer1 = (3*4)+(2*2)+(4*4)
Denom1 = sqrt((3^2)+(2^2)+(4^2))*sqrt((4^2)+(2^2)+(4^2))
Sim1 = Numer1/Denom1 #Correlation between SQL-Spatial
Sim1

#SQL-Python pair
Numer2 = (4*3)+(3*4)
Denom2 = sqrt((4^2)+(3^2))*sqrt((3^2)+(4^2))
Sim2 = Numer2/Denom2
Sim2 #Correlation between SQL-Python

#Part G - Item-Based Recommendation/Filtering
#Use entire dataset since focused on item-based
#Using binaryRatingMatrix as m1 was converted to it
r2 = as(m1, "binaryRatingMatrix")
IB.rec = Recommender(r2, "IBCF")
IB.pred = predict(IB.rec, r2, type = "ratings")
as(IB.pred[4,], "matrix")
