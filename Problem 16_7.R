#Problem 16.7 = Shampoo Sales

#Load Packages and Data
library(forecast)

Sys.setlocale(category = "LC_ALL", locale = "English")
Sys.setlocale("LC_TIME", "ENGLISH")

shampoo.df = read.csv("ShampooSales.csv")

#Part A - Creating time plot
#First create ts object
shampoo.ts = ts(shampoo.df$Shampoo.Sales, start = c(1995,1), end = c(1997,12),
                frequency = 12)
plot(shampoo.ts, xlab = "Quarter", ylab = "Sales",
     main = "Time Plot of Monthly Shampoo Sales \n from 1995-1998")

plot(shampoo.ts, xlab = "Time", ylab = "Sales ($)", log = 'xy',
     main = "Time Plot of Monthly Shampoo Sales \n from 1995-1998")
