#Problem 3.1

install.packages("ggplot2")
install.packages("forecast")
library(ggplot2)
library(forecast)

appship.df = read.csv("ApplianceShipments.csv")
appship.ts = ts(appship.df$Shipments, start = c(1985, 1), end = c(1989, 4), 
                frequency = 4)

#Time plot of data
plot(appship.ts, xlab = "year", ylab = "Shipments (in million dollars)")

#Zoomed in plot for range of 3500-5000
plot(appship.ts, xlab = "Year", ylab = "Shipments (in million dollars)",
     ylim = c(3500, 5000))

#Creating data frames for each quarter to be graphed
Q1 = ts(appship.ts[seq(1, 20, 4)])
Q2 = ts(appship.ts[seq(2, 20, 4)])
Q3 = ts(appship.ts[seq(3, 20, 4)])
Q4 = ts(appship.ts[seq(4, 20, 4)])

#Plots for each Quarter w/ range from 3500 to 5000
plot(Q1, xlab = "Year", ylab = "Shipments (in million dollars)", 
     ylim = c(3500, 5000))
lines(Q2, xlab = "Year", ylab = "Shipments (in million dollars)",
      col = "red")
lines(Q3, xlab = "Year", ylab = "Shipments (in million dollars)",
      col = "blue")
lines(Q4, xlab = "Year", ylab = "Shipments (in million dollars)",
      col = "green")
legend("bottomright", c("Q1", "Q2", "Q3", "Q4"), 
       col = c("black", "red", "blue", "green"), lty = 1)

#Line graph at a yearly aggregated level
yearly.ts = aggregate(appship.ts, FUN = mean)
plot(yearly.ts, xlab = "Year", ylab = "Shipments (in million dollars)",
     main = "Annual Shipment", ylim = c(3500, 5000))



