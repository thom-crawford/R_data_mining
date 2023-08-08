#Problem 17.1

#Load Packages and Data

library(forecast)

travel.df = read.csv("Sept11Travel.csv")
View(travel.df)

#Part A - Create Time Series for Air, partition data in Pre/Post
air.ts = ts(travel.df$Air.RPM..000s., start = c(1990,1), end = c(2004,4),
            frequency = 12)
nvalid = 32
ntrain = length(air.ts) - nvalid
train.ts = window(air.ts, start = c(1990, 1), end = c(1990,ntrain))
valid.ts = window(air.ts, start = c(1990, ntrain+1), end = c(1990, ntrain + nvalid))

#Plotting pre-event Air data
air.pre.ts = ts(travel.df$Air.RPM..000s., start = c(1990,1), end = c(2001,9), 
                frequency = 12)
plot(air.pre.ts, xlab = "Time", ylab = "Air RPM (000s)", main = "Time Plot of Air RPMs Pre-Event Series")

#Part D - Regression model
air.train.lm = tslm(train.ts ~ season, lambda = 0)
summary(air.train.lm)

fit.value = air.train.lm$fitted.values[1]
fit.value
act.value = train.ts[1]
act.value
residual.air = act.value - fit.value
residual.air

#Part E - Create ACF plot using residuals
Acf(residuals(air.train.lm), lag.max = 12, main = "ACF Plot for Residuals")
#checkresiduals(air.train.lm, lag.max = 12)

#Part F - Linear Regression Models for Air, Rail, and Auto (w/ seasonality)
#Create time series with pre and post, end = c(2004,4)
new.air.ts = ts(travel.df$Air.RPM..000s., start = c(1990, 1), end = c(2004,4),
                frequency = 12)
rail.ts = ts(travel.df$Rail.PM, start = c(1990, 1), end = c(2004,4), 
             frequency = 12)
auto.ts = ts(travel.df$VMT..billions., start = c(1990,1), end = c(2004,4),
             frequency = 12)
#Create partitions for use in regression models
air.train.ts = window(new.air.ts, start = c(1990,1), end = c(1990, ntrain))
air.valid.ts = window(new.air.ts, start = c(1990, ntrain+1), end = c(1990, ntrain+nvalid))

rail.train.ts = window(rail.ts, start = c(1990,1), end = c(1990, ntrain))
rail.valid.ts = window(rail.ts, start = c(1990, ntrain+1), end = c(1990, ntrain+nvalid))

auto.train.ts = window(auto.ts, start = c(1990,1), end = c(1990, ntrain))
auto.valid.ts = window(auto.ts, start = c(1990, ntrain+1), end = c(1990, ntrain+nvalid))

#Linear Regression for Air
train.air.season.lm = tslm(air.train.ts ~ trend + season)
train.air.season.lm.pred = forecast(train.air.season.lm, h=nvalid, level = 0)
plot(train.air.season.lm.pred, ylab = "Air Travel (000s)", xlab = "Time", bty = "l", xaxt = "n", flty = 2)
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(train.air.season.lm.pred$fitted, lwd = 2, col = "blue")
lines(air.valid.ts)

#Quadratic Regression for Rail
train.rail.season.lm = tslm(rail.train.ts ~ poly(trend, 2, raw = TRUE)+season)
train.rail.season.lm.pred = forecast(train.rail.season.lm, h=nvalid, level = 0)
plot(train.rail.season.lm.pred, ylab = "Rail Travel", xlab = "Time", bty = "l", xaxt = "n", flty = 2)
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(train.rail.season.lm.pred$fitted, lwd = 2, col = "blue")
lines(rail.valid.ts)

#Linear Regression for Auto
train.auto.season.lm = tslm(auto.train.ts ~ trend + season)
train.auto.season.lm.pred = forecast(train.auto.season.lm, h=nvalid, level = 0)
plot(train.auto.season.lm.pred, ylab = "Auto Travel (Billions)", xlab = "Time", bty = "l", xaxt = "n", flty = 2)
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(train.auto.season.lm.pred$fitted, lwd = 2, col = "blue")
lines(auto.valid.ts)