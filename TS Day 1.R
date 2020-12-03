# Time Series Analysis 
# Day 1

# Very short Introduction for Time Series Analysis

# Some packages to use, CHECK they are available or NOT !!!
library(astsa)
library(TSA)
library(forecast)
library(xts)

# ts object ---------------------------------------------------------------

y <- ts(matrix(rnorm(100), 100, 1), start = c(1961, 1), frequency = 12)
class(y)
head(y)
plot(y)

z <- ts(matrix(rnorm(300), 100, 3), start = c(1961, 1), frequency = 12)
class(z)
head(z) # as "matrix"
plot(z)
plot(z, plot.type = "single", lty = 1:3)


my.ts=sin(seq(pi,10*pi,0.1)) + rnorm(length(seq(pi,10*pi,0.1)))
plot(my.ts)
my.ts=ts(my.ts,start=1800)
plot(my.ts)

# Plotting (Visualization) ------------------------------------------------
help(package="astsa")

data()
birth <- astsa::birth
class(birth)
chicken <- astsa::chicken 
blood <- astsa::blood
gdp <- astsa::gdp

tsplot(birth)
tsplot(chicken)
tsplot(blood)
tsplot(gdp)

#Another approach
plot(ts(birth, start = 1948, frequency = 12),
     ylab="birth")

# Use of tsplot function
tsplot(jj, ylab="Quarterly Earnings per Share")
tsplot(globtemp, type="o", ylab="Global Temperature Deviations")


# The annual rainfall amounts recorded in Los Angeles, California, over more than 100 years
data(larain)
autoplot(larain,y='Inches',x='Year')

# Daily returns of the google stock from 08/20/04 - 09/13/06.
data(google)
autoplot(google,y='Inches',x='Year')

# Monthly spot price for crude oil
data(oil.price)
autoplot(oil.price,y='Inches',x='Year')

# Average air temperatures at Nottingham Castle in degrees Fahrenheit for 20 years
data(nottem)
autoplot(nottem,y='Inches',x='Year')

# Levels of Carbon Dioxide at Alert, Canada
data(co2)
autoplot(co2,y='co2')

# library(xts)
plot(sp500w, main = "S&P 500 Weekly Returns")

# About Trend
tsplot(chicken)
summary(fit <- lm(chicken~time(chicken))) # regress price on time
tsplot(chicken, ylab="cents per pound", col=4, lwd=2)
abline(fit)           # add the fitted regression line to the plot         

par(mfrow=c(2,1))
tsplot(resid(fit), main="detrended")
tsplot(diff(chicken), main="first difference")
# Diff function 
# For the first difference: x_{t} - x_{t-1}

dev.new()
par(mfrow=c(3,1))     # plot ACFs
acf1(chicken, 48, main="chicken")
acf1(resid(fit), 48, main="detrended")
acf1(diff(chicken), 48, main="first difference")
dev.off()

tsplot(soi, ylab="", main="Southern Oscillation Index")
lines(lowess(soi, f=.05), lwd=2, col=4) # El Nino cycle
lines(lowess(soi), lty=2, lwd=2, col=2) # trend (with default span)

# White Noise -------------------------------------------------------------

# model = ARIMA(p, d, q)
# Simulate n = 1000 observations from the WN model
WN_1 <- arima.sim(model = list(order = c(0, 0, 0)), n = 1000)
WN_2 <- arima.sim(model = list(order = c(0, 0, 0)), n = 1000,
                  mean = 4,
                  sd = 2)
par(mfrow=c(2,1))
ts.plot(WN_1)
ts.plot(WN_2)

# Estimating WN process

arima(WN_2, order = c(0, 0, 0))


# Random Walk -------------------------------------------------------------

# Random Walk (RW) is a simple example of a non-stationary
# process having the following properties
# 
# No specified mean or variance
# Strong dependence over time
# Its changes or increments are white noise (WN)

# Today = Y esterday + Noise
# Formally 
# Y_{t} = Y_{t-1} + epsion_{t} where epsion_{t} is mean zero WN

## set random number seed
set.seed(123)
## length of time series
TT <- 1000
## initialize {x_t} and {w_t}
xx <- ww <- rnorm(n = TT, mean = 0, sd = 1)
## compute values 2 thru TT
for (t in 2:TT) {  xx[t] <- xx[t - 1] + ww[t] }
plot.ts(xx, ylim=c(-5,55), main="random walk")
# diff() function 

# The random walk with a drift:
# Y_{t} = c + Y_{t-1} + epsion_{t},  c is constant drift

set.seed(154) # so you can reproduce the results
w = rnorm(200,0,1); 
x = cumsum(w) # cumulative sum
wd = w +.2; 
xd = cumsum(wd)
plot.ts(xd, ylim=c(-5,55), main="random walk with drift")
lines(x); lines(.2*(1:200), lty="dashed")

# Random Walk with Drift and Deterministic Trend
# Y_{t} = c + Bt + Y_{t-1} + epsion_{t},  c is drift, 
# Bt is deterministic trend 


# Make Stationariy --------------------------------------------------------

#' The nonstationarity will frequently be apparent in the time series plot of the series especially plot of the series and ACF.
#' There are two reasons causing non-stationary which are stochastic unit root or trend stationarity (deterministic trend).
#' Formal tests can help to determine whether a system contains a trend and whether the trend is deterministic or stochastic.
#' KPSS tests are used for testing a null hypothesis that an observable time series is stationary around a deterministic trend 
#' (i.e. trend-stationary) against the alternative of a unit root.
#' 
#' Ho: The process is stationary. H1: The process is not stationary.
#' If Ho is rejected at the first step, this following hypothesis is used for the second step of the test.
#' Ho: There is a deterministic trend. H1: There is a stochastic trend.
#' 
#' 

kpss.test(xx); adf.test(xx)

kpss.test(wd); adf.test(wd)

tsplot(chicken)
library(tseries) #for KPSS test

adf.test(WN_1); kpss.test(WN_1)

kpss.test(WN_1, null=c("Level")) #to check stationary or not
kpss.test(chicken, null=c("Level")) #to check stationary or not

kpss.test(chicken, null=c("Trend")) #to check deterministic or stochastic trend

# Other tests
# the null that x has a unit root.
adf.test(chicken); kpss.test(chicken) # Do not reject, has a  unit root

# Non-Stationary in Variance

# If the process is non-stationary in mean, it is also non-stationary in variance. 
# To solve this problem, we apply delta method using box-cox or power transformation.
data(airpass)
autoplot(airpass)
autoplot(log(airpass))

# For more details
# Source: https://ozancanozdemir.shinyapps.io/s497r4/#section-introduction

###
# Trend and Difference Stationary
# diff() function
# requires 3 arguments: x (the data), 
# lag (the lag at which to difference), 
# differences (the order of differencing d

# For example, first-differencing a time series will remove 
# a linear trend (i.e., differences=1); 

# twice-differencing will remove a quadratic trend (i.e., differences=2). 

# In addition, first-differencing a time series at a lag equal to the 
# period will remove a seasonal trend (e.g., set lag=12 for monthly data)

tsplot(birth)
tsplot(diff(birth, 1))

length(birth)
length(diff(birth))

adf.test(diff(birth)); kpss.test(diff(birth)) 

head(birth)
head(diff(birth))
head(diff(birth, lag = 4))

tsplot(diff(birth, lag = 4))

## twice-difference the CO2 data

plot(co2)
co2.D2 <- diff(co2) # with respect to lag
## plot the differenced data
plot(co2.D2, ylab = expression(paste(nabla^1, "CO"[1])))

co2.D2 <- diff(co2, differences = 2)
## plot the differenced data
plot(co2.D2, ylab = expression(paste(nabla^2, "CO"[2])))

adf.test(co2.D2); kpss.test(co2.D2)

## difference the differenced CO2 data
co2.D2D12 <- diff(co2.D2, lag = 12)
## plot the newly differenced data
plot(co2.D2D12, ylab = expression(paste(nabla, "(", nabla^2, 
                                        "CO"[2], ")")))

adf.test(co2.D2D12) ; kpss.test(co2.D2D12)
# Observed time series:
#  Fluctuate randomly
#  But behave similarly from one time period to the next
# Weak stationary: mean, variance, covariance constant over time.
# A stationary process can be modeled with fewer parameters.

# ACF and PACF ---------------------------------------------------------

# Lag 1 Autocorrelation:
# Correlation of stock djia
djia <- astsa::djia$Close
ts.plot(djia)
length(djia)


cor(djia[-2518], djia[-1])

# Lag 2 Autocorrelation:
# Correlation of Stock djia
cor(djia[-(2517:2518)],djia[-(1:2)])

# acf
acf(djia, lag.max = 2, plot = FALSE)
acf(djia, lag.max = 1000)

# The use of ACF and PACF for detecting model 
acf(chicken)
acf(chicken, type = 'correlation', na.action = na.pass)
pacf(chicken, na.action = na.pass)

tsplot(diff(chicken))
# OR 
acf(diff(chicken), lag.max = 10)
acf(diff(chicken), type='partial')

# AR and MA Process ------------------------------------------------------------

# Definition of AR (Autoregressive process with order p)
# X_t = sum(phi_k * X_(t-k)) + W_t  for k=1:p

# Definition of MA (Moving Average process with order q)
# X_t = sum(theta_k * Z_(t-k)) + Z_t  for k=1:q

# Simulation of ARMA
help("arima.sim")
Sim1 <- arima.sim(model=list(0,0,0), n=1000)

tsplot(Sim1)

# The use of arima.sim() function
# AR(1)

ar1sim=arima.sim(list(order = c(1,0,0), ar = 0.5), n = 800)
autoplot(ar1sim,main="TS Plot of Simulated Data")
acf2(ar1sim)

# MA(1)
ma1sim=arima.sim(list(order = c(0,0,1), ma = 0.5), n = 800)
autoplot(ma1sim)
acf2(ma1sim)
