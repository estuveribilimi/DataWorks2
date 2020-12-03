# Time Series Analysis 
# Day 2

# ARMA Process ------------------------------------------------------------

# Definition of AR (Autoregressive process with order p)
# X_t = sum(phi_k * X_(t-k)) + W_t  for k=1:p

# Definition of MA (Moving Average process with order q)
# X_t = sum(theta_k * Z_(t-k)) + Z_t  for k=1:q

ar1sim=arima.sim(list(order = c(1,0,0), ar = 0.5), n = 800)
autoplot(ar1sim,main="TS Plot of Simulated Data")
acf2(ar1sim)

ma1sim=arima.sim(list(order = c(0,0,1), ma = 0.5), n = 800)
autoplot(ma1sim)
acf2(ma1sim)

## Ggplot version
ggAcf(ar1sim)
ggPacf(ar1sim)

armasim=arima.sim(list(order = c(1,0,1), ar = 0.5, ma=1.5), n = 800)
autoplot(armasim, main="TS Plot of Simulated Data")
acf2(armasim)

autoplot(diff(armasim))

# Other examples 
ar2sim=arima.sim(n = 200, list(order = c(2,0,0), ar = c(1.5,-0.75)))
autoplot(ar2sim,main="TS Plot of Simulated Data")
acf2(ar2sim)

# AR(I)MA(2, 1, 2)
arma2sim=arima.sim(n=800,list(order = c(2,1,2),
                              ar=c(1.1,-0.44),ma=c(1.7,-0.5)))
autoplot(arma2sim,main="TS Plot of Simulated Non-stationary ARIMA Data")
acf2(arma2sim)

# Can not observe tails off, cut off properties !

# Model Fit ----------------------------------------------------

help("auto.arima")
# For the simulated data 
WN_1 <- arima.sim(model = list(order = c(0, 0, 0)), n = 1000)
tsplot(WN_1)
auto.arima(WN_1, trace = T)
# Work for min AIC default

ar1sim=arima.sim(list(order = c(1,0,0), ar = 0.5), n = 800)
auto.arima(ar1sim, trace = T)

# Fitting suitable ARMA model to the data
help("arima")
arima(chicken, order = c(1,0,0))

# to guess ar coefficient
arfit<-ar(chicken, aic=T, order.max = 3)
arfit
auto.arima(chicken)

tsplot(birth)
acf2(birth)
auto.arima(birth)


# OR 
forecast::Acf(birth)
forecast::Pacf(birth)


Autofit<-auto.arima(birth, trace = T)
forecast::checkresiduals(Autofit)
tsplot(birth)
# Use of sarima function
# AR(2) with mean 10:
x <- arima.sim(list(order = c(2, 0, 0),
                    ar = c(0.5, -.75)),
               n = 200) + 10

x_fit <- auto.arima(x)
x_fit

x_fit <- sarima(x, p = 2, d = 0, q = 0)
x_fit$fit$model
x_fit$ttable

# ARMA
# X_{t} = Phi * X_{t-1} + W_{t} + theta * W_{t-1}
x <- arima.sim(list(order = c(1, 0, 1),
                    ar = .9, ma = -.4), n = 500)

acf2(x)
x_fit <- sarima(x, p = 1, d = 0, q = 1)
x_fit$ttable

# Note : 
# Residual Analysis
# sarima() includes residual analysis graphic showing:
# i. Standardized residuals
# ii. Sample ACF of residuals
# iii. Normal Q-Q plot
# iv. Q-statistic p-values

# Meaning of Ljung-Box Test
# a type of statistical test of whether any of a group of 
# autocorrelations of a time series are different from zero. 
# Instead of testing randomness at each distinct lag, 
# it tests the "overall" randomness based on a number of lags, 
# and is therefore a portmanteau test.

# About lag plotting

autoplot(soi)
acf(soi,lag.max = 12)
pacf(soi,lag.max = 12)

# To see lags in integers use 
Acf(soi, lag.max = 12)
Pacf(soi, lag.max = 12)

# lag.plot(soi,lags=12)
lag1.plot(soi,max.lag=12)

# Note: the lowess fits are approximately linear,
# so that the sample autocorrelations are meaningful. 
# Also, we see strong positive linear relations at 
# lags h = 1; 2; 11; 12, and a negative linear relation at lags h = 6; 7
# These results match up well with peaks noticed in the ACF

# Seasonality -------------------------------------------------------------

# Lag plot 
install.packages("fpp")
library(fpp)

data(ausbeer)
head(ausbeer)
frequency(ausbeer)

autoplot(ausbeer)
# window() 
seasonplot(window(ausbeer, start=2002), 
           col = 1:length(window(ausbeer, start=2002)), 
           year.labels = T,labelgap = 0.5, season.labels = T)

# y_{t} plotted againts y_{t-k} for different k values
beer2 <- window(ausbeer, start=1992)
gglagplot(beer2)

gglagplot(ausbeer)

# The relationship is strongly positive at lags 4 and 8, 
# reflecting the strong seasonality in the data

ggsubseriesplot(ausbeer) +
  title("Seasonal subseries plot: ausbeer")

ggsubseriesplot(beer2) +
  title("Seasonal subseries plot: ausbeer")

# How to deseasonalize
frequency(ausbeer)
ts.stl_ausbeer <- stl(ausbeer,"periodic")  # decompose the TS

ts.sa_ausbeer <- seasadj(ts.stl_ausbeer)  # de-seasonalize
plot(ausbeer, type="l")  # original series
plot(ts.sa_ausbeer, type="l")  # seasonal adjusted

seasonplot(beer2, 4, col=rainbow(4), year.labels=TRUE, 
  main="Seasonal plot: ausbeer") 
# seasonal frequency set as 12 for monthly data.

nsdiffs(ausbeer)
nsdiffs(diff(ausbeer, lag=frequency(ausbeer), differences=1))
diff_ausbeer <- diff(ausbeer, lag=frequency(ausbeer), differences=1)

autoplot(diff_ausbeer)

# Forecasting -------------------------------------------------------------
# AR Process

# One-month US inflation rate, 
# Monthly observations from 1950 through 1990
install.packages("Ecdat")
library(Ecdat)
data(Mishkin, package = "Ecdat")
head(Mishkin)
inflation <- as.ts(Mishkin[, 1])
ts.plot(inflation) 
acf2(inflation, max.lag=10)
Acf(inflation, lag.max = 10)
# Simple AR(1) process
AR_inflation <- arima(inflation, order = c(1, 0, 0))

qqnorm(AR_inflation$residuals)
qqline(AR_inflation$residuals)

# Take fitted values 
AR_inflation_fitted <- inflation - residuals(AR_inflation)

ts.plot(inflation)
points(AR_inflation_fitted, type = "l", col = "red", lty = 2)
frequency(inflation)
# Auto.arima
AR_inflation <- auto.arima(inflation, trace=T)
AR_inflation
# The “residuals” in a time series model are what is 
# left over after fitting a model
qqnorm(AR_inflation$residuals)
hist(AR_inflation$residuals)

# Compute the Box–Pierce or Ljung–Box test statistic 
# for examining the null hypothesis of independence 
# in a given time series
# known as ‘portmanteau’ tests.
forecast::checkresiduals(AR_inflation)

# Consider the diff of data 
tsplot(diff(inflation))
AR_diff_inflation <- auto.arima(diff(inflation), trace=T)

forecast::checkresiduals(AR_diff_inflation)

# 1-step ahead forecasts

predict(AR_inflation)

# h-step ahead forecasts
predict(AR_inflation, n.ahead = 6)


# TS Analysis -------------------------------------------------------------

# Monthly spot price for crude oil, Cushing, 
# OK (in U.S. dollars per barrel), 01/1986 - 01/2006
autoplot(oil.price) +
  xlab("Monthly") + ylab("Spot price(US$/barrel)") +
  ggtitle("crude oil (01/1986 - 01/2006)")

adf.test(oil.price); kpss.test(oil.price)
summary(oil.price)

# Not so different 
ggsubseriesplot(oil.price)

# Seasonality
frequency(oil.price)
nsdiffs(oil.price)

diff_Oil <- diff(oil.price)
log_Oil <- log(oil.price)
difflog_oil <- diff(log_Oil)

autoplot(oil.price)
autoplot(diff_Oil)
autoplot(log_Oil)
autoplot(difflog_oil)

# Choose diff_oil or difflog_oil
adf.test(diff_Oil); kpss.test(diff_Oil)

acf2(diff_Oil)
acf2(difflog_oil)

diffModel <- auto.arima(diff_Oil)
diffModel 
sarima(diff_Oil, 1, 0, 2)

difflogModel <- auto.arima(difflog_oil)
difflogModel 
sarima(diff_Oil, 1, 0, 0, 1, 0, 0, 12)

# Log return 
n <- length(oil.price)
lrest <- log(oil.price[-1]/oil.price[-n])
# OR 
lrest <- diff(log(oil.price), lag=1)
autoplot(as.ts(lrest))
adf.test(as.ts(lrest)); kpss.test(as.ts(lrest))

auto.arima(as.ts(lrest))
sarima(lrest, 3, 0, 3)

acf(auto.arima(as.ts(lrest))$residuals)

autoplot(diff(as.ts(lrest)))
Model_oilprice <- auto.arima(diff(as.ts(lrest)))
sarima(lrest, 2, 0, 0)
forecast::checkresiduals(Model_oilprice$residuals)

Box.test(Model_oilprice$residuals,type="Ljung-Box")
Box.test(Model_oilprice$residuals,type="Box-Pierce")

acf(Model_oilprice$residuals)

# Forecasting 
predict(Model_oilprice, n.ahead = 6)

# Alternatively
lambda <- BoxCox.lambda(oil.price)
head(BoxCox(oil.price,lambda))


tsplot(oil.price)
tsplot(as.ts(BoxCox(oil.price,lambda)))
tsplot(diff(as.ts(BoxCox(oil.price,lambda))))
# References --------------------------------------------------------------

# http://r-statistics.co/Time-Series-Analysis-With-R.html

# https://rc2e.com/timeseriesanalysis

# https://faculty.washington.edu/ezivot/econ584/notes/unitroot.pdf

# https://nwfsc-timeseries.github.io/atsa-labs/

# https://debis.deu.edu.tr/userweb/onder.hanedar/dosyalar/Metin.pdf

# https://www.stat.pitt.edu/stoffer/tsa4/index.html

# https://github.com/PacktPublishing/Hands-On-Time-Series-Analysis-with-R

# https://rc2e.com/timeseriesanalysis

# https://bookdown.org/singh_pratap_tejendra/intro_time_series_r/
