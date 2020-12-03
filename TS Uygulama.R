# Data Set and Modeling 

# Manufacture of electrical equipment: computer, electronic and optical products. 
# Data adjusted by working days; Euro area (16 countries). 
# Industry new orders index. 2005=100 

data("elecequip")
head(elecequip)
tail(elecequip)
summary(elecequip)

hist(elecequip)

frequency(elecequip)
autoplot(elecequip)
nsdiffs(elecequip)

adf.test(elecequip); kpss.test(elecequip)
# Series has unit root, not stationary

# Se-seasonalize the series
elecequip %>% 
  stl(s.window='periodic') %>% 
  seasadj() -> eeadj

autoplot(eeadj)
nsdiffs(eeadj)

# There is no evidence of changing variance, 
# so we will not do a Box-Cox transformation.

# The data are clearly non-stationary, 
# as the series wanders up and down for long periods.
adf.test(eeadj); kpss.test(eeadj)

eeadj %>% 
  diff() %>% 
  ggtsdisplay(main="")

# The PACF shown in Figure 8.13 is suggestive of an AR(3) model. 
# So an initial candidate model is an ARIMA(3,1,0)

fit1 <- Arima(eeadj, order=c(3,1,0))
fit1
fit2 <- Arima(eeadj, order=c(3,1,1))
fit2

# Built-inf functions for modeling
auto.arima(eeadj)
forecast::checkresiduals(fit2)

Box.test(fit2$residuals,type = "Box-Pierce")
Box.test(fit2$residuals,type = "Ljung-Box")


plot(fit2$fitted)
points(eeadj, type="l", col=2)

plot(fit2$fitted, fit2$residuals)

sarima(eeadj, 3, 1, 1)

# Forecasting part

forecast(fit2)
length(forecast(fit2))

autoplot(forecast(fit2))


