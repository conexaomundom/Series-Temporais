rm(list = ls())
install.packages("forecast")
install.packages("openssl")
install.packages("swirl")
install.packages("plotly")
install.packages("ggfortify")
install.packages("tseries")
install.packages("aTSA")
install.packages("gridExtra")
install.packages("docstring")
install.packages("readr")
install.packages("here")
install.packages("corrplot")
install.packages("TSA")
install.packages("dygraphs")

library(assertthat)
library(dygraphs)
library(xts)
library(TTR)
library(tibble)
library(TSA)
library(gridExtra)
library(corrplot)
library(data.table)
library(tidyr)
library(dplyr)
library(swirl)
library(ggplot2)
library(forecast)
library(plotly)
library(ggfortify)
library(tseries)
library(aTSA)
library(gridExtra)
library(docstring)
library(readr)
library(here)

# diferenciar

#  sarima...
# caso sazonal pra prediçaõ 
#### Banco muito interessante

summary(MCD_2006)
str(MCD_2006)

# No lugar dos valores faltantes serão postos 0.
MCD_2006[is.na(MCD_2006)] <- 0
MCD_2006$Date <- as.Date(MCD_2006$Date, format = "%Y-%m-%d")
summary(MCD_2006)
str(MCD_2006)

## MCD_2006
plot(MCD_2006$Open, type = "l")
plot(MCD_2006$High, type = "l")
plot(MCD_2006$Low, type = "l")
plot(MCD_2006$Close, type = "l")


par(mfrow = c(2,2))
plot(MCD_2006$Open, type = "l")
plot(diff(MCD_2006$Open, 1), type = "l")
plot(diff(MCD_2006$Open, 2), type = "l")
plot(diff(MCD_2006$Open, 3), type = "l")

acf(na.omit(MCD_2006$Open))
acf(na.omit(diff(MCD_2006$Open, 1)))
acf(na.omit(diff(MCD_2006$Open, 2)))
acf(na.omit(diff(MCD_2006$Open, 3)))

par(mfrow = c(1,1))
plot(MCD_2006$High, type = "l")
plot(diff(MCD_2006$High, 1), type = "l")
plot(diff(MCD_2006$High, 2), type = "l")
plot(diff(MCD_2006$High, 3), type = "l")

acf(na.omit(MCD_2006$High))
acf(na.omit(diff(MCD_2006$High, 1)))
acf(na.omit(diff(MCD_2006$High, 2)))
acf(na.omit(diff(MCD_2006$High, 3)))

par(mfrow = c(2,2))
plot(MCD_2006$Low, type = "l")
plot(diff(MCD_2006$Low, 1), type = "l")
plot(diff(MCD_2006$Low, 2), type = "l")
plot(diff(MCD_2006$Low, 3), type = "l")

par(mfrow = c(2,2))
acf(na.omit(MCD_2006$Low))
acf(na.omit(diff(MCD_2006$Low, 1)))
acf(na.omit(diff(MCD_2006$Low, 2)))
acf(na.omit(diff(MCD_2006$Low, 3)))

par(mfrow = c(2,2))
plot(MCD_2006$Close, type = "l")
plot(diff(MCD_2006$Close, 1), type = "l")
plot(diff(MCD_2006$Close, 2), type = "l")
plot(diff(MCD_2006$Close, 3), type = "l")

par(mfrow = c(2,2))
acf(na.omit(MCD_2006$Close))
acf(na.omit(diff(MCD_2006$Close, 1)))
acf(na.omit(diff(MCD_2006$Close, 2)))
acf(na.omit(diff(MCD_2006$Close, 3)))

options(repr.plot.width=12, repr.plot.height=12) 
p1 = ggplot(MCD_2006, aes(Open)) + geom_histogram(bins = 50, aes(y = ..density..), col = "black", fill = "black", alpha = 0.3) + geom_density()# + xlim(c(0, 1000))
p2 = ggplot(MCD_2006, aes(High)) + geom_histogram(bins = 50, aes(y = ..density..), col = "black", fill = "black", alpha = 0.3) + geom_density()# + xlim(c(0, 1000))
p3 = ggplot(MCD_2006, aes(Low)) + geom_histogram(bins = 50, aes(y = ..density..), col =  "black", fill = "black", alpha = 0.3) + geom_density()# + xlim(c(0, 1000))
p4 = ggplot(MCD_2006, aes(Close)) + geom_histogram(bins = 50, aes(y = ..density..), col =  "black", fill = "black", alpha = 0.3) + geom_density()# + xlim(c(0, 1000))
grid.arrange(p1,p2,p3,p4, nrow=2,ncol=2)

# Create and plot Time Series - Open
  xts = xts(MCD_2006$Open, order.by=MCD_2006$Date)
  attr(xts, 'frequency') <- length(xts)/12
  ts = as.ts(xts, start = c(2006))
  
# Estacionario?
  adf.test(xts)

dygraph(xts, xlab = "Time", ylab = "High valor", main = "Séries Temporais") %>%
  # dySeries(labels.default()) %>%
  # dyOptions(colors = c("red")) %>%
  dyRangeSelector()

## Decomposing Time Series
tscomponents_add <- decompose(ts, type = "additive")
tscomponents_mul <- decompose(ts, type = "multiplicative")
plot(tscomponents_add, col = "red")
plot(tscomponents_mul, col = "blue")

xtsdiff1 <- diff(xts, differences=1)
tsdiff1 <- diff(ts, differences=1)
plot.xts(xtsdiff1, col = "blue")
tseries::adf.test(tsdiff1, alternative = "stationary", k = 0)
findfrequency(xts)          # find dominant frequency of original time series
findfrequency(xtsdiff1)     # find dominant frequency of differenced time series


## Selecting a Candidate ARIMA Model

Acf(xtsdiff1, lag.max=60)             # plot a correlogram
Acf(xtsdiff1, lag.max=60, plot=FALSE) # get the autocorrelation values

Pacf(xtsdiff1, lag.max=60)             # plot a partial correlogram
Pacf(xtsdiff1, lag.max=60, plot=FALSE) # get the partial autocorrelation values

tsarima240 <- auto.arima(head(xts, -240), max.p = 3, max.q = 3, max.d = 3) # excluding last 240 time series as test data
print(tsarima240)
autoplot(tsarima240)
tsarima120 <- auto.arima(head(xts, -120), max.p = 3, max.q = 3, max.d = 3) #120
print(tsarima120)
autoplot(tsarima120)
tsarima60 <- auto.arima(head(xts, -60), max.p = 3, max.q = 3, max.d = 3) #60
print(tsarima60)
autoplot(tsarima60)
tsarima30 <- auto.arima(head(xts, -30), max.p = 3, max.q = 3, max.d = 3) #30
print(tsarima30)
autoplot(tsarima30)
tsarima7 <- auto.arima(head(xts, -7), max.p = 3, max.q = 3, max.d = 3)   #7
print(tsarima7)
autoplot(tsarima7)


tsforecasts240 <- forecast::forecast(tsarima240, h = 240) # forecast the next 240 time series
tsforecasts120 <- forecast::forecast(tsarima120, h = 120) # forecast the next 120 time series
tsforecasts60 <- forecast::forecast(tsarima60, h = 60) # forecast the next 60 time series
tsforecasts30 <- forecast::forecast(tsarima30, h = 30) # forecast the next 30 time series
tsforecasts7 <- forecast::forecast(tsarima7, h = 7) # forecast the next 7 time series
autoplot(tsforecasts240)
accuracy(tsforecasts240, head(tail(xts, 240), 240))
accuracy(tsforecasts240, head(tail(xts, 240), 120))
accuracy(tsforecasts240, head(tail(xts, 240), 60))
accuracy(tsforecasts240, head(tail(xts, 240), 30))
accuracy(tsforecasts240, head(tail(xts, 240), 7))
autoplot(tsforecasts120)
accuracy(tsforecasts120, head(tail(xts, 120), 120))
accuracy(tsforecasts120, head(tail(xts, 120), 60))
accuracy(tsforecasts120, head(tail(xts, 120), 30))
accuracy(tsforecasts120, head(tail(xts, 120), 7))
autoplot(tsforecasts60)
accuracy(tsforecasts60, head(tail(xts, 60), 60))
accuracy(tsforecasts60, head(tail(xts, 60), 30))
accuracy(tsforecasts60, head(tail(xts, 60), 7))
autoplot(tsforecasts30)
accuracy(tsforecasts30, head(tail(xts, 30), 30))
accuracy(tsforecasts30, head(tail(xts, 30), 7))
autoplot(tsforecasts7)
accuracy(tsforecasts7, head(tail(xts, 7), 7))

# plot.ts(tsforecasts$residuals)            # make time plot of forecast errors
print('tsforecasts240')
ggplot(data.frame(residuals = tsforecasts240$residuals), aes(residuals)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + geom_density()# make a histogram
checkresiduals(tsforecasts240)
print('tsforecasts120')
ggplot(data.frame(residuals = tsforecasts120$residuals), aes(residuals)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + geom_density()# make a histogram
checkresiduals(tsforecasts120)
print('tsforecasts60')
ggplot(data.frame(residuals = tsforecasts60$residuals), aes(residuals)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + geom_density()# make a histogram
checkresiduals(tsforecasts60)
print('tsforecasts30')
ggplot(data.frame(residuals = tsforecasts30$residuals), aes(residuals)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + geom_density()# make a histogram
checkresiduals(tsforecasts30)
print('tsforecasts7')
ggplot(data.frame(residuals = tsforecasts7$residuals), aes(residuals)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + geom_density()# make a histogram
checkresiduals(tsforecasts7)
# 
# ggplot(data.frame(residuals = tsdiff1forecasts$residuals), aes(residuals)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + geom_density()# make a histogram
# checkresiduals(tsdiff1forecasts)


# Previsão
plot(forecast::forecast(ARIMAC, h=50, level = c(90,95,99)),shadecols = c("gray70", "gray50", "gray30"), fcol = "red", flwd = 0.9)
plot(forecast::forecast(ARIMAU, h=50, level = c(90,95,99)),shadecols = c("gray70", "gray50", "gray30"), fcol = "red", flwd = 0.9)

# Create and plot Time Series - High
xts = xts(MCD_2006$Close, order.by=MCD_2006$Date)
attr(xts, 'frequency') <- length(xts)/12
ts = as.ts(xts, start = c(2006))

# Estacionario?
adf.test(xts)

dygraph(xts, xlab = "Time", ylab = "High valor", main = "Séries Temporais") %>%
  # dySeries(labels.default()) %>%
  # dyOptions(colors = c("red")) %>%
  dyRangeSelector()

## Decomposing Time Series
tscomponents_add <- decompose(ts, type = "additive")
tscomponents_mul <- decompose(ts, type = "multiplicative")
plot(tscomponents_add, col = "red")
plot(tscomponents_mul, col = "blue")

xtsdiff1 <- diff(xts, differences=1)
tsdiff1 <- diff(ts, differences=1)
plot.xts(xtsdiff1, col = "blue")
tseries::adf.test(tsdiff1, alternative = "stationary", k = 0)
findfrequency(xts)          # find dominant frequency of original time series
findfrequency(xtsdiff1)     # find dominant frequency of differenced time series


## Selecting a Candidate ARIMA Model

Acf(xtsdiff1, lag.max=60)             # plot a correlogram
Acf(xtsdiff1, lag.max=60, plot=FALSE) # get the autocorrelation values

Pacf(xtsdiff1, lag.max=60)             # plot a partial correlogram
Pacf(xtsdiff1, lag.max=60, plot=FALSE) # get the partial autocorrelation values

tsarima240 <- auto.arima(head(xts, -240), max.p = 3, max.q = 3, max.d = 3) # excluding last 240 time series as test data
print(tsarima240)
autoplot(tsarima240)
tsarima120 <- auto.arima(head(xts, -120), max.p = 3, max.q = 3, max.d = 3) #120
print(tsarima120)
autoplot(tsarima120)
tsarima60 <- auto.arima(head(xts, -60), max.p = 3, max.q = 3, max.d = 3) #60
print(tsarima60)
autoplot(tsarima60)
tsarima30 <- auto.arima(head(xts, -30), max.p = 3, max.q = 3, max.d = 3) #30
print(tsarima30)
autoplot(tsarima30)
tsarima7 <- auto.arima(head(xts, -7), max.p = 3, max.q = 3, max.d = 3)   #7
print(tsarima7)
autoplot(tsarima7)


tsforecasts240 <- forecast::forecast(tsarima240, h = 240) # forecast the next 240 time series
tsforecasts120 <- forecast::forecast(tsarima120, h = 120) # forecast the next 120 time series
tsforecasts60 <- forecast::forecast(tsarima60, h = 60) # forecast the next 60 time series
tsforecasts30 <- forecast::forecast(tsarima30, h = 30) # forecast the next 30 time series
tsforecasts7 <- forecast::forecast(tsarima7, h = 7) # forecast the next 7 time series
autoplot(tsforecasts240)
accuracy(tsforecasts240, head(tail(xts, 240), 240))
accuracy(tsforecasts240, head(tail(xts, 240), 120))
accuracy(tsforecasts240, head(tail(xts, 240), 60))
accuracy(tsforecasts240, head(tail(xts, 240), 30))
accuracy(tsforecasts240, head(tail(xts, 240), 7))
autoplot(tsforecasts120)
accuracy(tsforecasts120, head(tail(xts, 120), 120))
accuracy(tsforecasts120, head(tail(xts, 120), 60))
accuracy(tsforecasts120, head(tail(xts, 120), 30))
accuracy(tsforecasts120, head(tail(xts, 120), 7))
autoplot(tsforecasts60)
accuracy(tsforecasts60, head(tail(xts, 60), 60))
accuracy(tsforecasts60, head(tail(xts, 60), 30))
accuracy(tsforecasts60, head(tail(xts, 60), 7))
autoplot(tsforecasts30)
accuracy(tsforecasts30, head(tail(xts, 30), 30))
accuracy(tsforecasts30, head(tail(xts, 30), 7))
autoplot(tsforecasts7)
accuracy(tsforecasts7, head(tail(xts, 7), 7))

# plot.ts(tsforecasts$residuals)            # make time plot of forecast errors
print('tsforecasts240')
ggplot(data.frame(residuals = tsforecasts240$residuals), aes(residuals)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + geom_density()# make a histogram
checkresiduals(tsforecasts240)
print('tsforecasts120')
ggplot(data.frame(residuals = tsforecasts120$residuals), aes(residuals)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + geom_density()# make a histogram
checkresiduals(tsforecasts120)
print('tsforecasts60')
ggplot(data.frame(residuals = tsforecasts60$residuals), aes(residuals)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + geom_density()# make a histogram
checkresiduals(tsforecasts60)
print('tsforecasts30')
ggplot(data.frame(residuals = tsforecasts30$residuals), aes(residuals)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + geom_density()# make a histogram
checkresiduals(tsforecasts30)
print('tsforecasts7')
ggplot(data.frame(residuals = tsforecasts7$residuals), aes(residuals)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + geom_density()# make a histogram
checkresiduals(tsforecasts7)
# 
# ggplot(data.frame(residuals = tsdiff1forecasts$residuals), aes(residuals)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + geom_density()# make a histogram
# checkresiduals(tsdiff1forecasts)


# Previsão
plot(forecast::forecast(ARIMAC, h=50, level = c(90,95,99)),shadecols = c("gray70", "gray50", "gray30"), fcol = "red", flwd = 0.9)
plot(forecast::forecast(ARIMAU, h=50, level = c(90,95,99)),shadecols = c("gray70", "gray50", "gray30"), fcol = "red", flwd = 0.9)


# Create and plot Time Series - Low
xts = xts(MCD_2006$Low, order.by=MCD_2006$Date)
attr(xts, 'frequency') <- length(xts)/12
ts = as.ts(xts, start = c(2006))

# Estacionario?
adf.test(xts)

dygraph(xts, xlab = "Time", ylab = "High valor", main = "Séries Temporais") %>%
  # dySeries(labels.default()) %>%
  # dyOptions(colors = c("red")) %>%
  dyRangeSelector()

## Decomposing Time Series
tscomponents_add <- decompose(ts, type = "additive")
tscomponents_mul <- decompose(ts, type = "multiplicative")
plot(tscomponents_add, col = "red")
plot(tscomponents_mul, col = "blue")

xtsdiff1 <- diff(xts, differences=1)
tsdiff1 <- diff(ts, differences=1)
plot.xts(xtsdiff1, col = "blue")
tseries::adf.test(tsdiff1, alternative = "stationary", k = 0)
findfrequency(xts)          # find dominant frequency of original time series
findfrequency(xtsdiff1)     # find dominant frequency of differenced time series


## Selecting a Candidate ARIMA Model

Acf(xtsdiff1, lag.max=60)             # plot a correlogram
Acf(xtsdiff1, lag.max=60, plot=FALSE) # get the autocorrelation values

Pacf(xtsdiff1, lag.max=60)             # plot a partial correlogram
Pacf(xtsdiff1, lag.max=60, plot=FALSE) # get the partial autocorrelation values

tsarima240 <- auto.arima(head(xts, -240), max.p = 3, max.q = 3, max.d = 3) # excluding last 240 time series as test data
print(tsarima240)
autoplot(tsarima240)
tsarima120 <- auto.arima(head(xts, -120), max.p = 3, max.q = 3, max.d = 3) #120
print(tsarima120)
autoplot(tsarima120)
tsarima60 <- auto.arima(head(xts, -60), max.p = 3, max.q = 3, max.d = 3) #60
print(tsarima60)
autoplot(tsarima60)
tsarima30 <- auto.arima(head(xts, -30), max.p = 3, max.q = 3, max.d = 3) #30
print(tsarima30)
autoplot(tsarima30)
tsarima7 <- auto.arima(head(xts, -7), max.p = 3, max.q = 3, max.d = 3)   #7
print(tsarima7)
autoplot(tsarima7)


tsforecasts240 <- forecast::forecast(tsarima240, h = 240) # forecast the next 240 time series
tsforecasts120 <- forecast::forecast(tsarima120, h = 120) # forecast the next 120 time series
tsforecasts60 <- forecast::forecast(tsarima60, h = 60) # forecast the next 60 time series
tsforecasts30 <- forecast::forecast(tsarima30, h = 30) # forecast the next 30 time series
tsforecasts7 <- forecast::forecast(tsarima7, h = 7) # forecast the next 7 time series
autoplot(tsforecasts240)
accuracy(tsforecasts240, head(tail(xts, 240), 240))
accuracy(tsforecasts240, head(tail(xts, 240), 120))
accuracy(tsforecasts240, head(tail(xts, 240), 60))
accuracy(tsforecasts240, head(tail(xts, 240), 30))
accuracy(tsforecasts240, head(tail(xts, 240), 7))
autoplot(tsforecasts120)
accuracy(tsforecasts120, head(tail(xts, 120), 120))
accuracy(tsforecasts120, head(tail(xts, 120), 60))
accuracy(tsforecasts120, head(tail(xts, 120), 30))
accuracy(tsforecasts120, head(tail(xts, 120), 7))
autoplot(tsforecasts60)
accuracy(tsforecasts60, head(tail(xts, 60), 60))
accuracy(tsforecasts60, head(tail(xts, 60), 30))
accuracy(tsforecasts60, head(tail(xts, 60), 7))
autoplot(tsforecasts30)
accuracy(tsforecasts30, head(tail(xts, 30), 30))
accuracy(tsforecasts30, head(tail(xts, 30), 7))
autoplot(tsforecasts7)
accuracy(tsforecasts7, head(tail(xts, 7), 7))

# plot.ts(tsforecasts$residuals)            # make time plot of forecast errors
print('tsforecasts240')
ggplot(data.frame(residuals = tsforecasts240$residuals), aes(residuals)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + geom_density()# make a histogram
checkresiduals(tsforecasts240)
print('tsforecasts120')
ggplot(data.frame(residuals = tsforecasts120$residuals), aes(residuals)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + geom_density()# make a histogram
checkresiduals(tsforecasts120)
print('tsforecasts60')
ggplot(data.frame(residuals = tsforecasts60$residuals), aes(residuals)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + geom_density()# make a histogram
checkresiduals(tsforecasts60)
print('tsforecasts30')
ggplot(data.frame(residuals = tsforecasts30$residuals), aes(residuals)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + geom_density()# make a histogram
checkresiduals(tsforecasts30)
print('tsforecasts7')
ggplot(data.frame(residuals = tsforecasts7$residuals), aes(residuals)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + geom_density()# make a histogram
checkresiduals(tsforecasts7)
# 
# ggplot(data.frame(residuals = tsdiff1forecasts$residuals), aes(residuals)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + geom_density()# make a histogram
# checkresiduals(tsdiff1forecasts)


# Previsão
plot(forecast::forecast(ARIMAC, h=50, level = c(90,95,99)),shadecols = c("gray70", "gray50", "gray30"), fcol = "red", flwd = 0.9)
plot(forecast::forecast(ARIMAU, h=50, level = c(90,95,99)),shadecols = c("gray70", "gray50", "gray30"), fcol = "red", flwd = 0.9)

# Create and plot Time Series - Close
xts = xts(MCD_2006$Close, order.by=MCD_2006$Date)
attr(xts, 'frequency') <- length(xts)/12
ts = as.ts(xts, start = c(2006))

# Estacionario?
adf.test(xts)

dygraph(xts, xlab = "Time", ylab = "High valor", main = "Séries Temporais") %>%
  # dySeries(labels.default()) %>%
  # dyOptions(colors = c("red")) %>%
  dyRangeSelector()

## Decomposing Time Series
tscomponents_add <- decompose(ts, type = "additive")
tscomponents_mul <- decompose(ts, type = "multiplicative")
plot(tscomponents_add, col = "red")
plot(tscomponents_mul, col = "blue")

xtsdiff1 <- diff(xts, differences=1)
tsdiff1 <- diff(ts, differences=1)
plot.xts(xtsdiff1, col = "blue")
tseries::adf.test(tsdiff1, alternative = "stationary", k = 0)
findfrequency(xts)          # find dominant frequency of original time series
findfrequency(xtsdiff1)     # find dominant frequency of differenced time series


## Selecting a Candidate ARIMA Model

Acf(xtsdiff1, lag.max=60)             # plot a correlogram
Acf(xtsdiff1, lag.max=60, plot=FALSE) # get the autocorrelation values

Pacf(xtsdiff1, lag.max=60)             # plot a partial correlogram
Pacf(xtsdiff1, lag.max=60, plot=FALSE) # get the partial autocorrelation values

tsarima240 <- auto.arima(head(xts, -240), max.p = 3, max.q = 3, max.d = 3) # excluding last 240 time series as test data
print(tsarima240)
autoplot(tsarima240)
tsarima120 <- auto.arima(head(xts, -120), max.p = 3, max.q = 3, max.d = 3) #120
print(tsarima120)
autoplot(tsarima120)
tsarima60 <- auto.arima(head(xts, -60), max.p = 3, max.q = 3, max.d = 3) #60
print(tsarima60)
autoplot(tsarima60)
tsarima30 <- auto.arima(head(xts, -30), max.p = 3, max.q = 3, max.d = 3) #30
print(tsarima30)
autoplot(tsarima30)
tsarima7 <- auto.arima(head(xts, -7), max.p = 3, max.q = 3, max.d = 3)   #7
print(tsarima7)
autoplot(tsarima7)


tsforecasts240 <- forecast::forecast(tsarima240, h = 240) # forecast the next 240 time series
tsforecasts120 <- forecast::forecast(tsarima120, h = 120) # forecast the next 120 time series
tsforecasts60 <- forecast::forecast(tsarima60, h = 60) # forecast the next 60 time series
tsforecasts30 <- forecast::forecast(tsarima30, h = 30) # forecast the next 30 time series
tsforecasts7 <- forecast::forecast(tsarima7, h = 7) # forecast the next 7 time series
autoplot(tsforecasts240)
accuracy(tsforecasts240, head(tail(xts, 240), 240))
accuracy(tsforecasts240, head(tail(xts, 240), 120))
accuracy(tsforecasts240, head(tail(xts, 240), 60))
accuracy(tsforecasts240, head(tail(xts, 240), 30))
accuracy(tsforecasts240, head(tail(xts, 240), 7))
autoplot(tsforecasts120)
accuracy(tsforecasts120, head(tail(xts, 120), 120))
accuracy(tsforecasts120, head(tail(xts, 120), 60))
accuracy(tsforecasts120, head(tail(xts, 120), 30))
accuracy(tsforecasts120, head(tail(xts, 120), 7))
autoplot(tsforecasts60)
accuracy(tsforecasts60, head(tail(xts, 60), 60))
accuracy(tsforecasts60, head(tail(xts, 60), 30))
accuracy(tsforecasts60, head(tail(xts, 60), 7))
autoplot(tsforecasts30)
accuracy(tsforecasts30, head(tail(xts, 30), 30))
accuracy(tsforecasts30, head(tail(xts, 30), 7))
autoplot(tsforecasts7)
accuracy(tsforecasts7, head(tail(xts, 7), 7))

# plot.ts(tsforecasts$residuals)            # make time plot of forecast errors
print('tsforecasts240')
ggplot(data.frame(residuals = tsforecasts240$residuals), aes(residuals)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + geom_density()# make a histogram
checkresiduals(tsforecasts240)
print('tsforecasts120')
ggplot(data.frame(residuals = tsforecasts120$residuals), aes(residuals)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + geom_density()# make a histogram
checkresiduals(tsforecasts120)
print('tsforecasts60')
ggplot(data.frame(residuals = tsforecasts60$residuals), aes(residuals)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + geom_density()# make a histogram
checkresiduals(tsforecasts60)
print('tsforecasts30')
ggplot(data.frame(residuals = tsforecasts30$residuals), aes(residuals)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + geom_density()# make a histogram
checkresiduals(tsforecasts30)
print('tsforecasts7')
ggplot(data.frame(residuals = tsforecasts7$residuals), aes(residuals)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + geom_density()# make a histogram
checkresiduals(tsforecasts7)
# 
# ggplot(data.frame(residuals = tsdiff1forecasts$residuals), aes(residuals)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + geom_density()# make a histogram
# checkresiduals(tsdiff1forecasts)


# Previsão
plot(forecast::forecast(ARIMAC, h=50, level = c(90,95,99)),shadecols = c("gray70", "gray50", "gray30"), fcol = "red", flwd = 0.9)
plot(forecast::forecast(ARIMAU, h=50, level = c(90,95,99)),shadecols = c("gray70", "gray50", "gray30"), fcol = "red", flwd = 0.9)

