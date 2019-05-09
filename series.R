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

plot(a4_OZONIO$Ozonio, type = "l")
acf(a4_OZONIO$Ozonio)
diff(a4_OZONIO$Ozonio, 3)
par(mfrow = c(2,2))
plot(a4_OZONIO$Ozonio, type = "l")
plot(diff(a4_OZONIO$Ozonio, 1), type = "l")
plot(diff(a4_OZONIO$Ozonio, 2), type = "l")
plot(diff(a4_OZONIO$Ozonio, 3), type = "l")
acf(dif1_OZONIO)
acf(diff(a4_OZONIO$Ozonio, 1))
acf(diff(a4_OZONIO$Ozonio, 2))
acf(diff(a4_OZONIO$Ozonio, 3))

#### Banco muito interessante

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

# Criando a série
Open <- ts(na.omit(diff(MCD_2006$Open, 1)),frequency = 20, start = c(2006,12))
High <- ts(na.omit(diff(MCD_2006$High, 1)),frequency = 20, start = c(2006,12))
Low <- ts(na.omit(diff(MCD_2006$Low, 1)),frequency = 20, start = c(2006,12))
Close <- ts(na.omit(diff(MCD_2006$Close, 1)),frequency = 20, start = c(2006,12))

## Stationarity
print(ticker)
adf.test(Open)
adf.test(High)
adf.test(Low)
adf.test(Close)

# sim nao rejeito a hipotese de estacionaridade.



## Decomposing Time Series
tscomponents <- decompose(ts)
plot(tscomponents, col = "red")

## Differencing a Time Series
xtsdiff1 <- diff(xts, differences=1)
tsdiff1 <- diff(ts, differences=1)
plot.xts(xtsdiff1, col = "blue")
findfrequency(xts)          # find dominant frequency of original time series
findfrequency(xtsdiff1)     # find dominant frequency of differenced time series

## Selecting a Candidate ARIMA Model
print(ticker)
print("Selecting a candidate ARIMA Model")
Acf(xtsdiff1, lag.max=60)             # plot a correlogram
Acf(xtsdiff1, lag.max=60, plot=FALSE) # get the autocorrelation values

Pacf(xtsdiff1, lag.max=60)             # plot a partial correlogram
Pacf(xtsdiff1, lag.max=60, plot=FALSE) # get the partial autocorrelation values

## Fitting an ARIMA Model
tsarima <- auto.arima(head(xts, -30), max.p = 3, max.q = 3, max.d = 3)

# excluding last 120 time series as test data
print(tsarima)
autoplot(tsarima)
print(ticker)

## Forecasting using an ARIMA Model
print(ticker)
tsforecasts <- forecast(tsarima, h = 30) # forecast the next 120 time series
acc <- accuracy(tsforecasts, head(tail(xts, 30), 7))
print(acc)
autoplot(tsforecasts)

print(ticker)

ggplot(data.frame(residuals = tsforecasts$residuals), aes(residuals)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + geom_density()# make a histogram
checkresiduals(tsforecasts)

nstall.packages("aTSA")



#***************************************************************************#
#*************** Criando series ***************#
#***************************************************************************#
banco <- read_csv("C:/Users/PIBIC_2/Desktop/a1_temperatura.csv")
serieC <- ts(banco[,2],frequency = 12, start = c(1976,1))
serieU <- ts(banco[,3],frequency = 12, start = c(1976,1))

# Independencia
acf(serieC)
acf(serieC, type = "partial")

acf(serieU)
acf(serieU, type = "partial")

#***************************************************************************#
#*************** Estudando as series ***************#
#***************************************************************************#
plot(decompose(serieC,type = "additive",filter = NULL))
plot(decompose(serieU,type = "additive",filter = NULL))

#***************************************************************************#
#*************** Estacionaridade ***************#
#***************************************************************************#
aTSA::adf.test(serieC) # aceitamos a estacionaridade
aTSA::adf.test(serieU) # aceitamos a estacionaridade

#***************************************************************************#
#*************** Escolha so modelo ARIMA ***************#
#***************************************************************************#
ARIMAC <- auto.arima(serieC)
ARIMAU <- auto.arima(serieU)

# Residuo padronizado
respadronC <- ARIMAC$residuals/ARIMAC$sigma2
respadronU <- ARIMAU$residuals/ARIMAU$sigma2

# Normalidade
library(nortest)
lillie.test(respadronC)
shapiro.test(respadronC)
qqnorm(respadronC); qqline(respadronC, col=2)

lillie.test(respadronU)
shapiro.test(respadronU)
qqnorm(respadronU); qqline(respadronU, col=2)

# Independencia dos residuos
acf(respadronC)
acf(respadronC, type = "partial")

acf(respadronU)
acf(respadronU, type = "partial")

# Previsão
plot(forecast::forecast(ARIMAC, h=50, level = c(90,95,99)),shadecols = c("gray70", "gray50", "gray30"), fcol = "red", flwd = 0.9)
plot(forecast::forecast(ARIMAU, h=50, level = c(90,95,99)),shadecols = c("gray70", "gray50", "gray30"), fcol = "red", flwd = 0.9)
