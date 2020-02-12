install.packages("ggplot2")
install.packages("ggfortify")
install.packages("forecast")
install.packages("urca")
install.packages("astsa")


library(ggplot2)
library(ggfortify)
library(forecast)
library(urca)
library(astsa)

autoplot(AirPassengers)
summary(AirPassengers)
AirPassengers

autoplot(AirPassengers, col = "purple", linetype = 6)
?autoplot

# Ajuste e diagnostico
sarima(AirPassengers, 2,1,1,0,1,0,12)




ndiffs(AirPassengers)
nsdiffs(AirPassengers)

dados <- auto.arima(AirPassengers)
plot(dados)


d_forecasts <- forecast(dados, level = 95, h = 50)
autoplot(d_forecasts)


###############
# METODOS DE PREVISAO
#############
# media, 1, significa um passo a frente.
meanf(AirPassengers, 1)

# naive 
naive(AirPassengers, 1)
summary(AirPassengers, 1)
snaive(AirPassengers, 1)

# drift
rwf(AirPassengers, 1 ,drift = TRUE)

