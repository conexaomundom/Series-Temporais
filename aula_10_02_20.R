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

autoplot(AirPassengers, col = "red", linetype = "dashed")
?autoplot

ndiffs(AirPassengers)
nsdiffs(AirPassengers)
