bonds


?bonds
autoplot(bonds)
ggseasonplot(bonds)

summary(bonds)

ndiffs(bonds)
nsdiffs(bonds)

modelo1 <- auto.arima(bonds, ic = "aic")
modelo3 <- auto.arima(bonds, ic = "bic")

de <- decompose(bonds)


d_sarima1 <- sarima(bonds, 1,0,2, 0,1,1, 12)
d_sarima2 <- sarima(bonds, 1,0,0, 1,0,0, 12)
d_sarima3 <- sarima(bonds, 1,1,2, 0,1,1, 12)
d_sarima4 <- sarima(bonds, 1,1,0, 1,0,0, 12)


