library(ggplot2)
library(ggfortify)
library(forecast)
library(urca)
library(astsa)
library(fma)
library(expsmooth)

?uselec
autoplot(uselec)
ggseasonplot(uselec)

summary(uselec)

ndiffs(uselec)
nsdiffs(uselec)

modelo1 <- auto.arima(uselec, ic = "aic")
modelo3 <- auto.arima(uselec, ic = "bic")


d_sarima1 <- sarima(uselec, 1,0,2, 0,1,1, 12)
d_sarima2 <- sarima(uselec, 1,0,0, 1,0,0, 12)
d_sarima3 <- sarima(uselec, 1,1,2, 0,1,1, 12)
d_sarima4 <- sarima(uselec, 1,1,0, 1,0,0, 12)

matrix(c(d_sarima1$AIC,d_sarima1$AICc,d_sarima1$BIC,
         d_sarima2$AIC,d_sarima2$AICc,d_sarima2$BIC,
         d_sarima3$AIC,d_sarima3$AICc,d_sarima3$BIC,
         d_sarima4$AIC,d_sarima4$AICc,d_sarima4$BIC),
         4,3, byrow = TRUE, 
         dimnames = list(c("modelo_aic","modelo_bic",
                           "modelo_aic_d1","modelo_bic_d1"),
                         c("aic", "aicc", "bic")))
# De acordo com a matriz formada pelos valores dos critérios de seleção de
# modelos, o modelo com melhor resultado foi o modelo selecionado pelo aic, porém
#  teve outro modelo com desempenho muito parecido que foi o mesmo modelo 
# escolhido pelo aic porem adicionando d = 1, como resultou no teste feito 
#  antes que resultou que a série necessitava de uma diferenciação para 
# tornala estacionária.