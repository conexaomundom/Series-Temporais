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
# sd(uselec)
# uselec
summary(uselec)
 
ndiffs(uselec)
nsdiffs(uselec)

modelo1 <- auto.arima(uselec, ic = "aic")
modelo2 <- auto.arima(uselec, ic = "bic")
modelo3 <- auto.arima(uselec, ic = "aic", d = 1)
modelo4 <- auto.arima(uselec, ic = "bic", d = 1)
modelo5 <- auto.arima(uselec, ic = "bic", d = 1, D = 1)

matrix(c(modelo1$aic,modelo1$aicc,modelo1$bic,
         modelo2$aic,modelo2$aicc,modelo2$bic,
         modelo3$aic,modelo3$aicc,modelo3$bic,
         modelo4$aic,modelo4$aicc,modelo4$bic,
         modelo5$aic,modelo5$aicc,modelo5$bic),
         5,3, byrow = TRUE, dimnames = list(c("modelo_aic",
         "modelo_bic", "modelo_aic_d1","modelo_bic_d1",
         "modelo_bic_d1D1"), c("aic", "aicc", "bic")))
# De acordo com a matriz formada pelos valores dos critérios de seleção de
# modelos, o modelo com melhor resultado foi o modelo selecionado pelo aic, porém
#  teve outro modelo com desempenho muito igual que foi o mesmo modelo 
# escolhido pelo aic porem adicionando d = 1, como resultou no teste feito 
#  antes que resultou que a série necessitava de uma diferenciação para 
# tornala estacionária.
# com o aic, bic, com d = 1, e com D = 1 também.

# ARIMA(1,0,2)(0,1,1)
# ARIMA(1,0,0)(0,1,1)
d_sarima01 <- sarima(uselec, 1,0,2, 0,1,1, 12) # Melhores modelos.
d_sarima02 <- sarima(uselec, 1,0,0, 0,1,1, 12) # Melhores modelos.
d_sarima1 <- sarima(uselec, 1,1,2, 0,1,1, 12)
d_sarima2 <- sarima(uselec, 1,1,0, 1,0,0, 12)
d_sarima3 <- sarima(uselec, 1,1,0, 1,1,0, 12)

# matrix(c(d_sarima1$AIC,d_sarima1$AICc,d_sarima1$BIC,
# d_sarima2$AIC,d_sarima2$AICc,d_sarima2$BIC,
#          d_sarima3$AIC,d_sarima3$AICc,d_sarima3$BIC,
#          d_sarima4$AIC,d_sarima4$AICc,d_sarima4$BIC,
#          d_sarima5$AIC,d_sarima5$AICc,d_sarima5$BIC),
#        5,3, byrow = TRUE, dimnames = list(c("modelo_aic",
#         "modelo_bic", "modelo_aic_d1","modelo_bic_d1",
#        "modelo_bic_d1D1"), c("aic", "aicc", "bic")))


# Faço análise de residuos 

d_sarima1 <- sarima(uselec, 1,1,2, 0,1,1, 12)
d_sarima2 <- sarima(uselec, 1,1,0, 1,0,0, 12)
d_sarima3 <- sarima(uselec, 1,1,0, 1,1,0, 12)

# Vamos lá, no primeiro modelo os residuos estão dentro do
# intervalo, entre -2, e 3, tranquilo, no plot
# do acf todos os lags estão dentro das linhas do intervalo
# de confiança, ou seja, a hipotese de resídos não 
# correlacionados não foi violada, no QQPLot eu não sei dizer
# porque segue a reta de normalidade apenas no meio, e nos 
# extremos os pontos não estão nem dentro do envelope.

# o modelos que passaria na análise de resíudos seria o primeiro
# modelo que inclusive está bem ajustado de acordo com o teste
# de Ljung-Box todos os pontos estão com p-valores altos.
# E também foi o único modelo em que todos parametros estimados
# foram significativos a qualquer nível nominal.


####
# Mas vamos seguir com os três modelos para a previsão
# e ver qual modelo preve de forma melhor.
####

# Começo os modelos de previsão.

###############
# METODOS DE PREVISAO
#############

uselec2 <- window(uselec, start = 1985, end = c(1995,10)) #truncar a serie retirando o ultimo ano
uselec3 <- window(uselec, start = c(1995,11)) # ultimo ano da serie

###############
###############
###############
###############

#sarimax

#sarimax
#a) dummy
# d = rep(0, length(pigs))
# d[3] = 1

fit.aic.xreg.dummy <- sarima(pigs, 2, 1, 0, 2, 0, 0, 12, xreg = d) #Não passou nos testes, mas aceitavel
fit.bic.xreg.dummy <- sarima(pigs, 2, 1, 0, 1, 0, 0, 12, xreg = d) #Não passou nos testes, mas aceitavel

# ARIMA(1,0,2)(0,1,1)
# ARIMA(1,0,0)(0,1,1)

#b) tendencia
t = decompose(uselec)$trend
fit01_xreg_tend <- sarima(uselec, 1,0,2, 0,1,1, 12, xreg = t) # Passou nos testes, mas aceitavel
fit02_xreg_tend <- sarima(uselec, 1,0,0, 0,1,1, 12, xreg = t) # Não passou nos testes, mas NÃO aceitavel
fit1_xreg_tend <- sarima(uselec, 1,1,2, 0,1,1, 12, xreg = t) # Não passou nos testes, mas NÃO aceitavel
fit2_xreg_tend <- sarima(uselec, 1,1,0, 1,0,0, 12, xreg = t) # Não passou nos testes, mas NÃO aceitavel
fit3_xreg_tend <- sarima(uselec, 1,1,0, 1,1,0, 12, xreg = t) # Não passou nos testes, mas NÃO aceitavel


#c) sazionalidade
s = decompose(uselec)$seasonal

fit01_xreg_seasonal <- sarima(uselec, 1,0,2, 0,1,1, 12, xreg = s) # Passou nos testes, mas NÃO aceitavel
fit02_xreg_seasonal <- sarima(uselec, 1,0,0, 0,1,1, 12, xreg = s) # Não passou nos testes, mas NÃO aceitavel
fit1_xreg_seasonal <- sarima(uselec, 1,1,2, 0,1,1, 12, xreg = s) # Não passou nos testes, mas NÃO aceitavel
fit2_xreg_seasonal <- sarima(uselec, 1,1,0, 1,0,0, 12, xreg = s) # Não passou nos testes, mas NÃO aceitavel
fit3_xreg_seasonal <- sarima(uselec, 1,1,0, 1,1,0, 12, xreg = s) # Não passou nos testes, mas NÃO aceitavel

############# tendencia, dummy, sazionalidade
# Excluindo as ultimas 12 observações
t2 <- t[-c(131:142)]
s2 <- s[-c(131:142)]

t3 <- t[c(131:142)]
s3 <- s[c(131:142)]
# ARIMA(1,0,2)(0,1,1)
# ARIMA(1,0,0)(0,1,1)
fit1_sarima_t_uselec2 <- arima(uselec2, order = c(1,0,2), seasonal = list(order = c(0,1,1)))
fit2_sarima_s_uselec2 <- arima(uselec2, order = c(1,0,0), seasonal = list(order = c(0,1,1)))
fit3_sarima_t_uselec2 <- arima(uselec2, order = c(1,0,2), seasonal = list(order = c(0,1,1)), xreg = t2)
fit4_sarima_s_uselec2 <- arima(uselec2, order = c(1,0,2), seasonal = list(order = c(0,1,1)), xreg = s2)

pred_uselec2_sarima1 <- predict(fit1_sarima_t_uselec2, n.ahead = 12)
pred_uselec2_sarima2 <- predict(fit2_sarima_s_uselec2, n.ahead = 12)
pred_uselec2_sarima1_t <- predict(fit3_sarima_t_uselec2, n.ahead = 12, xreg = t3, newxreg = t3)
pred_uselec2_sarima1_s <- predict(fit4_sarima_s_uselec2, n.ahead = 12, xreg = s3, newxreg = s3)

pred_modelo_aic <- accuracy(pred_uselec2_sarima1$pred, uselec3)[1, c(3,2,5)]
pred_modelo_bic <- accuracy(pred_uselec2_sarima2$pred, uselec3)[1, c(3,2,5)]
pred_modelo_aic_t <- accuracy(pred_uselec2_sarima1_t$pred, uselec3)[1, c(3,2,5)]
pred_modelo_aic_s <- accuracy(pred_uselec2_sarima1_s$pred, uselec3)[1, c(3,2,5)]

erros1 <- matrix(c(pred_modelo_aic[1],pred_modelo_aic[2],pred_modelo_aic[3],
         pred_modelo_bic[1],pred_modelo_bic[2],pred_modelo_bic[3],
         pred_modelo_aic_t[1],pred_modelo_aic_t[2],pred_modelo_aic_t[3],
         pred_modelo_aic_s[1],pred_modelo_aic_s[2],pred_modelo_aic_s[3]),
         4,3,byrow = TRUE,dimnames = list(c("modelo_aic", "modelo_bic",
                                          "modelo_aic_t", "modelo.aic.s"), c("MAE", "RMSE", "MAPE")))
# O melhor foi o modelo_aic_t
# Utilizando medidas de erro para a previsão podemos ver que 
# o melhor modelo para previsão foi o modelo_aic_t, de ordem
# tals.


# Previsão excluindo as 12 ultimas observações

length(uselec2)
length(uselec3)

uselecfit1 <- meanf(uselec2, h = 12)
uselecfit2 <- rwf(uselec2, h = 12)
uselecfit3 <- rwf(uselec2, h = 12, drift = TRUE)
uselecfit4 <- snaive(uselec2, h = 12)

autoplot(window(uselec2, start = 1985)) +
  autolayer(uselecfit1, series = "Mean", PI = FALSE) +
  autolayer(uselecfit2, series = "Naive", PI = FALSE) +
  autolayer(uselecfit3, series = "Naive com drift", PI = FALSE) +
  autolayer(uselecfit4, series = "Naive com sazonalidade", PI = FALSE) +
  ggtitle("Previsão para o total de eletricidade gerada") +
  guides(colour = guide_legend(title = "Previsão"))

pred_modelo_mean <- accuracy(uselecfit1, uselec3)[1, c(3,2,5)]
pred_modelo_naive <- accuracy(uselecfit2, uselec3)[1, c(3,2,5)]
pred_modelo_naived <- accuracy(uselecfit3, uselec3)[1, c(3,2,5)]
pred_modelo_snaive <- accuracy(uselecfit4, uselec3)[1, c(3,2,5)]

matrix(c(pred_modelo_mean[1],pred_modelo_mean[2],pred_modelo_mean[3],
         pred_modelo_naive[1],pred_modelo_naive[2],pred_modelo_naive[3],
         pred_modelo_naived[1],pred_modelo_naived[2],pred_modelo_naived[3],
         pred_modelo_snaive[1],pred_modelo_snaive[2],pred_modelo_snaive[3]),
       4,3,byrow = TRUE,dimnames = list(c("Média", "Naive",
                                          "Naive com drift", "Naive com Sazonalidade"), c("MAE", "RMSE", "MAPE")))


# Quem ganhou foi o quarto modelo, o naive com sazonalidade.  

##############################
# Alisamento exponencial
#############################
# Alisamento expoenncial aditivo e multiplicativo
air <- window(uselec, start = 1985)
fit1 <- hw(uselec2, seasonal = "additive", h = 12)
fit2 <- hw(uselec2, seasonal = "multiplicative", h = 12)

autoplot(air) +
  autolayer(fit1, series = "HW - aditivo", PI = FALSE) +
  autolayer(fit2, series = "HW - multiplicativo", PI = FALSE) +
  guides(colou = guide_legend(title = "Forecast"))

# Vamos comparar as previsões - Métodos (mean, naive, drift) e AE

pred_uselec2_ae_ad <- accuracy(fit1$mean, uselec3)[1, c(3,2,5)]
pred_uselec2_ae_mu <- accuracy(fit2$mean, uselec3)[1, c(3,2,5)]
pred_uselec2_meanf <- accuracy(uselecfit1$mean, uselec3)[1, c(3,2,5)]
pred_uselec2_rwf <- accuracy(uselecfit2$mean, uselec3)[1, c(3,2,5)]
pred_uselec2_rwf_drift <- accuracy(uselecfit3$mean, uselec3)[1, c(3,2,5)]
pred_uselec2_snive <- accuracy(uselecfit4$mean, uselec3)[1, c(3,2,5)]

erros2 <- matrix(c(pred_uselec2_ae_ad[1],pred_uselec2_ae_ad[2],pred_uselec2_ae_ad[3],
         pred_uselec2_ae_mu[1],pred_uselec2_ae_mu[2],pred_uselec2_ae_mu[3],
         pred_uselec2_meanf[1],pred_uselec2_meanf[2],pred_uselec2_meanf[3],
         pred_uselec2_rwf[1],pred_uselec2_rwf[2],pred_uselec2_rwf[3],
         pred_uselec2_rwf_drift[1],pred_uselec2_rwf_drift[2],pred_uselec2_rwf_drift[3],
         pred_uselec2_snive[1],pred_uselec2_snive[2],pred_uselec2_snive[3]),
       6,3,byrow = TRUE,dimnames = list(c("Alisamento exponencial aditivo","Alisamento exponencial mutiplicativo",
                                          "Média", "Naive", "Naive drift", "Naive sazonal"),
                                        c("MAE", "RMSE", "MAPE")))
rbind(erros1, erros2)

# O melhor modelo para previsão foi o modelo selecionado
# pelo critério de seleção de modelos AIC incorporado
# uma variável regressora a tendência.
# Foi o modelo tals, que passou nos resíduos assim,...