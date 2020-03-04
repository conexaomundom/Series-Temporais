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

# O banco já vem em formato de series temporais com inico em 
# Janeiro de 1949 e final em dezembro de 1960, contentendo
# 144 observações mensais do numero total de passageiros
# internacionais na casa dos mil durante os 11 anos estudados 
# da linha aérea Box & Jenkins.
AirPassengers

autoplot(AirPassengers, col = "red")
# Ao realizar um plot da série é possivel ver uma tendencia 
# crescente em todos os anos de estudo, e também uma sazonalidade
# bem presente, pois, anualmente o comportamento é o mesmo, 
# crescente, mas o mesmo, apenas se acentuando e deixando ainda
# mais claro.
ggseasonplot(AirPassengers)
# Ao fazer o plot já ficou claro uma tendencia cresvente ao longo
# dos anos, e agora ao realizar um plot para verficar se há
# realmente uma sazonalidade, é possivel ver uma pequena elevação
# até março, um comportameto um pouco constante entre março e maio
# e aí começa a elevação do pico em todos os anos entre julho e agosto
# talvez por ser o mês de férias, seria valido pesquisar se no país
# ou região da companhia aérea tem férias nessa época para explicar
# esse pico nessa época do ano, após esses meses há uma queda lenta de 
# agosto até novamabro e em seguida começa novamente o mesmo comportamento

summary(AirPassengers)
which(AirPassengers == 104)
which(AirPassengers == 622)

#  O minimo se encontra no mes de novembro de 1949 e o 
# maximo em julho de 1960. Esse valores nessas datas são
# explicados pela tendencia forte crescente o maximo ser no
# ultimo ano e o minimo no primeiro, e o pico e o minimo esta
# numa parte realmetente descrecenre.

# Vale salientar que do primeiro ao terceiro quartil só varia 180mil
# passageiros, mas do terceiro quartil para o maximo dobra o numero
#  de passageiros, ou seja, o pico nos meses no meio do ano são muito
# reelevantes porque eles diferenciam grandemente do restante do nuemro
# de passageiros em outras épocas ao longo do ano.

ndiffs(AirPassengers)
# Ao aplicar a função ndiffs no banco, resultará quantas vezes será necessário 
# diferenciar a série para torna la estacionária, ou seja, retirar a tendência 
# presente nela, que normalmente é 0, 1 ou 2, para o banco de numero de 
# passageiros de voos internacionais resultou em apenas uma diferenciação
# já resolve o problema de tendência.
nsdiffs(AirPassengers)
# Ao aplicar a função nsdiffs no banco, resultará quantas vezes será necessário 
# diferenciar a série na componente sazonal para torna la estacionária, ou seja,
# retirar a tendência presente da componente sazonal, que normalmente é 0, 1 ou
# 2 também, para o banco de numero de passageiros de voos internacionais resultou
# em apenas uma diferenciação também já resolve o problema de tendência na
# componente sazonal.

# Poderia explicar um pouco melhor sobre essa questão de componente sazonal
# fiquei um pouco na dúvida.

ajuste1 <- auto.arima(AirPassengers, ic = "aicc")
# ********************* Dúvida
# Como resultado do uso da função auto.arima diz a ordem do modelo, 
# utilizando o critério de seleção aicc, resultando numa estimação dos 
# parametros que explicam a série ARIMA(2,1,1)(0,1,0), em que foi estimado
# p = 2 no caso na parte regressima, d = 1, ou seja, a série precisa ser 
# diferenciada uma vez para corrigir o problema da tendência, mas já era esperado
#  pois ao utilizar a função ndiffs anteriormente tinha resultado que a série
# realmente necessitava de uma diferenciação e no parametro q da parte de
# médias móveis foi estimado 1. Além disso também é estimado a parte da componente
# sazonal em que resultou em P = 0, ou seja a parte autorregressiva da componente
# sazonal foi 0, D = 1 como esperado também que ao utilizar a função nsdiffs
# resultando que o numero de diferenciações para a parte sazonal ser estacionária 
# é 1 e Q = 0 também, ou seja, não há necessidade de parametro na parte de médias 
# móveis para a componente sazonal.
# ********************* Dúvida

ajuste2 <- auto.arima(AirPassengers, ic = "aic")
# Já utilizando o critério de seleção aic, resultando numa estimação dos 
# parametros bem diferente do ajuste utilizando o critério de seleção aicc
# a estimativa foi ARIMA(0,1,1)(2,1,0) em que foi estimado p = 0 no caso 
# na parte regressima, d = 1, ou seja, o já era esperado anteriormente tinha 
# resultado que a série e no parametro q da parte de médias móveis foi estimado 
# 1 também. Na parte da componente sazonal em que resultou em P = 2, ou seja a 
# parte autorregressiva da componente sazonal foi 2, D = 1 como esperado também 
# o numero de diferenciações para a parte sazonal ser estacionária é 1 e Q = 0 
# também, ou seja, não há necessidade de parametro na parte de médias móveis
# para a componente sazonal.

ajuste3 <- auto.arima(AirPassengers, ic = "bic")
# Já utilizando outro critério de seleção o bic, resultando numa estimação dos 
# parametros bem diferente dos dois ajustes feitos anteriormente
# que explicam a série ARIMA(1,1,0)(0,1,0) em que foi estimado p = 1 no caso 
# na parte regressima, d = 1, ou seja, o já era esperado anteriormente tinha 
# resultado que a série e no parametro q da parte de médias móveis foi estimado 
# 0 dirente dos dois ajustes anteriores. Na parte da componente sazonal em que
# resultou em P = 2, ou seja a parte autorregressiva da componente sazonal foi 2,
# D = 1 como esperado também o numero de diferenciações para a parte sazonal ser 
# estacionária é 1 e Q = 0 também, ou seja, não há necessidade de parametro na 
# parte de médias móveis para a componente sazonal.


# Observando os critérios de seleção AICC
ajuste1$aicc
ajuste2$aicc
ajuste3$aicc
# O menor aicc foi do primeiro ajuste em seguida o segundo e em seguida o terceiro.

# Observando os critérios de seleção AIC
ajuste1$aic
ajuste2$aic
ajuste3$aic
# O menor aic teve o mesmo desempenho de primeiro ajuste em seguida o segundo 
# e em seguida o terceiro.

# Observando os critérios de seleção BIC
ajuste1$bic
ajuste2$bic
ajuste3$bic
# Já utilizando outro critério de seleção BIC, o ajuste1 continuou a ser melhor
# modelo o com o menor BIC, já nesse critério o ajuste3 teve um desempenho melhor
# que o do segundo ajuste. 

# Concluindo que pelos critérios de seleção de ajuste o primeiro ajuste é o melhor
# ajuste, agora vamos ver pelos resíduos das previsões para ver qual dos três modelos
# é o melhor.

# ********************* Dúvida
# Posso chamar de modelo ??? creio eu que sim, já que eu...
# ********************* Dúvida

arima_dados <- arima(AirPassengers)
# ********************* Dúvida não sei para o que serve.
# autoplot(arima_dados)
# ********************* Dúvida
d_forecasts <- forecast(arima_dados, level = 95, h = 25)
autoplot(d_forecasts)
# ********************* Dúvida

######################################################
# METODOS DE PREVISAO
######################################################



# Métodos de previsão ingenuos.

# Apenas observando:
# método de média para previsão.
meanf(AirPassengers, h = 12)
# naive
naive(AirPassengers, h = 12)
snaive(AirPassengers, h = 12)
rwf(AirPassengers, h = 12, drift = TRUE)


# Plotando as previsões sem retirar as observações

AirPassengers_I <- window(AirPassengers, start = 1949, end = c(1960, 12))
autoplot(AirPassengers_I) +
  autolayer(meanf(AirPassengers_I, h = 12), series = "Média", PI = FALSE) +
  autolayer(naive(AirPassengers_I, h = 12), series = "Naive", PI = FALSE) +
  autolayer(snaive(AirPassengers_I, h = 12), series = "Naive Sazonal", PI = FALSE) +
  autolayer(rwf(AirPassengers_I, h = 12, drift = TRUE), series = "Naive com drift", PI = FALSE) +
  ggtitle("N Total de passageiros de companhias aéres interenacionais") + 
  guides(colour = guide_legend("Forecast"))

# Observando o grafico, a previsão pela média, e o naive, são constntes, demonstrando
# claramente uma previsão bem distante do comportamento natural da série, e mesmo 
# o método naive com drift não sendo uma constante podendo notar um certo crescimento
# ainda sim teve um comportamento bem distante da série, o melhor método ingênuo foi o 
# naive sazonal, que conseguiu captar o comportamento da série com as caracteristicas
# fortes sazonais. 

# Plotando as previsões retirando as ultimas 12 observações
AirPassengers_I12 <- window(AirPassengers, start = 1949, end = c(1959, 12))

AirPassengersfit1 <- meanf(AirPassengers_I12, h = 12)
# naive
AirPassengersfit2 <- naive(AirPassengers_I12, h = 12)
AirPassengersfit3 <- snaive(AirPassengers_I12, h = 12)
AirPassengersfit4 <- rwf(AirPassengers_I12, h = 12, drift = TRUE)


autoplot(window(AirPassengers, start = 1949)) +
  autolayer(AirPassengersfit1, series = "Média", PI = FALSE) +
  autolayer(AirPassengersfit2, series = "Naive", PI = FALSE) +
  autolayer(AirPassengersfit3, series = "Naive Sazonal", PI = FALSE) +
  autolayer(AirPassengersfit4, series = "Naive com drift", PI = FALSE) +
  ggtitle("N Total de passageiros de companhias aéres interenacionais") + 
  guides(colour = guide_legend("Forecast"))

# A mesma interpretação anterior.
# mas não capitou a tendência crescente.

######################################################
# Ajuste e diagnostico
######################################################
sarima(AirPassengers, 2,1,1, 0,1,0, 12)
