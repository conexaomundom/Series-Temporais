rm(list=ls())
install.packages("forecast")

###
# Pegando os dados, arrumando as vari√°veis
ficheiro = file.choose()
bd = read.table(file = ficheiro, header = FALSE, dec=".")
attach(bd)
icms_bahia <- bd
attach(icms_bahia)
igpdi <- icms_bahia$V1
icms_ba <- icms_bahia$V2
####

###
# An√°lise Descritiva Sem considerar a infla√ß√£o
summary(icms_ba)
boxplot(icms_ba, main = "ICMS mensal, \n Janeiro de 1995 |-| Agosto de 2019, Bahia", ylab = "ICMS")
# Maximo oBSERVA√á√ÉO 288
max(icms_ba)
which(icms_ba == max(icms_ba))
# M√≠nimo OBSERVA√á√ÉO 3
min(icms_ba) 
which(icms_ba == min(icms_ba))
# A mediana nesses anos do ICMS da Bahia foi
median(icms_ba)

###
# An√°lise Descritiva Considerando a infla√ß√£o
tam <- length(igpdi)
tam_icms <- length(icms_ba)

igpdi_d <- igpdi/igpdi[tam]
icms_d <- icms_ba / igpdi_d
summary(icms_d)
boxplot(cbind(icms_ba, icms_d), main = "ICMS mensal, \n Janeiro de 1995 |-| Agosto de 2019, Bahia", ylab = "ICMS")
###
# Podemos verificar que a serie deflacionada possui seus valores mais elevados
# alem de possuir um outlier no mÍs ___ do ano ___, alem de possuir a media e a 
# mediana mais elevada assim diminuindo a amplitude da serie.
###
# Colocando o ICMS da Bahia deflacionado em formato de s√©rie temporal
library(tseries)
icmsba_d.ts = ts(icms_d, start = c(1995,1), frequency = 12)
# Plot da s√©rie e plot do decompose
plot(icmsba_d.ts)
plot(decompose(icmsba_d.ts))
# Vemos no grafico uma clara tendencia de ordem crescente, tendencia essa
# que se torna bastante visivel apartir dos anos 2000, n„o demostrando nenhuma
# grande mudanÁa nesta tendencia desde ent„o, alem disso podemos ver uma sazionalidade
# no periodo anual e tambem nos meses dos anos, assim apresentando uma clara sazionalidade 
# em todo o deccorer da serie.

# ACF e PACF
acf(icmsba_d.ts)
pacf(icmsba_d.ts)
# Observa-se que todos os lag do ACF est„o fora do intervalo de confianÁa assim indicando
# uma possibilidade elevada de n„o ser representado por um modelo AR, alem disso o PACF
# apÛs o terceiro lag demonstra uma queda exponencial, se aproximando do zero e se tornando
# oscilatÛrio apÛs isso indicaria uma possÌvel representaÁ„o de um modelo MA(2), mas pela existÍncia
# de sazonalidade e da tendÍncia, est· serie possivelmete se encaixaria melhor em um modelo SARIMA,
# mas como n„o fomos apresentados para estes modelos n„o podemos analisar com maiores detalhes esta 
# possibilidade.
library(forecast)
auto.arima(icmsba_d.ts)
# Pela funÁ„o auto.arima temos indicios que o melhor modelo para representar esta serie
# seria o modelo MA(2) com uma diferenciaÁ„o, e que possui sazionalidade na serie assim sendo
# o modelo seria um SMA(2)

# Pegando a tendencia para 
icmsba_d_STL.ts <- stl(icmsba_d.ts, "periodic")
plot(icmsba_d.ts)
lines(icmsba_d_STL.ts$time.series[, "trend"], lwd = 1.4, col = "red")

# Verificamos em vermelho a tendencia da serie temporal, precisaremos destes valore para alguns possiveis
# ajusates para esta serie temporal, alem disso observa-se claramente a tendencia novamente, desta vez
# ainda mais facilmente por estar sobreposta a propria serie temporal.

###
# Teste de estacionaridade
library(tseries)
adf.test(icmsba_d.ts, alternative = "stationary")

# Ao nivel de significancia de 5%, ou seja, considerando a probabilidade do erro tipo I,
# probabilidade de rejeitar H0 dado que H0 È verdadeiro, igual a 5%, temos que a hipÛtese nula
# de n„o estacionaridade deve ser rejeitada, dado que p-valor, que representa a probabilidade de
# cometer este erro tipo I considerando os dados amostrais È de aproximadamente 0,01.
# Assim temos que esta serie temporal possui estacionaridade.


###
####

# Previs√£o 
library(forecast)
forecast(icmsba_d.ts, h=12, level = c(95,99))
plot(forecast(icmsba_d.ts, h=12, level = c(95,99)),shadecols = c( "#9ECAE1", "#6BAED6"), fcol = "darkblue", flwd = 1)
#Por conta da sazionalidade podemos prever por bastante tempo.  

####
# Primeira forma de retirar a tend√™ncia tomando uma diferen√ßa
####
dif_icmsba_d.ts <- diff(icmsba_d.ts)
plot(dif_icmsba_d.ts)
plot(decompose(dif_icmsba_d.ts))
# Vemos no grafico que a tendencia foi removida, assim apresentando aleatoriedade em seus dados
# observanfo a sazionalidade desta serie, temos que ela permanece proxima a da serie de dados original
# assim temos que a diferenciaÁ„o conseguiu remover a tendencia da serie

# ACF e PACF
acf(dif_icmsba_d.ts)
pacf(dif_icmsba_d.ts)
auto.arima(dif_icmsba_d.ts)
# Analisando o ACF e o PACF em conjundo gera-se uma duvida em qual modelo melhor se aplica para esta serie
# pois o PACF mantem sua queda exponencial apois o terceiro lag, mas o ACF apesar de n„o ser t„o claro demostra
# possiveis sinais da possibilidade da existencia da um AR na serie, assim gerando duvidas entre um modelo ARMA e um modelo MA
# lembrando que pela existencia da sazionalidade teriamos que incluir um componente para esta sazionalidade no modelo
# Como a funÁ„o auto.arima temos indicios de que o modelo que melhor se aplica para esta serie temporal seria um 
# MA(2) com o componente se sazionalidade, como foi aplicada uma diferenciaÁ„o na serie original que tinha como
# um possivel modelo de melhor aplicaÁ„o um MA(2) com o componente sazional e uma diferenciaÁ„o ent„o este resultado
# seria o esperado.

###
# Teste de estacionaridade
library(tseries)
adf.test(dif_icmsba_d.ts, alternative = "stationary")
# Novamente considerando a probabilidade do erro tipo I, probabilidade de rejeitar H0 dado que H0
# È verdadeiro, igual a 5%, temos que a hipÛtese nula de n„o estacionaridade deve ser rejeitada, 
# dado que p-valor, que representa a probabilidade de cometer este erro tipo I considerando os dados
# amostrais È de aproximadamente 0,01.
# Assim temos que esta serie temporal continua a possuir estacionaridade mesmo apos a aplicaÁ„o de uma diferenciaÁ„o.

###

# Previs√£o 
library(forecast)
forecast(dif_icmsba_d.ts, h=12, level = c(95,99))
plot(forecast(dif_icmsba_d.ts, h=12, level = c(95,99)),shadecols = c( "#9ECAE1", "#6BAED6"), fcol = "darkblue", flwd = 1)
#Por conta da sazionalidade podemos prever por bastante tempo.  

####
# Segunda forma de retirar a tend√™ncia taxa de crescimento, log
####
log_icmsba_d.ts <- log(icmsba_d.ts)
plot(log_icmsba_d.ts)
plot(decompose(log_icmsba_d.ts))
# Vemos no grafico que a tendencia de ordem crescente se mantem, tendencia essa
# que se mantem bastante visivel apartir dos anos 2000, n„o demostrando nenhuma
# grande mudanÁa em comparaÁ„o com a serie sem a aplicaÁ„o do log, alem disso podemos
# observar uma alteraÁ„o na sazionalidade desta serie, mas mantendo-se sazional
# assim n„o possuindo uma alteraÁ„o significativa nestes aspectos da serie temporal original.

# ACF e PACF
acf(log_icmsba_d.ts)
pacf(log_icmsba_d.ts)
# Novamente observa-se que todos os lag do ACF est„o fora do intervalo de confianÁa indicando
# uma possibilidade elevada de n„o ser representado por um modelo AR, alem disso o PACF novamente
# apÛs o terceiro lag demonstra uma queda exponencial, indicando de forma ainda mais direta uma
# possÌvel representaÁ„o de um modelo MA(2), mas por se manter a existÍncia de sazonalidade e da
# tendÍncia, est· serie possivelmete se encaixaria melhor em um modelo SARIMA.


# Teste de estacionaridade
library(tseries)
adf.test(log_icmsba_d.ts, alternative = "stationary")

# Novamente considerando a probabilidade do erro tipo I, probabilidade de rejeitar H0 dado que H0
# È verdadeiro, igual a 5%, temos que a hipÛtese nula de n„o estacionaridade deve ser rejeitada, 
# dado que p-valor, que representa a probabilidade de cometer este erro tipo I considerando os dados
# amostrais È de aproximadamente 0,01.
# Assim temos que esta serie temporal continua a possuir estacionaridade mesmo apos a aplicaÁ„o do log.


###

####

# Previs√£o 
library(forecast)
forecast(icmsba_d.ts, h=12, level = c(95,99))
plot(forecast(icmsba_d.ts, h=12, level = c(95,99)),shadecols = c( "#9ECAE1", "#6BAED6"), fcol = "darkblue", flwd = 1)
#Por conta da sazionalidade podemos prever por bastante tempo.  


####
# Terceira forma de serie removendo a tendencia

icms.sem.tend.ts <- icmsba_d.ts - icmsba_d_STL.ts$time.series[, "trend"]
plot(icms.sem.tend.ts)
plot(decompose(icms.sem.tend.ts))
# Vemos no grafico que a tendencia foi removida, assim apresentando aleatoriedade em seus dados
# observanfo a sazionalidade desta serie, temos que ela permanece proxima a da serie de dados original
# mas como o principal objetivo desta transformaÁ„o È o de remover a tendencia, ele foi visivelmente alcanÁado.

# ACF e PACF
acf(icms.sem.tend.ts)
pacf(icms.sem.tend.ts)
# Apartir do ACF e do PACF n„o somos capazes de termos uma ideia de qual seria o modelo que 
# melhor representaria esta serie temporal, sendo assim nescessario uma analise mais aprofundada
# para poder determinarmos qual seria o modelo que melhor representaria esta serie temporal.

# Teste de estacionaridade
library(tseries)
adf.test(icms.sem.tend.ts)
# Novamente considerando a probabilidade do erro tipo I, probabilidade de rejeitar H0 dado que H0
# È verdadeiro, igual a 5%, temos que a hipÛtese nula de n„o estacionaridade deve ser rejeitada, 
# dado que p-valor, que representa a probabilidade de cometer este erro tipo I considerando os dados
# amostrais È de aproximadamente 0,01.
# Assim temos que esta serie temporal continua a possuir estacionaridade mesmo apos remoÁ„o de sua tendencia.



library(forecast)
forecast(icms.sem.tend.ts, h=12, level = c(95,99))
plot(forecast(icms.sem.tend.ts, h=12, level = c(95,99)),shadecols = c( "#9ECAE1", "#6BAED6"), fcol = "darkblue", flwd = 1)
