##            Bibliotecas
library(xts)
library(forecast)
library(xlsx)
library(csv)
library(pastecs)
library(boot)
library(tseries)
library(aTSA)
##            Lendo tabelas do arquivo excell

BTC_Price = read_excel("Tabela 1.xls")
BTC_Resposta_Price = read_excel("Tabela 2.xlsx")

##           Criando as Séries Temporais

TS_BTC_Price <- ts(BTC_Price[,2:2],start=c(2017,1),frequency=365)
TS_BTC_Resposta_Price <- ts(BTC_Resposta_Price[,2:2],start=c(2019,182),frequency=365)


##            Analisando os dados

plot(TS_BTC_Price)

##            Analisando Tendencia

trend.test(TS_BTC_Price,plot=TRUE)


##          Analisando estacionariedade

acf(TS_BTC_Price)## Resultado = não estacionário
acf(diff(TS_BTC_Price))

##          Análise da estacionariedade do gráfico já ajustado

plot(diff(log(TS_BTC_Price)))

##        analisando necessidade de médias móveis

acf(diff(log(TS_BTC_Price)))## os valores estão proximos da media, logo nao sera necessario media movel

##      Construção do modelo ARIMA

ajuste<-arima(log(TS_BTC_Price),c(1,1,1),seasonal = list(order=c(1,1,1),period = 31))
pred<- predict(ajuste,n.ahead = 31 )
str(ts3 <- c(TS_BTC_Price, TS_BTC_Resposta_Price))
ts.plot(TS_BTC_Price,TS_BTC_Resposta_Price)
ts.plot(TS_BTC_Price,exp(pred$pred),lty=c(1,3),TS_BTC_Resposta_Price)

Av<-exp(pred$pred)
Resultado<-data.frame(Predito=as.matrix(Av), Realizado=as.matrix(TS_BTC_Resposta_Price))
dif=0
Resultado$Dif=dif
(Resultado[1,]$Price)-(Resultado[1,]$Predito)
Resultado
for(i in 1:31){
  Resultado[i,]$Dif=Resultado[i,]$Price-Resultado[i,]$Predito
  if(Resultado[i,]$Dif<0)
  {
    Resultado[i,]$Dif=Resultado[i,]$Dif*-1
  }
  Resultado[i,]$Dif=Resultado[i,]$Dif/Resultado[i,]$Predito
}
med=0
for(i in 1:31){
  
  med=med+Resultado[i,]$Dif
}

##          Imprimir a média da diferença entre o previsto e o realizado

A=(med/31)*100
B=paste(A, "%", sep="")
print(B)

