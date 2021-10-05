## Curso Modelos de regressão no software R - Módulo 1 ##
## Teste e ajuste de normalidade ##

## Leitura dos dados
dados<-read.table("C:/Users/hp/OneDrive - Experimental Analytics Corporation/Short course/Modelos de regressão no software R - Módulo 1/dados.csv",h=T,sep=",")

##Teste de homogeneidade e normalidade de variância
##Calculando assimetria e curtose no R
## Tipos de curtose:
## C=3 - mesocurtica
## C>3 - platicúrtica
## C<3 - leptocúrtica
## Tipos de assimetria
## AS=0 - Média, moda e mediana coincidem
## AS>0 - cauda da distribuição do lado direito do gráfico
## AS<0 - cauda da distribuição do lado esquerdo do gráfico
require(e1071)
kurtosis(dados$PROD, na.rm = TRUE,type=3)
skewness(dados$PROD,type=1)
library(fBasics)
basicStats(dados$PROD)

##Rodando o teste de Kolmogorov-Smirnov
ks.test(dados$PROD,"pnorm")

##Rodando o teste de Shapiro-wilk
shapiro.test(dados$PROD)

## Calculando teste de Bartlet
bartlett.test(dados$PROD~dados$Trat, dados)

##Remoção de outliers
#------------------------------------------------------------------------------------------
# identificar/remover os pontos discrepantes/influentes
dados[1,3]<-20

dados$Trat<-as.factor(dados$Trat)
dados$Rep<-as.factor(dados$Rep)
m<-aov(PROD~Trat+Rep,dados)
layout(1)
plot(residuals(m)~Trat+Rep, dados)

dados1<-dados[!dados$PROD %in% boxplot.stats(dados$PROD)$out,]

#
#------------------------------------------------------------------------------------------
# análise com os pontos removidos
m1<-aov(PROD~Trat+Rep,dados1)
layout(1)
plot(residuals(m1)~Trat+Rep, dados1)
boxplot.stats(dados1$PROD)$out

## Exercício
## Faça o teste de normalidade e remoção de outlier para todas as variáveis