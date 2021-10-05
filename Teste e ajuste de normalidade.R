## Curso Modelos de regress�o no software R - M�dulo 1 ##
## Teste e ajuste de normalidade ##

## Leitura dos dados
dados<-read.table("C:/Users/hp/OneDrive - Experimental Analytics Corporation/Short course/Modelos de regress�o no software R - M�dulo 1/dados.csv",h=T,sep=",")

##Teste de homogeneidade e normalidade de vari�ncia
##Calculando assimetria e curtose no R
## Tipos de curtose:
## C=3 - mesocurtica
## C>3 - platic�rtica
## C<3 - leptoc�rtica
## Tipos de assimetria
## AS=0 - M�dia, moda e mediana coincidem
## AS>0 - cauda da distribui��o do lado direito do gr�fico
## AS<0 - cauda da distribui��o do lado esquerdo do gr�fico
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

##Remo��o de outliers
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
# an�lise com os pontos removidos
m1<-aov(PROD~Trat+Rep,dados1)
layout(1)
plot(residuals(m1)~Trat+Rep, dados1)
boxplot.stats(dados1$PROD)$out

## Exerc�cio
## Fa�a o teste de normalidade e remo��o de outlier para todas as vari�veis