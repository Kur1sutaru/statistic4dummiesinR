## Curso Modelos de regressão no software R - Módulo 1 ##
## Gráficos de correlação ##

## Leitura dos dados
dados<-read.table("caminhododiretorio/Modelos de regressão no software R - Módulo 1/dados.csv",h=T,sep=",")
dados1<-dados[,c(-2)]

library(PerformanceAnalytics)
chart.Correlation(dados1, histogram=TRUE, pch=19)

library(psych)
pairs.panels(dados1, scale=TRUE)

library(corrplot)
corrplot.mixed(cor(dados1), order="hclust", tl.col="black")

library(GGally)
ggpairs(dados1)

ggcorr(dados1, nbreaks=8, palette='RdGy', label=TRUE, label_size=5, label_color='white')

library(ggcorrplot)
ggcorrplot(cor(dados1), p.mat = cor_pmat(dados1), hc.order=TRUE, type='lower')
