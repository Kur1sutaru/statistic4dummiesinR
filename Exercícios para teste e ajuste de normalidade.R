## Curso Modelos de regressão no software R - Módulo 1 ##
## Resolução dos exercícios de teste de normalidade e remoção de outlier ##

## Exercício
## Faça o teste de normalidade e remoção de outlier para todas as variáveis

## Leitura dos dados
dados<-read.table("C:/Users/hp/OneDrive - Experimental Analytics Corporation/Short course/Modelos de regressão no software R - Módulo 1/dados.csv",h=T,sep=",")

##Rodando teste de normalidade, curtose, simetria e homogeneidade de variância para todas as características
resultado<-list()
for(i in 3:ncol(dados))
{
  require(e1071)
  kur<-kurtosis(dados[,i], na.rm = TRUE,type=3)
  ske<-skewness(dados[,i],type=1)
  
  ##Rodando o teste de Kolmogorov-Smirnov
  kst<-ks.test(dados[,i],"pnorm")
  
  ##Rodando o teste de Shapiro-wilk
  sht<-shapiro.test(dados[,i])
  
  ## Calculando teste de Bartlet
  bt<-bartlett.test(dados[,i]~dados$Trat, dados)
  
  ## Montando o dataframe
  resultado[[i]]<-data.frame(Característica=colnames(dados[i]),
                      Curtose=kur,Simetria=ske,
                      Kol_Teste=kst$p.value,Sha_Teste=sht$p.value,
                      Bar_Teste=bt$p.value)
}

resultado_final<-data.frame(do.call(rbind,resultado))

## Removendo outliers
dados[1,3]<-20

for(i in 3:ncol(dados))
{
  dados[,i]<-ifelse(!dados[,i] %in% boxplot.stats(dados[,i])$out,dados[,i],NA) 
}

write.table(resultado_final,"C:/Users/hp/OneDrive - Experimental Analytics Corporation/Short course/Modelos de regressão no software R - Módulo 1/Teste_de_normalidade.csv",row.names=F, quote=F,sep=",")
write.table(dados,"C:/Users/hp/OneDrive - Experimental Analytics Corporation/Short course/Modelos de regressão no software R - Módulo 1/dados_novo.csv",row.names=F,quote=F,sep=",")


