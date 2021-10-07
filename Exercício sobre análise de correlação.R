## Curso Modelos de regressão no software R - Módulo 1 ##
## Análise de correlação ##

## Exercício
## Faça todos os gráficos e cálculo dos teste de correlação para todas as combinações de variáveis

## Ler o arquivo de dados
dados<-read.table("caminhododiretorio/dados.csv",h=T,sep=",")

## Vizualizar os dados
library("ggpubr")
graficos_vizualizaçao_de_dados<-list()
for (i in 3:ncol(dados))
{
  for (j in 3:ncol(dados))
  {
    if(i!=j)
    {
      if(colnames(dados[i])=="PROD")
      {
        xlab<-"sacos por hectare"
      } else if (colnames(dados[i])=="ALP")
      {
        xlab<-"centímetros"
      } else if (colnames(dados[i])=="MAT")
      {
        xlab<-"dias"
      } else if (colnames(dados[i])=="ACM"|colnames(dados[i])=="FER")
      {
        xlab<-"Porcentagem"
      } else {
        xlab<-"Contagem"
      }
      
      if(colnames(dados[j])=="PROD")
      {
        ylab<-"sacos por hectare"
      } else if (colnames(dados[j])=="ALP")
      {
        ylab<-"centímetros"
      } else if (colnames(dados[j])=="MAT")
      {
        ylab<-"dias"
      } else if (colnames(dados[j])=="ACM"|colnames(dados[j])=="FER")
      {
        ylab<-"Porcentagem"
      } else {
        ylab<-"Contagem"
      }
      temp<-data.frame(cbind(dados[,i],dados[,j]))
      graficos_vizualizaçao_de_dados[[paste(i,j)]]<-ggscatter(data = temp,x = "X1", y = "X2", 
                add = "reg.line", conf.int = TRUE, 
                cor.coef = TRUE, cor.method = "pearson",
                xlab = xlab, ylab = ylab)
      
    }
  }
}

## Teste de shapiro-Wilk e gráfico de normalidade
norm.test<-list()
graficos_normalidade<-list()
for (i in 3:ncol(dados))
{
  names<-colnames(dados[i])
  norm.test[[names]]<-shapiro.test(dados[,i])
  graficos_normalidade[[names]]<-ggqqplot(dados[,i], ylab = names)
}
norm.testfinal<-data.frame(do.call(rbind,norm.test))
norm.testfinal1<-data.frame(Variável=row.names(norm.testfinal),Valor=as.numeric(norm.testfinal$statistic), p.Value=as.numeric(norm.testfinal$p.value))

## Rodando as correlações
correlacoes<-list()
for (i in 3:ncol(dados))
{
  for(j in 3:ncol(dados))
  {
    temppearson<-cor.test(dados[,i],dados[,j],method = "pearson")
    tempspearman<-cor.test(dados[,i],dados[,j],method = "spearman")
    
    dados1<-data.frame(aggregate(x = dados[3:ncol(dados)], by = list(Trat=dados$Trat), FUN = mean))
    temppearsonmedia<-cor.test(dados1[,i-1],dados1[,j-1],method = "pearson")
    tempspearmanmedia<-cor.test(dados1[,i-1],dados1[,j-1],method = "spearman")
    correlacoes[[paste(i,j)]]<-data.frame(Comparaçao=paste(colnames(dados[i]),colnames(dados[j]),sep="-"),
                                          rPearson=temppearson$estimate,pvaluerPearson=temppearson$p.value,
                                          rSpearman=tempspearman$estimate,pvaluerSpearman=tempspearman$p.value,
                                          rPearsonmedia=temppearsonmedia$estimate,pvaluerPearsonmedia=temppearsonmedia$p.value,
                                          rSpearmanmedia=tempspearmanmedia$estimate,pvaluerSpearmanmedia=tempspearmanmedia$p.value)
  }
}
correlacoesfinal<-data.frame(do.call(rbind,correlacoes))

## Salve os resultados em uma pasta
write.table(norm.testfinal1,"caminhododiretorio/resultado_teste_normalidade.csv", quote=F, row.names=F,sep=",")
write.table(correlacoesfinal,"caminhododiretorio/Modelos de regressão no software R - Módulo 1/resultado_correlacoes.csv", quote=F, row.names=F,sep=",")
pdf("caminhododiretorio/Modelos de regressão no software R - Módulo 1/gráficos_preliminares.pdf",width=6,height=4,paper='special')
graficos_vizualizaçao_de_dados
dev.off()
pdf("caminhododiretorio/gráficos_normalidade.pdf",width=6,height=4,paper='special')
graficos_normalidade
dev.off()
