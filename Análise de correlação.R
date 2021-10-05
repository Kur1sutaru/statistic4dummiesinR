## Curso Modelos de regressão no software R - Módulo 1 ##
## Análise de correlação ##

## Leitura dos dados
dados<-read.table("C:/Users/Mateus/Desktop/Curso de regressão no R/DADOS TUBERCULOSE/dados-all.csv",h=T,sep=";")

## Vizualisando dados
library("ggpubr")
ggscatter(dados, x = "GTA", y = "Tuberculosis.categÃ³rica", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "GTA", ylab = "Tuberculose")

## Testes preliminares
## Teste de normalidade de Shapiro-Wilk para normalidade
shapiro.test(dados$PROD)

## Análise visual de normalidade dos dados
ggqqplot(dados$PROD, ylab = "Produtividade em sacos por hectare")

##Trabalhando com arquivo de dados completo
## Correlação de Pearson
cor(dados$PROD,dados$Trat, method = "pearson")
cor.test(dados$PROD,dados$Trat, method = "pearson")

cor(dados,dados, method = "pearson")
cor.test(dados,dados, method = "pearson")
# aqui dá erro pq nao roda pra matriz inteira o cor.test tem que fazer par a par

## Correlação de Spearman
cor(dados$PROD,dados$Trat, method = "spearman")
cor.test(dados$PROD,dados$Trat, method = "spearman")

cor(dados,dados, method = "spearman")
cor.test(dados,dados, method = "spearman")

# spearman rankea os valores do menor para o maior
# so faz sentido utilizar se é legal rankear os valores

# calcular o valor de p para todas as variaveis juntas: #p-value
#  cor.mtest <- function(mat, ...) {
#    mat <- as.matrix(mat)
#    n <- ncol(mat)
#    p.mat<- matrix(NA, n, n)
#    diag(p.mat) <- 0
#    for (i in 1:(n - 1)) {
#      for (j in (i + 1):n) {
#        tmp <- cor.test(mat[, i], mat[, j], ...)
#        p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
#      }
#    }
#    colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
#    p.mat
#  }
# p.mat
#p.mat <- cor.mtest(test)
# dim(p.mat)


# Pearson era para dados paramétricos e Spearman para dados não paramétricos


##Trabalhando com arquivo de média de tratamento
dados1<-aggregate(x = dados[3:ncol(dados)], by = list(Trat=dados$Trat), FUN = mean)

## Vizualisando dados
ggscatter(dados1, x = "Trat", y = "PROD", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Tratamento", ylab = "PROD")

## Testes pre liminares
## Teste de normalidade de Shapiro-Wilk para normalidade
shapiro.test(dados1$PROD)

## Análise visual de normalidade dos dados
ggqqplot(dados1$PROD, ylab = "Produtividade em sacos por hectare")

## Correlação de Pearson
cor(dados1$PROD,dados1$Trat, method = "pearson")
cor.test(dados1$PROD,dados1$Trat, method = "pearson")

cor(dados1,dados1, method = "pearson")
cor.test(dados1,dados1, method = "pearson")

## Correlação de Spearman
cor(dados1$PROD,dados1$Trat, method = "spearman")
cor.test(dados1$PROD,dados1$Trat, method = "spearman")

cor(dados1,dados1, method = "spearman")
cor.test(dados1,dados1, method = "spearman")

## Exercício
## Faça todos os gráficos e cálculo dos teste de correlação para todas as combinações de variáveis
## Salve os resultados em uma pasta
