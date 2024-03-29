## Curso Modelos de regress�o no software R - M�dulo 1 ##
## An�lise de correla��o ##

## Leitura dos dados
dados<-read.table("C:/Users/Mateus/Desktop/Curso de regress�o no R/DADOS TUBERCULOSE/dados-all.csv",h=T,sep=";")

## Vizualisando dados
library("ggpubr")
ggscatter(dados, x = "GTA", y = "Tuberculosis.categórica", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "GTA", ylab = "Tuberculose")

## Testes preliminares
## Teste de normalidade de Shapiro-Wilk para normalidade
shapiro.test(dados$PROD)

## An�lise visual de normalidade dos dados
ggqqplot(dados$PROD, ylab = "Produtividade em sacos por hectare")

##Trabalhando com arquivo de dados completo
## Correla��o de Pearson
cor(dados$PROD,dados$Trat, method = "pearson")
cor.test(dados$PROD,dados$Trat, method = "pearson")

cor(dados,dados, method = "pearson")
cor.test(dados,dados, method = "pearson")
# aqui d� erro pq nao roda pra matriz inteira o cor.test tem que fazer par a par

## Correla��o de Spearman
cor(dados$PROD,dados$Trat, method = "spearman")
cor.test(dados$PROD,dados$Trat, method = "spearman")

cor(dados,dados, method = "spearman")
cor.test(dados,dados, method = "spearman")

# spearman rankea os valores do menor para o maior
# so faz sentido utilizar se � legal rankear os valores

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


# Pearson era para dados param�tricos e Spearman para dados n�o param�tricos


##Trabalhando com arquivo de m�dia de tratamento
dados1<-aggregate(x = dados[3:ncol(dados)], by = list(Trat=dados$Trat), FUN = mean)

## Vizualisando dados
ggscatter(dados1, x = "Trat", y = "PROD", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Tratamento", ylab = "PROD")

## Testes pre liminares
## Teste de normalidade de Shapiro-Wilk para normalidade
shapiro.test(dados1$PROD)

## An�lise visual de normalidade dos dados
ggqqplot(dados1$PROD, ylab = "Produtividade em sacos por hectare")

## Correla��o de Pearson
cor(dados1$PROD,dados1$Trat, method = "pearson")
cor.test(dados1$PROD,dados1$Trat, method = "pearson")

cor(dados1,dados1, method = "pearson")
cor.test(dados1,dados1, method = "pearson")

## Correla��o de Spearman
cor(dados1$PROD,dados1$Trat, method = "spearman")
cor.test(dados1$PROD,dados1$Trat, method = "spearman")

cor(dados1,dados1, method = "spearman")
cor.test(dados1,dados1, method = "spearman")

## Exerc�cio
## Fa�a todos os gr�ficos e c�lculo dos teste de correla��o para todas as combina��es de vari�veis
## Salve os resultados em uma pasta
