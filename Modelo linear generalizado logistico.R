## Curso Modelos de regressão no software R - Módulo 2 ##

## Modelos lineares generalizados ##

## Modelos lineares são utilizados para variáveis com distribuição normal
## A grande maioria das variáveis não tem distribuição normal
## Para este tipo de variáveis nós precisamos usar os modelos conhecidos como lineares generalizados

## Como identificar se a minha variável tem distribuição normal????
## Através dos testes de normalidade como Kolmogorov-Smirnov e Shapiro-Wilk
## Vocês já aprenderam a analizar estes testes no curso Modelos de regressão linear - módulo 1

## Os modelos lineares generalizados são utilizados para analizar
## variáveis que possuem distribuição não normal tais como:
## dados binários, dados de contagem, probabilidades, proporções, etc

## Leitura dos dados
dados<-read.table("caminhododiretorio/tuberculose-dados-all.csv",h=T,sep=";")

# Sumarizar os dados
library(dplyr)
continuous <- select_if(dados, is.numeric)
statistics<-summary(continuous)
write.csv(statistics, "dados-tuberculose-reg-stat.csv")

# Rodando o modelo para analizar o uso de smartphone pela população
# Para este tipo de modelo a distribuição utilizada é binomial
# family - distribuição dos nossos dados
# documentação https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/glm

#Renomear GTA
colnames(dados)[1]<-("GTA")
colnames(dados)[13]<-("tuberculosis_cat")

# Remover as GTAS repetidas
dados<-dados[!duplicated(dados$GTA),]

glm1 <- glm(formula= tuberculosis_cat ~ TOTAL.ABATIDOS + QTD_PARTE_PARTADA, data=dados,
                family=binomial)
summary(glm1)

glm2 <- glm(formula= tuberculosis_cat ~ TOTAL.ABATIDOS, data=dados,
            family=binomial)
summary(glm2)

glm3 <- glm(formula= tuberculosis_cat ~ QTD_PARTE_PARTADA, data=dados,
            family=binomial)
summary(glm3)

# Teste de Hosmer-Lemeshow
library(ResourceSelection)
hoslem.test(dados$tuberculosis_cat, fitted(glm1))

# Fazendo predições
teste<-data.frame(Sexo=c(1,1,2,2),Idade=c(30,65,40,58), Smartphone=c(0,1,1,1))
predict <- predict(glm1, dados, type = 'response')
predict

# predict - ele vai dar um numero entre 0 e 1 assim, maior que 0.5 vai ser meu 1
# menor que 0.5 meu 0 logo, um valor 0.34 significa minha classe 0
# nesse exemplo, 0 usa smarthphone, e 1 não usa smarthphone

#              1         2         3         4 
#           0.1051911 0.9690878 0.3670668 0.9111694
# era assim 0            1         1         1
#           acertou    acertou    errou     acertou
# https://www.rdocumentation.org/packages/car/versions/3.0-10/topics/Predict

# Matrix de confusão
dados_conf_matrix <- table(dados$tuberculosis_cat, predict > 0.5)
dados_conf_matrix

# nas colunas a matriz tem os valores reais e nas linhas os valores preditos
#      FALSE TRUE
##0     1    0
##1     1    2
# o modelo classificou uma errada a diagonal principal é o que o modelo acertou
###     FALSE TRUE
##0      1    0  linha classe zero
##1    - 1 -   2 linha classe um
###    aqui é o erro
###

# Calculando a acurácia
acuracia <- sum(diag(dados_conf_matrix)) / sum(dados_conf_matrix)
acuracia

# Calculando a precisão
precision <- function(matrix) {
  # True positive
  tp <- matrix[2, 2]
  # false positive
  fp <- matrix[1, 2]
  return (tp / (tp + fp))
}

recall <- function(matrix) {
  
  tp <- matrix[2, 2]# true positive
  fn <- matrix[2, 1]# false negative
  return (tp / (tp + fn))
}

prec <- precision(dados_conf_matrix)
prec
rec <- recall(dados_conf_matrix)
rec

f1 <- 2 * ((prec * rec) / (prec + rec))
f1

# Plotando os resultados
library(ROCR)
ROCRpred <- prediction(predict, teste$Smartphone)
ROCRperf <- performance(ROCRpred, 'tpr', 'fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2, 1.7))

#fazer smote para fazer a amostragem


