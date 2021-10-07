## Curso Modelos de regressão no software R - Módulo 2 ##

## Modelos lineares generalizados ##

## Distribuição de Poisson
## Utilizadas para variáveis de contagem tais como número de esporos de fungos, 
## número de habitantes, número de pessoas doentes, número de insetos, número de plantas afetadas, etc
## Normalmente a variância residual é alta para este tipo de distribuição

## Leitura dos dados
dados<-read.table("caminhododiretorio/Modelos de regressão no software R - Módulo 2/dados_contagem.csv",h=T,sep=",")

# Sumarizar os dados
library(dplyr)
continuous <- select_if(dados, is.numeric)
summary(continuous)

# Rodando o modelo para analizar o número de infectados nas cidades de São Paulo e Rio de Janeiro 
# Para este tipo de modelo a distribuição utilizada é Poisson
glm1 <- glm(formula= Número_de_Infectados ~ Sexo + Cidade, data=dados,
            family=poisson)
summary(glm1)

glm2 <- glm(formula= Número_de_Infectados ~ Sexo, data=dados,
            family=poisson)
summary(glm2)

glm3 <- glm(formula= Número_de_Infectados ~ Cidade, data=dados,
            family=poisson)
summary(glm3)

# Fazendo predições
teste<-data.frame(Sexo=c("Homem","Mulher","Homem","Mulher"),Cidade=c("Sâo Paulo","Sâo Paulo","Rio de Janeiro","Rio de Janeiro"), Número_de_Infectados=c(23,35,10,5))
predict <- round(predict(glm1, teste, type = 'response'),0)

# Calculando a acurácia
acuracia <- cor(teste$Número_de_Infectados,predict, method = "pearson")
acuracia
acuracia <- cor(teste$Número_de_Infectados,predict, method = "spearman")
acuracia
