## Curso Modelos de regress�o no software R - M�dulo 2 ##

## Modelos lineares generalizados ##

## Distribui��o de Poisson
## Utilizadas para vari�veis de contagem tais como n�mero de esporos de fungos, 
## n�mero de habitantes, n�mero de pessoas doentes, n�mero de insetos, n�mero de plantas afetadas, etc
## Normalmente a vari�ncia residual � alta para este tipo de distribui��o

## Leitura dos dados
dados<-read.table("C:/Users/hp/OneDrive - Experimental Analytics Corporation/Short course/Modelos de regress�o no software R - M�dulo 2/dados_contagem.csv",h=T,sep=",")

# Sumarizar os dados
library(dplyr)
continuous <- select_if(dados, is.numeric)
summary(continuous)

# Rodando o modelo para analizar o n�mero de infectados nas cidades de S�o Paulo e Rio de Janeiro 
# Para este tipo de modelo a distribui��o utilizada � Poisson
glm1 <- glm(formula= N�mero_de_Infectados ~ Sexo + Cidade, data=dados,
            family=poisson)
summary(glm1)

glm2 <- glm(formula= N�mero_de_Infectados ~ Sexo, data=dados,
            family=poisson)
summary(glm2)

glm3 <- glm(formula= N�mero_de_Infectados ~ Cidade, data=dados,
            family=poisson)
summary(glm3)

# Fazendo predi��es
teste<-data.frame(Sexo=c("Homem","Mulher","Homem","Mulher"),Cidade=c("S�o Paulo","S�o Paulo","Rio de Janeiro","Rio de Janeiro"), N�mero_de_Infectados=c(23,35,10,5))
predict <- round(predict(glm1, teste, type = 'response'),0)

# Calculando a acur�cia
acuracia <- cor(teste$N�mero_de_Infectados,predict, method = "pearson")
acuracia
acuracia <- cor(teste$N�mero_de_Infectados,predict, method = "spearman")
acuracia