## Curso Modelos de regress�o no software R - M�dulo 2 ##

## Modelos lineares generalizados ##

## Distribui��o de Poisson
## As vezes o valor da vari�ncia condicional � muito maior que o valor da m�dia condicional
## para caracteres com distribui��o de Poisson. Neste caso n�s falamos que os dados de overdispersion.
## Para estes casos precisamos checar a distribui��o quasiPoisson

## Leitura dos dados
dados<-read.table("C:/Users/hp/OneDrive - Experimental Analytics Corporation/Short course/Modelos de regress�o no software R - M�dulo 2/dados_contagem.csv",h=T,sep=",")


# Sumarizar os dados
library(dplyr)
continuous <- select_if(dados, is.numeric)
summary(continuous)

# Rodando o modelo para analizar o uso de smartphone pela popula��o
# Para este tipo de modelo a distribui��o utilizada � binomial
glm1 <- glm(formula= N�mero_de_Infectados ~ Sexo + Cidade, data=dados,
            family=quasipoisson)
summary(glm1)

# Se o valor do par�metro de dispers�o para a distribui��o quasipoisson
# for maior que 1 significa que temos overdispersion, e se o valor 
# for menor do que 1 significa que temos underdispersion
glm2 <- glm(formula= N�mero_de_Infectados ~ Sexo, data=dados,
            family=quasipoisson)
summary(glm2)

glm3 <- glm(formula= N�mero_de_Infectados ~ Cidade, data=dados,
            family=quasipoisson)
summary(glm3)

# Comparando modelos
anova(glm1, glm2, glm3, test="Chisq")

# Calcualndo os intervalos de confian�a baseados em Cameron e Trivedi (2009)
library(sandwich)
cov.m1 <- vcovHC(glm2, type="HC0")
std.err <- sqrt(diag(cov.m1))
r.est <- cbind(Estimate= coef(glm2), "Robust SE" = std.err,
               "Pr(>|z|)" = 2 * pnorm(abs(coef(glm2)/std.err), lower.tail=FALSE),
               LL = coef(glm2) - 1.96 * std.err,
               UL = coef(glm2) + 1.96 * std.err)

# Teste do residuo
with(glm2, cbind(res.deviance = deviance, df = df.residual,
                 p = pchisq(deviance, df.residual, lower.tail=FALSE)))

with(glm1, cbind(res.deviance = deviance, df = df.residual,
                 p = pchisq(deviance, df.residual, lower.tail=FALSE)))

with(glm3, cbind(res.deviance = deviance, df = df.residual,
                 p = pchisq(deviance, df.residual, lower.tail=FALSE)))

# Fazendo predi��es
teste<-data.frame(Sexo=c("Homem","Mulher","Homem","Mulher"),Cidade=c("S�o Paulo","S�o Paulo","Rio de Janeiro","Rio de Janeiro"), N�mero_de_Infectados=c(23,35,10,5))
predict <- predict(glm1, teste, type = 'response')

# Calculando a acur�cia
acuracia <- cor(teste$N�mero_de_Infectados,predict, method = "pearson")
acuracia
acuracia <- cor(teste$N�mero_de_Infectados,predict, method = "spearman")
acuracia

# A regress�o de Poisson � calculada via m�xima verossimilhna�a.
# Por isso � necess�rio um grande n�mero de observa��es.