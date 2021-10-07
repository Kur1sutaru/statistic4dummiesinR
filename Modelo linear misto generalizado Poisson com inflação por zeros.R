## Curso Modelos de regressão no software R - Módulo 2 ##

## Modelos mistos lineares generalizados ##

## A distribuição zero inflated Poisson Regression é utilizada quando nós temos dados de contagem com uma grande quantidade de zeros
## Modelos mistos deve ser utilizado quando nós temos efeito fixo e aleatório

require(glmmTMB)
require(lme4)
require(parallel)
library(bbmle)
library(boot)
library(ggplot2)
library(GGally)

## Leitura dos dados
dados<-read.table("caminhododiretorio/Modelos de regressão no software R - Módulo 2/dados_contagem_modelos_mistos.csv",h=T,sep=",")

## Transformando os efeitos em fatores
dados <- within(dados, {
  Variedade <- factor(Variedade)
  Local <- factor(Local)
  Ano <- factor(Ano)
  Rep <- factor(Rep)
})

## Análise gráfica
ggpairs(dados[, c("Variedade", "Local", "Ferrugem")])

ggplot(dados, aes(x = Variedade, y = Ferrugem)) +
  stat_sum(aes(size = ..n.., group = 1)) +
  scale_size_area(max_size=10)

ggplot(dados, aes(x = Local, y = Ferrugem)) +
  stat_sum(aes(size = ..n.., group = 1)) +
  scale_size_area(max_size=10)

ggplot(dados, aes(x = Ano, y = Ferrugem)) +
  stat_sum(aes(size = ..n.., group = 1)) +
  scale_size_area(max_size=10)

# Rodar o modelo de regressão generalizada com distribuição de Poisson
m <- glmmTMB(Ferrugem~(Variedade+Local)+(1|Ano),
                         data=dados,
                         ziformula=~1,
                         family=poisson)

print(m, corr = FALSE)
summary(m)

## Testando uma binomial negativa
m1 <- update(m,family=nbinom2)
summary(m1)

m2 <- update(m,family=nbinom1)
summary(m2)

# Calculando o intervalo de confiança
confint(m)  ## Wald/delta-method CIs
confint(m,parm="theta_")  ## Wald/delta-method CIs
confint(m,parm=1,method="profile")

# Extrair efeito fixo
fe<-fixef(m)
fe

# Extrair efeito aleatório
rr <- ranef(m)
rr

# Extrair resíduo
res<-residuals(m)
res
sig<-sigma(m)
sig

# Extrair matriz de variância para efeitos aleatório
var<-VarCorr(m)
var

# Fazendo bootstrap 
fm1 <- glmmTMB(Ferrugem~(Variedade+Local)+(1|Ano),
               data=dados,
               ziformula=~1,
               family=poisson)

## single parametric bootstrap step: refit with data simulated from original model
fm1R <- refit(fm1, simulate(fm1)[[1]])
## the bootMer function from lme4 provides a wrapper for doing multiple refits
##   with a specified summary function
b1 <- lme4::bootMer(fm1, FUN=function(x) fixef(x)$zi, nsim=20, .progress="txt")
if (requireNamespace("boot")) {
  boot.ci(b1,type="perc")
}

# Fazendo predições
predict(m, allow.new.levels=TRUE)
newdata <- with(dados, expand.grid(Ano=unique(Ano), Variedade=unique(Variedade), Local=unique(Local)))
predict(m, newdata=newdata, allow.new.levels=TRUE)

# Modelos disponíveis no pacote glmmTMB
getCapabilities(what = "all", check = FALSE)
