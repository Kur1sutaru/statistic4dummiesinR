## Curso Modelos de regressão no software R - Módulo 2 ##

## Exemplo final ##
library(ggplot2)
library(GGally)
library(pscl)
library(lme4)
library(glmmTMB)

## Leitura dos dados
dados<-read.table("C:/Users/hp/OneDrive - Experimental Analytics Corporation/Short course/Modelos de regressão no software R - Módulo 2/exemplo_final.csv",h=T,sep=",")

## Transformando os efeitos em fatores
dados <- within(dados, {
  Variedade <- factor(Variedade)
  Local <- factor(Local)
  Ano <- factor(Ano)
  Rep <- factor(Rep)
})

## Análise gráfica
ggpairs(dados[, c("Variedade", "Local", "Resistencia")])
ggpairs(dados[, c("Variedade", "Local", "Plantas_mortas")])
ggpairs(dados[, c("Variedade", "Local", "Severidade")])

ggplot(dados, aes(x = Variedade, y = Resistencia)) +
  stat_sum(aes(size = ..n.., group = 1)) +
  scale_size_area(max_size=10)

ggplot(dados, aes(x = Local, y = Resistencia)) +
  stat_sum(aes(size = ..n.., group = 1)) +
  scale_size_area(max_size=10)

ggplot(dados, aes(x = Ano, y = Resistencia)) +
  stat_sum(aes(size = ..n.., group = 1)) +
  scale_size_area(max_size=10)


ggplot(dados, aes(x = Variedade, y = Plantas_mortas)) +
  stat_sum(aes(size = ..n.., group = 1)) +
  scale_size_area(max_size=10)

ggplot(dados, aes(x = Local, y = Plantas_mortas)) +
  stat_sum(aes(size = ..n.., group = 1)) +
  scale_size_area(max_size=10)

ggplot(dados, aes(x = Ano, y = Plantas_mortas)) +
  stat_sum(aes(size = ..n.., group = 1)) +
  scale_size_area(max_size=10)


ggplot(dados, aes(x = Variedade, y = Severidade)) +
  stat_sum(aes(size = ..n.., group = 1)) +
  scale_size_area(max_size=10)

ggplot(dados, aes(x = Local, y = Severidade)) +
  stat_sum(aes(size = ..n.., group = 1)) +
  scale_size_area(max_size=10)

ggplot(dados, aes(x = Ano, y = Severidade)) +
  stat_sum(aes(size = ..n.., group = 1)) +
  scale_size_area(max_size=10)

## Testando modelos para Resistência
m1<-lm(Resistencia~Variedade+ Local + Ano, dados)
m2<-glm(formula= Resistencia~Variedade+ Local + Ano, data=dados, family=binomial)
m3<-glm(formula= Resistencia~Variedade+ Local + Ano, data=dados, family=poisson)
m4<-glm(formula= Resistencia~Variedade+ Local + Ano, data=dados, family=quasipoisson)
m5<-zeroinfl(Resistencia~Variedade + Local + Ano, data = dados)
m6<-lmer(Resistencia~Variedade + Local + (1 | Ano), data = dados)
m7<-glmer(Resistencia~Variedade + Local + (1 | Ano), data = dados, family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
m8<-glmer(Resistencia~Variedade + Local + (1 | Ano), data = dados, family = poisson, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
m9<-glmmTMB(Resistencia~(Variedade+Local)+(1|Ano), data=dados, ziformula=~1, family=poisson)

## Testando modelos para Plantas_mortas
m1<-lm(Plantas_mortas~Variedade+ Local + Ano, dados)
m2<-glm(formula= Plantas_mortas~Variedade+ Local + Ano, data=dados, family=binomial)
m3<-glm(formula= Plantas_mortas~Variedade+ Local + Ano, data=dados, family=poisson)
m4<-glm(formula= Plantas_mortas~Variedade+ Local + Ano, data=dados, family=quasipoisson)
m5<-zeroinfl(Plantas_mortas~Variedade + Local + Ano, data = dados)
m6<-lmer(Plantas_mortas~Variedade + Local + (1 | Ano), data = dados)
m7<-glmer(Plantas_mortas~Variedade + Local + (1 | Ano), data = dados, family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
m8<-glmer(Plantas_mortas~Variedade + Local + (1 | Ano), data = dados, family = poisson, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
m9<-glmmTMB(Plantas_mortas~(Variedade+Local)+(1|Ano), data=dados, ziformula=~1, family=poisson)

## Testando modelos para Severidade
m1<-lm(Severidade~Variedade+ Local + Ano, dados)
m2<-glm(formula= Severidade~Variedade+ Local + Ano, data=dados, family=binomial)
m3<-glm(formula= Severidade~Variedade+ Local + Ano, data=dados, family=poisson)
m4<-glm(formula= Severidade~Variedade+ Local + Ano, data=dados, family=quasipoisson)
m5<-zeroinfl(Severidade~Variedade + Local + Ano, data = dados)
m6<-lmer(Severidade~Variedade + Local + (1 | Ano), data = dados)
m7<-glmer(Severidade~Variedade + Local + (1 | Ano), data = dados, family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
m8<-glmer(Severidade~Variedade + Local + (1 | Ano), data = dados, family = poisson, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
m9<-glmmTMB(Severidade~(Variedade+Local)+(1|Ano), data=dados, ziformula=~1, family=poisson)
