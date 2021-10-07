## Curso Modelos de regressão no software R - Módulo 2 ##

## Modelos lineares generalizados ##

## Distribuição de zero-inflated Poisson regression
## Em alguns casos nós trabalhamos com valores de contagem que contêm uma quantidade muito grande de zeros
## Neste caso não é recomendado o uso da distribuição de Poisson ou quasiPoisson
## Recomenda-se o uso da distribuição zero-inflated Poisson

require(ggplot2)
require(pscl)
require(boot)

## Leitura dos dados
dados<-read.table("caminhododiretorio/Modelos de regressão no software R - Módulo 2/dados_contagem_com_zeros.csv",h=T,sep=",")

## Vizualização dos dados
## Histograma onde no eixo x temos uma escala de log10 
ggplot(dados, aes(Plantas_infectadas)) + geom_histogram() + scale_x_log10()

## Rodando o modelo
summary(m1 <- zeroinfl(Plantas_infectadas ~ Tratamento, data = dados))

## Comparando com o modelo nulo
mnull <- update(m1, . ~ 1)

pchisq(2 * (logLik(m1) - logLik(mnull)), df = 1, lower.tail = FALSE)

## Rodando o modelo com distribuição de Poisson
summary(p1 <- glm(Plantas_infectadas ~ Tratamento, family = poisson, data = dados))

## Comparando os modelos
vuong(p1, m1)

# Desmarais Bruce A., Harden Jeffrey J., 2013, "Testing for Zero Inflation in Count Models: Bias Correction for the Vuong Test" Stata Journal, 13, 4, 810-835
# Wilson P., 2015, "The misuse of the Vuong test for non-nested models to test for zero-inflation" Economics Letters, 127, 51-53
