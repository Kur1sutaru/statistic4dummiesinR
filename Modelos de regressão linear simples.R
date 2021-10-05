## Curso Modelos de regressão no software R - Módulo 1 ##
## Modelo de Regressão linear simples e múltipla, e seleção de modelos ##

## Modelo de regressão linear simples

## Leitura dos dados
dados<-read.table("C:/Users/Mateus/Desktop/Curso de regressão no R/dados.csv",h=T,sep=",")

## modelo y = B0 + B1X + e
#y - variável resposta
#B0 - coeficiente linear da reta
#B1 - coeficiente angular da reta
#X - variável independente
#e - erro experimental

##Produtividade
##Modelo de regressão no R
reglin<-lm(PROD~Trat, dados)

anova(reglin)

summary(reglin)

##Predição via modelo de regressão
predict(reglin)

##Plotando a regressão
plot(dados$Trat, dados$PROD) #diagrama de dispersão
abline(reglin) #reta da regressão ajustada

##Rodando com as médias dos tratamentos
dados1<-aggregate(dados[, 3:ncol(dados)], list(Trat=dados$Trat), mean)

##Modelo de regressão no R
reglin<-lm(PROD~Trat, dados1)

anova(reglin)

summary(reglin)

##Predição via modelo de regressão
predict(reglin)

##Plotando a regressão
plot(dados1$Trat, dados1$PROD) #diagrama de dispersão
abline(reglin) #reta da regressão ajustada

# quanto mais variaveis o coef de determinação aumenta Multiple R squared
# não usar essa métrica usar o coef deter ajustado adjusted r-squared
# adjusted r-squared pondera o coef de acordo com o num de variaveis
# as vezes inserir mais um novo parametro nao ajuda a explicar o modelo
## seleção de modelos logo a seguir

##Regressão múltipla
dados2<-data.frame(dados, TRAT2=dados$Trat^2, TRAT3=dados$Trat^3, TRATr=sqrt(dados$Trat), TRATl=log(dados$Trat), TRATi=1/dados$Trat, TRATi2=1/dados$Trat^2)

# ajuste do modelo quadrático
m1 <- lm(PROD~Trat+TRAT2, data=dados2) # ou lm(PROD~Trat+I(TRAT^2), data=dados2)
summary(m1)
layout(matrix(c(1,1,2,3,4,5),2,3))
plot(PROD~Trat, dados2)
lines(fitted(m1)~Trat, dados2)
plot(m1)

# ajuste do modelo cúbico
m2 <- lm(PROD~Trat+TRAT2+TRAT3, data=dados2) # ou lm(PROD~Trat+I(TRAT^2)+I(TRAT^3), data=dados2)
summary(m2)
layout(matrix(c(1,1,2,3,4,5),2,3))
plot(PROD~Trat, dados2)
lines(fitted(m2)~Trat, dados2)
plot(m2)

# variavel independente + 1/variavel
# ajuste do modelo recíproco
m3 <- lm(PROD~Trat+TRATi, data=dados2) 
summary(m3)
layout(matrix(c(1,1,2,3,4,5),2,3))
plot(PROD~Trat, dados2)
lines(fitted(m3)~Trat, dados2)
plot(m3)

# ajuste do modelo quadrado do recíproco
m4 <- lm(PROD~Trat+TRATi2, data=dados2) 
summary(m4)
layout(matrix(c(1,1,2,3,4,5),2,3))
plot(PROD~Trat, dados2)
lines(fitted(m4)~Trat, dados2)
plot(m4)

# ajuste do modelo da raiz quadrada
m5 <- lm(PROD~Trat+TRATr, data=dados2) 
summary(m5)
layout(matrix(c(1,1,2,3,4,5),2,3))
plot(PROD~Trat, dados2)
lines(fitted(m5)~Trat, dados2)
plot(m5)

# ajuste do modelo logaritimico
m6 <- lm(PROD~Trat+TRATl, data=dados2) 
summary(m6)
layout(matrix(c(1,1,2,3,4,5),2,3))
plot(PROD~Trat, dados2)
lines(fitted(m6)~Trat, dados2)
plot(m6)

# entender o coef de determinação ajustado
# Adjusted R-squared

##Seleção de modelos
# ajuste do modelo global (todas as variáveis)
m7 <- lm(PROD~Trat+TRAT2+TRAT3+TRATr+TRATl+TRATi+TRATi2, data=dados2)
summary(m7)
layout(matrix(c(1,1,2,3,4,5),2,3))
plot(PROD~Trat, dados2)
lines(fitted(m7)~Trat, dados2)
plot(m7)

# explicação dos gráficos
# https://data.library.virginia.edu/diagnostic-plots/

# Seleção de modelos/variáveis
step(m7, direction="both")
step(m7, direction="both", k=log(nrow(dados2)))

# o melhor modelo é a ultima linha
# Coefficients:
# (Intercept)         Trat  
#        77.5          8.5  

# O modelo linear foi escolhido como o melhor

# MODELO COM TODAS AS VARIÁVEIS
lmMod <- lm(PROD ~ . , data = dados2)
selectedMod <- step(lmMod)
summary(selectedMod)


# Testeando multicolinearidade
# car::vif salvam os resultados acima de 1000 nao se admite

all_vifs <- car::vif(selectedMod)
print(all_vifs)

signif_all <- names(all_vifs)

while(any(all_vifs > 100)){
  var_with_max_vif <- names(which(all_vifs == max(all_vifs)))  # get the var with max vif
  signif_all <- signif_all[!(signif_all) %in% var_with_max_vif]  # remove
  myForm <- as.formula(paste("PROD ~ ", paste (signif_all, collapse=" + "), sep=""))  # new formula
  selectedMod1 <- lm(myForm, data=dados2)  # re-build model with new formula
  all_vifs <- car::vif(selectedMod1)
}
summary(selectedMod1)

car::vif(selectedMod1)

# o script acima exclui do nosso modelo todos os que possuem multicolinearidade
# maior que 100 - foi setado ali while(any(all_vifs > 100)){

# Selecionando variáveis
all_vars <- names(selectedMod1[[1]])[-1]  # names of all X variables
# Get the non-significant vars
summ <- summary(selectedMod1)  # model summary
pvals <- summ[[4]][, 4]  # get all p values
not_significant <- character()  # init variables that aren't statsitically significant
not_significant <- names(which(pvals > 0.1))
not_significant <- not_significant[!not_significant %in% "(Intercept)"]  # remove 'intercept'. Optional!

# If there are any non-significant variables, 
while(length(not_significant) > 0){
  all_vars <- all_vars[!all_vars %in% not_significant[1]]
  myForm <- as.formula(paste("PROD ~ ", paste (all_vars, collapse=" + "), sep=""))  # new formula
  selectedMod2 <- lm(myForm, data=dados2)  # re-build model with new formula
  
  # Get the non-significant vars.
  summ <- summary(selectedMod2)
  pvals <- summ[[4]][, 4]
  not_significant <- character()
  not_significant <- names(which(pvals > 0.1))
  not_significant <- not_significant[!not_significant %in% "(Intercept)"]
}
summary(selectedMod2)

# Verificando melhores sub conjuntos
library(leaps)
dados3<-dados2[,c(1,4:10)]
regsubsetsObj <- regsubsets(x=dados3 ,y=dados2$PROD, nbest = 2, really.big = T)
layout(1)
plot(regsubsetsObj, scale = "adjr2")  # regsubsets plot based on R-sq

leapSet <- leaps(x=dados3 ,y=dados2$PROD, nbest = 1, method = "adjr2") 

selectVarsIndex <- leapSet$which[4, ]  # pick selected vars
newData <- cbind(PROD=dados2$PROD, dados3[, selectVarsIndex])  # new data for building selected model
selectedMod3 <- lm(PROD ~ ., data=newData)  # build model
summary(selectedMod3)

library(FactoMineR)
regMod <- RegBest(y=dados2$PROD, x = dados3)
regMod$all  # summary of best model of all sizes based on Adj A-sq
regMod$best  # best model

library(subselect)
results <- anneal(cor(dados3), kmin=1, kmax=ncol(predictors_df)-1, nsol=4, niter=10, setseed=TRUE)  # perform annealing<
print(results$bestsets)

num_vars <- 3
selectVarsIndex <- results$bestsets[num_vars, 1:num_vars]
newData <- cbind(PROD=dados2$PROD, dados3[, selectVarsIndex])  # new data for building selected model
selectedMod4 <- lm(PROD ~ ., data=newData)  # build model
summary(selectedMod4)

# Comparando modelos usando ANOVA
# ANOVA
anova(m7, selectedMod1, selectedMod2, selectedMod3, selectedMod4)

## Exercício sobre regressão linear simples e múltipla, e seleção de modelos.
## Faça as análises de regressão simples e múltipla, e seleção de modelos para todas as variáveis independentes.
