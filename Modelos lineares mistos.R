## Curso Modelos de regressão no software R - Módulo 1 ##

## Modelos mistos lineares ##

## Leitura dos dados
dados<-read.table("C:/Users/Mateus/Desktop/Curso de regressão no R/dados.csv",h=T,sep=",")

## Rodando modelos mistos utilizando o pacote lme4
library(lme4)
dados$Trat<-as.factor(dados$Trat)
dados$Rep<-as.factor(dados$Rep)
modelo = lmer(PROD ~ Trat + (1 | Rep), data = dados)

summary.modelo <- summary(modelo)
summary.modelo

## Calculando o intervalo de confiança
confint(modelo)

## Estimativas dos efeitos aleatórios
ranef(modelo)$Rep 
coef(modelo)$Rep 

library(merTools)

predictInterval(modelo)   # for various model predictions, possibly with new data

REsim(modelo)             # mean, median and sd of the random effect estimates

plotREsim(REsim(modelo))  # plot the interval estimates

## Fazendo predições
predict(modelo, re.form=NA)
predict(modelo)

##Rodando ANOVA para os efeitos fixos
anova.df <- data.frame(stats::anova(modelo))
anova.df <- cbind(Effect = rownames(anova.df),anova.df)

# Calculando o Sigma
anova.df$Sigma <- stats::sigma(modelo)

# Renomeando a coluna do p-value
colnames(anova.df)[which(colnames(anova.df) == "Pr..F.")] <- "P.value"

# Criando um ranking de significãncia
anova.df$Sig <- ifelse(anova.df$P.value <= 0.1 & anova.df$P.value >= 0.05,
                       ".","")
anova.df$Sig <- ifelse(anova.df$P.value < 0.05 & anova.df$P.value >= 0.01,
                       "*",anova.df$Sig)
anova.df$Sig <- ifelse(anova.df$P.value < 0.01 & anova.df$P.value >= 0.001,
                       "**",anova.df$Sig)
anova.df$Sig <- ifelse(anova.df$P.value < 0.001,"***",anova.df$Sig)

##Estimando o coeficiente de variação CV
anova.df$CV <- (anova.df$Sigma/mean(dados$PROD))*100

##Calculando os contrastes entre médias dos efeitos fixos
delta.df<-data.frame(summary(pairs(emmeans::lsmeans(modelo,
                                                    as.formula(~ Trat),
                                                    adjust = "none",
                                                    lmer.df = "kenward-roger"))))
# Renomeando as colunas
colnames(delta.df)[which(colnames(delta.df) == "df")] <- "DF"
colnames(delta.df)[which(colnames(delta.df) == "contrast")] <- "Contrast"
colnames(delta.df)[which(colnames(delta.df) == "estimate")] <- "Estimate"
colnames(delta.df)[which(colnames(delta.df) == "p.value")] <- "P.value"

# Calculando o intervalo de confiança
delta.df$Lower.95.CI <- delta.df$Estimate - qt(p = 1-0.05/2, df = delta.df$DF)*delta.df$SE
delta.df$Upper.95.CI <- delta.df$Estimate + qt(p = 1-0.05/2, df = delta.df$DF)*delta.df$SE
delta.df$Lower.90.CI <- delta.df$Estimate - qt(p = 1-0.10/2, df = delta.df$DF)*delta.df$SE
delta.df$Upper.90.CI <- delta.df$Estimate + qt(p = 1-0.10/2, df = delta.df$DF)*delta.df$SE

##Calculando lsmeans para o efeito fixo
lsmean.df<-data.frame(emmeans::CLD(emmeans::lsmeans(modelo,
                                                    as.formula(~ Trat),
                                                    adjust = "none",
                                                    lmer.df = "kenward-roger"),
                                   reversed = T))

# Renaming some columns
colnames(lsmean.df)[which(colnames(lsmean.df) == "df")] <- "DF"
colnames(lsmean.df)[which(colnames(lsmean.df) == "lsmean")] <- "Estimate"
colnames(lsmean.df)[which(colnames(lsmean.df) == "lower.CL")] <- "Lower.95.CI"
colnames(lsmean.df)[which(colnames(lsmean.df) == "upper.CL")] <- "Upper.95.CI"
colnames(lsmean.df)[which(colnames(lsmean.df) == ".group")] <- "Letter.Group"
lsmean.df$Lower.90.CI <- lsmean.df$Estimate - qt(p = 1-0.10/2, df = lsmean.df$DF)*lsmean.df$SE
lsmean.df$Upper.90.CI <- lsmean.df$Estimate + qt(p = 1-0.10/2, df = lsmean.df$DF)*lsmean.df$SE

# Adicioanndo o teste t
lsmean.df$T.value = lsmean.df$Estimate/lsmean.df$SE
# Adicionando o p value para o teste t
lsmean.df$P.value <- pt(abs(lsmean.df$T.value),df = lsmean.df$DF, lower.tail = F)*2

# Trocando os números da coluna Letter.group por letras
lsmean.df$Letter.Group<-as.character(lsmean.df$Letter.Group)
lsmean.df$Letter.Group<-gsub(" ","", lsmean.df$Letter.Group, fixed = T)
lsmean.df <- within(lsmean.df, Letter.Group[Letter.Group == "1"] <- 'a')
lsmean.df <- within(lsmean.df, Letter.Group[Letter.Group == "2"] <- 'b')
lsmean.df <- within(lsmean.df, Letter.Group[Letter.Group == "3"] <- 'c')
lsmean.df <- within(lsmean.df, Letter.Group[Letter.Group == "4"] <- 'd')
lsmean.df <- within(lsmean.df, Letter.Group[Letter.Group == "5"] <- 'e')
lsmean.df <- within(lsmean.df, Letter.Group[Letter.Group == "6"] <- 'f')
lsmean.df <- within(lsmean.df, Letter.Group[Letter.Group == "7"] <- 'g')
lsmean.df <- within(lsmean.df, Letter.Group[Letter.Group == "8"] <- 'h')
lsmean.df <- within(lsmean.df, Letter.Group[Letter.Group == "9"] <- 'i')
lsmean.df <- within(lsmean.df, Letter.Group[Letter.Group == "12"] <- 'ab')
lsmean.df <- within(lsmean.df, Letter.Group[Letter.Group == "123"] <- 'abc')
lsmean.df <- within(lsmean.df, Letter.Group[Letter.Group == "1234"] <- 'abcd')
lsmean.df <- within(lsmean.df, Letter.Group[Letter.Group == "12345"] <- 'abcde')
lsmean.df <- within(lsmean.df, Letter.Group[Letter.Group == "123456"] <- 'abcdef')
lsmean.df <- within(lsmean.df, Letter.Group[Letter.Group == "1234567"] <- 'abcdefg')
lsmean.df <- within(lsmean.df, Letter.Group[Letter.Group == "23"] <- 'bc')
lsmean.df <- within(lsmean.df, Letter.Group[Letter.Group == "234"] <- 'bcd')
lsmean.df <- within(lsmean.df, Letter.Group[Letter.Group == "2345"] <- 'bcde')
lsmean.df <- within(lsmean.df, Letter.Group[Letter.Group == "23456"] <- 'bcdef')
lsmean.df <- within(lsmean.df, Letter.Group[Letter.Group == "234567"] <- 'bcdefg')
lsmean.df <- within(lsmean.df, Letter.Group[Letter.Group == "34"] <- 'cd')
lsmean.df <- within(lsmean.df, Letter.Group[Letter.Group == "345"] <- 'cde')
lsmean.df <- within(lsmean.df, Letter.Group[Letter.Group == "3456"] <- 'cdef')
lsmean.df <- within(lsmean.df, Letter.Group[Letter.Group == "34567"] <- 'cdefg')
lsmean.df <- within(lsmean.df, Letter.Group[Letter.Group == "45"] <- 'de')
lsmean.df <- within(lsmean.df, Letter.Group[Letter.Group == "456"] <- 'def')
lsmean.df <- within(lsmean.df, Letter.Group[Letter.Group == "4567"] <- 'defg')
lsmean.df <- within(lsmean.df, Letter.Group[Letter.Group == "56"] <- 'ef')
lsmean.df <- within(lsmean.df, Letter.Group[Letter.Group == "567"] <- 'efg')
lsmean.df <- within(lsmean.df, Letter.Group[Letter.Group == "67"] <- 'fg')
lsmean.df <- within(lsmean.df, Letter.Group[Letter.Group == "678"] <- 'fgh')
lsmean.df <- within(lsmean.df, Letter.Group[Letter.Group == "78"] <- 'gh')

## Exercícios
# Crie modelos lineares mistos com tratamento como efeito fixo e as demais variáveis como efeito aleatório
# Depois verifique qual modelo foi melhor
