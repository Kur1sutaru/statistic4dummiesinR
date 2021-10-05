## Curso Modelos de regressão no software R - Módulo 1 ##

## Resolução dos exercícios de Modelos mistos lineares ##

## Leitura dos dados
dados<-read.table("C:/Users/hp/OneDrive - Experimental Analytics Corporation/Short course/Modelos de regressão no software R - Módulo 1/dados.csv",h=T,sep=",")

library(lme4)
library(merTools)

resultado<-list()
for(i in colnames(dados[-c(1,2)]))
{
  ## Rodando modelos mistos utilizando o pacote lme4
  resp_var<-dados[,i]
  indep_var<-dados[,!colnames(dados)%in%i]
  dados1<- data.frame(lapply(dados[colnames(indep_var)], factor),resp_var=dados[,i])
  
  modelo1 = lmer(as.numeric(resp_var) ~ Trat + (1|Rep) + (1|dados1[,3])
                + (1|dados1[,4]) + (1|dados1[,5]) + (1|dados1[,6])
                + (1|dados1[,7]) + (1|dados1[,8]) + (1|dados1[,9]), data = dados1)
  
  summary.modelo <- summary(modelo1)
  summary.modelo
  
  modelo2 = lmer(as.numeric(resp_var) ~ Trat + (1|Rep) + (1|dados1[,3])
                 + (1|dados1[,4]) + (1|dados1[,5]) + (1|dados1[,6])
                 + (1|dados1[,7]) + (1|dados1[,8]), data = dados1)
  
  modelo3 = lmer(as.numeric(resp_var) ~ Trat + (1|Rep) + (1|dados1[,3])
                 + (1|dados1[,4]) + (1|dados1[,5]) + (1|dados1[,6])
                 + (1|dados1[,7]), data = dados1)
  
  modelo4 = lmer(as.numeric(resp_var) ~ Trat + (1|Rep) + (1|dados1[,3])
                 + (1|dados1[,4]) + (1|dados1[,5]) + (1|dados1[,6]), data = dados1)
  
  modelo5 = lmer(as.numeric(resp_var) ~ Trat + (1|Rep) + (1|dados1[,3])
                 + (1|dados1[,4]) + (1|dados1[,5]), data = dados1)
  
  modelo6 = lmer(as.numeric(resp_var) ~ Trat + (1|Rep) + (1|dados1[,3])
                 + (1|dados1[,4]), data = dados1)
  
  modelo7 = lmer(as.numeric(resp_var) ~ Trat + (1|Rep) + (1|dados1[,3]), data = dados1)
  
  modelo8 = lmer(as.numeric(resp_var) ~ Trat + (1|Rep), data = dados1)
  
  modelo_final<-list(modelo1=modelo1,modelo2=modelo2,modelo3=modelo3,
                     modelo4=modelo4,modelo5=modelo5,modelo6=modelo6,
                     modelo7=modelo7,modelo8=modelo8)
  
  # Seleção de modelos/variáveis
  bic<-data.frame(BIC(modelo1,modelo2,modelo3,modelo4,modelo5,modelo6,modelo7,modelo8))
  aic<-AIC(modelo1,modelo2,modelo3,modelo4,modelo5,modelo6,modelo7,modelo8)
  
  modelo_selecionado<-rownames(bic[bic$BIC==min(bic$BIC),])
  
  modelo<-modelo_final[[modelo_selecionado]]
  ## Calculando o intervalo de confiança
  tryCatch({
    intervalo_confianca<-confint(modelo)
  }, error = function(error_condition) {
    intervalo_confianca<-NA
  })
  
  ## Estimativas dos efeitos aleatórios
  efeito_aleatorio<-ranef(modelo) 
  coeficiente_efeito_aleatorio<-coef(modelo) 
  
  predicao_intervalo_confianca<-predictInterval(modelo)   # for various model predictions, possibly with new data
  
  metricas_efeito_aleatorio<-REsim(modelo)             # mean, median and sd of the random effect estimates
  
  plotREsim(REsim(modelo))  # plot the interval estimates
  
  ## Fazendo predições
  predicao_sem_efeito_aleatorio<-predict(modelo, re.form=NA)
  predicao_com_efeito_aleatorio<-predict(modelo)
  
  ##Rodando ANOVA para os efeitos fixos
  anova.df <- data.frame(stats::anova(modelo))
  anova.df <- cbind(Effect = rownames(anova.df),anova.df)
  
  # Calculando o Sigma
  anova.df$Sigma <- stats::sigma(modelo)
  
  # Renomeando a coluna do p-value
  colnames(anova.df)[which(colnames(anova.df) == "Pr..F.")] <- "P.value"
  
  ##Estimando o coeficiente de variação CV
  anova.df$CV <- (anova.df$Sigma/mean(dados$PROD))*100
  
  tryCatch({
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
  }, error = function(error_condition) {
    delta.df<-NA
  })
  
  tryCatch({
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
  }, error = function(error_condition) {
    lsmean.df<-NA
  })
  
  resultado[[i]]<-list(Variável=i,BIC=bic,AIC=aic,Intervalo_de_confiança=intervalo_confianca,
                       Efeito_aleatório=efeito_aleatorio,Coeficiente_dos_efeitos_aleatórios=coeficiente_efeito_aleatorio,
                       Predição_intervalo_de_confiança=predicao_intervalo_confianca,Métricas_efeitos_aleatórios=metricas_efeito_aleatorio,
                       Predição_sem_consider_efeitos_aleatórios=predicao_sem_efeito_aleatorio,Predição_considerando_efeitos_aleatórios=predicao_com_efeito_aleatorio,
                       ANOVA=anova.df,Contraste=delta.df,Lsmean=lsmean.df)
}

capture.output(resultado, file = "C:/Users/hp/OneDrive - Experimental Analytics Corporation/Short course/Modelos de regressão no software R - Módulo 1/Resultado_modelos_mistos.txt")
