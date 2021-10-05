## Curso Modelos de regressão no software R - Módulo 1 ##

## Resolução dos exercícios de Modelo de Regressão linear simples e múltipla, e seleção de modelos ##

## Leitura dos dados
dados<-read.table("C:/Users/hp/OneDrive - Experimental Analytics Corporation/Short course/Modelos de regressão no software R - Módulo 1/dados.csv",h=T,sep=",")

resultados<-list()
for(i in 3:ncol(dados))
{
  Resp_var<-dados[,i]
  variável_independente<-dados[,-i]
  
  ## Rodando modelo linear usando lm
  m1 <- lm(Resp_var~., data=variável_independente)
  summary(m1)
  
  # Seleção de modelos/variáveis
  step(m1, direction="both")
  
  modelo_selecionado1 <- step(m1)
  summary(modelo_selecionado1)
  
  # Testando multicolinearidade
  all_vifs <- car::vif(modelo_selecionado1)
  print(all_vifs)
  
  signif_all <- names(all_vifs)
  
  while(any(all_vifs > 4)){
    var_with_max_vif <- names(which(all_vifs == max(all_vifs)))  # get the var with max vif
    signif_all <- signif_all[!(signif_all) %in% var_with_max_vif]  # remove
    myForm <- as.formula(paste("Resp_var ~ ", paste (signif_all, collapse=" + "), sep=""))  # new formula
    modelo_selecionado2 <- lm(myForm, data=dados)  # re-build model with new formula
    all_vifs <- car::vif(modelo_selecionado2)
  }
  summary(modelo_selecionado2)
  
  car::vif(modelo_selecionado2)
  
  # Selecionando variáveis
  all_vars <- names(modelo_selecionado2[[1]])[-1]  # names of all X variables
  # Get the non-significant vars
  summ <- summary(modelo_selecionado2)  # model summary
  pvals <- summ[[4]][, 4]  # get all p values
  not_significant <- character()  # init variables that aren't statsitically significant
  not_significant <- names(which(pvals > 0.1))
  not_significant <- not_significant[!not_significant %in% "(Intercept)"]  # remove 'intercept'. Optional!
  
  tryCatch({
    # If there are any non-significant variables, 
    while(length(not_significant) > 0){
      all_vars <- all_vars[!all_vars %in% not_significant[1]]
      myForm <- as.formula(paste("Resp_var ~ ", paste (all_vars, collapse=" + "), sep=""))  # new formula
      modelo_selecionado3 <- lm(myForm, data=dados)  # re-build model with new formula
      
      # Get the non-significant vars.
      summ <- summary(modelo_selecionado3)
      pvals <- summ[[4]][, 4]
      not_significant <- character()
      not_significant <- names(which(pvals > 0.1))
      not_significant <- not_significant[!not_significant %in% "(Intercept)"]
    }
    summary(modelo_selecionado3)
  }, error = function(error_condition) {
    modelo_selecionado3<-NA
  })
  
  # Verificando melhores sub conjuntos
  library(FactoMineR)
  tryCatch({
    regMod <- RegBest(y=Resp_var, x = dados[,-i])
    regMod$all  # summary of best model of all sizes based on Adj A-sq
    modelo_selecionado4<-regMod$best  # best model
  }, error = function(error_condition) {
    modelo_selecionado4<-NA
  })
  
  library(subselect)
  tryCatch({
    results <- anneal(cor(dados[,-i]), kmin=1, kmax=ncol(dados[,-i])-1, nsol=4, niter=10, setseed=TRUE)  # perform annealing<
    print(results$bestsets)
    
    num_vars <- 3
    selectVarsIndex <- results$bestsets[num_vars, 1:num_vars]
    newData <- cbind(Resp_var=dados[,i], dados[, selectVarsIndex])  # new data for building selected model
    modelo_selecionado5 <- lm(Resp_var ~ ., data=newData)  # build model
    summary(modelo_selecionado5)
  }, error = function(error_condition) {
    modelo_selecionado5<-NA
  })
  
  # Comparando modelos usando ANOVA
  # ANOVA
  anova(modelo_selecionado1, modelo_selecionado2, modelo_selecionado3, modelo_selecionado4, modelo_selecionado5)
  
  resultados[[i]]<-anova(modelo_selecionado1, modelo_selecionado2, modelo_selecionado3, modelo_selecionado4, modelo_selecionado5)
  
}

capture.output(resultados, file = "C:/Users/hp/OneDrive - Experimental Analytics Corporation/Short course/Modelos de regressão no software R - Módulo 1/Resultados.txt")
