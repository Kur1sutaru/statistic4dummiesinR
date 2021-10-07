## Curso Modelos de regressão no software R - Módulo 2 ##

## Modelos mistos lineares generalizados ##

## A regressão logistíca é utilizada quando nós temos dados binários (0 e 1)
## Modelos mistos deve ser utilizado quando nós temos efeito fixo e aleatório

require(ggplot2)
require(GGally)
require(reshape2)
require(lme4)
require(compiler)
require(parallel)
require(boot)
require(lattice)
require(dplyr)

## Leitura dos dados
dados<-read.table("caminhododiretorio/tuberculose-dados-all.csv",h=T,sep=";")

#Renomear GTA
colnames(dados)[1]<-("GTA")
colnames(dados)[13]<-("tuberculosis_cat")

# Remover as GTAS repetidas
dados<-dados[!duplicated(dados$GTA),]

# Rodar o modelo de regressão logistica tuberculosis_cat ~ TOTAL.ABATIDOS
# Transformar em fatores

to.factors<-function(dados, variables){
  for (variable in variables){
    dados[[variable]] <- as.factor(dados[[variable]]) 
  } 
  return(dados) }
nomes_col<-(colnames(dados[,-c(9:12,19)]))
dados<-to.factors(dados,nomes_col)


modelo <- glm(tuberculosis_cat ~ TOTAL.ABATIDOS, data = dados, family = binomial)

print(modelo, corr = TRUE)

summary(modelo)
predict(modelo, type="response")

predito<-modelo %>%predict(dados, type="response")

# Calculando o intervalo de confiança
dados <- sqrt(diag(vcov(modelo)))
dados
# table of estimates with 95% CI
(tab <- cbind(Est = fixef(modelo), LL = fixef(modelo) - 1.96 * dados, UL = fixef(modelo) + 1.96 *
                dados))



# Fazendo bootstrap 
sampler <- function(dat, clustervar, replace = TRUE, reps = 1) {
  cid <- unique(dat[, clustervar[1]])
  ncid <- length(cid)
  recid <- sample(cid, size = ncid * reps, replace = TRUE)
  if (replace) {
    rid <- lapply(seq_along(recid), function(i) {
      cbind(NewID = i, RowID = sample(which(dat[, clustervar] == recid[i]),
                                      size = length(which(dat[, clustervar] == recid[i])), replace = TRUE))
    })
  } else {
    rid <- lapply(seq_along(recid), function(i) {
      cbind(NewID = i, RowID = which(dat[, clustervar] == recid[i]))
    })
  }
  dat <- as.data.frame(do.call(rbind, rid))
  dat$Replicate <- factor(cut(dat$NewID, breaks = c(1, ncid * 1:reps), include.lowest = TRUE,
                              labels = FALSE))
  dat$NewID <- factor(dat$NewID)
  return(dat)
}

set.seed(20)
tmp <- sampler(dados, "Sexo", reps = 1000)
bigdata <- cbind(tmp, dados[tmp$RowID, ])

f <- fixef(m)
r <- getME(m, "theta")

cl <- makeCluster(4)
clusterExport(cl, c("bigdata", "f", "r"))
clusterEvalQ(cl, require(lme4))

myboot <- function(i) {
  object <- try(glmer(Smartphone ~ Sexo + Ano + (1 | NewID), data = bigdata, subset = Replicate == i, family = binomial,
                      nAGQ = 1, start = list(fixef = f, theta = r)), silent = TRUE)
  if (class(object) == "try-error")
    return(object)
  c(fixef(object), getME(object, "theta"))
}

start <- proc.time()
res <- parLapplyLB(cl, X = levels(bigdata$Replicate), fun = myboot)
end <- proc.time()

# Calcula a proporção de modelos que convergiram
success <- sapply(res, is.numeric)
mean(success)

# Combinar os resultados que convergiram
bigres <- do.call(cbind, res[success])

# Calcular 2.5th e 97.5th percentil para 95% CI
(ci <- t(apply(bigres, 1, quantile, probs = c(0.025, 0.975))))

# Todos os resultados
finaltable <- cbind(Est = c(f, r), SE = c(se, NA), BootMean = rowMeans(bigres),
                    ci)
# round and print
round(finaltable, 3)

# Fazendo predições
str(p0 <- predict(m))            # fitted values
str(p1 <- predict(m,re.form=NA))  # fitted values, unconditional (level-0)
newdata <- with(dados, expand.grid(Ano=unique(Ano), Sexo=unique(Sexo), Idade=unique(Idade)))
str(p2 <- predict(m,newdata))    # new data, all RE
str(p3 <- predict(m,newdata,re.form=NA)) # new data, level-0
str(p4 <- predict(m,newdata,re.form= ~(1|Idade))) # explicitly specify RE
stopifnot(identical(p2, p4))


### QTD_PARTE_PARTADA
modelo2 <- glm(tuberculosis_cat ~ QTD_PARTE_PARTADA, data = dados, family = binomial)

print(modelo2, corr = TRUE)

summary(modelo2)
predict(modelo2, type="response")

predito<-modelo2 %>%predict(dados, type="response")

# Calculando o intervalo de confiança
dados <- sqrt(diag(vcov(modelo2)))
dados
# table of estimates with 95% CI
(tab <- cbind(Est = fixef(modelo2), LL = fixef(modelo2) - 1.96 * dados, UL = fixef(modelo2) + 1.96 *
                dados))

