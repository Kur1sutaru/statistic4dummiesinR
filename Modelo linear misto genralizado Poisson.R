## Curso Modelos de regressão no software R - Módulo 2 ##

## Modelos mistos lineares generalizados ##

## A distribuição de Poisson é utilizada quando nós temos dados de contagem
## Modelos mistos deve ser utilizado quando nós temos efeito fixo e aleatório

require(ggplot2)
require(GGally)
require(lme4)
require(parallel)

## Leitura dos dados
dados<-read.table("C:/Users/hp/OneDrive - Experimental Analytics Corporation/Short course/Modelos de regressão no software R - Módulo 2/dados_contagem_modelos_mistos.csv",h=T,sep=",")

## Transformando os efeitos em fatores
dados <- within(dados, {
  Variedade <- factor(Variedade)
  Local <- factor(Local)
  Ano <- factor(Ano)
  Rep <- factor(Rep)
})

## Análise gráfica
ggpairs(dados[, c("Variedade", "Local", "N_lagartas")])

ggplot(dados, aes(x = Variedade, y = N_lagartas)) +
  stat_sum(aes(size = ..n.., group = 1)) +
  scale_size_area(max_size=10)

ggplot(dados, aes(x = Local, y = N_lagartas)) +
  stat_sum(aes(size = ..n.., group = 1)) +
  scale_size_area(max_size=10)

ggplot(dados, aes(x = Ano, y = N_lagartas)) +
  stat_sum(aes(size = ..n.., group = 1)) +
  scale_size_area(max_size=10)

# Rodar o modelo de regressão generalizada com distribuição de Poisson
m <- glmer(N_lagartas ~ Variedade + Local + (1 | Ano), data = dados, family = poisson, control = glmerControl(optimizer = "bobyqa"),
           nAGQ = 10)

print(m, corr = FALSE)

# Calculando o intervalo de confiança
se <- sqrt(diag(vcov(m)))
# table of estimates with 95% CI
(tab <- cbind(Est = fixef(m), LL = fixef(m) - 1.96 * se, UL = fixef(m) + 1.96 *
                se))

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
tmp <- sampler(dados, "Ano", reps = 100)
bigdata <- cbind(tmp, dados[tmp$RowID, ])

f <- fixef(m)
r <- getME(m, "theta")

cl <- makeCluster(4)
clusterExport(cl, c("bigdata", "f", "r"))
clusterEvalQ(cl, require(lme4))

myboot <- function(i) {
  object <- try(glmer(N_lagartas ~ Variedade + Local + (1 | NewID), data = bigdata, subset = Replicate == i, family = poisson,
                      nAGQ = 1, start = list(fixef = f, theta = r)), silent = TRUE)
  if (class(object) == "try-error")
    return(object)
  c(fixef(object), getME(object, "theta"))
}

start <- proc.time()
res <- parLapplyLB(cl, X = levels(bigdata$Replicate), fun = myboot)
end <- proc.time()

# Calcula a proporção de modelos que convergiram calculate proportion of models that successfully converged
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
newdata <- with(dados, expand.grid(Ano=unique(Ano), Variedade=unique(Variedade), Local=unique(Local)))
str(p2 <- predict(m,newdata))    # new data, all RE
str(p3 <- predict(m,newdata,re.form=NA)) # new data, level-0
str(p4 <- predict(m,newdata,re.form= ~(1|Ano))) # explicitly specify RE
stopifnot(identical(p2, p4))
