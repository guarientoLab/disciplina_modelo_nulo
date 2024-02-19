#os exempos abaixo foram feitos para uma matriz de especies por sites chamada caraj2. para rodar com seus dados basta importar a tabela e sustituir o nome da tabela nas funcoes


#Modelo nulo para testar associacao de especies e deerminadas localidades 
#install.packages("ca")
#install.packages("vegan")

library(ca)
library(vegan)
data(sipoo)
modelo <- oecosimu(sipoo, decorana, "swap",burnin=100, thin=10, statistic="evals") #foi utilizado o metodo swap de aleatorizacao mas outros podem ser implementados

densityplot(permustats(modelo), as.table = TRUE, layout = c(1,4))

## Traditional nestedness statistics (number of checkerboard units)
nestedchecker(sipoo)
test<-oecosimu(sipoo, nestedchecker, "r0")
densityplot(permustats(test), as.table = TRUE, layout = c(1,4))

## sequential model, one-sided test, a vector statistic

out <- oecosimu(sipoo, decorana, "swap", burnin=100, thin=10, 
   statistic="evals", alt = "greater")
out

## Inspect the swap sequence as a time series object
plot(as.ts(out))
lag.plot(as.ts(out))
acf(as.ts(out))
## Density plot
densityplot(permustats(out), as.table = TRUE, layout = c(1,4))

## Use quantitative null models to compare
## mean Bray-Curtis dissimilarities
data(dune)
View(dune)
meandist <- function(x) mean(vegdist(x, "bray"))
mbc1 <- oecosimu(dune, meandist, "r2dtable")
mbc1
densityplot(permustats(mbc1), as.table = TRUE, layout = c(1,4))

densityplot(out, as.table = TRUE)

data(dune)
meandist <- function(x) mean(vegdist(x, "bray"))
mbc1 <- oecosimu(dune, meandist, "r2dtable")
mbc1
## defina ua funcaoo que aleatoriza celulas em cada linha

f <- function(x) {
  apply(x, 2, function(z) sample(z, length(z)))
}


## Script para calculo do C-Score
library(vegan)

library(bipartite)

modelo2 <- oecosimu(dados2, bipartite::C.score, "swap", burnin=100, thin=10, statistic="evals", nsimul=100) #especies por sites 

print(modelo2)

densityplot(null.model, as.table = TRUE)
## Fim do script

##Mapa de temperatura

getwd()
dados<-read.csv("nest.csv")

dados2<-df <- subset(dados, select = -1)
dados2<-t(dados2)
out <- nestedtemp(dados2)
plot(out)


plot(out, kind="incid")

##Outro metodo para calculo do C-score

nestedchecker(dados2)
oecosimu(dados2, nestedchecker, "swap",nsimul=1000)
result<-oecosimu(dados2, nestedchecker, "quasiswap", statistic = "C.score",nsimul=1000)
densityplot(result, as.table = TRUE)