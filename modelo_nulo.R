#os exempos abaixo foram feitos para uma matriz de especies por sites chamada caraj2. para rodar com seus dados basta importar a tabela e sustituir o nome da tabela nas funcoes


#Modelo nulo para testar associacao de especies e deerminadas localidades 
#install.packages("ca")
#install.packages("vegan")

library(ca)
library(vegan)
data(sipoo)

# carregar a matriz de dados dados.csv
dados<-read.csv("dados.csv")
dados_n <- read.csv("nest.csv")

# deletar a primeira coluna da matriz dados
dados2 <- df <- subset(dados, select = -1)
dados2_n <- df <- subset(dados_n, select = -1)


#fazer um codigo para calcular a soma das linhas da matriz dados2
soma<-apply(dados2, 1, sum)

#fazer um codigo para transformar a matriz dados2 em uma matriz binaria
dados2<-ifelse(dados2>0, 1, 0)



modelo <- oecosimu(dados2, decorana, "swap",burnin=100, thin=10, statistic="evals") 

densityplot(permustats(modelo), as.table = TRUE, layout = c(1,4))

## Traditional nestedness statistics (number of checkerboard units)



nestedchecker(dados2)
test<-oecosimu(dados2, nestedchecker, "swap")
densityplot(permustats(test), as.table = TRUE, layout = c(1,4))

## sequential model, one-sided test, a vector statistic

out <- oecosimu(dados2, decorana, "swap", burnin=100, thin=10, 
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
mbc1 <- oecosimu(dados2, meandist, "r2dtable")
mbc1
densityplot(permustats(mbc1), as.table = TRUE)

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

out <- nestedtemp(dados2)
plot(out)


plot(out, kind="incid")

summary(out)

##Mapa de temperatura

out <- nestedtemp(dados2_n)
plot(out)


plot(out, kind="incid")

summary(out)


##Outro metodo para calculo do C-score

nestedchecker(dados2)
oecosimu(dados2, nestedchecker, "swap",nsimul=1000)
result<-oecosimu(dados2, nestedchecker, "quasiswap", statistic = "C.score",nsimul=1000)
densityplot(result, as.table = TRUE)