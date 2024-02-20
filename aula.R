# código para importar dados.csv

getwd()

dados<-read.csv("dados.csv",sep = ";",header = T) # Windows
dados<-read.csv("dados.csv") 
dados

#código para eliminar a primeira coluna de dados
dados2 <- dados[,-1]

dados2

# Código para somar as linhas da matriz dados2
soma<-apply(dados2, 1, sum)
soma

# código que transforma a matriz dados2 em uma matriz binária
dados2_b<-ifelse(dados2>0, 1, 0)
dados2_b

# carregar a biblioteca vegan
install.packages("vegan")
library(vegan)

# fazer um código para calcular o númerode checkboards
nestedchecker(dados2_b)

# fazer o modelo nulo desta métrica estatística
modelo <- oecosimu(dados2_b, nestedchecker, "swap")
modelo
densityplot(permustats(modelo), as.table = TRUE)

# fazer um modelo nulo quantitativo para comparar as médias das dissimilaridades de Bray-Curtis

library(vegan)
vegdist(dados2, "bray")
meandist <- function(x) mean(vegdist(x, "bray"))
meandist(dados2)

modelo_quant <- oecosimu(dados2, meandist, "quasiswap_count")
modelo_quant
densityplot(permustats(modelo_quant), as.table = TRUE)



# fazer um modelo nulo quantitativo para comparar autovetores de uma DCA
library(vegan)
data(dune)

modelo <- oecosimu(dune, decorana, "swap",burnin=100, thin=10, statistic="evals") 

densityplot(permustats(modelo), as.table = TRUE, layout = c(1,4))
