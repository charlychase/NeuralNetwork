#Librerias
library(devtools)
library(dplyr)
library(readr)
library(kohonen)
library(Hmisc)
library(car) 
library(factoextra)
library(ape)
library(ggplot2)
library(ggdendro)
library(matlab)
########################

#Funciones
infoColumna <- function(n, numClusters, carta, grups){
  
  degradadoazul <- function(n){
    return(rgb(0,0.4,1,alpha=seq(0,1,1/n)))}
  
  datPrueba <- datSOMTotal
  datPrueba$grup <- grups[carta$unit.classif]
  
  lista <- vector("list", nClus)
  
  for (i in 1:nClus){
    a <- subset(datPrueba,grup==i)
    lista[i]<- a[n]
  }
  
  par(mfrow=c(2,1))
  plot(carta, type = "property", property = getCodes(carta)[,n], palette = degradadoazul ,main = colnames(datSOMTotal)[n], shape = "straight")
  add.cluster.boundaries(carta, clustering = grups)
  boxplot(lista, main = colnames(datSOMTotal)[n],xlab='groups', col='orange', border = 'brown')
}
########################################################


#####
#MAIN
#####
set.seed(999)

#repositori <- "C:\\Users\\Carlos\\Desktop\\redes"
repositori <- "C:/Users/llosanba/Desktop/treball de redes neuronals/shiny/Proyecto2/shiny_20_6/"
#repositori <- "C:\\Users\\usuario\\Documents\\Nofre JR\\UV\\Master\\Xarxes neuronals\\Treball"



setwd(repositori)

source(file.path("Scripts" ,"datTotal.R"), local = T)

#Comprobar los porteros
# a <- subset(datTotal, Pos=='GK')
# a$Player

datSOMTotal <- select(datTotal, -c("Player", "Nation",  "Pos",  "Age", "Tjugado", "Cmp.Total", "Cmp.short", "Cmp.medium", "Cmp.long",
                                   "A-xA",'PK','PKatt',"FK","Sh/90", "SoT/90", "G/Sh","G/SoT","xG", "npxG", "npxG/Sh",
                                   "G-xG", "np:G-xG","TklD%","Past","Press%","BlocKShSv",'Succ%','Megs','Rec%', "team", "league" ))


dat <- select(datSOMTotal, c( "Att.Total", "TotDist", "PrgDist", 
                              "Att.short",   "Att.medium", "Att.long",
                              "Ast",  "KP", "1/3", "PPA" , "CrsPA", "Prog","Gls", "Sh", "SoT","Tkl", "TklW", "Def 3rd", "Mid 3rd", "Att 3rd",'TklD', "AttD", 
                              "Press", "PressSucc", "PressDef 3rd", "PressMid 3rd", "PressAtt 3rd","Blocks", "BlockSh", 
                              "BlockPass",  "Int", "Clr" , "Err",'Touches',	'DefPen',	'Def3rd',	'Mid3rd',	
                              'Att3rd',	'Att Pen',	'Live',	'Succ', 'Att', '#Pl',	 'Carries',	'TotDist',	'PrgDist',	'Targ',	
                              'Rec',	'Miscon',	'Dispos'))/datTotal$"Tjugado"





datSOMTotal <- cbind(select(datSOMTotal, -c( "Att.Total", "TotDist", "PrgDist", 
                                             "Att.short",   "Att.medium", "Att.long",
                                             "Ast",  "KP", "1/3", "PPA" , "CrsPA", "Prog","Gls", "Sh", "SoT","Tkl", "TklW", "Def 3rd", "Mid 3rd", "Att 3rd",'TklD', "AttD", 
                                             "Press", "PressSucc", "PressDef 3rd", "PressMid 3rd", "PressAtt 3rd","Blocks", "BlockSh", 
                                             "BlockPass",  "Int", "Clr" , "Err",'Touches',	'DefPen',	'Def3rd',	'Mid3rd',	
                                             'Att3rd',	'Att Pen',	'Live',	'Succ', 'Att', '#Pl',	 'Carries',	'TotDist',	'PrgDist',	'Targ',	
                                             'Rec',	'Miscon',	'Dispos')), dat)

datSOMTotalscaled <- scale(datSOMTotal, center=T, scale=T)


##############################
#SOM sense PCA
###############################



carteTotal <- som(datSOMTotalscaled, grid=somgrid(5,6, "hexagonal"), rlen=1000)

plot(carteTotal, shape='straight')

nb <- table(carteTotal$unit.classif)

nb

dc <- dist(getCodes(carteTotal))

cah <- hclust(dc, method = "ward.D2", members = nb)

plot(cah,ylab="", sub="")
# plot(as.phylo(cah),type = "unrooted", rotate.tree = 90)
# ggdendrogram(cah)

str(carteTotal)

nClus=6


rect.hclust(cah, nClus)

grupos <- cutree(cah, nClus)


grupos

carteTotal$unit.classif

datCluster <- select(datTotal, c("Player","team", "Pos"))
datCluster$grup <- grupos[carteTotal$unit.classif]

#Crean los dataframes datClusteri

for (i in 1:nClus){
  assign(paste('datCluster', i, sep=''),subset(datCluster, grup==i))
}


#Un intento de poner leyendas
# leg <- character(nClus)
# 
# for (i in 1:nClus){
#   leg[i] <- paste("Grupo",i,sep=" ")
# }



#Representamos los grupos

a <- vector('character', nClus)

for (i in 1:nClus){
  a[i] <- paste('Grupo', i, sep=' ')
}
a
colfijo <- c("steelblue1", "sienna1", "yellowgreen", "red", "yellow", "purple", "grey", "brown", "darkslategray1", "indianred1", "wheat1", "tan1")

plot(carteTotal, type = "mapping", bgcol = colfijo[1:nClus][grupos], shape = "straight")
add.cluster.boundaries(carteTotal, clustering = grupos)
legend('bottom',legend=a, col=colfijo[1:nClus])

#Distancia entre vecinos
plot(carteTotal, type = "dist.neighbours", shape = "straight")
add.cluster.boundaries(carteTotal, clustering = grupos)


# par(mfrow=c(4,4))
# for (j in 1:(ncol(datSOMTotal)-1)){
#   
#   degradadoazul <- function(n){
#     return(rgb(0,0.4,1,alpha=seq(0,1,1/n)))}
#   
#   plot(carteTotal, type = "property", property = getCodes(carteTotal)[,j], palette = degradadoazul ,main = colnames(datSOMTotal)[j], shape = "straight")
#   add.cluster.boundaries(carteTotal, clustering = grupos)
# }



nombreCol <- colnames(datSOMTotal)
a <- 1:length(nombreCol)
names(a) <- nombreCol
a



infoColumna(1,nClus, carteTotal, grupos)


################################
#Con PCA
###############################

PCA <- prcomp(datSOMTotal, center = TRUE,scale. = TRUE, retx=TRUE, tol= 0.1)

fviz_eig(PCA)

summary(PCA)

#Esto es para poner una tolerancia dependiendo de la varianza acumulada
varAcum <- vector(mode = 'double', length = length(PCA$sdev))

varAcum[1] <- PCA$sdev[1]^2/sum(PCA$sdev^2)

for (i in 2:length(PCA$sdev)){
  varAcum[i] <- varAcum[i-1]+ PCA$sdev[i]^2/sum(PCA$sdev^2)
}

varRel <- PCA$sdev^2/sum(PCA$sdev^2)
varRel
plot(varAcum)

plot(cumsum(varRel),type= "h")
hist(cumsum(varRel))

a <- 
  a

tol=0.3

ind <- ifelse(cumsum(varRel) > tol, 1, 0)

ind

num <-match(1,ind)

barplot(cumsum(varRel), space=0, col="coral",xlab="Dimensiones" ,ylab="Varianza relativa acumulada")
lines(0:length(varRel),tol*ones(c(length(varRel)+1,1)), col="blue")
axis(1,at=num-0.5,labels = as.character(num))





varAcum
length(varAcum)
length(varRel)

datPCA <- PCA$x

cartePCA <- som(datPCA, grid=somgrid(5,6, "hexagonal"), rlen=1000)

plot(cartePCA, shape='straight')

nbPCA <- table(cartePCA$unit.classif)

nbPCA

dcPCA <- dist(getCodes(cartePCA))

cahPCA <- hclust(dcPCA, method = "ward.D2", members = nbPCA)

plot(cahPCA)

str(cartePCA)

nClus=6


rect.hclust(cahPCA, nClus)

gruposPCA <- cutree(cahPCA, nClus)


gruposPCA

cartePCA$unit.classif



datClusterPCA <- select(datTotal, c("Player","team", "Pos"))
datClusterPCA$grup <- grupos[cartePCA$unit.classif]

#Crean los dataframes datClusteri

for (i in 1:nClus){
  assign(paste('datClusterPCA', i, sep=''),subset(datClusterPCA, grup==i))
}

#Representamos los grupos
colfijo <- c("steelblue1", "sienna1", "yellowgreen", "red", "yellow", "purple", "grey", "brown", "darkslategray1", "indianred1", "wheat1", "tan1")

plot(cartePCA, type = "mapping", bgcol = colfijo[1:nClus][grupos], shape = "straight")
add.cluster.boundaries(cartePCA, clustering = grupos)

#Distancia entre vecinos
plot(cartePCA, type = "dist.neighbours", shape = "straight")
add.cluster.boundaries(cartePCA, clustering = grupos)

nombreCol <- colnames(datPCA)
a <- 1:length(nombreCol)
names(a) <- nombreCol
a


infoColumna(15,nClus, cartePCA, grupos)



