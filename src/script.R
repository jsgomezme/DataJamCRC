###Limpiando el entorno
rm(list=ls())
install.packages(c("ConvergenceClubs", "reshape", "reshape2", "tidyr"))
require(ConvergenceClubs)
require(reshape)
require(reshape2)
require(tidyr)

datos <- read.csv("data/meandownstreamt.csv", header=T, sep = ";")


# Cluster Countries using GDP from year 1970 to year 2003
clubs <- findClubs(datos,  dataCols=2:25, unit_names = 1, refCol=25,
                   time_trim = 1/3, HACmethod = "FQSB",
                   cstar = 0,
                   cstar_method = 'incremental',
                   cstar_increment = 0.1)
mclubs <- mergeClubs(clubs, mergeMethod='PS', mergeDivergent=FALSE)
summary(mclubs)
H <- computeH(datos[,-1], quantity="H")
mod<-estimateMod(H, time_trim = 1/3, HACmethod = "FQSB")
mod
#clubes<-cbind(mclubs[[1]]$unit_names, mclubs[[2]]$unit_names, mclubs[[3]]$unit_names, mclubs[[4]]$unit_names, mclubs[[5]]$unit_names, mclubs[[6]]$unit_names, mclubs[[7]]$unit_names, mclubs[[8]]$unit_names, mclubs[[9]]$unit_names, mclubs[[10]]$unit_names)
#write.csv2(clubes,"data/clubesvozfija.csv")
#clubes<-cbind(mclubs[[1]]$unit_names, mclubs[[2]]$unit_names, mclubs[[3]]$unit_names, mclubs[[4]]$unit_names, mclubs[[5]]$unit_names)
#write.csv2(clubes,"data/clubesintfijo.csv") 
clubes<-cbind(mclubs[[1]]$unit_names, mclubs[[2]]$unit_names, mclubs[[3]]$unit_names, mclubs[[4]]$unit_names, mclubs[[5]]$unit_names,mclubs[[6]]$unit_names,mclubs[[7]]$unit_names, mclubs[[9]]$unit_names, mclubs[[10]]$unit_names)
write.csv2(clubes,"data/clubestv.csv") 
clubes<-cbind(mclubs[[1]]$unit_names, mclubs[[2]]$unit_names, mclubs[[3]]$unit_names, mclubs[[4]]$unit_names, mclubs[[5]]$unit_names)
write.csv2(clubes,"data/clubesmeandw.csv") 
clubes<-cbind(mclubs[[1]]$unit_names, mclubs[[2]]$unit_names, mclubs[[3]]$unit_names)
write.csv2(clubes,"data/indice.csv") 

#Mismo código por variable

plot(mclubs, clubs=NULL, avgTP = TRUE, legend=TRUE)

###Exportando el mapa como png
png(file="figures/meandownstreamTP.png",height = 800,width = 800) #Especifiaciones del archivo
#title(main = "T??tulo",cex=1) #T??tulo
plot(mclubs, clubs=NULL, avgTP = TRUE, legend=TRUE, plot_args = list(type='o'))
#
dev.off()


##Exportando el mapa como svg
svg(file="figures/meandownstreamTP.svg",height = 400,width = 400) #Especifiaciones del archivo
#title(main = "T??tulo",cex=1) #T??tulo
plot(mclubs, clubs=NULL, avgTP = TRUE, legend=TRUE, plot_args = list(type='o'))
#text(centroides,AcrProv,cex=3)  # Nombre de los departamentos
dev.off()
