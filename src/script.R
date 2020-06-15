###Limpiando el entorno
rm(list=ls())
install.packages(c("prettymapr","maps","classInt","ggplot2", "ConvergenceClubs", "reshape", "reshape2", "tidyr", "rgdal", "tmap", "GISTools"))
require(classInt)
require(ggplot2)
require(ConvergenceClubs)
require(reshape)
require(reshape2)
require(tidyr)
require(rgdal)
require(tmap)
require(maps)
require(prettymapr)
require(GISTools)

########
#Convergence club algorithm for vozfija
########

datos <- read.csv("data/vozfijat.csv", header=T, sep = ";")

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

#Exporting results 
clubes<-cbind(mclubs[[1]]$unit_names, mclubs[[2]]$unit_names, mclubs[[3]]$unit_names, mclubs[[4]]$unit_names, mclubs[[5]]$unit_names, mclubs[[6]]$unit_names, mclubs[[7]]$unit_names, mclubs[[8]]$unit_names, mclubs[[9]]$unit_names, mclubs[[10]]$unit_names)
write.csv2(clubes,"data/clubesvozfija.csv")

#Plotting transition paths
png(file="figures/transitionpathvozfija.png",height = 1000,width = 1000, bg = "transparent") #Especifiaciones del archivo
plot(mclubs, clubs=NULL, avgTP = TRUE, legend=TRUE)
dev.off()


########
#Convergence club algorithm for tv
########

datos <- read.csv("data/tvt.csv", header=T, sep = ";")

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

#Exporting results 
clubes<-cbind(mclubs[[1]]$unit_names, mclubs[[2]]$unit_names, mclubs[[3]]$unit_names, mclubs[[4]]$unit_names, mclubs[[5]]$unit_names,mclubs[[6]]$unit_names,mclubs[[7]]$unit_names, mclubs[[9]]$unit_names, mclubs[[10]]$unit_names)
write.csv2(clubes,"data/clubestv.csv") 

#Plotting transition paths
png(file="figures/transitionpathtv.png",height = 1000,width = 1000, bg = "transparent") #Especifiaciones del archivo
plot(mclubs, clubs=NULL, avgTP = TRUE, legend=TRUE)
dev.off()

########
#Convergence club algorithm for intfijo
########

datos <- read.csv("data/intfijot.csv", header=T, sep = ";")

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

#Exporting results 
clubes<-cbind(mclubs[[1]]$unit_names, mclubs[[2]]$unit_names, mclubs[[3]]$unit_names, mclubs[[4]]$unit_names, mclubs[[5]]$unit_names)
write.csv2(clubes,"data/clubesintfijo.csv") 

#Plotting transition paths
png(file="figures/transitionpathintfijo.png",height = 1000,width = 1000, bg = "transparent") #Especifiaciones del archivo
plot(mclubs, clubs=NULL, avgTP = TRUE, legend=TRUE)
dev.off()


########
#Convergence club algorithm for meandownstream
########

datos <- read.csv("data/meandownstreamt.csv", header=T, sep = ";")

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

#Exporting results 
clubes<-cbind(mclubs[[1]]$unit_names, mclubs[[2]]$unit_names, mclubs[[3]]$unit_names, mclubs[[4]]$unit_names, mclubs[[5]]$unit_names)
write.csv2(clubes,"data/clubesmeandw.csv") 

#Plotting transition paths
png(file="figures/transitionpathmeandw.png",height = 1000,width = 1000, bg = "transparent") #Especifiaciones del archivo
plot(mclubs, clubs=NULL, avgTP = TRUE, legend=TRUE)
dev.off()



########
#Plotting Maps
########


###Abriendo el archvo con el "shape"
mapa <- readOGR("data/mpio.shp",layer="mpio")
mapa = mapa[mapa$DPTO_CCDGO !=88,]
###Verificando el mapa
plot(mapa)

###Cargando datos
write.csv(mapa@data,file = "data/MyData.csv") #Exportando la base del "shape"
datos <- read.csv("clubes.csv",header=TRUE,sep=",")
#datos<- read.table(file = "clipboard", sep = "\t", header=TRUE)
#datos <- read_excel("base.xlsx", sheet = 1)
#datos<-datos[datos$DPTO !=88,]

###Tratamiento de datos
datos$ID_ESPACIA <- mapa@data$ID_ESPACIA #A??adiendo el id a la base
row.names(datos) <- row.names(mapa) #Pegando los nombre de filas del mapa a la base
mapa.datos <- SpatialPolygonsDataFrame(mapa,datos) #Emparejando las filas de ambos archivos y crea un nuevo archivo

######
### Plot clubes de Internet fijo
######

plotvar <- mapa.datos$club_intfijo #Creando la nueva variabe - Verificar que sea numerica el error puede estar en el archivo excel
nclr <- 6 # Numero de colores
plotclr <- c("darkgoldenrod4","darkgoldenrod2","gold","yellow","black","white")
# Creando paleta de colores rgb
class <- classIntervals(plotvar,nclr,style="equal") #Creando los intervalos
colcode <- findColours(class,plotclr) #Codificado los colores con los intervalos

## Exportando el mapa como png
png(file="figures/Clubes_internet_fijo.png",height = 10000,width = 10000, bg = "transparent") #Especifiaciones del archivo
plot (mapa.datos, col=colcode, border="gray35", axes=F) #Graficando
# Leyenda
legend ("bottomleft", 
        title = "Clubes de Internet Fijo",
        legend = c ("Club1", "Club2", "Club3", "Club4", "No Converge", "Sin datos"),
        fill= attr(colcode,"palette"),
        cex=14)
# Flecha hacia el norte
addnortharrow(pos = "topleft", padin = c(0.25, 0.25), scale = 14,
              lwd = 1, border = "black", cols = c("white", "black"),
              text.col = "black")
# Escala en Km
addscalebar(widthhint = 0.25,
            unitcategory = "metric", htin = 0.6, padin = c(0.1, 1.5),
            style = "bar", bar.cols = c("black", "white"), lwd = 1,
            linecol = "black", tick.cex = 2, labelpadin = 2, label.cex = 14,
            label.col = "black", pos = "topright")
dev.off()

## Exportando el mapa como SVG
svg(file="figures/Clubes_internet_fijo.svg",height = 10000,width = 10000, bg = "transparent") #Especifiaciones del archivo
plot (mapa.datos, col=colcode, border="gray35", axes=F) #Graficando
# Leyenda
legend ("bottomleft", 
        title = "Clubes de Internet Fijo",
        legend = c ("Club1", "Club2", "Club3", "Club4", "No Converge", "Sin datos"),
        fill= attr(colcode,"palette"),
        cex=14)
# Flecha hacia el norte
addnortharrow(pos = "topleft", padin = c(0.25, 0.25), scale = 14,
              lwd = 1, border = "black", cols = c("white", "black"),
              text.col = "black")
# Escala en Km
addscalebar(widthhint = 0.25,
            unitcategory = "metric", htin = 0.6, padin = c(0.1, 1.5),
            style = "bar", bar.cols = c("black", "white"), lwd = 1,
            linecol = "black", tick.cex = 2, labelpadin = 2, label.cex = 14,
            label.col = "black", pos = "topright")
dev.off()

######
### Plot clubes de Velocidad media de bajada
######

plotvar <- mapa.datos$club_meandw #Creando la nueva variabe - Verificar que sea numerica el error puede estar en el archivo excel
nclr <- 6 # Numero de colores
plotclr <- c("darkgreen","forestgreen","green3","lawngreen","black","white")
# Creando paleta de colores rgb
class <- classIntervals(plotvar,nclr,style="equal") #Creando los intervalos
colcode <- findColours(class,plotclr) #Codificado los colores con los intervalos

## Exportando el mapa como png
png(file="figures/Clubes_velocidad_bajada.png",height = 10000,width = 10000, bg = "transparent") #Especifiaciones del archivo
plot (mapa.datos, col=colcode, border="gray35", axes=F) #Graficando
# Leyenda
legend ("bottomleft", 
        title = "Clubes de velocidad de bajada",
        legend = c ("Club1", "Club2", "Club3", "Club4", "No Converge", "Sin datos"),
        fill= attr(colcode,"palette"),
        cex=14)
# Flecha hacia el norte
addnortharrow(pos = "topleft", padin = c(0.25, 0.25), scale = 14,
              lwd = 1, border = "black", cols = c("white", "black"),
              text.col = "black")
# Escala en Km
addscalebar(widthhint = 0.25,
            unitcategory = "metric", htin = 0.6, padin = c(0.1, 1.5),
            style = "bar", bar.cols = c("black", "white"), lwd = 1,
            linecol = "black", tick.cex = 2, labelpadin = 2, label.cex = 14,
            label.col = "black", pos = "topright")
dev.off()

## Exportando el mapa como SVG
svg(file="figures/Clubes_velocidad_bajada.svg",height = 10000,width = 10000, bg = "transparent") #Especifiaciones del archivo
plot (mapa.datos, col=colcode, border="gray35", axes=F) #Graficando
# Leyenda
legend ("bottomleft", 
        title = "Clubes de velocidad de bajada",
        legend = c ("Club1", "Club2", "Club3", "Club4", "No Converge", "Sin datos"),
        fill= attr(colcode,"palette"),
        cex=14)
# Flecha hacia el norte
addnortharrow(pos = "topleft", padin = c(0.25, 0.25), scale = 14,
              lwd = 1, border = "black", cols = c("white", "black"),
              text.col = "black")
# Escala en Km
addscalebar(widthhint = 0.25,
            unitcategory = "metric", htin = 0.6, padin = c(0.1, 1.5),
            style = "bar", bar.cols = c("black", "white"), lwd = 1,
            linecol = "black", tick.cex = 2, labelpadin = 2, label.cex = 14,
            label.col = "black", pos = "topright")
dev.off()


######
### Plot clubes de voz fija
######

plotvar <- mapa.datos$club_vozfija #Creando la nueva variabe - Verificar que sea numerica el error puede estar en el archivo excel
nclr <- 11 # Numero de colores
plotclr <- c("darkred","red4","red3","red2","red1","red","salmon2","salmon1","salmon","black","white")
# Creando paleta de colores rgb
class <- classIntervals(plotvar,nclr,style="equal") #Creando los intervalos
colcode <- findColours(class,plotclr) #Codificado los colores con los intervalos

## Exportando el mapa como png
png(file="figures/Clubes_voz_fija.png",height = 10000,width = 10000, bg = "transparent") #Especifiaciones del archivo
plot (mapa.datos, col=colcode, border="gray35", axes=F) #Graficando
# Leyenda
legend ("bottomleft", 
        title = "Clubes de voz fija",
        legend = c ("Club1", "Club2", "Club3", "Club4", "Club5", "Club6", "Club7", "Club8", "Club9","No Converge", "Sin datos"),
        fill= attr(colcode,"palette"),
        cex=14)
# Flecha hacia el norte
addnortharrow(pos = "topleft", padin = c(0.25, 0.25), scale = 14,
              lwd = 1, border = "black", cols = c("white", "black"),
              text.col = "black")
# Escala en Km
addscalebar(widthhint = 0.25,
            unitcategory = "metric", htin = 0.6, padin = c(0.1, 1.5),
            style = "bar", bar.cols = c("black", "white"), lwd = 1,
            linecol = "black", tick.cex = 2, labelpadin = 2, label.cex = 14,
            label.col = "black", pos = "topright")
dev.off()

## Exportando el mapa como SVG
svg(file="figures/Clubes_voz_fija.svg",height = 10000,width = 10000, bg = "transparent") #Especifiaciones del archivo
plot (mapa.datos, col=colcode, border="gray35", axes=F) #Graficando
# Leyenda
legend ("bottomleft", 
        title = "Clubes de voz fija",
        legend = c ("Club1", "Club2", "Club3", "Club4", "Club5", "Club6", "Club7", "Club8", "Club9","No Converge", "Sin datos"),
        fill= attr(colcode,"palette"),
        cex=14)
# Flecha hacia el norte
addnortharrow(pos = "topleft", padin = c(0.25, 0.25), scale = 14,
              lwd = 1, border = "black", cols = c("white", "black"),
              text.col = "black")
# Escala en Km
addscalebar(widthhint = 0.25,
            unitcategory = "metric", htin = 0.6, padin = c(0.1, 1.5),
            style = "bar", bar.cols = c("black", "white"), lwd = 1,
            linecol = "black", tick.cex = 2, labelpadin = 2, label.cex = 14,
            label.col = "black", pos = "topright")
dev.off()

######
### Plot clubes de tv
######

plotvar <- mapa.datos$club_tv #Creando la nueva variabe - Verificar que sea numerica el error puede estar en el archivo excel
nclr <- 11 # Numero de colores
plotclr <- c("darkblue","blue3","blue1","blue","dodgerblue2",
             "dodgerblue","deepskyblue","cyan","lightblue1", "black","white")
# Creando paleta de colores rgb
class <- classIntervals(plotvar,nclr,style="equal") #Creando los intervalos
colcode <- findColours(class,plotclr) #Codificado los colores con los intervalos

## Exportando el mapa como png
png(file="figures/Clubes_tv.png",height = 10000,width = 10000, bg = "transparent") #Especifiaciones del archivo
plot (mapa.datos, col=colcode, border="gray35", axes=F) #Graficando
# Leyenda
legend ("bottomleft", 
        title = "Clubes de TV",
        legend = c ("Club1", "Club2", "Club3", "Club4", "Club5", "Club6", "Club7", "Club8", "Club9","No Converge", "Sin datos"),
        fill= attr(colcode,"palette"),
        cex=14)
# Flecha hacia el norte
addnortharrow(pos = "topleft", padin = c(0.25, 0.25), scale = 14,
              lwd = 1, border = "black", cols = c("white", "black"),
              text.col = "black")
# Escala en Km
addscalebar(widthhint = 0.25,
            unitcategory = "metric", htin = 0.6, padin = c(0.1, 1.5),
            style = "bar", bar.cols = c("black", "white"), lwd = 1,
            linecol = "black", tick.cex = 2, labelpadin = 2, label.cex = 14,
            label.col = "black", pos = "topright")
dev.off()

## Exportando el mapa como SVG
svg(file="figures/Clubes_tv.svg",height = 10000,width = 10000, bg = "transparent") #Especifiaciones del archivo
plot (mapa.datos, col=colcode, border="gray35", axes=F) #Graficando
# Leyenda
legend ("bottomleft", 
        title = "Clubes de TV",
        legend = c ("Club1", "Club2", "Club3", "Club4", "Club5", "Club6", "Club7", "Club8", "Club9","No Converge", "Sin datos"),
        fill= attr(colcode,"palette"),
        cex=14)
# Flecha hacia el norte
addnortharrow(pos = "topleft", padin = c(0.25, 0.25), scale = 14,
              lwd = 1, border = "black", cols = c("white", "black"),
              text.col = "black")
# Escala en Km
addscalebar(widthhint = 0.25,
            unitcategory = "metric", htin = 0.6, padin = c(0.1, 1.5),
            style = "bar", bar.cols = c("black", "white"), lwd = 1,
            linecol = "black", tick.cex = 2, labelpadin = 2, label.cex = 14,
            label.col = "black", pos = "topright")
dev.off()


