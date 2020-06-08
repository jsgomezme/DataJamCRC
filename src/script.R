###Limpiando el entorno
rm(list=ls())

datos <- read.csv("data/data.csv", header=T, sep = ";")
install.packages("ConvergenceClubs")
data("filteredGDP")
library(ConvergenceClubs)
install.packages("reshape")
library(reshape)

# Cluster Countries using GDP from year 1970 to year 2003
clubs <- findClubs(datos,  dataCols=3:26, unit_names = 2, refCol=26,
                   time_trim = 1/3, HACmethod = "FQSB",
                   cstar = 0,
                   cstar_method = 'incremental',
                   cstar_increment = 0.1)
mclubs <- mergeClubs(clubs, mergeMethod='PS', mergeDivergent=FALSE)
summary(mclubs)


# }
# NOT RUN {
# Cluster Countries using GDP from year 1970 to year 2003
clubs <- findClubs(filteredGDP,  dataCols=2:35, unit_names = 1, refCol=35,
                   time_trim = 1/3, HACmethod = "AQSB", cstar = 0)

# }
# NOT RUN {
# }


c<-cbind(datos$id_municipio)
findClubs( datos, datos[,2],refCol=4,  unit_names = NULL, time_trim = 1/3, HACmethod = "FQSB", cstar = 0, cstar_method ="fixed", cstar_increment = 0.1, cstar_cap = 3 )


H <- computeH(datos[,-2], quantity="H")
mod<-estimateMod(H, time_trim = 1/3, HACmethod = "FQSB")
write.csv2(clubs,"data/clubes.csv") 
