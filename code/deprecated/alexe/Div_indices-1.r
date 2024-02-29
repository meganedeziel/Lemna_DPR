###Indice de Shannon (div. spécifique)
#Doit être fait avec la biomasse initiale (I)
setwd("C:/Users/polyc/OneDrive - UQAM/maîtrise en biologie/Expérimentation/Expérience 2/Données")
BiomasseI <- read.csv("Biomasse.csv", header = TRUE, sep = ";", row.names = 1)

summary(Biomasse)

library(vegan)
Ind.Shann <- diversity(BiomasseI, index="shannon")
Ind.Shann

BiomasseI$Shannon <- Ind.Shann

#Doc Excel
install.packages("xlsx")
library("xlsx")

write.xlsx(BiomasseI, file = "Shannon.xlsx", sheetName = "Shannon", 
           col.names = TRUE, row.names = TRUE, append = FALSE)


##Diversité fonctionnelle fondamentale (indice de dispersion fonctionnelle)
#Calcul de DF pour chaque cultures avec les valeurs de traits moyennes au temps 0
#la biomasse est la biomasse relative initiale des espèces au temps 0
setwd("C:/Users/polyc/OneDrive - UQAM/maîtrise en biologie/Expérimentation/Expérience 2/Données/résultats traits")

##SF
traitSF <-read.csv("TraitSF.csv", header = TRUE, sep = ";", row.name=1) 
M.biomass <-read.csv("M.biomasse.csv", header = TRUE, sep = ";", row.name=1)
M.biomasse<-as.matrix(M.biomass)

library(FD)

#calcul de distance 
d=dist(traitSF)

#FDis
DFSF <- fdisp(d, M.biomasse)

DFSF$FDis

##SLA
traitSLA <-read.csv("TraitSLA.csv", header = TRUE, sep = ";", row.name=1) 
M.biomass <-read.csv("M.biomasse.csv", header = TRUE, sep = ";", row.name=1)
M.biomasse<-as.matrix(M.biomass)

library(FD)

#calcul de distance 
d=dist(traitSLA)

#FDis
DFSLA <- fdisp(d, M.biomasse)

DFSLA$FDis

##MI
traitMI <-read.csv("TraitMI.csv", header = TRUE, sep = ";", row.name=1) 
M.biomass <-read.csv("M.biomasse.csv", header = TRUE, sep = ";", row.name=1)
M.biomasse<-as.matrix(M.biomass)

library(FD)

#calcul de distance 
d=dist(traitMI)

#FDis
DFMI <- fdisp(d, M.biomasse)

DFMI$FDis

##SF, MI et SLA
Traits <-read.csv("Traits.csv", header = TRUE, sep = ";", row.name=1) 
M.biomass <-read.csv("M.biomasse.csv", header = TRUE, sep = ";", row.name=1)
M.biomasse<-as.matrix(M.biomass)

library(FD)

#calcul de distance 
d=dist(Traits)

#FDis
DF <- fdisp(d, M.biomasse)

DF$FDis


##Diversité fonctionnelle fondamentale 2 (indice de dispersion fonctionnelle
#Calcul de DF pour chaque cultures avec les valeurs de traits des monocultures à chaque temps pour chaque bloc (ex.J20B1)
#la biomasse est la biomasse relative initiale des espèces au temps 0
#Pour chaque blocJours, modifier la base de données traits et runner ce même script. 
#Exemple de base de données dans Data2 DFftraits

##Diversité fonctionnelle réalisée (indice de dispersion fonctionnelle)
#Calcul de DF pour chaque culture avec les valeurs de traits de cette culture.
#la biomasse estla biomasse initales des espèces au temps 0
setwd("C:/Users/polyc/OneDrive - UQAM/maîtrise en biologie/Expérimentation/Expérience 2/Données/résultats traits")

Traitsr <-read.csv("Traitsr.csv", header = TRUE, sep = ";", row.name=1) 
M.biomassr <-read.csv("M.biomasser.csv", header = TRUE, sep = ";", row.name=1)
M.biomasser<-as.matrix(M.biomassr)

library(FD)

#calcul de distance 
d=dist(Traitsr)

#FDis
DFr <- fdisp(d, M.biomasser)

DFr$FDis

#Pour chaque BlocJour, modifier la base de données et runner ce même script. 
#Exemple de base de données dans Data2 DFrtraits

