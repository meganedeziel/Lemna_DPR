library(ade4)
library(adespatial)
library(ape)
library(vegan)
library(MASS)
library(paleofire)
library(ggplot2)
1library(factoextra)
library(ggthemes)
library(devtools)
library(ggpubr)
library(lme4)
library(car)
library(tidyverse)

###Corrélation entre les traits au J1
setwd("C:/Users/polyc/OneDrive - UQAM/maîtrise en biologie/Expérimentation/Expérience 2/Données/résultats traits")
TraitsJ1 <- read.csv("TraitsJ1.csv", header = TRUE, sep = ";")
cor(TraitsJ1$MI, TraitsJ1$SF)
cor(TraitsJ1$MI, TraitsJ1$SLA)
cor(TraitsJ1$SF, TraitsJ1$SLA)

par(mfrow=c(2,2))
plot(TraitsJ1$MI ~ TraitsJ1$SF, main = "MI en fonction de SF",
     xlab = "SF", ylab = "MI") 

plot(TraitsJ1$MI ~ TraitsJ1$SLA, main = "MI en fonction de SLA",
     xlab = "SLA", ylab = "MI")

plot(TraitsJ1$SF ~ TraitsJ1$SLA, main = "SF en fonction de SLA",
     xlab = "SLA", ylab = "SF")
 

### la diversité fonctionnelle Fondamentale (SF)
setwd("C:/Users/polyc/OneDrive - UQAM/maîtrise en biologie/Expérimentation/Expérience 2/Données")
J1 <- read.csv("J1.csv", header = TRUE, sep = ";")
J20 <- read.csv("J20.csv", header = TRUE, sep = ";")
J40 <- read.csv("J40.csv", header = TRUE, sep = ";")
J60 <- read.csv("J60.csv", header = TRUE, sep = ";")

#J20
par(mfrow=c(2,2))
plot(J20$RYT ~ J20$DFSF, main = "RYT en fonction de la DFSF",
     xlab = "DFSF", ylab = "RYT") 

plot(J20$NE ~ J20$DFSF, main = "NE en fonction de la DFSF",
     xlab = "DFSF", ylab = "NE") 

plot(J20$CE ~ J20$DFSF, main = "CE en fonction de la DFSF",
     xlab = "DFSF", ylab = "CE") 

plot(J20$SE ~ J20$DFSF, main = "SE en fonction de la DFSF",
     xlab = "DFSF", ylab = "SE") 

cor(J20$RYT, J20$DFSF)
cor(J20$NE, J20$DFSF)
cor(J20$CE, J20$DFSF)
cor(J20$SE, J20$DFSF)

#J40
par(mfrow=c(2,2))
plot(J40$RYT ~ J40$DFSF, main = "RYT en fonction de la DFSF",
     xlab = "DFSF", ylab = "RYT") 

plot(J40$NE ~ J40$DFSF, main = "NE en fonction de la DFSF",
     xlab = "DFSF", ylab = "NE") 

plot(J40$CE ~ J40$DFSF, main = "CE en fonction de la DFSF",
     xlab = "DFSF", ylab = "CE") 

plot(J40$SE ~ J40$DFSF, main = "SE en fonction de la DFSF",
     xlab = "DFSF", ylab = "SE") 

cor(J40$RYT, J40$DFSF)
cor(J40$NE, J40$DFSF)
cor(J40$CE, J40$DFSF)
cor(J40$SE, J40$DFSF)

#J60
par(mfrow=c(2,2))
plot(J60$RYT ~ J60$DFSF, main = "RYT en fonction de la DFSF",
     xlab = "DFSF", ylab = "RYT") 

plot(J60$NE ~ J60$DFSF, main = "NE en fonction de la DFSF",
     xlab = "DFSF", ylab = "NE") 

plot(J60$CE ~ J60$DFSF, main = "CE en fonction de la DFSF",
     xlab = "DFSF", ylab = "CE") 

plot(J60$SE ~ J60$DFSF, main = "SE en fonction de la DFSF",
     xlab = "DFSF", ylab = "SE") 

cor(J60$RYT, J60$DFSF)
cor(J60$NE, J60$DFSF)
cor(J60$CE, J60$DFSF)
cor(J60$SE, J60$DFSF)

###La diversité fonctionnelle fondamentale (SLA)
setwd("C:/Users/polyc/OneDrive - UQAM/maîtrise en biologie/Expérimentation/Expérience 2/Données")
J1 <- read.csv("J1.csv", header = TRUE, sep = ";")
J20 <- read.csv("J20.csv", header = TRUE, sep = ";")
J40 <- read.csv("J40.csv", header = TRUE, sep = ";")
J60 <- read.csv("J60.csv", header = TRUE, sep = ";")

#J20
par(mfrow=c(2,2))
plot(J20$RYT ~ J20$DFSLA, main = "RYT en fonction de la DFSLA",
     xlab = "DFSLA", ylab = "RYT") 

plot(J20$NE ~ J20$DFSLA, main = "NE en fonction de la DFSLA",
     xlab = "DFSLA", ylab = "NE") 

plot(J20$CE ~ J20$DFSLA, main = "CE en fonction de la DFSLA",
     xlab = "DFSLA", ylab = "CE") 

plot(J20$SE ~ J20$DFSLA, main = "SE en fonction de la DFSLA",
     xlab = "DFSLA", ylab = "SE") 

cor(J20$RYT, J20$DFSLA)
cor(J20$NE, J20$DFSLA)
cor(J20$CE, J20$DFSLA)
cor(J20$SE, J20$DFSLA)

#J40
par(mfrow=c(2,2))
plot(J40$RYT ~ J40$DFSLA, main = "RYT en fonction de la DFSLA",
     xlab = "DFSLA", ylab = "RYT") 

plot(J40$NE ~ J40$DFSLA, main = "NE en fonction de la DFSLA",
     xlab = "DFSLA", ylab = "NE") 

plot(J40$CE ~ J40$DFSLA, main = "CE en fonction de la DFSLA",
     xlab = "DFSLA", ylab = "CE") 

plot(J40$SE ~ J40$DFSLA, main = "SE en fonction de la DFSLA",
     xlab = "DFSLA", ylab = "SE") 

cor(J40$RYT, J40$DFSLA)
cor(J40$NE, J40$DFSLA)
cor(J40$CE, J40$DFSLA)
cor(J40$SE, J40$DFSLA)

#J60
par(mfrow=c(2,2))
plot(J60$RYT ~ J60$DFSLA, main = "RYT en fonction de la DFSLA",
     xlab = "DFSLA", ylab = "RYT") 

plot(J60$NE ~ J60$DFSLA, main = "NE en fonction de la DFSLA",
     xlab = "DFSLA", ylab = "NE") 

plot(J60$CE ~ J60$DFSLA, main = "CE en fonction de la DFSLA",
     xlab = "DFSLA", ylab = "CE") 

plot(J60$SE ~ J60$DFSLA, main = "SE en fonction de la DFSLA",
     xlab = "DFSLA", ylab = "SE") 

cor(J60$RYT, J60$DFSLA)
cor(J60$NE, J60$DFSLA)
cor(J60$CE, J60$DFSLA)
cor(J60$SE, J60$DFSLA)

###La diversité fonctionnelle fondamentale (MI)
setwd("C:/Users/polyc/OneDrive - UQAM/maîtrise en biologie/Expérimentation/Expérience 2/Données")
J1 <- read.csv("J1.csv", header = TRUE, sep = ";")
J20 <- read.csv("J20.csv", header = TRUE, sep = ";")
J40 <- read.csv("J40.csv", header = TRUE, sep = ";")
J60 <- read.csv("J60.csv", header = TRUE, sep = ";")

#J20
par(mfrow=c(2,2))
plot(J20$RYT ~ J20$DFMI, main = "RYT en fonction de la DFMI",
     xlab = "DFMI", ylab = "RYT") 

plot(J20$NE ~ J20$DFMI, main = "NE en fonction de la DFMI",
     xlab = "DFMI", ylab = "NE") 

plot(J20$CE ~ J20$DFMI, main = "CE en fonction de la DFMI",
     xlab = "DFMI", ylab = "CE") 

plot(J20$SE ~ J20$DFMI, main = "SE en fonction de la DFMI",
     xlab = "DFMI", ylab = "SE") 

cor(J20$RYT, J20$DFMI)
cor(J20$NE, J20$DFMI)
cor(J20$CE, J20$DFMI)
cor(J20$SE, J20$DFMI)

#J40
par(mfrow=c(2,2))
plot(J40$RYT ~ J40$DFMI, main = "RYT en fonction de la DFMI",
     xlab = "DFMI", ylab = "RYT") 

plot(J40$NE ~ J40$DFMI, main = "NE en fonction de la DFMI",
     xlab = "DFMI", ylab = "NE") 

plot(J40$CE ~ J40$DFMI, main = "CE en fonction de la DFMI",
     xlab = "DFMI", ylab = "CE") 

plot(J40$SE ~ J40$DFMI, main = "SE en fonction de la DFMI",
     xlab = "DFMI", ylab = "SE") 

cor(J40$RYT, J40$DFMI)
cor(J40$NE, J40$DFMI)
cor(J40$CE, J40$DFMI)
cor(J40$SE, J40$DFMI)

#J60
par(mfrow=c(2,2))
plot(J60$RYT ~ J60$DFMI, main = "RYT en fonction de la DFMI",
     xlab = "DFMI", ylab = "RYT") 

plot(J60$NE ~ J60$DFMI, main = "NE en fonction de la DFMI",
     xlab = "DFMI", ylab = "NE") 

plot(J60$CE ~ J60$DFMI, main = "CE en fonction de la DFMI",
     xlab = "DFMI", ylab = "CE") 

plot(J60$SE ~ J60$DFMI, main = "SE en fonction de la DFMI",
     xlab = "DFMI", ylab = "SE") 

cor(J60$RYT, J60$DFMI)
cor(J60$NE, J60$DFMI)
cor(J60$CE, J60$DFMI)
cor(J60$SE, J60$DFMI)

###La diversité fonctionnelle fondamentale (ALL)
setwd("C:/Users/polyc/OneDrive - UQAM/maîtrise en biologie/Expérimentation/Expérience 2/Données")
J1 <- read.csv("J1.csv", header = TRUE, sep = ";")
J20 <- read.csv("J20.csv", header = TRUE, sep = ";")
J40 <- read.csv("J40.csv", header = TRUE, sep = ";")
J60 <- read.csv("J60.csv", header = TRUE, sep = ";")

#J20
par(mfrow=c(2,2))
plot(J20$RYT ~ J20$DFfALL, main = "RYT en fonction de la DFfALL",
     xlab = "DFfALL", ylab = "RYT") 

plot(J20$NE ~ J20$DFfALL, main = "NE en fonction de la DFfALL",
     xlab = "DFfALL", ylab = "NE") 

plot(J20$CE ~ J20$DFfALL, main = "CE en fonction de la DFfALL",
     xlab = "DFfALL", ylab = "CE") 

plot(J20$SE ~ J20$DFfALL, main = "SE en fonction de la DFfALL",
     xlab = "DFfALL", ylab = "SE") 

cor(J20$RYT, J20$DFfALL)
cor(J20$NE, J20$DFfALL)
cor(J20$CE, J20$DFfALL)
cor(J20$SE, J20$DFfALL)

#J40
par(mfrow=c(2,2))
plot(J40$RYT ~ J40$DFfALL, main = "RYT en fonction de la DFfALL",
     xlab = "DFfALL", ylab = "RYT") 

plot(J40$NE ~ J40$DFfALL, main = "NE en fonction de la DFfALL",
     xlab = "DFfALL", ylab = "NE") 

plot(J40$CE ~ J40$DFfALL, main = "CE en fonction de la DFfALL",
     xlab = "DFfALL", ylab = "CE") 

plot(J40$SE ~ J40$DFfALL, main = "SE en fonction de la DFfALL",
     xlab = "DFfALL", ylab = "SE") 

cor(J40$RYT, J40$DFfALL)
cor(J40$NE, J40$DFfALL)
cor(J40$CE, J40$DFfALL)
cor(J40$SE, J40$DFfALL)

#J60
par(mfrow=c(2,2))
plot(J60$RYT ~ J60$DFfALL, main = "RYT en fonction de la DFfALL",
     xlab = "DFfALL", ylab = "RYT") 

plot(J60$NE ~ J60$DFfALL, main = "NE en fonction de la DFfALL",
     xlab = "DFfALL", ylab = "NE") 

plot(J60$CE ~ J60$DFfALL, main = "CE en fonction de la DFfALL",
     xlab = "DFfALL", ylab = "CE") 

plot(J60$SE ~ J60$DFfALL, main = "SE en fonction de la DFfALL",
     xlab = "DFfALL", ylab = "SE") 

cor(J60$RYT, J60$DFfALL)
cor(J60$NE, J60$DFfALL)
cor(J60$CE, J60$DFfALL)
cor(J60$SE, J60$DFfALL)

###Différence des traits entre J60 et J1
setwd("C:/Users/polyc/OneDrive - UQAM/maîtrise en biologie/Expérimentation/Expérience 2/Données/résultats traits")
Diff.Lm <- read.csv("Diff.Lm.csv", header = TRUE, sep = ";")
Diff.Sp <- read.csv("Diff.Sp.csv", header = TRUE, sep = ";")
Diff.Wc <- read.csv("Diff.Wc.csv", header = TRUE, sep = ";")
Diff.Lt <- read.csv("Diff.Lt.csv", header = TRUE, sep = ";")

par(mfrow=c(2,2))

##SF
boxplot(Diff.Lm$Diff.SF ~ Diff.Lm$Esp.Compagne, main = "Différence dans la SF de Lm entre le J60 et le J1",
        xlab = "Espèce compagne", ylab = "Diff. de SF", cex.axis =0.5)

boxplot(Diff.Sp$Diff.SF ~ Diff.Sp$Esp.Compagne, main = "Différence dans la SF de Sp entre le J60 et le J1",
        xlab = "Espèce compagne", ylab = "Diff. de SF", cex.axis =0.5)

boxplot(Diff.Wc$Diff.SF ~ Diff.Wc$Esp.Compagne, main = "Différence dans la SF de Wc entre le J60 et le J1",
        xlab = "Espèce compagne", ylab = "Diff. de SF", cex.axis =0.5)

boxplot(Diff.Lt$Diff.SF ~ Diff.Lt$Esp.Compagne, main = "Différence dans la SF de Lt entre le J60 et le J1",
        xlab = "Espèce compagne", ylab = "Diff. de SF", cex.axis =0.5)

#MI
boxplot(Diff.Lm$Diff..MI ~ Diff.Lm$Esp.Compagne, main = "Différence dans la MI de Lm entre le J60 et le J1",
        xlab = "Espèce compagne", ylab = "Diff. de MI", cex.axis =0.5)

boxplot(Diff.Sp$Diff..MI ~ Diff.Sp$Esp.Compagne, main = "Différence dans la MI de Sp entre le J60 et le J1",
        xlab = "Espèce compagne", ylab = "Diff. de MI", cex.axis =0.5)

boxplot(Diff.Wc$Diff..MI ~ Diff.Wc$Esp.Compagne, main = "Différence dans la MI de Wc entre le J60 et le J1",
        xlab = "Espèce compagne", ylab = "Diff. de MI", cex.axis =0.5)

boxplot(Diff.Lt$Diff..MI ~ Diff.Lt$Esp.Compagne, main = "Différence dans la MI de Lt entre le J60 et le J1",
        xlab = "Espèce compagne", ylab = "Diff. de  MI", cex.axis =0.5)

#SLA
boxplot(Diff.Lm$Diff.SLA ~ Diff.Lm$Esp.Compagne, main = "Différence dans la SLA de Lm entre le J60 et le J1",
        xlab = "Espèce compagne", ylab = "Diff. de SLA", cex.axis =0.5)

boxplot(Diff.Sp$Diff.SLA ~ Diff.Sp$Esp.Compagne, main = "Différence dans la SLA de Sp entre le J60 et le J1",
        xlab = "Espèce compagne", ylab = "Diff. de SLA", cex.axis =0.5)

boxplot(Diff.Wc$Diff.SLA ~ Diff.Wc$Esp.Compagne, main = "Différence dans la SLA de Wc entre le J60 et le J1",
        xlab = "Espèce compagne", ylab = "Diff. de SLA", cex.axis =0.5)

boxplot(Diff.Lt$Diff.SLA ~ Diff.Lt$Esp.Compagne, main = "Différence dans la SLA de Lt entre le J60 et le J1",
        xlab = "Espèce compagne", ylab = "Diff. de  SLA", cex.axis =0.5)



### la diversité fonctionnelle Fondamentale 2 (ALL)

setwd("C:/Users/polyc/OneDrive - UQAM/maîtrise en biologie/Expérimentation/Expérience 2/Données")
J20 <- read.csv("J20.csv", header = TRUE, sep = ";")
J40 <- read.csv("J40.csv", header = TRUE, sep = ";")
J60 <- read.csv("J60.csv", header = TRUE, sep = ";")

#J20
par(mfrow=c(2,2))
plot(J20$RYT ~ J20$DFf2, main = "RYT en fonction de la DFf2",
     xlab = "DFf2", ylab = "RYT") 

plot(J20$NE ~ J20$DFf2, main = "NE en fonction de la DFf2",
     xlab = "DFf2", ylab = "NE") 

plot(J20$CE ~ J20$DFf2, main = "CE en fonction de la DFf2",
     xlab = "DFf2", ylab = "CE") 

plot(J20$SE ~ J20$DFf2, main = "SE en fonction de la DFf2",
     xlab = "DFf2", ylab = "SE") 

cor(J20$RYT, J20$DFf2)
cor(J20$NE, J20$DFf2)
cor(J20$CE, J20$DFf2)
cor(J20$SE, J20$DFf2)

#J40
par(mfrow=c(2,2))
plot(J40$RYT ~ J40$DFf2, main = "RYT en fonction de la DFf2",
     xlab = "DFf2", ylab = "RYT") 

plot(J40$NE ~ J40$DFf2, main = "NE en fonction de la DFf2",
     xlab = "DFf2", ylab = "NE") 

plot(J40$CE ~ J40$DFf2, main = "CE en fonction de la DFf2",
     xlab = "DFf2", ylab = "CE") 

plot(J40$SE ~ J40$DFf2, main = "SE en fonction de la DFf2",
     xlab = "DFf2", ylab = "SE") 

cor(J40$RYT, J40$DFf2)
cor(J40$NE, J40$DFf2)
cor(J40$CE, J40$DFf2)
cor(J40$SE, J40$DFf2)

#J60
par(mfrow=c(2,2))
plot(J60$RYT ~ J60$DFf2, main = "RYT en fonction de la DFf2",
     xlab = "DFf2", ylab = "RYT") 

plot(J60$NE ~ J60$DFf2, main = "NE en fonction de la DFf2",
     xlab = "DFf2", ylab = "NE") 

plot(J60$CE ~ J60$DFf2, main = "CE en fonction de la DFf2",
     xlab = "DFf2", ylab = "CE") 

plot(J60$SE ~ J60$DFf2, main = "SE en fonction de la DFf2",
     xlab = "DFf2", ylab = "SE") 

cor(J60$RYT, J60$DFf2)
cor(J60$NE, J60$DFf2)
cor(J60$CE, J60$DFf2)
cor(J60$SE, J60$DFf2)

### la diversité fonctionnelle réalisée (ALL)

#J20
par(mfrow=c(2,2))
plot(J20$RYT ~ J20$DFr, main = "RYT en fonction de la DFr",
     xlab = "DFr", ylab = "RYT") 

plot(J20$NE ~ J20$DFr, main = "NE en fonction de la DFr",
     xlab = "DFr", ylab = "NE")

plot(J20$CE ~ J20$DFr, main = "CE en fonction de la DFr",
     xlab = "DFr", ylab = "CE") 

plot(J20$SE ~ J20$DFr, main = "SE en fonction de la DFr",
     xlab = "DFr", ylab = "SE") 

cor(J20$RYT, J20$DFr)
cor(J20$NE, J20$DFr)
cor(J20$CE, J20$DFr)
cor(J20$SE, J20$DFr)

#J40
par(mfrow=c(2,2))
plot(J40$RYT ~ J40$DFr, main = "RYT en fonction de la DFr",
     xlab = "DFr", ylab = "RYT") 

plot(J40$NE ~ J40$DFr, main = "NE en fonction de la DFr",
     xlab = "DFr", ylab = "NE") 

plot(J40$CE ~ J40$DFr, main = "CE en fonction de la DFr",
     xlab = "DFr", ylab = "CE") 

plot(J40$SE ~ J40$DFr, main = "SE en fonction de la DFr",
     xlab = "DFr", ylab = "SE") 

cor(J40$RYT, J40$DFr)
cor(J40$NE, J40$DFr)
cor(J40$CE, J40$DFr)
cor(J40$SE, J40$DFr)

#J60
par(mfrow=c(2,2))
plot(J60$RYT ~ J60$DFr, main = "RYT en fonction de la DFr",
     xlab = "DFr", ylab = "RYT") 

plot(J60$NE ~ J60$DFr, main = "NE en fonction de la DFr",
     xlab = "DFr", ylab = "NE") 

plot(J60$CE ~ J60$DFr, main = "CE en fonction de la DFr",
     xlab = "DFr", ylab = "CE") 

plot(J60$SE ~ J60$DFr, main = "SE en fonction de la DFr",
     xlab = "DFr", ylab = "SE") 

cor(J60$RYT, J60$DFr)
cor(J60$NE, J60$DFr)
cor(J60$CE, J60$DFr)
cor(J60$SE, J60$DFr)

###Dif entre DFf, DF2 et DFr J60
par(mfrow=c(1,1))

#Graph
moyDFf <- mean(J60$DFfALL)
moyDFf2 <- mean(J60$DFf2)
moyDFr <- mean(J60$DFr)
moyDF <- c(moyDFf, moyDFf2, moyDFr)

ETDFf <- sd(J60$DFfALL/sqrt(33))
ETDFf2 <- sd(J60$DFf2/sqrt(33))
ETDFr <- sd(J60$DFr/sqrt(33))
ETDF <- c(ETDFf, ETDFf2, ETDFr)

DFgraf <- barplot(moyDF, ylab = "Dispersion fonctionnelle", xlab = "indice de DF", names.arg = c("DFf", "DFf2", "DFr"),
                   main = "Différence entre les indices de DF", ylim = c(0, 10)) 
arrows(DFgraf, moyDF - ETDF, DFgraf, moyDF + ETDF, lwd = 2, angle = 90, length = 0.1, code = 3)


#Boxplot
setwd("C:/Users/polyc/OneDrive - UQAM/maîtrise en biologie/Expérimentation/Expérience 2/Données/résultats traits")
DFind <- read.csv("Diff.ind.csv", header = TRUE, sep = ";")
summary(DFind)
str(DFind)
library(ggplot2)
library(dplyr)
library(tidyverse)

Lm <- DFind %>% 
        filter (Composition == "LmSp")

Composition <- DFind$Composition
DF.ind <- DFind$DF.Ind.
Fdis <- DFind$Fdis
ggplot(DFind, aes(x = Composition,  y = Fdis, colour = DF.ind)) + geom_boxplot() + 
        ylab(" Dispersion fonctionnelle") + theme_classic()


#### modèles linéaires 
setwd("C:/Users/polyc/OneDrive - UQAM/maîtrise en biologie/Expérimentation/Expérience 2/Données")
J60 <- read.csv("J60.csv", header = TRUE, sep = ";")
J60$Bloc <- as.factor(J60$Bloc)


par(mfrow=c(1,3))


#NE
NEDFflm <- lm(NE ~ FFD + (1|Bloc), J60) 
plot(NE ~ FFD, data = J60) 
abline(NEDFflm)
summary(NEDFflm)

NEDFf2lm <- lm(NE ~ FFD2 + (1|Bloc), J60) 
plot(NE ~ FFD2, data = J60) 
abline(NEDFf2lm)
summary(NEDFf2lm)

NEDFrlm <- lm(NE ~ RFD + (1|Bloc), J60)
plot(NE ~ RFD, data = J60) 
abline(NEDFrlm)
summary(NEDFrlm)


#CE
CEDFflm <- lm(CE ~ FFD + (1|Bloc), J60) 
plot(CE ~ FFD, data = J60) 
abline(CEDFflm)
summary(CEDFflm)

CEDFf2lm <- lm(CE ~ FFD2 + (1|Bloc), J60)
plot(CE ~ FFD2, data = J60) 
abline(CEDFf2lm)
summary(CEDFf2lm)

CEDFrlm <- lm(CE ~ RFD + (1|Bloc), J60) 
plot(CE ~ RFD, data = J60) 
abline(CEDFrlm)
summary(CEDFrlm)

#SE
SEDFflm <- lm(SE ~ FFD + (1|Bloc), J60) 
plot(SE ~ FFD, data = J60, main = "Se en fonction de DFfond") 
abline(SEDFflm)
summary(SEDFflm)

SEDFf2lm <- lm(SE ~ FFD2 + (1|Bloc), J60)
plot(SE ~ FFD2, data = J60, main = "Se en fonction de DFfond2") 
abline(SEDFf2lm)
summary(SEDFf2lm)

SEDFrlm <- lm(SE ~ RFD + (1|Bloc), J60) 
plot(SE ~ RFD, data = J60, main = "Se en fonction de DFréalisée") 
abline(SEDFrlm)
summary(SEDFrlm)
###pas significaitf###



###Conditions d'application

##NEDFflm
#Valeurs extrêmes et homogéméité des variances
par(mfrow=c(2,2))
plot(rstudent(NEDFflm) ~ fitted(NEDFflm),main = "Résidus de student",
     ylab = "Résidus de Student", xlab = "Valeurs prédites")
##Pas de valeur extrême ni de patron particulier. 

#Normalité des résidus
qqnorm(rstudent(NEDFflm))
qqline(rstudent(NEDFflm))
shapiro.test(NEDFflm$residuals)
#la normalité semble respectée

#Effet de levier
HatNEDFflm <- hatvalues(NEDFflm)
plot(hatvalues(NEDFflm), ylim = c(0, 0.3), ylab = "Hat values",
     xlab = "Observations", main = "Effet de levier")
abline(h = 2 * mean(HatNEDFflm), lty = 2)
#certaines valeurs ont un effet de levier

#Influence des observations
plot(cooks.distance(NEDFflm), ylab = "Distance de Cook", xlab = "Observations",
     ylim = c(0, 0.2), main = "Influence des observations")
#pas de valeur supérieure à 1, donc pas d'influence particulière d'une observation


##NEDFf2lm
#Valeurs extrêmes et homogéméité des variances
par(mfrow=c(2,2))
plot(rstudent(NEDFf2lm) ~ fitted(NEDFf2lm),main = "Résidus de student",
     ylab = "Résidus de Student", xlab = "Valeurs prédites")
##Pas de valeur extrême ni de patron particulier. 

#Normalité des résidus
qqnorm(rstudent(NEDFf2lm))
qqline(rstudent(NEDFf2lm))
shapiro.test(NEDFf2lm$residuals)
#la normalité semble respectée

#Effet de levier
HatNEDFf2lm <- hatvalues(NEDFf2lm)
plot(hatvalues(NEDFf2lm), ylim = c(0, 0.3), ylab = "Hat values",
     xlab = "Observations", main = "Effet de levier")
abline(h = 2 * mean(HatNEDFf2lm), lty = 2)
#certaines valeurs ont un effet de levier

#Influence des observations
plot(cooks.distance(NEDFf2lm), ylab = "Distance de Cook", xlab = "Observations",
     ylim = c(0, 0.2), main = "Influence des observations")
#pas de valeur supérieure à 1, donc pas d'influence particulière d'une observation


##NEDFrlm
#Valeurs extrêmes et homogéméité des variances
par(mfrow=c(2,2))
plot(rstudent(NEDFrlm) ~ fitted(NEDFrlm),main = "Résidus de student",
     ylab = "Résidus de Student", xlab = "Valeurs prédites")
##Pas de valeur extrême ni de patron particulier. 

#Normalité des résidus
qqnorm(rstudent(NEDFrlm))
qqline(rstudent(NEDFrlm))
shapiro.test(NEDFrlm$residuals)
#la normalité semble respectée

#Effet de levier
HatNEDFrlm <- hatvalues(NEDFrlm)
plot(hatvalues(NEDFrlm), ylim = c(0, 0.3), ylab = "Hat values",
     xlab = "Observations", main = "Effet de levier")
abline(h = 2 * mean(HatNEDFrlm), lty = 2)
#certaines valeurs ont un effet de levier

#Influence des observations
plot(cooks.distance(NEDFrlm), ylab = "Distance de Cook", xlab = "Observations",
     ylim = c(0, 0.2), main = "Influence des observations")
#pas de valeur supérieure à 1, donc pas d'influence particulière d'une observation

##CEDFflm
#Valeurs extrêmes et homogéméité des variances
par(mfrow=c(2,2))
plot(rstudent(CEDFflm) ~ fitted(CEDFflm),main = "Résidus de student",
     ylab = "Résidus de Student", xlab = "Valeurs prédites")
##Pas de valeur extrême ni de patron particulier. 

#Normalité des résidus
qqnorm(rstudent(CEDFflm))
qqline(rstudent(CEDFflm))
shapiro.test(CEDFflm$residuals)
#la normalité semble respectée

#Effet de levier
HatCEDFflm <- hatvalues(CEDFflm)
plot(hatvalues(CEDFflm), ylim = c(0, 0.3), ylab = "Hat values",
     xlab = "Observations", main = "Effet de levier")
abline(h = 2 * mean(HatCEDFflm), lty = 2)
#certaines valeurs ont un effet de levier

#Influence des observations
plot(cooks.distance(CEDFflm), ylab = "Distance de Cook", xlab = "Observations",
     ylim = c(0, 0.2), main = "Influence des observations")
#pas de valeur supérieure à 1, donc pas d'influence particulière d'une observation

##CEDFf2lm
#Valeurs extrêmes et homogéméité des variances
par(mfrow=c(2,2))
plot(rstudent(CEDFf2lm) ~ fitted(CEDFf2lm),main = "Résidus de student",
     ylab = "Résidus de Student", xlab = "Valeurs prédites")
##Pas de valeur extrême ni de patron particulier. 

#Normalité des résidus
qqnorm(rstudent(CEDFf2lm))
qqline(rstudent(CEDFf2lm))
shapiro.test(CEDFf2lm$residuals)
#la normalité semble respectée

#Effet de levier
HatCEDFf2lm <- hatvalues(CEDFf2lm)
plot(hatvalues(CEDFf2lm), ylim = c(0, 0.3), ylab = "Hat values",
     xlab = "Observations", main = "Effet de levier")
abline(h = 2 * mean(HatCEDFf2lm), lty = 2)
#certaines valeurs ont un effet de levier

#Influence des observations
plot(cooks.distance(CEDFf2lm), ylab = "Distance de Cook", xlab = "Observations",
     ylim = c(0, 0.2), main = "Influence des observations")
#pas de valeur supérieure à 1, donc pas d'influence particulière d'une observation

##CEDFrlm
#Valeurs extrêmes et homogéméité des variances
par(mfrow=c(2,2))
plot(rstudent(CEDFrlm) ~ fitted(CEDFrlm),main = "Résidus de student",
     ylab = "Résidus de Student", xlab = "Valeurs prédites")
##Pas de valeur extrême ni de patron particulier. 

#Normalité des résidus
qqnorm(rstudent(CEDFrlm))
qqline(rstudent(CEDFrlm))
shapiro.test(CEDFrlm$residuals)
#la normalité semble respectée

#Effet de levier
HatCEDFrlm <- hatvalues(CEDFrlm)
plot(hatvalues(CEDFrlm), ylim = c(0, 0.3), ylab = "Hat values",
     xlab = "Observations", main = "Effet de levier")
abline(h = 2 * mean(HatCEDFrlm), lty = 2)
#certaines valeurs ont un effet de levier

#Influence des observations
plot(cooks.distance(CEDFrlm), ylab = "Distance de Cook", xlab = "Observations",
     ylim = c(0, 0.2), main = "Influence des observations")
#pas de valeur supérieure à 1, donc pas d'influence particulière d'une observation

##SEDFflm
#Valeurs extrêmes et homogéméité des variances
par(mfrow=c(2,2))
plot(rstudent(SEDFflm) ~ fitted(SEDFflm),main = "Résidus de student",
     ylab = "Résidus de Student", xlab = "Valeurs prédites")
##Pas de valeur extrême ni de patron particulier. 

#Normalité des résidus
qqnorm(rstudent(SEDFflm))
qqline(rstudent(SEDFflm))
shapiro.test(SEDFflm$residuals)
#la normalité n'est pas respectée

#Effet de levier
HatSEDFflm <- hatvalues(SEDFflm)
plot(hatvalues(SEDFflm), ylim = c(0, 0.3), ylab = "Hat values",
     xlab = "Observations", main = "Effet de levier")
abline(h = 2 * mean(HatSEDFflm), lty = 2)
#certaines valeurs ont un effet de levier

#Influence des observations
plot(cooks.distance(SEDFflm), ylab = "Distance de Cook", xlab = "Observations",
     ylim = c(0, 0.2), main = "Influence des observations")
#pas de valeur supérieure à 1, donc pas d'influence particulière d'une observation

##SEDFf2lm
#Valeurs extrêmes et homogéméité des variances
par(mfrow=c(2,2))
plot(rstudent(SEDFf2lm) ~ fitted(SEDFf2lm),main = "Résidus de student",
     ylab = "Résidus de Student", xlab = "Valeurs prédites")
##Pas de valeur extrême ni de patron particulier. 

#Normalité des résidus
qqnorm(rstudent(SEDFf2lm))
qqline(rstudent(SEDFf2lm))
shapiro.test(SEDFf2lm$residuals)
#la normalité n'est pas respectée

#Effet de levier
HatSEDFf2lm <- hatvalues(SEDFf2lm)
plot(hatvalues(SEDFf2lm), ylim = c(0, 0.3), ylab = "Hat values",
     xlab = "Observations", main = "Effet de levier")
abline(h = 2 * mean(HatSEDFf2lm), lty = 2)
#certaines valeurs ont un effet de levier

#Influence des observations
plot(cooks.distance(SEDFf2lm), ylab = "Distance de Cook", xlab = "Observations",
     ylim = c(0, 0.2), main = "Influence des observations")
#pas de valeur supérieure à 1, donc pas d'influence particulière d'une observation

##SEDFrlm
#Valeurs extrêmes et homogéméité des variances
par(mfrow=c(2,2))
plot(rstudent(SEDFrlm) ~ fitted(SEDFrlm),main = "Résidus de student",
     ylab = "Résidus de Student", xlab = "Valeurs prédites")
##Pas de valeur extrême ni de patron particulier. 

#Normalité des résidus
qqnorm(rstudent(SEDFrlm))
qqline(rstudent(SEDFrlm))
shapiro.test(SEDFrlm$residuals)
#la normalité n'est pas respectée

#Effet de levier
HatSEDFrlm <- hatvalues(SEDFrlm)
plot(hatvalues(SEDFrlm), ylim = c(0, 0.3), ylab = "Hat values",
     xlab = "Observations", main = "Effet de levier")
abline(h = 2 * mean(HatSEDFrlm), lty = 2)
#certaines valeurs ont un effet de levier

#Influence des observations
plot(cooks.distance(SEDFrlm), ylab = "Distance de Cook", xlab = "Observations",
     ylim = c(0, 0.2), main = "Influence des observations")
#pas de valeur supérieure à 1, donc pas d'influence particulière d'une observation





#### Modèle linéaire DFf-DFf2-DFr
setwd("C:/Users/polyc/OneDrive - UQAM/maîtrise en biologie/Expérimentation/Expérience 2/Données")
J60 <- read.csv("J60.csv", header = TRUE, sep = ";")

par(mfrow=c(2,3))
##NE
NEf.f2 <- lm(NE ~ DFf.DFf2 + (1|Bloc), J60) 
plot(NE ~ DFf.DFf2, data = J60, main = "Ne en fonction de DFfond-DFfond2") 
abline(NEf.f2)
summary(NEf.f2)

NEf.r <- lm(NE ~ DFf.DFr + (1|Bloc), J60) 
plot(NE ~ DFf.DFr, data = J60, main = "Ne en fonction de DFfond-DFr") 
abline(NEf.r)
summary(NEf.r)

NEf2.r <- lm(NE ~ DFf2.DFr + (1|Bloc), J60) 
plot(NE ~ DFf2.DFr, data = J60, main = "Ne en fonction de DFfond2-DFr") 
abline(NEf2.r)
summary(NEf2.r)

##CE
CEf.f2 <- lm(CE ~ DFf.DFf2 + (1|Bloc), J60) 
plot(CE ~ DFf.DFf2, data = J60, main = "Ce en fonction de DFfond-DFfond2") 
abline(CEf.f2)
summary(CEf.f2)

CEf.r <- lm(CE ~ DFf.DFr + (1|Bloc), J60) 
plot(CE ~ DFf.DFr, data = J60, main = "Ce en fonction de DFfond-DFr") 
abline(CEf.r)
summary(CEf.r)

CEf2.r <- lm(CE ~ DFf2.DFr + (1|Bloc), J60) 
plot(CE ~ DFf2.DFr, data = J60, main = "Ce en fonction de DFfond2-DFr") 
abline(CEf2.r)
summary(CEf2.r)


