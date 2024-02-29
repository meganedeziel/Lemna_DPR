setwd("C:/Users/polyc/OneDrive - UQAM/maîtrise en biologie/Expérimentation/Expérience 2/Données")
polycultures2 <- read.csv("Polycultures2.csv", header = TRUE, sep = ";")
J1 <- read.csv("J1.csv", header = TRUE, sep = ";")
J20 <- read.csv("J20.csv", header = TRUE, sep = ";")
J40 <- read.csv("J40.csv", header = TRUE, sep = ";")
J60 <- read.csv("J60.csv", header = TRUE, sep = ";")

library(ade4)
library(adespatial)
library(ape)
library(vegan)
library(MASS)
library(paleofire)
library(ggplot2)
library(factoextra)
library(ggthemes)
library(devtools)
library(ggpubr)
library(lme4)
library(nortest)

### RYT, Ne, Ce et SE en fonction du temps (figure)
par(mfrow=c(2,2))

##Boxplot
boxplot(polycultures2$RYT ~ polycultures2$Jours, main = "RYT en fonction du temps",
        xlab = "Jours de croissance", ylab = "RYT") 

boxplot(polycultures2$NE ~ polycultures2$Jours, main = "NE en fonction du temps",
        xlab = "Jours de croissance", ylab = "NE") 

boxplot(polycultures2$CE ~ polycultures2$Jours, main = "CE en fonction du temps",
        xlab = "Jours de croissance", ylab = "CE")  

boxplot(polycultures2$SE ~ polycultures2$Jours, main = "SE en fonction du temps",
        xlab = "Jours de croissance", ylab = "SE") 

##graphiques de moyennes
par(mfrow=c(1,3))

#RYT
moyRYTJ1 <- mean(J1$RYT)
moyRYTJ20 <- mean(J20$RYT)
moyRYTJ40 <- mean(J40$RYT)
moyRYTJ60 <- mean(J60$RYT)
moyRYT <- c(moyRYTJ1, moyRYTJ20, moyRYTJ40, moyRYTJ60)

ICRYTJ1 <- 0.005487 #basée sur le t test (IC.sup - IC. inf)/2
ICRYTJ20 <- 0.071455
ICRYTJ40 <- 0.0847965
ICRYTJ60 <- 0.167198
ICRYT <- c(ICRYTJ1, ICRYTJ20, ICRYTJ40, ICRYTJ60)

RYTgraf <- barplot(moyRYT, ylab = "RYT", xlab = "Jours de croissance", names.arg = c("1", "20", "40", "60"),
                   main = "Le RYT moyen en fonction du temps", ylim = c(0, 2)) 
arrows(RYTgraf, moyRYT - ICRYT, RYTgraf, moyRYT + ICRYT, lwd = 2, angle = 90, length = 0.1, code = 3)
abline(h = 1, lty = 2, lwd = 3, col = "#00CCCC")

#NE
moyNEJ1 <- mean(J1$NE)
moyNEJ20 <- mean(J20$NE)
moyNEJ40 <- mean(J40$NE)
moyNEJ60 <- mean(J60$NE)
moyNE <- c(moyNEJ1, moyNEJ20, moyNEJ40, moyNEJ60)

ICNEJ1 <- 0.42035785
ICNEJ20 <- 45.1658
ICNEJ40 <- 127.8593
ICNEJ60 <- 202.18225
ICNE <- c(ICNEJ1, ICNEJ20, ICNEJ40, ICNEJ60)

NEgraf <- barplot(moyNE, ylab = "NE", xlab = "Days of growth ", names.arg = c("1", "20", "40", "60"),
                   main = "Net biodiversity effect", ylim = c(-200, 1000), cex.axis = 1.3, cex.names = 1.3, cex.main = 1.8, cex.lab = 1.5)
arrows(NEgraf, moyNE - ICNE, NEgraf, moyNE + ICNE, lwd = 2, angle = 90, length = 0.1, code = 3)
abline(h = 0, lty = 2, lwd = 3, col = "#0066CC")



#CE
moyCEJ1 <- mean(J1$CE)
moyCEJ20 <- mean(J20$CE)
moyCEJ40 <- mean(J40$CE)
moyCEJ60 <- mean(J60$CE)
moyCE <- c(moyCEJ1, moyCEJ20, moyCEJ40, moyCEJ60)

ICCEJ1 <- 0.41342825
ICCEJ20 <- 40.227915
ICCEJ40 <- 97.632
ICCEJ60 <- 275.22065
ICCE <- c(ICCEJ1, ICCEJ20, ICCEJ40, ICCEJ60)

CEgraf <- barplot(moyCE, ylab = "CE", xlab = "Days of growth", names.arg = c("1", "20", "40", "60"),
                  main = "Complementarity effect", ylim = c(-200, 1000), cex.axis = 1.3, cex.names = 1.3, cex.main = 1.8, cex.lab = 1.5) 
arrows(CEgraf, moyCE - ICCE, CEgraf, moyCE + ICCE, lwd = 2, angle = 90, length = 0.1, code = 3)
abline(h = 0, lty = 2, lwd = 3, col = "#0066CC")

#SE
moySEJ1 <- mean(J1$SE)
moySEJ20 <- mean(J20$SE)
moySEJ40 <- mean(J40$SE)
moySEJ60 <- mean(J60$SE)
moySE <- c(moySEJ1, moySEJ20, moySEJ40, moySEJ60)

ICSEJ1 <- 0.1933158
ICSEJ20 <- 18.2512725
ICSEJ40 <- 63.43974
ICSEJ60 <- 165.701265
ICSE <- c(ICSEJ1, ICSEJ20, ICSEJ40, ICSEJ60)

SEgraf <- barplot(moySE, ylab = "SE", xlab = "Days of growth", names.arg = c("1", "20", "40", "60"),
                  main = "Selection effect", ylim = c(-500, 500), cex.axis = 1.3, cex.names = 1.3, cex.main = 1.8, cex.lab = 1.5) 
arrows(SEgraf, moySE - ICSE, SEgraf, moySE + ICSE, lwd = 2, angle = 90, length = 0.1, code = 3)
abline(h = 0, lty = 2, lwd = 3, col = "#0066CC")



###Anova diff en fonction du temps (jours de croissance)
setwd("C:/Users/polyc/OneDrive - UQAM/maîtrise en biologie/Expérimentation/Expérience 2/Données")
polycultures2 <- read.csv("Polycultures2.csv", header = TRUE, sep = ";")
polycultures2$Jours <- as.factor(polycultures2$Jours)
polycultures2$Bloc <- as.factor(polycultures2$Bloc)


#NE
anoNE <- aov(NE ~ Jours + Bloc , data = polycultures2)
summary(anoNE)
TukeyHSD(anoNE, which = "Jours" )  

#CE
anoCE <- aov(CE ~ Jours + Bloc , data = polycultures2)
summary(anoCE)
TukeyHSD(anoCE, which = "Jours" )  

#SE
anoSE <- aov(SE ~ Jours + Bloc , data = polycultures2)
summary(anoSE)
TukeyHSD(anoSE, which = "Jours" )  

##conditions d'application ***Ne sont pas respectées 
par(mfrow=c(2,2))

#AnoNE
plot(anoNE)
shapiro.test(anoNE$residuals)

#AnoCE
plot(anoCE)
shapiro.test(anoCE$residuals)

#AnoSE
plot(anoSE)
shapiro.test(anoSE$residuals)




###Ttests Diff de 1 ou 0 à chaque temps. 

#RYT
TtestRYT1 <- t.test(J1$RYT, mu = 1)
TtestRYT1

TtestRYT20 <- t.test(J20$RYT, mu = 1)
TtestRYT20

TtestRYT40 <- t.test(J40$RYT, mu = 1)
TtestRYT40

TtestRYT60 <- t.test(J60$RYT, mu = 1)
TtestRYT60

#NE
TtestNE1 <- t.test(J1$NE, mu = 0)
TtestNE1

TtestNE20 <- t.test(J20$NE, mu = 0)
TtestNE20

TtestNE40 <- t.test(J40$NE, mu = 0)
TtestNE40

TtestNE60 <- t.test(J60$NE, mu = 0)
TtestNE60

#SE
TtestSE1 <- t.test(J1$SE, mu = 0)
TtestSE1

TtestSE20 <- t.test(J20$SE, mu = 0)
TtestSE20

TtestSE40 <- t.test(J40$SE, mu = 0)
TtestSE40

TtestSE60 <- t.test(J60$SE, mu = 0)
TtestSE60

#CE
TtestCE1 <- t.test(J1$CE, mu = 0)
TtestCE1

TtestCE20 <- t.test(J20$CE, mu = 0)
TtestCE20

TtestCE40 <- t.test(J40$CE, mu = 0)
TtestCE40

TtestCE60 <- t.test(J60$CE, mu = 0)
TtestCE60



###Conditions d'application
par(mfrow=c(1,1))
#NE
NE1res <- J1$NE - mean(J1$NE)
NE1res
shapiro.test(NE1res)
ad.test(NE1res)
qqnorm(NE1res)
qqline(NE1res)

NE20res <- J20$NE - mean(J20$NE)
NE20res
shapiro.test(NE20res)
ad.test(NE20res)
qqnorm(NE20res)
qqline(NE20res)

NE40res <- J40$NE - mean(J40$NE)
NE40res
shapiro.test(NE40res)
ad.test(NE40res)
qqnorm(NE40res)
qqline(NE40res)

NE60res <- J60$NE - mean(J60$NE)
NE60res
shapiro.test(NE60res)
ad.test(NE60res)
qqnorm(NE60res)
qqline(NE60res)

#SE
SE1res <- J1$SE - mean(J1$SE)
SE1res
shapiro.test(SE1res)
ad.test(SE1res)
qqnorm(SE1res)
qqline(SE1res)

SE20res <- J20$SE - mean(J20$SE)
SE20res
shapiro.test(SE20res)
ad.test(SE20res)
qqnorm(SE20res)
qqline(SE20res)

SE40res <- J40$SE - mean(J40$SE)
SE40res
shapiro.test(SE40res)
ad.test(SE40res)
qqnorm(SE40res)
qqline(SE40res)

SE60res <- J60$SE - mean(J60$SE)
SE60res
shapiro.test(SE60res)
ad.test(SE60res)
qqnorm(SE60res)
qqline(SE60res)

#CE
CE1res <- J1$CE - mean(J1$CE)
CE1res
shapiro.test(CE1res)
ad.test(CE1res)
qqnorm(CE1res)
qqline(CE1res)

CE20res <- J20$CE - mean(J20$CE)
CE20res
shapiro.test(CE20res)
ad.test(CE20res)
qqnorm(CE20res)
qqline(CE20res)

CE40res <- J40$CE - mean(J40$CE)
CE40res
shapiro.test(CE40res)
ad.test(CE40res)
qqnorm(CE40res)
qqline(CE40res)

CE60res <- J60$CE - mean(J60$CE)
CE60res
shapiro.test(CE60res)
ad.test(CE60res)
qqnorm(CE60res)
qqline(CE60res)



###Quels mélanges offrent les meilleurs rendement? 

#RYT
boxplot(J20$RYT ~ J20$Composition, main = "RYT en fonction de de la composition au jour 20",
        xlab = "Composition", ylab = "RYT", cex.axis = 0.7) 

boxplot(J40$RYT ~ J40$Composition, main = "RYT en fonction de de la composition au jour 40",
        xlab = "Composition", ylab = "RYT", cex.axis = 0.7) 

boxplot(J60$RYT ~ J60$Composition, main = "RYT en fonction de de la composition au jour 60",
        xlab = "Composition", ylab = "RYT", cex.axis = 0.7) 

#NE
boxplot(J20$NE ~ J20$Composition, main = "NE en fonction de de la composition au jour 20",
        xlab = "Composition", ylab = "NE", cex.axis = 0.7) 

boxplot(J40$NE ~ J40$Composition, main = "NE en fonction de de la composition au jour 40",
        xlab = "Composition", ylab = "NE", cex.axis = 0.7) 

boxplot(J60$NE ~ J60$Composition, main = "NE en fonction de de la composition au jour 60",
        xlab = "Composition", ylab = "NE", cex.axis = 0.7)

#CE
boxplot(J20$CE ~ J20$Composition, main = "CE en fonction de de la composition au jour 20",
        xlab = "Composition", ylab = "CE", cex.axis = 0.7) 

boxplot(J40$CE ~ J40$Composition, main = "CE en fonction de de la composition au jour 40",
        xlab = "Composition", ylab = "CE", cex.axis = 0.7) 

boxplot(J60$CE ~ J60$Composition, main = "CE en fonction de de la composition au jour 60",
        xlab = "Composition", ylab = "CE", cex.axis = 0.7)

#SE
boxplot(J20$SE ~ J20$Composition, main = "SE en fonction de de la composition au jour 20",
        xlab = "Composition", ylab = "SE", cex.axis = 0.7) 

boxplot(J40$SE ~ J40$Composition, main = "SE en fonction de de la composition au jour 40",
        xlab = "Composition", ylab = "SE", cex.axis = 0.7) 


boxplot(J60$SE ~ J60$Composition, main = "SE en fonction de de la composition au jour 60",
        xlab = "Composition", ylab = "SE", cex.axis = 0.7)


