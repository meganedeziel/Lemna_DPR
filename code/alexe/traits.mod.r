#### modèles linéaires Ne, Ce et SE en fonctions des indices de DF
setwd("C:/Users/polyc/OneDrive - UQAM/maîtrise en biologie/Expérimentation/Expérience 2/Données")
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
library(nlme)
library(MuMIn)

J60$Bloc <- as.factor (J60$Bloc)
J60$Composition <- as.factor (J60$Composition)

J60 <-J60 [,-1]
J60$dummy <- factor(1)

#### tranformer les données 

##NE en fonction de la DF fondamentale
modNEFFD = lme (NE ~ FFD, random=list(dummy = pdBlocked(list(pdIdent(~Composition-1), pdIdent(~Bloc-1)))), data=J60)
summary(modNEFFD)
r.squaredGLMM(modNEFFD)
plot(NE ~ FFD, data = J60) 
abline(modNEFFD)

#validation du modèle
par(mfrow=c(1,2))
plot(fitted(modNEFFD), resid(modNEFFD,type="pearson"))
qqnorm(resid(modNEFFD,type="pearson"))
qqline(resid(modNEFFD,type="pearson"))
shapiro.test(modNEFFD$residuals)


##NE en fonction de la DF fondamentale 2
modNEFFD2 = lme (NE ~ FFD2, random=list(dummy = pdBlocked(list(pdIdent(~Composition-1), pdIdent(~Bloc-1)))), data=J60)
summary(modNEFFD2)
r.squaredGLMM(modNEFFD2)
plot(NE ~ FFD2, data = J60) 
abline(modNEFFD2)

#validation du modèle
par(mfrow=c(1,2))
plot(fitted(modNEFFD2), resid(modNEFFD2,type="pearson"))
qqnorm(resid(modNEFFD2,type="pearson"))
qqline(resid(modNEFFD2,type="pearson"))
shapiro.test(modNEFFD2$residuals)

##NE en fonction de la DF réalisée
modNERFD = lme (NE ~ RFD, random=list(dummy = pdBlocked(list(pdIdent(~Composition-1), pdIdent(~Bloc-1)))), data=J60)
summary(modNERFD)
r.squaredGLMM(modNERFD)
plot(NE ~ RFD, data = J60) 
abline(modNERFD)

#validation du modèle
par(mfrow=c(1,2))
plot(fitted(modNERFD), resid(modNERFD,type="pearson"))
qqnorm(resid(modNERFD,type="pearson"))
qqline(resid(modNERFD,type="pearson"))
shapiro.test(modNERFD$residuals)




##CE en fonction de la DF fondamentale
modCEFFD = lme (CE ~ FFD, random=list(dummy = pdBlocked(list(pdIdent(~Composition-1), pdIdent(~Bloc-1)))), data=J60)
summary(modCEFFD)
r.squaredGLMM(modCEFFD)
plot(CE ~ FFD, data = J60) 
abline(modCEFFD)

#validation du modèle
par(mfrow=c(1,2))
plot(fitted(modCEFFD), resid(modNEFFD,type="pearson"))
qqnorm(resid(modCEFFD,type="pearson"))
qqline(resid(modCEFFD,type="pearson"))
shapiro.test(modCEFFD$residuals)


##CE en fonction de la DF fondamentale 2
modCEFFD2 = lme (CE ~ FFD2, random=list(dummy = pdBlocked(list(pdIdent(~Composition-1), pdIdent(~Bloc-1)))), data=J60)
summary(modCEFFD2)
r.squaredGLMM(modCEFFD2)
plot(CE ~ FFD2, data = J60) 
abline(modCEFFD2)

#validation du modèle
par(mfrow=c(1,2))
plot(fitted(modCEFFD2), resid(modCEFFD2,type="pearson"))
qqnorm(resid(modCEFFD2,type="pearson"))
qqline(resid(modCEFFD2,type="pearson"))
shapiro.test(modCEFFD2$residuals)

##CE en fonction de la DF réalisée
modCERFD = lme (CE ~ RFD, random=list(dummy = pdBlocked(list(pdIdent(~Composition-1), pdIdent(~Bloc-1)))), data=J60)
summary(modCERFD)
r.squaredGLMM(modCERFD)
plot(CE ~ RFD, data = J60) 
abline(modCERFD)

#validation du modèle
par(mfrow=c(1,2))
plot(fitted(modCERFD), resid(modCERFD,type="pearson"))
qqnorm(resid(modCERFD,type="pearson"))
qqline(resid(modCERFD,type="pearson"))
shapiro.test(modCERFD$residuals)

