setwd("C:/Users/polyc/OneDrive - UQAM/ma?trise en biologie/Exp?rimentation/Exp?rience 2/Donn?es")
data <- read.csv("Alex-AnneData.csv")

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
library(emmeans)

data<-Alex_AnneData

data <-data [,-1]
data$Bloc <- as.factor (data$Bloc)
data$Jours <- as.factor (data$Jours)
data$Composition <- as.factor (data$Composition)

# Plot Raw data - NE against Jours
PlotNE <- ggplot(data, aes(x = Jours, y = NE)) + 
  geom_jitter(position=position_jitter(0.2)) + 
  stat_summary(fun.data="mean_sdl",  fun.args = list(mult=1), #mean_sdl computes the mean plus or minus a constant times the standard deviation (here the constant (mult))
               geom="pointrange", color = "red")+
  theme_classic() +
  labs(x="days", y="NE") 

PlotNE # We can see clearly how the variance of NE increases over time

# Model testing the effect of Jours on NE with Composition and Bloc as random factors and weighted by Jours.
# For adding crossed random factors using lme
data$dummy <- factor(1)

# Model
modNE = lme (NE ~ Jours, 
              random=list(dummy = pdBlocked(list(pdIdent(~Composition-1),
                                                 pdIdent(~Bloc-1)))), 
              data=data,
              weights = varIdent(form = ~1 | Jours),method="REML",control =list(msMaxIter = 1000, msMaxEval = 1000))

# Anova and Summary of the model
anova(modNE)
summary(modNE)

# Model validation
par(mfrow=c(1,2))
plot(fitted(modNE), resid(modNE,type="pearson"))
qqnorm(resid(modNE,type="pearson"))
qqline(resid(modNE,type="pearson"))


# Post-Hoc analysis - Tukey
contrast <- emmeans(modNE, "Jours")
pairs(contrast)




# Plot Raw data - CE against Jours
PlotCE <- ggplot(data, aes(x = Jours, y = CE)) + 
  geom_jitter(position=position_jitter(0.2)) + 
  stat_summary(fun.data="mean_sdl",  fun.args = list(mult=1), 
               geom="pointrange", color = "red")+
  theme_classic() +
  labs(x="Days", y="CE") 

PlotCE 

# Model testing the effect of Jours on CE with Composition and Bloc as random factors and weighted by Days.
# For adding crossed random factors using lme
data$dummy <- factor(1)

# Model
modCE = lme (CE ~ Jours, 
             random=list(dummy = pdBlocked(list(pdIdent(~Composition-1),
                                                pdIdent(~Bloc-1)))), 
             data=data,
             weights = varIdent(form = ~1 | Jours),method="REML",control =list(msMaxIter = 1000, msMaxEval = 1000))

# Anova and Summary of the model
anova(modCE)
summary(modCE)

# Model validation
par(mfrow=c(1,2))
plot(fitted(modCE), resid(modCE,type="pearson"))
qqnorm(resid(modCE,type="pearson"))
qqline(resid(modCE,type="pearson"))


# Post-Hoc analysis - Tukey
contrast <- emmeans(modCE, "Jours")
pairs(contrast)




# Plot Raw data - SE against Jours
PlotSE <- ggplot(data, aes(x = Jours, y = SE)) + 
  geom_jitter(position=position_jitter(0.2)) + 
  stat_summary(fun.data="mean_sdl",  fun.args = list(mult=1), 
               geom="pointrange", color = "red")+
  theme_classic() +
  labs(x="Days", y="SE") 

PlotSE

# Model testing the effect of Jours on SE with Composition and Bloc as random factors and weighted by Jours.
# For adding crossed random factors using lme
data$dummy <- factor(1)

# Model
modSE = lme (SE ~ Jours, 
             random=list(dummy = pdBlocked(list(pdIdent(~Composition-1),
                                                pdIdent(~Bloc-1)))), 
             data=data,
             weights = varIdent(form = ~1 | Jours),method="REML",control =list(msMaxIter = 1000, msMaxEval = 1000))

# Anova and Summary of the model
anova(modSE)
summary(modSE)

# Model validation
par(mfrow=c(2,2))
plot(fitted(modSE), resid(modSE,type="pearson"))
qqnorm(resid(modSE,type="pearson"))
qqline(resid(modSE,type="pearson"))


# Post-Hoc analysis - Tukey
contrast <- emmeans(modSE, "Jours")
pairs(contrast)

