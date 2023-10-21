### La biomasse finale des cultures à 60 jours en fonction de la composition des mélanges (figures)
setwd("C:/Users/polyc/OneDrive - UQAM/maîtrise en biologie/Expérimentation/Expérience 2/Données")
Cultures20 <- read.csv("CulturesJ20.csv", header = TRUE, sep = ";")
Cultures40 <- read.csv("CulturesJ40.csv", header = TRUE, sep = ";")
Cultures60 <- read.csv("CulturesJ60.csv", header = TRUE, sep = ";")

boxplot(Cultures20$Biomassef ~ Cultures20$Composition, main = "La biomasse finale en fonction de la composition au J20",
        xlab = "Composition", ylab = "Biomasse (mg)", cex.axis = 0.5)

boxplot(Cultures40$Biomassef ~ Cultures40$Composition, main = "La biomasse finale en fonction de la composition au J40",
        xlab = "Composition", ylab = "Biomasse (mg)", cex.axis = 0.5)

boxplot(Cultures60$Biomassef ~ Cultures60$Composition, main = "La biomasse finale en fonction de la composition au J60",
        xlab = "Composition", ylab = "Biomasse (mg)", cex.axis = 0.5) 

##Graphique de moyennes
#J20
#J40
#J60
moyB60 <- c(3196.26666666667,
            2334.5,
            3305.03333333333,
            136.366666666667,
            2932.6,
            2809.2,
            2788.8,
            2940.4,
            2676.66666666667,
            2963.06666666667,
            2951.26666666667,
            2420.36666666667,
            2465.56666666667,
            2528.00,
            3093.53333333333)
SDB60 <- c(139.174758247806,
           161.514117030062,
           168.606356147488,
           37.1395117541054,
           108.175413102987,
           228.187970760958,
           160.713907301145,
           40.1622957510887,
           191.415159622569,
           264.921504097589,
           242.098024224352,
           235.322955389678,
           179.14118268375,
           69.5832594809859,
           100.71153525457)


B60graf <- barplot(moyB60, ylab = "Biomasse", xlab = "Composition", names.arg = c("Lm", "Sp", "Wc", "Lt", "LmSp", "LmWc", "LmLt", "SpWc", "SpLt", "WcLt", "LmSpWc", "LmSpLt", "LmWcLt", "SpWcLt", "LmSpWcLt"),
                   main = "La biomasse en fonction de la composition au J60", ylim = c(100, 3500), cex.names = 0.53) 
arrows(B60graf, moyB60 - SDB60, B60graf, moyB60 + SDB60, lwd = 2, angle = 90, length = 0.1, code = 3)

