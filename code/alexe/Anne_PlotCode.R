library(ggplot2)

## PROPORTIONS 

setwd("C:/Users/polyc/OneDrive - UQAM/maîtrise en biologie/Expérimentation/Expérience 2/Données")

# Create two different data frames. One at species level and the other at community level
# The data frame at community level contain the SD and SE as they are the info over the total
DataSpecies <- read.csv("DataSpecies.csv", header = TRUE, sep = ";") 
DataCommunities <- read.csv("DataCom.csv", header = TRUE, sep = ";")

# This is to order manually the communities based on their total biomass.
# Both data frame must match to plot correctly
DataSpecies$Composition=factor(DataSpecies$Composition,levels=c("Lt","Sp","Lm","Wc","SpLt","SpWc",
                        "LmSp","LmLt","LmSpWc","LmWc","WcLt","SpWcLt","LmSpLt","LmWcLt","LmSpWcLt"))

DataCommunities$Composition=factor(DataCommunities$Composition,levels=c("Lt","Sp","Lm","Wc","SpLt","SpWc",
                        "LmSp","LmLt","LmSpWc","LmWc","WcLt","SpWcLt","LmSpLt","LmWcLt","LmSpWcLt"))

# Plot the figure
ggplot()+
  geom_bar(data=DataSpecies,aes(x=Composition, y=Biomass, group=Species,fill=Species),
           stat="identity", position="stack") + # This plots the stacked bars showing
  # species Biomass within communities.
  geom_errorbar(data=DataCommunities,aes(x=Composition, y=Biomass,ymin=Biomass-SE,ymax=Biomass+SE), 
                width=.2, position=position_dodge(.9)) + # This plots the errorbars (SD) 
  # over the total Biomasse at community level.
  theme_classic() +# This is just to leave the background white.
  theme (axis.text.x = element_text (angle=45, hjust = 1)) # And this just to write
# "composition" names (x-axis) in an angle of 45 degrees (Clearer).



#### SECOND OPTION AT COMMUNITY LEVEL ####


# We create a new variable - NUMBER OF SPECIES
DataCommunities[,5]=2 # Create the new column
colnames(DataCommunities)[5] <- "nbsp" # Rename column 
DataCommunities$nbsp [DataCommunities$Composition=="Lm"]<-1 #Give the value to each community
DataCommunities$nbsp [DataCommunities$Composition=="Sp"]<-1
DataCommunities$nbsp [DataCommunities$Composition=="Wc"]<-1
DataCommunities$nbsp [DataCommunities$Composition=="Lt"]<-1
DataCommunities$nbsp [DataCommunities$Composition=="LmSpLt"]<-3
DataCommunities$nbsp [DataCommunities$Composition=="LmWcLt"]<-3
DataCommunities$nbsp [DataCommunities$Composition=="SpWcLt"]<-3
DataCommunities$nbsp [DataCommunities$Composition=="LmSpWcLt"]<-4

DataCommunities$nbsp<-as.factor(DataCommunities$nbsp) # Convert nbsp to a factor


ggplot(DataCommunities %>% arrange(nbsp, M) %>%
         dplyr::mutate(Composition=factor(Composition, levels=Composition)), # This orders
       # the communities in an increasing gradient based on their M and grouped by nbsp
       aes(x=Composition,y=M, fill = nbsp))+ #define axis and fill bars in a color depending
       # on the nbsp level
  geom_bar(stat="identity", show.legend=FALSE) + # Call the bars
  geom_errorbar(data=DataCommunities, aes(x=Composition, y=M,
                                      ymin = M-SD, ymax = M+SD),width=.2,
                position=position_dodge(.9)) + # Now the errorbars. Define min and max (SD values)
  theme_classic() + # This is just to leave the background white.
  theme (axis.text.x = element_text (angle=45, hjust = 1)) + # And this just to write
  # "composition" names (x-axis) in an angle of 45 degrees (Clearer).
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", "26B2E9")) # Define the colours
  # you want




