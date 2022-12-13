#######  Example 2 #######

####  load required libraries ####
# loading libraries seperately
library("data.table") # efficient caclulations/summaries in data.table format
library("ggplot2") # plotting library
library("patchwork") # construct layout for plots
library("ggbeeswarm") # add beeswarm plot to ggplot2
library("report")
library("emmeans")

# alternatively load everything in one line:
# lapply(X =  c("readxl", "data.table", "ggplot2", "patchwork", "ggbeeswarm"), FUN = library, character.only=T)

####  import data from file ####
NorDT <- fread(file = "Data/NORData.csv")

####  inspect data  ####
summary(NorDT) # summary - summary statistics of columns

#### check assumptions - for now variance homogeneity

boxplot(DiscrimationIndex ~ Group, NorDT) # quick visual check
fligner.test(DiscrimationIndex ~ Group, NorDT) # formal test

#### start with lm()

LmNOR <- lm(formula = DiscrimationIndex ~ Group, data = NorDT)

#### check for validity of model
plot(LmNOR)
shapiro.test(LmNOR$residuals)

#### run ANOVA
AnovaTest <- anova(LmNOR)
report(AnovaTest)

#### post-hoc test 
MeanEstimates <- emmeans(object = LmNOR, specs = "Group") # calculate marginal means
contrast(MeanEstimates, method = "trt.vs.ctrl", ref="WT") # for help type: ?`contrast-methods`

####  make plot 

ggplot(data = NorDT, aes(x=Group, y=DiscrimationIndex, colour=Group))+
  geom_beeswarm(cex = 3, size=2)+
  geom_errorbar(data = NorDT[,.(DiscrimationIndex=mean(DiscrimationIndex)),by=Group],
                aes(ymin = DiscrimationIndex, ymax = DiscrimationIndex),
                width = 0.6)+ 
  scale_y_continuous(name = "Discrimination Index", limits = c(-0.3,0.3))+
  xlab(label = "")+
  scale_colour_manual(values = c("WT"="black", "GFP"="#aa2233", "hm4Di" = "#1E88E5"))+
  theme_classic()+
  theme(axis.line.x = element_blank(), axis.ticks.x = element_blank())




