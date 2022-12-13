#######  Example 1 #######

####  install packages - only required if not installed already ####
install.packages(c("readxl", "data.table", "ggplot2", "patchwork", "ggbeeswarm", "report"))

####  load required libraries ####
# loading libraries seperately
library("readxl") # import excel files
library("data.table") # efficient caclulations/summaries in data.table format
library("ggplot2") # plotting library
library("patchwork") # construct layout for plots
library("ggbeeswarm") # add beeswarm plot to ggplot2
library("report")

# alternatively load everything in one line:
# lapply(X =  c("readxl", "data.table", "ggplot2", "patchwork", "ggbeeswarm"), FUN = library, character.only=T)

####  import data from file ####
DataPPR <- read_excel("Data/DataPPR.xlsx")

####  inspect data  ####
View(DataPPR) # View - for small data sets
str(DataPPR) # str - show structure
summary(DataPPR) # summary - summary statistics of columns

####  convert to data.table ####
setDT(DataPPR) # convert to data.table
str(DataPPR) # data unchanged but no as data.table (modifiable by "reference")
print(x = DataPPR, nrows = 5) # show top 5 and bottom 5 rows

#### add meaning/levels to Group ####
DataPPR[,Group := factor(x = Group, levels = c("WT", "KO")),]

####  reshape data to make it easier to use  ####
DT <- melt.data.table(
  data = as.data.table(DataPPR),
  id.vars = c("Slice", "Group"),
  measure.vars = c("Pulse1", "Pulse2"),
  variable.name = "Pulse",
  value.name = "Amplitude"
)

# DT[ i,  j,  by ] # + extra arguments
#     |   |   |
#     |   |    -------> grouped by what?
#     |    -------> what to do?
#     ---> on which rows?

####  substitute string (delete "Pulse" in the words "Pulse1" and "Pulse2") ####
DT[, Pulse := gsub(pattern = "Pulse", replacement = "", x = Pulse), ]

####  make plot for amplitudes ####

AmplitudePlot <- ggplot(data = DT, aes(x = as.factor(Pulse), y = Amplitude, group = Slice, colour = Group)) + # input for plot
  geom_point(alpha = 0.5, size = 4) + # add points
  geom_line() + # add layer with lines
  facet_wrap(facets = ~Group) + # split plot by "Group" variable
  scale_y_continuous(name = "Amplitude (mV)", limits = c(0, 5), expand = c(0, 0)) + # add information to y axis
  scale_x_discrete(name = "Pulse Number") + # add name to x axis
  scale_colour_manual(values = c("WT" = "black", "KO" = "red"), name = "") + # define colours manually, add name, remove legend title
  theme_classic() + # use "classic" theme
  theme(strip.background = element_blank(), strip.placement = "outside") # change elements of theme (remove background from facet label and shift text)

DataPPR[, PPR := Pulse2/Pulse1, ]

PPRPlot <- ggplot(data = DataPPR, aes(
  x = Group,
  y = PPR,
  group = Group,
  colour = Group
)) +
  ggbeeswarm::geom_beeswarm(size = 4, alpha = 0.5, cex = 6) +
  scale_y_continuous(name = "PPR") +
  scale_x_discrete(name = "", position = "top") +
  scale_colour_manual(values = c("WT" = "black", "KO" = "red"), name = "") +
  theme_classic() +
  theme(legend.position = "None", axis.line.x = element_blank(), axis.ticks.x = element_blank())

AmplitudePlot + PPRPlot + plot_annotation(tag_levels = "A") + plot_layout(widths = c(2, 1), guides = "collect") &
  theme(
    axis.text = element_text(size = 16, colour = "black"),
    strip.text = element_text(size = 16),
    axis.title = element_text(size = 18),
    plot.tag = element_text(size = 22)
  )


##### Actual analysis starts here!
#### check assumptions (normal, variance homogeneity, independent)
boxplot(formula = PPR ~ Group, data=DataPPR) # get an impression for variance and normality
var.test(formula = PPR ~ Group, data=DataPPR) # check if data violates variance homogeneity

ggplot(data = DataPPR, aes(sample=PPR))+  #qq plot for normality
  stat_qq()+
  stat_qq_line()+
  facet_wrap(~Group)+
  theme_minimal()

DataPPR[,shapiro.test(PPR),by=Group] # alternatively formal test - with caution

#### perform t-test
TTest <- t.test(PPR ~ Group, data=DataPPR, alternative = "two.sided", var.equal = F) # var.equal = F is also default so you don't necessarily need to write it

####  report results
report(TTest)

TTest # alternatively just return the test - not every function is supported by the "report" package





