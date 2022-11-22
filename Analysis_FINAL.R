library(tidyverse)
library(dotwhisker)
library(ggplot2)

#This is all of my efforts to visualize the data - should be mirrored in each experiment
#I explored relationships and played with the data here - not all of this is important/relevant to the story
#I ended up using Graphpad Prism for my other data as the R plots never looked that great


#############################
##############################
#######               #########
####### Experiment #1 ##########
#######               #########
##############################
#############################

Dynamic_Data <- read.csv(file="Experiment_1_DATA.csv", header=T)

## Treatment as a factor (instead of numeric)
Dynamic_Data$Treatment <- as.factor(Dynamic_Data$Treatment)
#Dynamic_Data$Time_Start <- as.numeric(Dynamic_Data$Time_Start)

####################
### Linear Model ###
####################

Dynamic_Model <- lm(Tpref ~ Treatment, data=Dynamic_Data)
summary(Dynamic_Model)

## Including interaction ##
Interaction_Model1 <- lm(Tpref ~ Treatment*Weight.g., data=Dynamic_Data) ## BEST MODEL SO FAR ##
summary(Interaction_Model1)

Interaction_Model3 <- lm(Tpref ~ Treatment*Length.mm., data=Dynamic_Data) ## BEST MODEL SO FAR ## 
summary(Interaction_Model3)

#############
### ANOVA ###
#############

anova1 <- aov(Tpref ~ Treatment, data = Dynamic_Data)
summary(anova1)

anova1.2 <- aov(Passage. ~ Treatment, data = Dynamic_Data)
summary(anova1.2)

anova1.3 <- aov(Distance.m. ~ Treatment, data = Dynamic_Data)
summary(anova1.3)

anova1.4 <- aov(Length.mm. ~ Treatment, data = Dynamic_Data)
summary(anova1.4)

NEW_DATA <- na.omit(Dynamic_Data)
anova_passage <- aov(Passage. ~ Treatment, data = NEW_DATA)
summary(anova_passage)
TukeyHSD(anova_passage)

library(ANOVAreplication)
data1 <- data.frame(Dynamic_Data$Tpref, Dynamic_Data$Treatment)
pooled.sd(data1)

data1.2 <- data.frame(Dynamic_Data$Distance.m., Dynamic_Data$Treatment)
pooled.sd(data1.2)

#####################
### Visualization ### --> think of a way to incorporate treatment as color for ggplots
#####################

#Colorblind safe color pallate#
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#Treatment vs Tpref
(ggplot(Dynamic_Data, aes(x=Treatment, y=Tpref, color=Treatment))
  +geom_boxplot(size=1)
  +theme( axis.line = element_line(colour = "black", size = 0.5, linetype = "solid"))
  +theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  +theme_classic()
  +theme(legend.position='none')
  +scale_y_continuous(name="Tpref (°C)", limits=c(7, 23))
  +scale_colour_manual(values=cbPalette)
  +xlab("Treatment (°C)")
)

#### effect of Age ####
TEST_age <- read.csv(file="TEST_age.csv", header=T)
TEST_age$Experiment <- as.factor(TEST_age$Experiment)

(ggplot(TEST_age, aes(x=Experiment, y=Tpref, color=Experiment))
  +geom_boxplot(size=1)
  +theme( axis.line = element_line(colour = "black", size = 0.5, linetype = "solid"))
  +theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  +theme_classic()
  +theme(legend.position='none')
  +scale_y_continuous(name="Tpref (°C)", limits=c(7, 23))
  +scale_colour_manual(values=cbPalette)
  +xlab("Age")
)



#Treatment vs Passage  -- also need to take out really high value - 500
(ggplot(Dynamic_Data, aes(x=Treatment, y=Passage.))
  +geom_boxplot())

#Treatment vs Length
(ggplot(Dynamic_Data, aes(x=Treatment, y=Length.mm., colour=Treatment))
  +geom_boxplot())

#Treatment vs Weight
(ggplot(Dynamic_Data, aes(x=Treatment, y=Weight.g., colour=Treatment))
  +geom_boxplot())

#Effects plot??
library(effects)
plot(allEffects(Interaction_Model3),ylab="Tpref (°C)",xlab="Length (mm)", ylim=c(7,23))
plot(allEffects(Interaction_Model1),ylab="Tpref (°C)",xlab="Body Weight (g)", ylim=c(7,23))
plot(allEffects(Dynamic_Model),ylab="Tpref (°C)", xlab="Treatment (°C)", ylim=c(7,23))

summary(allEffects(Interaction_Model1))
allEffects(Interaction_Model1)

##gg effects plots??##


(ggplot(Dynamic_Data, aes(x=Weight.g., y=Tpref, group=Treatment, color=Treatment))
  +geom_smooth(method=lm)
  +geom_point()
  +facet_grid(. ~ Treatment)
  +scale_colour_manual(values=cbPalette)
  +scale_y_continuous(name="Tpref (°C)", limits=c(7, 23))
  +theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  +theme_bw()
  +theme(legend.position='none')
  +xlab("Body Weight (g)")
)


## TEST SCATTER PLOT 1 + 2 ##

TEST <- read.csv(file="TEST.csv", header=T)
TEST$Experiment <- as.factor(TEST$Experiment)

TEST2 <- read.csv(file="TEST2.csv", header=T)
TEST2$Experiment <- as.factor(TEST2$Experiment)

#Without any color#
(ggplot(TEST, aes(x=Weight.g., y=Tpref))
  +geom_point()
  +geom_smooth(method=lm)
  +scale_y_continuous(name="Tpref (°C)", limits=c(7, 23))
  +theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  +theme_classic()
  +theme(legend.position='right')
  +xlab("Body Weight (g)")
  )

# Length instead of Weight
(ggplot(TEST2, aes(x=Length.mm., y=Tpref))
  +geom_point()
  +geom_smooth(method=lm)
  +scale_y_continuous(name="Tpref (°C)", limits=c(7, 23))
  +theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  +theme_classic()
  +theme(legend.position='right')
  +xlab("Total Length (mm)")
)


#with color as experiments#
(ggplot(TEST, aes(x=Weight.g., y=Tpref, color=Experiment))
  +geom_point()
  +geom_smooth(method=lm)
  +scale_colour_manual(values=cbPalette)
  +scale_y_continuous(name="Tpref (°C)", limits=c(7, 23))
  +theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  +theme_classic()
  +theme(legend.position='right')
  +xlab("Body Weight (g)")
)

#############################
##############################
#######               #########
####### Experiment #2 ##########
#######               #########
##############################
#############################

Dynamic_Data2 <- read.csv(file="Experiment_2_DATA.csv", header=T)

## Treatment as a factor (instead of numeric)
Dynamic_Data2$Treatment <- as.factor(Dynamic_Data2$Treatment)
Dynamic_Data2$Time_Start <- as.numeric(Dynamic_Data2$Time_Start)

####################
### Linear Model ###
####################

Dynamic_Model2 <- lm(Tpref ~ Treatment, data=Dynamic_Data2)
summary(Dynamic_Model2)

## Including interaction ##
Interaction_Model1.2 <- lm(Tpref ~ Treatment*Weight.g., data=Dynamic_Data2) ## BEST MODEL SO FAR ##
summary(Interaction_Model1.2)

Interaction_Model3.2 <- lm(Tpref ~ Treatment*Length.mm., data=Dynamic_Data2) ## BEST MODEL SO FAR ## 
summary(Interaction_Model3)

#############
### ANOVA ###
#############

anova2 <- aov(Tpref ~ Treatment, data = Dynamic_Data2)
summary(anova2)
TukeyHSD(anova2)

anova2.4 <- aov(Length.mm. ~ Treatment, data = Dynamic_Data2)
summary(anova2.4)


library(ANOVAreplication)
data2 <- data.frame(Dynamic_Data2$Tpref, Dynamic_Data2$Treatment)
pooled.sd(data2)

data2.2 <- data.frame(Dynamic_Data2$Distance.m., Dynamic_Data2$Treatment)
pooled.sd(data2.2)

#####################
### Visualization ### --> think of a way to incorporate treatment as color for ggplots
#####################

#Treatment vs Tpref
(ggplot(Dynamic_Data2, aes(x=Treatment, y=Tpref, color=Treatment))
  +geom_boxplot(size=1)
  +theme(legend.position = "top")
  +theme( axis.line = element_line(colour = "black", size = 0.5, linetype = "solid"))
  +theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  +theme_classic()
  +theme(legend.position='none')
  +scale_y_continuous(name="Tpref (°C)", limits=c(7, 23))
  +scale_colour_manual(values=cbPalette)
  +xlab("Treatment (°C)")
)

#Treatment vs Passage  -- also need to take out really high value - 500
(ggplot(Dynamic_Data2, aes(x=Treatment, y=Passage.))
  +geom_boxplot())

#Treatment vs Length
(ggplot(Dynamic_Data2, aes(x=Treatment, y=Length.mm., colour=Treatment))
  +geom_boxplot())

#Treatment vs Weight
(ggplot(Dynamic_Data2, aes(x=Treatment, y=Weight.g., colour=Treatment))
  +geom_boxplot())

#Effects plot??
library(effects)
plot(allEffects(Interaction_Model3.2),ylab="Tpref (°C)",xlab="Length (mm)", ylim=c(7,23))
plot(allEffects(Interaction_Model1.2),ylab="Tpref (°C)",xlab="Body Weight (g)", ylim=c(7,23))
plot(allEffects(Dynamic_Model2),ylab="Tpref (°C)", xlab="Treatment (°C)", ylim=c(7,23))

summary(allEffects(Interaction_Model1.2))
allEffects(Interaction_Model1.2)

##gg effects plots??##


(ggplot(Dynamic_Data2, aes(x=Weight.g., y=Tpref, group=Treatment, color=Treatment))
  +geom_smooth(method=lm)
  +geom_point()
  +facet_grid(. ~ Treatment)
  +scale_colour_manual(values=cbPalette)
  +scale_y_continuous(name="Tpref (°C)", limits=c(7, 23))
  +theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  +theme_bw()
  +theme(legend.position='none')
  +xlab("Body Weight (g)")
)


#############################
##############################
#######               #########
####### Experiment #3 ##########
#######               #########
##############################
#############################

Dynamic_Data3 <- read.csv(file="Experiment_3_DATA.csv", header=T)

## Treatment as a factor (instead of numeric)
Dynamic_Data3$Treatment <- as.factor(Dynamic_Data3$Treatment)
Dynamic_Data3$Time_Start <- as.numeric(Dynamic_Data3$Time_Start)

####################
### Linear Model ###
####################

Dynamic_Model3 <- lm(Tpref ~ Treatment, data=Dynamic_Data3)
summary(Dynamic_Model3)

## Including interaction ##
Interaction_Model1.3 <- lm(Tpref ~ Treatment*Weight.g., data=Dynamic_Data3) ## BEST MODEL SO FAR ##
summary(Interaction_Model1.3)

Interaction_Model3.3 <- lm(Tpref ~ Treatment*Length.mm., data=Dynamic_Data3) ## BEST MODEL SO FAR ## 
summary(Interaction_Model3)

#############
### ANOVA ###
#############

anova3 <- aov(Tpref ~ Treatment, data = Dynamic_Data3)
summary(anova3)
TukeyHSD(anova3)

anova3.2 <- aov(Upper_Avoidance ~ Treatment, data = Dynamic_Data3)
summary(anova3.2)
TukeyHSD(anova3.2)

anova3.3 <- aov(Lower_Avoidance ~ Treatment, data = Dynamic_Data3)
summary(anova3.3)
TukeyHSD(anova3.3)

anova3.4 <- aov(Passage. ~ Treatment, data = Dynamic_Data3)
summary(anova3.4)

anova3.5 <- aov(Distance.m. ~ Treatment, data = Dynamic_Data3)
summary(anova3.5)

anova3.6 <- aov(Length.mm. ~ Treatment, data = Dynamic_Data3)
summary(anova3.6)
TukeyHSD(anova3.6)

library(ANOVAreplication)
data3 <- data.frame(Dynamic_Data3$Tpref, Dynamic_Data3$Treatment)
pooled.sd(data3)

data3.2 <- data.frame(Dynamic_Data3$Distance.m., Dynamic_Data3$Treatment)
pooled.sd(data3.2)

#####################
### Visualization ### --> think of a way to incorporate treatment as color for ggplots
#####################

#Treatment vs Tpref
(ggplot(Dynamic_Data3, aes(x=Treatment, y=Tpref, color=Treatment))
  +geom_boxplot(size=1)
  +theme(legend.position = "top")
  +theme( axis.line = element_line(colour = "black", size = 0.5, linetype = "solid"))
  +theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  +theme_classic()
  +theme(legend.position='none')
  +scale_y_continuous(name="Tpref (°C)", limits=c(7, 23))
  +scale_colour_manual(values=cbPalette)
  +xlab("Treatment (°C)")
)

IQR(Dynamic_Data3$Tpref)

#Treatment vs Passage  -- also need to take out really high value - 500
(ggplot(Dynamic_Data3, aes(x=Treatment, y=Passage.))
  +geom_boxplot())

#Treatment vs Length
(ggplot(Dynamic_Data3, aes(x=Treatment, y=Length.mm., colour=Treatment))
  +geom_boxplot())

#Treatment vs Weight
(ggplot(Dynamic_Data3, aes(x=Treatment, y=Weight.g., colour=Treatment))
  +geom_boxplot())

#Effects plot??
library(effects)
plot(allEffects(Interaction_Model3.3),ylab="Tpref (°C)",xlab="Length (mm)", ylim=c(7,23))
plot(allEffects(Interaction_Model1.3),ylab="Tpref (°C)",xlab="Weight (g)", ylim=c(7,23))
plot(allEffects(Dynamic_Model3),ylab="Tpref (°C)", xlab="Treatment (°C)", ylim=c(7,23))

summary(allEffects(Interaction_Model1.3))
allEffects(Interaction_Model1.3)

##gg effects plots??##


(ggplot(Dynamic_Data3, aes(x=Weight.g., y=Tpref, group=Treatment, color=Treatment))
  +geom_smooth(method=lm)
  +geom_point()
  +facet_grid(. ~ Treatment)
  +scale_colour_manual(values=cbPalette)
  +scale_y_continuous(name="Tpref (°C)", limits=c(7, 23))
  +theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  +theme_bw()
  +theme(legend.position='none')
  +xlab("Body Weight (g)")
)

####DISTANCE BETWEEN EXPERIMENTS####
Dist_Data <- read.csv(file = "Distance_Exp.csv", header = T)

anova_dist <- aov(Distance ~ Experiment, data = Dist_Data)
summary(anova_dist)
