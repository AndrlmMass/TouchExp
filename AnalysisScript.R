df <- read.csv("ConvertedTouchScores5.csv")
path <- getwd()

NuCond1 <- rep(c("Positive"),each=150)
df[1:150,76] <- NuCond1
NuCond2 <- rep(c("Negative"),each=150)
df[151:300,76] <- NuCond2

path <- getwd()
library(lme4)
library(nlme)
library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)
library(lsr)
library(effsize)
library(ggsignif)
library("gridExtra")
attach(df)


# First, locate the outliers

ViolinIQR <- function(Item,Colnam){
  print(ggplot(df, aes(x=Condition,y=Item,fill=Condition))+
          geom_violin(trim =FALSE)+
          stat_summary(fun = "mean", geom = "crossbar", width = 0.4, colour = "black")+
          geom_point(position = position_jitter(width = .1, height = .4))+
          ggtitle(Colnam))
  IQROutliers <- (boxplot.stats(Item)$out)
  BP <- boxplot.stats(Item)
  print(which(Item %in% BP$out))
  return (IQROutliers)
}


#Specify which columns are relevant
ColList <- c(2,4,6,9:16,18,20,69:70,72,74:86)
colnamnam <- c() 

for (i in 1:length(ColList)){
  u <- ColList[i]
  t <- names(df[u])
  colnamnam <- append(colnamnam,t)
}

#Loop through the columns by pressing <ENTER> to move forward
Approv <- 1
for (i in 1:length(ColList)){
  ColumnVec <- df[,ColList[i]]
  ColumnName <- colnamnam[i]
  print(ViolinIQR(ColumnVec,ColumnName))
  Approv <- readline(prompt = "Press enter to see the next plot.")
  if (Approv != ""){
    break
  } 
}

#Loop through all the relevant columns to check for outliers

vector.is.empty <- function(x) return(length(x) ==0 )

for (i in 1:length(ColList)){ # goes from 1 to length of ColList
  IDX <- ColList[i] # For each "i" the column number is assigned to "IDX"
  BP <- boxplot.stats(df[IDX])
  OutIDX <- which(df[IDX] %in% BP$out)
  if (vector.is.empty(OutIDX)==FALSE){
    for (u in 1:length(OutIDX)){
      IDX2 <- OutIDX[u]
      df[IDX2,IDX] <- NA
    }
  }
}


## mixed effect linear model ##

#Correlation between appropriate and expectation

ExpVec <- cat(df[11:150,20],df[161:300,20])

plot(Expectation ~ 1 + Appropriate)

cor(Expectation, Appropriate)

## t-tests
#Function that runs t-test, shows boxplot and prints effect size for all variables 
#grouped by condition
ColList2 <- c(4,9:16,18,20,74:86) #Relevant columns
colnamnam2 <- c() # Empty vector to be filled with column names

for (i in 1:length(ColList2)){
  t <- names(df[ColList2[i]])
  colnamnam2 <- append(colnamnam2,t)
}

DatTestBeast <- function(Var, nam){
  print(ggplot(df, aes(x=Condition,y=Var,fill=Condition))+
    geom_violin(trim =FALSE)+
    stat_summary(fun = "mean", geom = "crossbar", width = 0.4, colour = "black")+
    geom_point(position = position_jitter(width = .07, height = .5))+
    ggtitle(nam))
  print(t.test(Var~Condition))
  print(cohen.d(Var~Condition, data = df))
}

Approv2 <- 1
for (i in 1:length(ColList2)){
  Approv2 <- readline(prompt = "Press <enter> to see the next plot.")
  if (Approv2 != ""){
    break
  } else {
    ColumnVec2 <- df[,ColList2[i]]
    ColumnName2 <- colnamnam2[i]
    DatTestBeast(ColumnVec2,ColumnName2)
  }
}

# Do needs differ between the conditions?

#Violinplot w/significance
ggplot(df,aes(x=Condition, y=PositiveAff, fill = Condition))+
  geom_violin(alpha=0.5,position="identity",scale = "count",trim=F)+
  stat_summary(fun = "mean", geom = "crossbar", width = 0.4, colour = "black")+
  geom_point(size=1.2, position = position_jitter(0.05))+coord_flip()+
  geom_signif(comparisons = list(c("Positive","Negative")),map_signif_level = TRUE, 
              y_position = 6)+
  theme(plot.margin = margin(t = 20,r = 20,b = 25, l = 12),
        axis.title.x = element_text(size=16),axis.text.x = element_text(size=14),
        axis.title.y = element_text(margin=margin(r=20),size = 16),
        legend.position = "none",axis.text=element_text(size=10),
        axis.text.y = element_text(size=14))+
  scale_y_continuous(breaks=seq(0,6,1))+
  labs(x = "",y="Positive affect")


# Same as above, but showing all nine needs in a 3x3 array

#Function to plot violins, just insert variable and its string into the function as below. 
VioComp <- function(viovar, varnam3){
  return(ggplot(df,aes(x=Condition, y=viovar, fill = Condition))+
    geom_violin(alpha=0.5,position="identity",scale = "count",trim=F)+
    stat_summary(fun = "mean", geom = "crossbar", width = 0.4, colour = "black")+
    geom_point(size=1.2, position = position_jitter(0.05))+coord_flip()+
    geom_signif(comparisons = list(c("Positive","Negative")),map_signif_level = TRUE, 
                y_position = 6)+
    theme(plot.margin = margin(t = 20,r = 20,b = 25, l = 0.3),
          axis.title.x = element_text(size=16),axis.text.x = element_text(size=14),
          axis.title.y = element_text(margin=margin(r=20),size = 16),
          legend.position = "none",axis.text=element_text(size=10),
          axis.text.y = element_text(size=14))+
    scale_y_continuous(breaks=seq(0,6,1))+
    labs(x = "",y=varnam3))
}

GNM2 <- VioComp(GrandNeedMean,"GrandNeedMean")
Rel <- VioComp(Relatedness,"Relatedness")
Aut <- VioComp(Autonomy,"Autonomy")
Pop <- VioComp(Popularity,"Popularity")
Comp <- VioComp(Competence,"Competence")
SeEs <- VioComp(SelfEsteem,"Self-esteem")
SeAc <- VioComp(SelfActualization,"Self-actualization")
Sec <- VioComp(Security,"Security")
Phy <- VioComp(PhysicalThriving,"Physical thriving")
Plea <- VioComp(PleasureStimulation,"PleasureStimulation")
PA <- VioComp(PositiveAff,"Positive affect")
NAf <- VioComp(NegativeAff,"Negative affect")
CoAf <- VioComp(CompositeAff,"Composite affect")

grid.arrange(GNM2, Rel, Aut, Pop, Comp, SeEs, SeAc, Sec, Phy, Plea, ncol = 5, nrow = 2)
grid.arrange(PA,NAf,CoAf, nrow = 1)

#Split violin-plots
ggplot(df,aes(x=0, y=PositiveAff, fill = Condition))+
  introdataviz::geom_split_violin(alpha=0.5,position="identity",
                                  scale = "count",trim=F)+coord_flip()+
  geom_boxplot(width = .05, alpha = .2, fatten = NULL, 
               show.legend = FALSE,position = position_dodge(.175))+
  stat_summary(fun = "mean", geom = "pointrange", 
               position = position_dodge(.175))+
  theme(plot.margin = margin(t = 20,r = 10,b = 10, l = 10),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size = 16),legend.position = c(0.8, 0.8), 
        axis.text=element_text(size=10),legend.title = element_text(size=16))+
  scale_y_continuous(breaks=seq(0,6,1))+
  labs(x = "Density",y="Positive affect")+
  scale_fill_discrete(name = "Touch experience", 
                      labels = c("Negative","Positive"))


#Cowplot 
ggplot(df, aes(x=PositiveAff,fill = Condition))+
  geom_density(alpha=0.8,bw = "nrd")+
  theme(plot.margin = margin(t = 20,r = 10,b = 10, l = 10), 
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size = 16),legend.position = c(0.8, 0.8), 
        axis.text=element_text(size=10),legend.title = element_text(size=16))+
  scale_fill_discrete(name = "Touch experience")+
  labs(y="Density")


  

ggsave(filename = "GrandNeedMean by Cond.tif",path = path, width = 5, height = 4, device='tiff', dpi=300)


group = as.factor(rep(c(1,2), each=200))




## Does Needs predict affect better than Touch?
# SHOULD CONSIDER VIF - NOT SURE WHY YET

#Test multicollinearity between predictors

library(car)
library(quantmod)
library(MASS)
library(corrplot)
library(lm.beta)
library(faraway)
library(ggpmisc)

RelCol <- df[,75:83]
varvar <- cor(RelCol, method = "pearson")
vif(RelCol)

for (i in 1:ncol(varvar)){
  print(mean(varvar[,i]))
}

## Linear model predicting positive affect with needs and touch grouped into two different models ##

#Region
# POSITIVE AFFECT - ADD THE "FULL MODEL" VERSION TOO
Condition = as.factor(Condition)

ggplot(df, aes(x=GrandNeedMean,y=CompositeAff, colour = Condition))+
  stat_poly_eq(aes(label = paste(after_stat(eq.label))), size = 6)+
  stat_poly_line()+
  geom_point(size = 3, position = position_jitter(0.05), aes(colour = Condition))


  geom_smooth(formula = y ~ x, method="lm", aes(colour = Condition, group = Condition), size = 3)+
geom_smooth(formula =  y ~ x, method = 'lm',size = 2, colour = 'black', se = F)+
geom_smooth(formula = y ~ x, method="lm", aes(colour = Condition, group = Condition))+
aes(colour = Condition, shape = Condition), 

# Needs predicting positive affect
lm1 <- lm(CompositeAff ~ Relatedness + Competence)

summary(lm1)

lm.beta(lm1)

# Physical touch as  predictor 
lm2 <- lm(PositiveAff ~ Humidity + Velocity + Roughness + Intensity)

summary(lm2)

lm.beta(lm2)

# NEGATIVE AFFECT

# Need as predictor
lm3 <- lm(NegativeAff ~ Relatedness + Competence)

summary(lm3)

lm.beta(lm3)

lm4 <- lm(NegativeAff ~ Relatedness + Competence + SelfActualization + 
            Security + SelfEsteem + PleasureStimulation + PhysicalThriving + 
            Autonomy + Popularity)

summary(lm4)

lm.beta(lm4)

# Physical touch  as predictor
lm5 <- lm(NegativeAff ~ Roughness + Intensity)

summary(lm5)

lm.beta(lm5)

lm6 <- lm(NegativeAff ~ Roughness + Intensity + Humidity + Velocity)

summary(lm6)

lm.beta(lm6)

# COMPOSITE AFFECT

# Need as predictor
lm3 <- lm(CompositeAff ~ Relatedness + Competence)

summary(lm3)

lm.beta(lm3)

lm4 <- lm(CompositeAff ~ Relatedness + Competence + SelfActualization + 
            Security + SelfEsteem + PleasureStimulation + PhysicalThriving + 
            Autonomy + Popularity)

summary(lm4)

lm.beta(lm4)

# Physical touch  as predictor
lm5 <- lm(CompositeAff ~ Roughness + Intensity)

summary(lm5)

lm.beta(lm5)

lm6 <- lm(CompositeAff ~ Roughness + Intensity + Humidity + Velocity)

summary(lm6)

lm.beta(lm6)

# Touch context regression analyses

lm7 <- lm(PositiveAff ~ Pleasantness + Comfortable)

summary(lm7)

