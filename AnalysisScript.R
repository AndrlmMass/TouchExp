df <- read.csv("ConvertedTouchScores.csv")

library(ggplot2)
library(lsr)
library(effsize)
attach(df)


# First, locate the outliers

ViolinIQR <- function(Item,Colnam){
  print(ggplot(df, aes(x=Condition,y=Item,fill=Condition))+
          geom_violin(trim =FALSE)+
          stat_summary(fun = "mean", geom = "crossbar", width = 0.4, colour = "black")+
          geom_point(position = position_jitter(width = .07, height = .5))+
          ggtitle(Colnam))
  IQROutliers <- (boxplot.stats(Item)$out)
  return (IQROutliers)
}

#Loop through all the relevant columns to check for outliers

#Specify which columns are relevant
ColList <- c(2,4,6,9:16,18,20,69:70,72,74:86)
colnamnam <- c() 

for (i in 1:length(ColList)){
  u <- ColList[i]
  t <- names(df[u])
  colnamnam <- append(colnamnam,t)
}

#Loop through the columns by pressing "1" to move forward
Approv <- 1
for (i in 1:length(ColList)){
  ColumnVec <- df[,ColList[i]]
  ColumnName <- colnamnam[i]
  ViolinIQR(ColumnVec,ColumnName)
  Approv <- readline(prompt = "Press enter to see the next plot.")
  if (Approv != ""){
    break
  } 
}

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
  ColumnVec2 <- df[,ColList2[i]]
  ColumnName2 <- colnamnam2[i]
  DatTestBeast(ColumnVec2,ColumnName2)
  Approv2 <- readline(prompt = "Press enter to see the next plot.")
  if (Approv2 != ""){
    break
  } 
}

## Does Needs predict affect?

plot(NegativeAff, GrandNeedMean)

lm1 <- lm(CompositeAff ~ Relatedness + GrandNeedMean)

summary(lm1)


par(mfrow=c(1,1))
plot(lm1)



