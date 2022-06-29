df <- read.csv("ConvertedTouchScores.csv")

library(lme4)
library(nlme)
library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)
attach(df)


# First, locate the outliers

ViolinIQR <- function(Item,Colnam){
  print(Colnam)
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
ColList <- c(2,4,6,9:16,18,20,22:70,74:86)
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
  print(ViolinIQR(ColumnVec,ColumnName))
  Approv <- readline(prompt = "Press enter to see the next plot.")
  if (Approv != ""){
    break
  } 
}

#Loop through all the relevant columns to check for outliers

vector.is.empty <- function(x) return(length(x) ==0 )

for (i in 1:length(ColList)){ # goes from 1 to length of ColList
  print(IDX <- ColList[i]) # For each "i" the column number is assigned to "IDX"
  BP <- boxplot.stats(df[IDX])
  print(OutIDX <- which(df[IDX] %in% BP$out))
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


  

