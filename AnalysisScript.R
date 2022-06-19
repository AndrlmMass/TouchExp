df <- read.csv("ConvertedTouchScores.csv")

library(ggplot2)
attach(df)


# First, locate the outliers

ViolinIQR <- function(Item,Colnam){
  print(Colnam)
  print(ggplot(df, aes(x=Condition,y=Item,fill=Condition))+
          geom_violin(trim =FALSE)+
          stat_summary(fun = "mean", geom = "crossbar", width = 0.4, colour = "black")+
          geom_point(position = position_jitter(width = .03, height = .01))+
          ggtitle(Colnam))
  IQROutliers <- (boxplot.stats(Item)$out)
  return (IQROutliers)
  }

#Loop through all the relevant columns to check for outliers

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
  ViolinIQR(ColumnVec,ColumnName)
  Approv <- readline(prompt = "Press enter to see the next plot.")
  if (Approv != ""){
    break
  } 
}


  

