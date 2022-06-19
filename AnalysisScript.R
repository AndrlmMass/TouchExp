df <- read.csv("ConvertedTouchScores.csv")

library(ggplot2)
attach(df)


# First, locate the outliers

ViolinIQR <- function(Item){
  print(ggplot(df, aes(x=Condition,y=Item,fill=Condition))+
          geom_violin(trim =FALSE)+
          stat_summary(fun = "mean", geom = "crossbar", width = 0.4, colour = "black")+
          geom_point(position = position_jitter(w=0.1, h=0)))
  IQROutliers <- (boxplot.stats(Item)$out)
  return (IQROutliers)
}

ViolinIQR(TypeOfTouch)

#Loop through all the relevant columns to check for outliers

#Specify which columns are relevant
ColList <- c(2,4,6,9:16,18,20,22:70,74:86)

Approv <- 1
for (i in 1:length(ColList)){
  if (Approv != 1){
    break
  } else {
    Column <- df[,ColList[i]]
    ViolinIQR(Column)
    Approv <- readline(prompt = "Do you want to check the next column? Press 1, exit by pressing anything else.")
  }
}



for (i in 1:5){
  if (i > 3){
    break
  } else if
    print(i)
}

