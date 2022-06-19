df <- read.delim("ConvertedTouchScores.csv")

library(ggplot2)
attach(my_data)
# First, locate the outliers

ViolinIQR <- function(Item){
  print(ggplot(my_data, aes(x=Condition,y=Item,fill=Condition))+
          geom_violin(trim =FALSE)+
          stat_summary(fun = "mean", geom = "crossbar", width = 0.4, colour = "black")+
          geom_point(position = position_jitter(w=0.1, h=0)))
  IQROutliers <- (boxplot.stats(Item)$out)
  return (IQROutliers)
}

ViolinIQR(GrandNeedMean)