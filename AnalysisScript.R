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

## Does Needs predict affect?

plot(NegativeAff, GrandNeedMean)

lm1 <- lm(NegativeAff ~ GrandNeedMean)

summary(lm1)

par(mfrow=c(1,1))
plot(lm1)

## t-tests
DatTestBeast <- function(Var){
  boxplot(Var~Condition)
  return (t.test(Var~Condition))
}

Effin <- 

# Appropriate
t.test(Appropriate~Condition)
boxplot(Appropriate~Condition)  

cohen.d(Appropriate~Condition, data = df)

# Expectation
t.test(Expectation~Condition)
boxplot(Expectation ~ Condition)

cohen.d(Expectation~Condition, data = df)

# Relation2Partner

t.test(Relation2Partner~Condition)
boxplot(Relation2Partner~Condition)

cohen.d(Relation2Partner~Condition, data = df)

# Roughness

t.test(Roughness~Condition)
boxplot(Roughness~Condition)

cohen.d(Roughness~Condition, data = df)

