setwd("~/Projects/TouchExp/MainStudy/Rscript2/TouchExp")
path1 <- getwd()

df3 <- read.csv("ConvertedTouchScores5.csv")

NuCond1 <- rep(c("Positive"),each=150)
df3[1:150,76] <- NuCond1
NuCond2 <- rep(c("Negative"),each=150)
df3[151:300,76] <- NuCond2

library(tibble)
library(dplyr)
library(ggplot2)
library(forcats)
library(ggthemes)
library(reshape)
attach(df3)

for (u in 1:nrow(df3)){
  if (df3[u,23] == "RestaurantBar"){
    df3[u,23] <- "Restaurant/Bar"
  } 
}

#Plot LocLabl by condition

# apply the summation per value 

LocNam <- c("Work", "Work","Restaurant/Bar","Restaurant/Bar","Other's home", 
            "Other's home", "Public Building", "Public Building", "Street",
            "Street","Vehicle","Vehicle", "Other", "Other",
            "Public outside area", "Public outside area","Home","Home")
LocVal <- c(8,52,9,35,7,10,7,10,6,10,2,4,2,2,12,5,97,32)
LocCond <- c('Positive','Negative','Positive','Negative','Positive','Negative',
             'Positive','Negative','Positive','Negative','Positive','Negative',
             'Positive','Negative','Positive','Negative','Positive','Negative')
LocDf <- data.frame(LocNam,LocVal,LocCond)

LocDf$LocNam <- factor(LocDf$LocNam,levels = c("Work","Restaurant/Bar",
                                               "Other's home","Public Building", 
                                               "Street","Vehicle", "Other", 
                                               "Public outside area", "Home"))

ggplot(LocDf,aes(x=LocNam, y=LocVal, fill = LocCond))+
  geom_bar(stat = "identity",position = position_dodge())+
  labs(fill = "Touch experience", y = "Count", x = "")+
  scale_y_continuous(breaks=seq(0,100,10),expand = c(0.02, 0), limits=c(0,100))+
  theme(axis.text.x = element_text(angle = 45,hjust = 1, size = 14),
        axis.title.y = element_text(size = 18),
        legend.position = c(0.4, 0.8), axis.text.y=element_text(size=14),
        legend.title = element_text(size=18),legend.text = element_text(size=16),
        plot.margin = margin(t = 20,r = 20,b = 15,l = 15))+
  scale_fill_colorblind()+
  guides(fill = guide_legend(reverse=TRUE))

ggsave(filename = "LocationByCondition3.tif",path = path1, width = 7, height = 7, device='tiff', dpi=300)

#Plot IntLabl for each condition, not combined

# Group A, aka. positive group
table(df3[1:150,7])

Cond1 <- rep(c("A"),each=21)

IntLablNam <- c("Comfort","Affection","Greeting/Farewell","Reunion","Support",
                "Care","Induce/Express joy","Intimacy/Closeness","Friendly",
                "Appreciation/Gratitude","Sexual desire","Celebration/Congratulation",
                "Reaffirmation","Pride","Flirty","Consolidation",
                "Apologize","Sympathy","Respect","Relax","Other")
IntLablNam <- factor(IntLablNam,levels=IntLablNam)
IntVarNum <- c(35,30,25,10,8,7,6,5,4,3,2,1,1,1,1,1,1,1,1,1,5)

IntPosFram <- data.frame(IntVarNum,IntLablNam,Cond1)
attach(IntPosFram)

ggplot(IntPosFram,aes(x = IntLablNam, y=IntVarNum))+
  geom_bar(stat="identity",aes(fill=Cond1),width=0.9)+
  labs(y = "Count", x = "")+
  theme(axis.text.x = element_text(angle = 60,hjust = 1.05),
        axis.title.y = element_text(size = 16),
        legend.position = c(0.8, 0.8), axis.text=element_text(size=12),
        legend.title = element_text(size=16), 
        legend.text = element_text(size=12),
        plot.margin = margin(t = 20,r = 20,b = 10,l = 15))+
  scale_y_continuous(breaks=seq(0,40,5),expand = c(0.01, 0),limits=c(0,40))+
  scale_fill_discrete(labels=c('Positive'))+
  labs(fill = "Touch experience")

# Group B, aka. negative group
table(df3[151:300,7])

Cond2 <- rep(c("B"),each=30)

IntLablNam2 <- c("Greeting/Farewell","Friendly","Affection","Comfort",
                "Sexual desire","Flirty","Be funny",
                "Capture attention","Appreciation/gratitude","Attraction",
                "Intimacy/closeness","Save time","Harm","Support","Accidental",
                "Truce",
                "Stop movement","Congratulation/Celebration","Induce reaction","Solidarity",
                "Care","Steal","Protect","Alienate","Recognition","Reunion",
                "Sympathy","Make contact","Therapeutic","Other")
IntLablNam2 <- factor(IntLablNam2,levels=IntLablNam2)
IntVarNum2 <- c(48,17,10,9,6,5,4,3,3,3,3,3,
                3,3,2,2,2,2,2,1,1,1,1,1,1,1,1,1,1,8)

IntNegFram <- data.frame(IntVarNum2,IntLablNam2,Cond2)
attach(IntNegFram)

ggplot(IntNegFram,aes(x = IntLablNam2, y=IntVarNum2))+
  geom_bar(stat="identity",aes(fill=Cond2),width = 0.9)+
  labs(y = "Count", x = "")+
  theme(axis.text.x = element_text(angle = 60,hjust = 1.05),
        axis.title.y = element_text(size = 16),
        legend.position = c(0.8, 0.8), axis.text=element_text(size=12),
        legend.title = element_text(size=16), 
        legend.text = element_text(size=12),
        plot.margin = margin(t = 20,r = 20,b = 10,l = 15))+
  scale_y_continuous(breaks=seq(0,50,5),expand = c(0.01, 0),limits=c(0,50))+
  scale_fill_manual(values=c("#619CFF"),name = "Touch experience",
                    labels=c('Negative'))

  



 