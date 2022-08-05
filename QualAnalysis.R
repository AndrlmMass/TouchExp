setwd("~/TouchExp")
path1 <- getwd()

df3 <- read.csv("ConvertedTouchScores5.csv")

library(tibble)
library(dplyr)
library(ggplot2)
attach(df3)

for (i in 1:150){
  if (df3[i,7] == "Other"){
    print(df3[i,8])
  }
}
for (i in 151:300){
  if (df3[i,7] == "Other"){
    print(df3[i,8])
  }
}


#Plot LocLabl by condition

ggplot(df3,aes(LocLabl, fill = Condition))+
  geom_bar(position = position_dodge())+
  labs(y = "Count", x = "")+
  scale_y_continuous(breaks=seq(0,100,10),expand = c(0.01, 0))+
  theme(axis.text.x = element_text(size=12,angle = 60,hjust = 1),
        axis.title.y = element_text(size = 16),axis.text.y = element_text(size=12),
        legend.position = c(0.8, 0.8), axis.text=element_text(size=10),
        legend.title = element_text(size=16),legend.text = element_text(size=12))+
  scale_fill_discrete(name = "Touch experience", labels = c("Positive", "Negative"))

ggsave(filename = "LocBothGroups.tif",path = path1, width = 8, height = 8, 
       device='tiff', dpi=300)

#Plot IntLabl for each condition, not combined

# Group A, aka. positive group
table(df3[1:150,7])

Cond1 <- rep(c("Positive"),each=21)

IntLablNam <- c("Comfort","Affection","Greeting/Farewell","Reunion","Support",
                "Care","Induce/Express joy","Intimacy/closeness","Friendly",
                "Appreciation/gratitude","Sexual desire",
                "Celebration/Congratulation","Reaffirmation","Pride","Flirty",
                "Consolidation","Apologize","Sympathy","Respect","Relax",
                "Other")
IntLablNam <- factor(IntLablNam,levels=IntLablNam)
IntVarNum <- c(35,30,25,10,8,7,6,5,4,3,2,2,1,1,1,1,1,1,1,1,5)

IntPosFram <- data.frame(IntVarNum,IntLablNam,Cond1)
attach(IntPosFram)

ggplot(IntPosFram,aes(x = IntLablNam, y=IntVarNum))+
  geom_bar(stat="identity",aes(fill=Cond1),width=0.9)+
  labs(y = "Count", x = "")+
  theme(axis.text.x = element_text(angle = 60,hjust = 1.05),
        axis.title.y = element_text(size = 16),
        legend.position = c(0.78, 0.8), axis.text=element_text(size=12),
        legend.title = element_text(size=16), 
        legend.text = element_text(size=12),
        plot.margin = margin(t = 20,r = 20,b = 10,l = 15))+
  scale_y_continuous(breaks=seq(0,40,5),expand = c(0.01, 0),limits=c(0,40))+
  labs(fill = "Touch experience")

ggsave(filename = "IntPosGroup2.tif",path = path, width = 8, height = 8, 
       device='tiff', dpi=300)

# Group B, aka. negative group
table(df3[151:300,7])

Cond2 <- rep(c("Negative"),each=31)

IntLablNam2 <- c("Greeting/Farewell","Friendly","Affection","Comfort",
                 "Sexual desire","Flirty","Be funny",
                 "Capture attention","Appreciation/gratitude","Attraction",
                 "Intimacy/closeness","Save time","Harm","Support","Accidental",
                 "Truce","Support action","Stop movement",
                 "Celebration/Congratulation","Induce reaction","Solidarity",
                 "Care","Steal","Protect","Alienate","Recognition","Reunion",
                 "Sympathy","Make contact","Therapeutic","Other")
IntLablNam2 <- factor(IntLablNam2,levels=IntLablNam2)
IntVarNum2 <- c(48,17,10,9,6,5,4,3,3,3,3,3,
                3,3,2,2,2,2,2,2,1,1,1,1,1,1,1,1,1,1,8)

IntNegFram <- data.frame(IntVarNum2,IntLablNam2,Cond2)
attach(IntNegFram)

ggplot(IntNegFram,aes(x = IntLablNam2, y=IntVarNum2))+
  geom_bar(stat="identity",aes(fill=Cond2),width = 0.9)+
  labs(y = "Count", x = "")+
  theme(axis.text.x = element_text(angle = 60,hjust = 1.05,size=10),
        axis.title.y = element_text(size = 16),
        legend.position = c(0.78, 0.8), axis.text.y=element_text(size=12),
        legend.title = element_text(size=16), 
        legend.text = element_text(size=12),
        plot.margin = margin(t = 20,r = 20,b = 10,l = 15))+
  scale_y_continuous(breaks=seq(0,50,5),expand = c(0.01, 0),limits=c(0,50))+
  scale_fill_manual(values=c("#619CFF"),name = "Touch experience")


ggsave(filename = "IntNegGroup.tif",path = path1, width = 8, height = 8, 
       device='tiff', dpi=300)

