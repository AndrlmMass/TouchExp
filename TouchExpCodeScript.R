#Specifies the working directory
setwd("~/Projects/TouchExp/MainStudy/Rscript2/TouchExp")

#Import A-condition file
my_data <- read.delim("TouchAComplete.txt")
my_data$Condition <- rep(c("A"),each=150)
colnames(my_data)[12] <- "Perception"

#Import B-condition file
my_data2 <- read.delim("TouchBComplete.txt")
my_data2$Condition <- rep(c("B"),each=150)
colnames(my_data2)[12] <- "Perception"

#Use rbind to combine the two condition dataframes
my_data <- rbind(my_data,my_data2)

#Remove irrelevant columns
my_data <- subset(my_data,select = -c(2:5))

#install.packages("tidyverse")
library(tibble)

#### Converting values and labels ####

# Converting column names

new.names1 <- c("ID","TypeOfTouch","OthTouchType","InterPartner","OtherIntPartner","Initiative", "Intention","Perception","Relation2Partner","Roughness","Pleasantness","Power","Comfortable","Duration","Appropriate","Humidity","HumApplic","Velocity","VeloApplic","Expectation","Location","ComplDiffTask","NewSensation","SenseOfContact","BecomeSelf","StructPredict","OtherSeekAdvice","TrueInterest","PosQualities","CloseConnect","PhysPleasure","EnoughExercise","MasterChal","DeepPurpose","RoutinesHabits","SelfRespect","Influence","BodyNeeded","Freedom","SatisfiedSelf","SourceOfStimuli","Safe","WellBeing","Capable","TrueSelf","Awe","StrongImpact","StrongIntimacy")
new.names2 <- c("Age","Gender","GenderType","ElapsedTime")

colnames(my_data)[1:48] <- new.names1
colnames(my_data)[69:72] <- new.names2

my_data[my_data == ""] <- NA

##Create function for conversion of qualitative data to numeric

DaGrandConvertar <- function(CharVector,Col){
  NewVector <- rep(NA,nrow(my_data))
  l <- 1
  for (i in 1:nrow(my_data)){
    if (is.element(my_data[i,Col],CharVector)){
      NewVector[l] <- which(CharVector == my_data[i,Col])
      l <- l + 1
    } else if (is.na(my_data[i,Col])==TRUE){
      Sz <- length(CharVector)+1
      NewVector[l] <- Sz
      l <- l + 1
    }
  }
  return (NewVector)
}

##Convert type of touch##
# Hug = 1
# Hand on the shoulder = 2
# Kiss = 3
# Caress = 4
# Handshake = 5
# Other = 6

TypeVector <- c("Hug","Hand on the shoulder","Kiss","Caress","Handshake")
my_data[,2] <- DaGrandConvertar(TypeVector,2)

##Convert RelationToPartner dots to numerics##
# Number of dots converted to numeric. E.g., "." = 1, ".." = 2. 

Rel2Partnr <- c(".","..","...","....",".....","......",".......")
my_data[,9] <- DaGrandConvertar(Rel2Partnr,9)

##Convert Initiative##
# Me = 1, Both = 2, Other = 3

Init2Touch <- c("I mainly took the initiative","We both took the initiative","The interaction partner mainly took the initiative")
my_data[,6] <- DaGrandConvertar(Init2Touch,6)

##Convert gender to numerics##
# Male = 1, Female = 2, Non-binary = 3

GendItems <- c("Male","Female","Non-binary")
my_data[,70] <- DaGrandConvertar(GendItems,70)


#Alter values in Touch Partner column to numeric

for (i in 1:nrow(my_data)) {
  if (my_data[i, 4] == "Partner" | my_data[i, 4] == "Family member"){
    my_data[i,4] = 1
  } else if (my_data[i, 4] == "Friend" | my_data[i, 4] == "Colleague"| my_data[i, 4] == "Acquaintance"){
    my_data[i,4] = 2
  } else if (my_data[i,4] == "Stranger"){
    my_data[i,4] = 3
  } else if (is.na(my_data[i,5]) == FALSE){
    my_data[i,2] = 4
  }
}

# Converting Humidity values to NA if "not applicable" is true
for (i in 1:nrow(my_data)){
  if (is.na(my_data[i,17]) == FALSE){
    my_data[i,16] = NA
    my_data[i,17] = 1
  }
}
# Converting Velocity values to NA if "Not applicable" is true
for (i in 1:nrow(my_data)){
  if (is.na(my_data[i,19]) == FALSE){
    my_data[i,18] = NA
    my_data[i,19] = 1
  }
}

##Create function that combines qual answers into new data frame##

QualAns <- data.frame(matrix(NA,nrow = 300,ncol = 5))
colnames(QualAns) <- c("AlternTouch","AlternPartnr","Intention","Perception","Location")

QualCombiner <- function(Col){
  t <- 1
  QualVec <- rep(NA,nrow(my_data))
  for (i in 1:nrow(my_data)){
    if (is.na(my_data[i,Col])==FALSE){
      QualVec[t] <- my_data[i,Col]
      t <- t + 1
    }
  }
  return (QualVec)
}

#Use function to combine all qual answers from relevant items into one
#data frame
QualAns[,1] <- QualCombiner(3)
QualAns[,2] <- QualCombiner(5)
QualAns[,3] <- QualCombiner(7)
QualAns[,4] <- QualCombiner(8)
QualAns[,5] <- QualCombiner(21)

# Converting need and affect values
# String -> integer
for (i in 22:68){
  for (u in 1:nrow(my_data)){
    if (my_data[u,i] == "Not at all"| my_data[u,i] == "Not at all (1)"){
      my_data[u,i] = 1
    } else if (my_data[u,i] == "A little" | my_data[u,i] == "A little (2)"){
      my_data[u,i] = 2
    } else if (my_data[u,i] == "Moderately" | my_data[u,i] == "Moderately (3)"){
      my_data[u,i] = 3
    } else if (my_data[u,i] == "Quite a bit" | my_data[u,i] == "Quite a bit (4)"){
      my_data[u,i] = 4
    } else if (my_data[u,i] == "Very much" | my_data[u,i] == "Extremely (5)"){
      my_data[u,i] = 5
    } else {
      sprintf("There is something wrong in the following row: %s", i)
    }
  }
}

# Converting time spent to numerical minutes
for (i in 1:nrow(my_data)){
  StringTime <- my_data[i,72];
  IntTime <- as.numeric(sapply(strsplit(StringTime, " "), "[[", 1))
  my_data[i,72] = IntTime
}

# Creating aggregated need values
# Change character values to numerical to make mean measure possible.
my_data[,22:69] <- sapply(my_data[,22:69],as.numeric)
my_data[,72] <- sapply(my_data[,72],as.numeric)
my_data[,17] <- sapply(my_data[,17],as.numeric)
my_data[,19] <- sapply(my_data[,19],as.numeric)
my_data[,4] <- sapply(my_data[,4],as.numeric)
my_data[,70] <- sapply(my_data[,70],as.character)

# Grand need mean by participant
my_data$GrandNeedMean <- rowMeans(my_data[,22:48])
my_data$Autonomy <- rowMeans(my_data[,c("TrueInterest","Freedom", "TrueSelf")])
my_data$Competence <- rowMeans(my_data[,c("ComplDiffTask", "MasterChal","Capable")])
my_data$Relatedness <- rowMeans(my_data[,c("SenseOfContact","CloseConnect","StrongIntimacy")])
my_data$SelfActualization <- rowMeans(my_data[,c("BecomeSelf", "DeepPurpose","Awe")])
my_data$PhysicalThriving <- rowMeans(my_data[,c("EnoughExercise","BodyNeeded","WellBeing")])
my_data$PleasureStimulation <- rowMeans(my_data[,c("NewSensation","PhysPleasure","SourceOfStimuli")])
my_data$Security <- rowMeans(my_data[,c("StructPredict","RoutinesHabits", "Safe")])
my_data$SelfEsteem <- rowMeans(my_data[,c("PosQualities","SatisfiedSelf","SelfRespect")])
my_data$Popularity <- rowMeans(my_data[,c("StrongImpact","OtherSeekAdvice","Influence")])
my_data$PositiveAff <- rowMeans(my_data[,c("Proud","Interested","Excited","Enthusiastic","Inspired","Alert","Determined","Active","Attentive","Strong")])
my_data$NegativeAff <- rowMeans(my_data[,c("Nervous","Upset","Scared","Jittery","Afraid","Hostile","Irritable","Guilty","Distressed","Ashamed")])
my_data$CompositeAff <- NA

for (i in 1:nrow(my_data)){
  Comp <- (my_data[i,84])-(my_data[i,85])
  my_data[i,86] = Comp
}

#Write to csv file for analyses in new script. 

write.csv(my_data,"C:\\Users\\andre\\OneDrive\\Documents\\TouchExp\\ConvertedTouchScores.csv", row.names = FALSE)
write.csv(QualAns,"C:\\Users\\andrlm\\OneDrive\\Documents\\TouchExp\\QualitativeAnswersPool3.csv")
