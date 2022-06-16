#Specifies the working directory
setwd("~/Projects/TouchExp/MainStudy/Rscript/TouchExp")
t <- ("BoisBeBois")

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

#### Converting values and labels ####

# Converting column names

new.names1 <- c("ID","TypeOfTouch","OthTouchType","InterPartner","OtherIntPartner","Initiative", "Intention","Perception","Relation2Partner","Roughness","Pleasantness","Power","Comfortable","Duration","Appropriate","Humidity","HumApplic","Velocity","VeloApplic","Expectation","Location","ComplDiffTask","NewSensation","SenseOfContact","BecomeSelf","StructPredict","OtherSeekAdvice","TrueInterest","PosQualities","CloseConnect","PhysPleasure","EnoughExercise","MasterChal","DeepPurpose","RoutinesHabits","SelfRespect","Influence","BodyNeeded","Freedom","SatisfiedSelf","SourceOfStimuli","Safe","WellBeing","Capable","TrueSelf","Awe","StrongImpact","StrongIntimacy")
new.names2 <- c("Age","Gender","GenderType","ElapsedTime")

colnames(my_data)[1:48] <- new.names1
colnames(my_data)[69:72] <- new.names2

attach(my_data)

print("André bør få betalt for dette")