test <- read.csv("...test.csv")
train <- read.csv("...train.csv") # Read in data

test$Survived <- NA # Set var Survived to NA

combi <- rbind(train, test) # Combine train/test sets into dataset combi

combi$Name <- as.character(combi$Name) # Set var Name to class 'string'

strsplit(combi$Name, split='[,.]') # Split var Name by regular expressions '[,.]'

combi$Title <- sapply(combi$Name, FUN = function(x) { strsplit(x, split='[,.]')[[1]][2]}) #Apply strsplit function over all elements of var Name
#Save into new var Title

combi$Title <- sub(' ','', combi$Title) # Substitute spaces for nothing in all var Title elements

combi$Title[combi$Title %in% c('Mme','Mlle')] <- 'Mlle' # Replace unique elements 'Mme' and 'Mlle' in var Title with element name 'Mlle'

combi$Title <- factor(combi$Title) # Change var Title to class 'factor'

combi$FamilySize <- combi$SibSp + combi$Parch + 1 # Combine vars SibSp and Parch into new var FamilySize

combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]}) # Apply strsplit function over var Name, extract first element from strsplit, save into new var Surname

combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="") # Paste together as class 'string' vars FamilySize and Surname separated by nothing into new var FamilyID

combi$FamilyID[combi$FamilySize <= 2] <- 'Small' # Parse and replace elements less than or equal to 2 in var FamilySize into var FamilyID as element'Small'

famIDs <- data.frame(table(combi$FamilyID)) # Save table of var FamilyID into a dataframe

famIDs <- famIDs[famIDs$Freq <= 2,] # Display elements in var FamIDs that are less than or equal to 2

combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small' # Parse and replace elements less than or equal to 2 in var Var1 from dataframe FamIDs into var FamilyID as element 'Small'

combi$FamilyID <- factor(combi$FamilyID) # Convert var FamilyID into class 'factor'

train <- combi[1:891,]
test <- combi[892:1309,] # Reassign original row ranges to train/test datasets 

library(rpart) # Load Recursive Partitioning and Regression Trees library

Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize, data=combi[!is.na(combi$Age),], method="anova") 
# Insert missing elements in var Age with ANOVA-predicted scores using recursive partitioning function rpart

combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])

combi$Embarked[c(62,830)] = 'S'

combi$Embarked <- factor(combi$Embarked)

combi$Fare[1044] <- median(combi$Fare, na.rm=T)

combi$FamilyID2 <- combi$FamilyID

combi$FamilyID2 <- as.character(combi$FamilyID2)

combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'

combi$FamilyID2 <- factor(combi$FamilyID2)

train <- combi[1:891,]
test <- combi[892:1309,]

library(party)

set.seed(415)

fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID, data = train, controls=cforest_unbiased(ntree=2000, mtry=3))


Prediction <- predict(fit, test, OOB=T, type='response') 

submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction) 

write.csv(submit, file = 'ConditionalForest.csv', row.names=F) 
