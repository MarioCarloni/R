test <- read.csv("...test.csv")
train <- read.csv("...train.csv")

test$Survived <- NA

combi <- rbind(train, test)

combi$Name <- as.character(combi$Name)

strsplit(combi$Name, split='[,.]')

combi$Title <- sapply(combi$Name, FUN = function(x) { strsplit(x, split='[,.]')[[1]][2]})

combi$Title <- sub(' ','', combi$Title)

combi$Title[combi$Title %in% c('Mme','Mlle')] <- 'Mlle'

combi$Title <- factor(combi$Title)

combi$FamilySize <- combi$SibSp + combi$Parch + 1 #Combine vars SibSp and Parch into new var FamilySize

combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})

combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="") #Paste together as class 'string' vars FamilySize and Surname separated by nothing into new var FamilyID

combi$FamilyID[combi$FamilySize <= 2] <- 'Small' #Parse and replace elements less than or equal to 2 in var FamilySize into var FamilyID as element'Small'

famIDs <- data.frame(table(combi$FamilyID)) #Save table of var FamilyID into a dataframe

famIDs <- famIDs[famIDs$Freq <= 2,]

combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small' #Parse and replace elements less than or equal to 2 in var Var1 from dataframe FamIDs into var FamilyID as element 'Small'

combi$FamilyID <- factor(combi$FamilyID) #Convert var FamilyID into class 'factor'

train <- combi[1:891,]
test <- combi[892:1309,]

library(rpart)

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID, data=train, method='class')

Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize, data=combi[!is.na(combi$Age),], method="anova") 

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

install.packages('randomForest')

library(randomForest)

set.seed(415)

fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID2, data=train, importance=TRUE, ntree=2000)

install.packages('party')

library(party)

set.seed(415)

fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID, data = train, controls=cforest_unbiased(ntree=2000, mtry=3))


Prediction <- predict(fit, test, OOB=T, type='response') 

submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction) 

write.csv(submit, file = 'ConditionalForest.csv', row.names=F) 
