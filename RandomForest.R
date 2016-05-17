library(randomForest)
library(rpart)

#import data
train_set <- read.csv("train.csv", sep = ",", header = TRUE)
test_set <- read.csv("test.csv", sep = ",", header =  TRUE)
test_set$Survived <- NA

#All data
Titanic <- rbind(train_set, test_set)

#head(train_set)
summary(Titanic)

#change Survived and Pclass to factors
Titanic$Survived <- factor(Titanic$Survived, levels = c(0,1))
Titanic$Pclass <- factor(Titanic$Pclass, ordered = TRUE, levels = c(3,2,1))
Titanic$Embarked <- factor(Titanic$Embarked, levels = c("C", "Q", "S"))

#Check NAs
which(is.na(Titanic$Age))
which(is.na(Titanic$Embarked))
which(is.na(Titanic$Fare))
which(Titanic$Fare == 0)
which(is.na(Titanic$Cabin))

#Function to compute Mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#Impute missing Embarked
Titanic[which(is.na(Titanic$Embarked)),]$Embarked <- getmode(Titanic$Embarked)

#predict missing age
Titanic_Age_NA <- subset(Titanic, is.na(Titanic$Age))
Titanic_Age <- subset(Titanic, !is.na(Titanic$Age))

#Random Forest
model_age <- randomForest(Age ~ Pclass + Sex + SibSp + Parch + Fare,
                          data = Titanic_Age, na.action = na.omit)
#Decision Tree
model_age_rpart <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare,
                         data = Titanic_Age, method = "anova")
  

Titanic$Age[which(is.na(Titanic$Age))] <- predict(model_age, Titanic_Age_NA)

#Investigate missing values for Fare
Titanic$Embarked[which(is.na(Titanic$Fare))]
Titanic$Embarked[which(Titanic$Fare == 0)]
Titanic$Pclass[which(Titanic$Fare==0)]
Titanic$Pclass[which(is.na(Titanic$Fare))]

#Calculate Mean Fare grouped by Pclass and Embarked
fare_mean <- aggregate( formula = Fare~Pclass+Embarked, 
                        data = Titanic,
                        FUN = mean)

Titanic$Fare[which(Titanic$Fare==0 & Titanic$Pclass==1)] <- fare_mean$Fare[which(fare_mean$Embarked=='S' & fare_mean$Pclass==1)]
Titanic$Fare[which(Titanic$Fare==0 & Titanic$Pclass==2)] <- fare_mean$Fare[which(fare_mean$Embarked=='S' & fare_mean$Pclass==2)]
Titanic$Fare[which(is.na(Titanic$Fare) & Titanic$Pclass==3)] <- fare_mean$Fare[which(fare_mean$Embarked=='S' & fare_mean$Pclass==3)]
Titanic$Fare[which(Titanic$Fare==0 & Titanic$Pclass==3)] <- fare_mean$Fare[which(fare_mean$Embarked=='S' & fare_mean$Pclass==3)]

train <- Titanic[!is.na(Titanic$Survived),]
test <- Titanic[is.na(Titanic$Survived),]

summary(test)

#Run random forest on train set
set.seed(123)
train_model <- randomForest(Survived ~ Pclass + Sex + Age + Parch + SibSp + Fare + Embarked,
                            data = train,
                            ntree = 500,
                            importance = TRUE,
                            na.action = na.omit)

Predicted_Survival <- predict(train_model, test)
Predictions <- data.frame(PassengerId = test$PassengerId, Survived = Predicted_Survival)

write.csv(Predictions, file = "My_submission_16May2016_2.csv", row.names = FALSE)

