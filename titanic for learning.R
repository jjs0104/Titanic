library(rpart)
library(rpart.plot)
library(caret)
library(e1071)

# Naming the data set
df <- read.csv("Desktop/johnny/R/train.csv",header=T, na.strings=c("","NA"))
str(df)
summary(df)

# Change structure
# df$Sex <- as.character(df$Sex)
# summary(df)

# Treating NA's / blank spaces - first step
m <- mean(df$Age, na.rm=T)
df$Age[is.na(df$Age)] <- m

df$Embarked[is.na(df$Embarked)] <- 'S'
df$Cabin <- NULL
df$Name <- NULL
df$Ticket <- NULL
df$Survived <- as.factor(df$Survived)

# Mixing data
mixed <- df[sample(1:nrow(df)),]

# Splitting data
CL <- round(nrow(df)*0.7)
train <- df[1:CL,]
test <- df[(CL+1):nrow(df),]

# Modelling
model <- rpart(Survived~., train)
rpart.plot(model)

# Prediction
predict(model, test)
test$prediction <- predict(model, test, type='class')

# Evaluation without using the ConfusionMatrix function - Finding the accuracy
c <- test$Survived == test$prediction
sum(c) / nrow(test)

# Create final model
model <- rpart(Survived~., df)
rpart.plot(model)
