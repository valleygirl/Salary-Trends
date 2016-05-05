randomForest1000.csv <- read.csv("RandomForest/RanForest1000.csv", header = T)

attach(randomForest1000.csv)

str(randomForest1000.csv)

names(randomForest1000.csv)

#randomForest1000.csv[, 'LCA_CASE_EMPLOYER_NAME'] <- as.numeric(randomForest1000.csv[, 'LCA_CASE_EMPLOYER_NAME'])

#randomForest1000.csv[, 'LCA_CASE_EMPLOYER_CITY'] <- as.numeric(randomForest1000.csv[, 'LCA_CASE_EMPLOYER_CITY'])

#randomForest1000.csv[, 'LCA_CASE_EMPLOYER_POSTAL_CODE'] <- as.numeric(randomForest1000.csv[, 'LCA_CASE_EMPLOYER_POSTAL_CODE'])

#randomForest1000.csv[, 'LCA_CASE_JOB_TITLE'] <- as.numeric(randomForest1000.csv[, 'LCA_CASE_JOB_TITLE'])

#randomForest1000.csv[, 'CODE'] <- as.numeric(randomForest1000.csv[, 'CODE'])

# my changes below
LCA_CASE_EMPLOYER_NAME <- randomForest1000.csv[, 'LCA_CASE_EMPLOYER_NAME']
LCA_CASE_EMPLOYER_CITY <- randomForest1000.csv[, 'LCA_CASE_EMPLOYER_CITY']
LCA_CASE_JOB_TITLE <- randomForest1000.csv[, 'LCA_CASE_JOB_TITLE']
SALARY_RANGE <- randomForest1000.csv[, 'SALARY_RANGE']
prediction <- data.frame(LCA_CASE_EMPLOYER_NAME, LCA_CASE_EMPLOYER_CITY,
                         LCA_CASE_JOB_TITLE, SALARY_RANGE)


#install.packages("caret")

#install.packages("ggplot2")

install.packages("randomForest")

#install.packages("e1071")

library(caret)

library(ggplot2)

library(randomForest)

library(e1071)

# my changes
inTrain <- createDataPartition(y = prediction$SALARY_RANGE, p = .7, list = FALSE)
#inTrain <- createDataPartition(y = randomForest1000.csv$SALARY_RANGE, p = .7, list = FALSE)

training <- prediction[inTrain,]
#training <- randomForest1000.csv[inTrain,]

testing <- prediction[ - inTrain,]
#testing <- randomForest1000.csv[ - inTrain,]

ranForestModel <- train(SALARY_RANGE ~ ., data = training, method = 'rf', prox = TRUE)

ranForestModel

getTree(ranForestModel$finalModel, k = 2)

#predicting the Testing set

pred <- predict(ranForestModel, testing)

pred

testing$predRight <- pred == testing$SALARY_RANGE

#table(pred, testing$SALARY_RANGE)

#qplot(LCA_CASE_EMPLOYER_CITY,LCA_CASE_JOB_TITLE, colour=predRight,data=testing,main='Predictions')

#qplot(LCA_CASE_EMPLOYER_CITY,LCA_CASE_EMPLOYER_NAME, colour=predRight,data=testing,main='Predictions')

#qplot(LCA_CASE_EMPLOYER_NAME,LCA_CASE_JOB_TITLE, colour=predRight,data=testing,main='Predictions')

