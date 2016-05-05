lca <- read.csv("RandomForest/RandomForestTraining_data.csv", header = T)
lca<-lca[lca["LCA_CASE_WAGE_RATE_UNIT"]=="Year",]

attach(lca)
str(lca)
names(lca)

LCA_CASE_EMPLOYER_NAME <- lca[, 'LCA_CASE_EMPLOYER_NAME']
LCA_CASE_EMPLOYER_CITY <- lca[, 'LCA_CASE_EMPLOYER_CITY']
LCA_CASE_JOB_TITLE <- lca[, 'LCA_CASE_JOB_TITLE']
SALARY <- as.numeric(lca[, 'LCA_CASE_WAGE_RATE_FROM'])
basedata <- data.frame(LCA_CASE_EMPLOYER_NAME, LCA_CASE_EMPLOYER_CITY,
                       LCA_CASE_JOB_TITLE, SALARY)
basedata[c("SALARY_RANGE")] <- ""

basedata$SALARY_RANGE[basedata$SALARY<50000] <- "Below 50000"
basedata$SALARY_RANGE[basedata$SALARY>=50000 & basedata$SALARY<75000] <- "50000 to 75000"
basedata$SALARY_RANGE[basedata$SALARY>=75000 & basedata$SALARY<100000] <- "75000 to 100000"
basedata$SALARY_RANGE[basedata$SALARY>=100000 & basedata$SALARY<125000] <- "100000 to 125000"
basedata$SALARY_RANGE[basedata$SALARY>=125000 & basedata$SALARY<150000] <- "125000 to 150000"
basedata$SALARY_RANGE[basedata$SALARY>=150000 & basedata$SALARY<200000] <- "150000 to 200000"
basedata$SALARY_RANGE[basedata$SALARY>200000] <- "Above 200000"

LCA_CASE_EMPLOYER_NAME <- basedata[, 'LCA_CASE_EMPLOYER_NAME']
LCA_CASE_EMPLOYER_CITY <- basedata[, 'LCA_CASE_EMPLOYER_CITY']
LCA_CASE_JOB_TITLE <- basedata[, 'LCA_CASE_JOB_TITLE']
SALARY_RANGE <- basedata[, 'SALARY_RANGE']
prediction <- data.frame(LCA_CASE_EMPLOYER_NAME, LCA_CASE_EMPLOYER_CITY,
                         LCA_CASE_JOB_TITLE, SALARY_RANGE)


#install.packages("caret")

#install.packages("ggplot2")

#install.packages("randomForest")

#install.packages("e1071")

library(caret)

library(ggplot2)

library(randomForest)

library(e1071)

# my changes
inTrain <- createDataPartition(y = prediction$SALARY_RANGE, p = .7, list = FALSE)

training <- prediction[inTrain,]

testing <- prediction[ - inTrain,]

ranForestModel <- train(SALARY_RANGE ~ ., data = training, method = 'rf', prox = TRUE)

ranForestModel

getTree(ranForestModel$finalModel, k = 2)

LCA_CASE_EMPLOYER_NAME <- "A2Z DEVELOPMENT CENTER, INC."
LCA_CASE_EMPLOYER_CITY <- "SAN JOSE"
LCA_CASE_JOB_TITLE <- "QUALITY ASSURANCE ENGINEER II"
pred <- predict(ranForestModel, data.frame(
  LCA_CASE_EMPLOYER_NAME,
  LCA_CASE_EMPLOYER_CITY,
  LCA_CASE_JOB_TITLE))

pred

save(ranForestModel,file = "ranForestModel.RData")

