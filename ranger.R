# I pulled this ecample using Ranger from Kaggle.
# I modified it to use 1/3 hold out for testing.
# It achieves a log loss of 0.4976162 using the ranger package on 
#     numeric data with no tuning.

library(ranger)
library(randomForest)

directory <- "C:/Users/lajackso/Documents/GitHub/cardif/"
fileData <- paste0(directory,"train.csv")
data <- read.csv(fileData)

set.seed(1959)
library(caret)
inTrain = createDataPartition(data$target, p = 2/3, list = FALSE)
train = data[inTrain,]
test = data[-inTrain,]
rm(data)

# Use only numeric cols
train <- train[, sapply(train, is.numeric)]
test <- test[, sapply(test, is.numeric)]
colnames(train)

print("Fix NA")
train <- na.roughfix(train)
train$target <- as.factor(train$target)

test <- na.roughfix(test)

print("Start estimation")
rf.fit <- ranger(target~.,data=train,num.trees = 300,verbose=TRUE,write.forest=TRUE,probability=TRUE)

yhat <- predict(rf.fit, test)$predictions[,2]

# calculate logloss; p is a probability vector; y is a vector of zero or one.
logloss <- function(p,y) {
      xp <- function(pv) {return(max(min(pv,1-10^{-15}),10^{-15}))}
      p <- sapply(p,xp)
      loss <- -sum(y*log(p)+(1-y)*log(1-p))/length(p)
      return(loss)
}

logloss(yhat,test$target)

library(caret)
confusionMatrix(round(yhat,0),test$target)

write.csv(data.frame(ID = test$ID, PredictedProb = yhat), "random_forest_benchmark.csv", row.names = F)



