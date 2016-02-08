# Log Loss function
# calculate logloss; p is a probability vector; y is a vector of zero or one.
logloss <- function(p,y) {
    xp <- function(pv) {return(max(min(pv,1-10^{-15}),10^{-15}))}
    p <- sapply(p,xp)
    loss <- -sum(y*log(p)+(1-y)*log(1-p))/length(p)
    return(loss)
}
# example log loss
p<-c(0.4,0.4,.55,.55,.55); y<-c(0,0,1,1,1);
logloss(p,y)
# Log Loss of all predictions 0.5 (Kaggle Leaderboard: All 0.5 Benchmark = 0.69315)
logloss(rep(0.5,2500+7500),c(rep(0,2500),rep(1,7500)))
# Get data
# directory <- "/home/jack/R/cardif/"
directory <- "C:/Users/lajackso/Documents/GitHub/cardif/"
fileData <- paste0(directory,"train.csv")
data <- read.csv(fileData)
# rows and columns
dim(data)
# fraction complete cases
sum(complete.cases(data))/dim(data)[1]
# identify numeric columns (convert others to factors)
numericCols <- which(sapply(data, is.numeric))
data[,-(numericCols)] <- lapply(data[,-(numericCols)] , factor)
# summary for numerical columns
summary(data[,numericCols])
# summary for factor columns
summary(data[,-(numericCols)])
# fraction of ones in target
sum(data$target)/dim(data)[1]

