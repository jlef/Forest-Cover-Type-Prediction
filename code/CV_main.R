library(plyr)
library(dplyr)
library(caret)

library(extraTrees)

setwd("C:/Users/GALT/Desktop/Forest Cover Type Prediction/code/submissions/New folder")


####################################
# Train Set
####################################
train.set = read.csv(file='../../../data/train.csv',head=TRUE,sep=",",stringsAsFactors=F)

train.set$Soil_Type7 = NULL
train.set$Soil_Type15 = NULL


X.train <- select(train.set, Elevation:Horizontal_Distance_To_Fire_Points)

#hydrology lower or higher
X.train$HydrologyVerticalOrientation <- (X.train$Vertical_Distance_To_Hydrology > 0) + 0

X.train$Horizontal_Hydrology_Roadways <- abs(X.train$Horizontal_Distance_To_Roadways) - abs(X.train$Horizontal_Distance_To_Hydrology)
X.train$Horizontal__Hydrology_Fire_Points <- abs(X.train$Horizontal_Distance_To_Hydrology) - abs(X.train$Horizontal_Distance_To_Fire_Points)
X.train$Elevation <- log(X.train$Elevation)

#Total Hillshade
X.train$Hillshade_Total <- with(X.train, Hillshade_9am + Hillshade_Noon + Hillshade_3pm)


X.train <- cbind(X.train, select(train.set, Wilderness_Area1:Soil_Type40))



y.train <- train.set[,c("Id","Cover_Type")]


####################################
# Test Set
####################################

test.set = read.csv(file='../../../data/test.csv',head=TRUE,sep=",",stringsAsFactors=F)

test.set$Soil_Type7 = NULL
test.set$Soil_Type15 = NULL


X.test <- select(test.set, Elevation:Horizontal_Distance_To_Fire_Points)

#hydrology lower or higher
X.test$HydrologyVerticalOrientation <- (X.test$Vertical_Distance_To_Hydrology > 0) + 0

X.test$Horizontal_Hydrology_Roadways <- abs(X.test$Horizontal_Distance_To_Roadways) - abs(X.test$Horizontal_Distance_To_Hydrology)
X.test$Horizontal__Hydrology_Fire_Points <- abs(X.test$Horizontal_Distance_To_Hydrology) - abs(X.test$Horizontal_Distance_To_Fire_Points)
X.test$Elevation <- log(X.test$Elevation)

#Total Hillshade
X.test$Hillshade_Total <- with(X.test, Hillshade_9am + Hillshade_Noon + Hillshade_3pm)


X.test <- cbind(X.test, select(test.set, Wilderness_Area1:Soil_Type40))



X.test$type <- "test"
X.train$type <- "train"

X.all <- rbind(X.test, X.train)
temp <- X.all[,"type"]
X.all[,"type"] <- NULL
X.all <- data.frame(Id=c(test.set$Id, train.set$Id), scale(X.all, center=T, scale=T))



X.all$type <- temp
X.train <- X.all[X.all$type=="train",]
X.train$type <- NULL

X.test <- X.all[X.all$type=="test",]
X.test$type <- NULL

rm(X.all, temp)

Y_eT <- data.frame(Id=test.set[,"Id"], Cover_Type=NA)



##ET
eT_fit <- extraTrees(X.train
                     ,as.factor(y.train[,2])
                     ,ntree=500
                     ,nodesize = 1
                     ,numRandomCuts = 5
                     ,evenCuts = FALSE
                     ,numThreads = 6
)
Y_eT[, 2] <- predict(eT_fit, X.test)


example = read.csv(file='../../../data/sampleSubmission.csv',head=TRUE,sep=",")

example$Id <- Y_eT[, 1]
example$Cover_Type <- as.numeric(Y_eT[, 2])

write.csv((example),file='submission.csv',row.names=FALSE)
