library(plyr)
library(dplyr)
library(caret)

library(extraTrees)


train.set = read.csv(file='../data/train.csv',head=TRUE,sep=",",stringsAsFactors=F)

train.set$Soil_Type7 = NULL
train.set$Soil_Type15 = NULL


X <- select(train.set, Elevation:Horizontal_Distance_To_Fire_Points)

####################################
# Feature Eng.
####################################

#hydrology lower or higher
X$HydrologyVerticalOrientation <- (X$Vertical_Distance_To_Hydrology > 0) + 0

X$Horizontal_Hydrology_Roadways <- abs(X$Horizontal_Distance_To_Roadways) - abs(X$Horizontal_Distance_To_Hydrology)

X$Horizontal__Hydrology_Fire_Points <- abs(X$Horizontal_Distance_To_Hydrology) - abs(X$Horizontal_Distance_To_Fire_Points)
X$Horizontal__Hydrology_Fire_Points[is.infinite(X$Horizontal__Hydrology_Fire_Points)] <- 0

X$Elevation <- log(X$Elevation)

#Total Hillshade
X$Hillshade_Total <- with(X, Hillshade_9am + Hillshade_Noon + Hillshade_3pm)

X <- cbind(X, select(train.set, Wilderness_Area1:Soil_Type40))
X <- data.frame(scale(X, center=T, scale=T))

y <- train.set[,c("Id","Cover_Type")]

#results matrix
Y_eT <- y
Y_eT[,-1] <- NA



####################################
# Create folds
####################################
set.seed(117)
folds <- createFolds(y$Cover_Type, k=3, list=T) #10 fold cross validation


####################################
# Training parameters
####################################
foldIndex <- 0;



for(i in 1:length(folds)){
  
  print(paste0("i: ", i))
  
  fold.X.train <- X[-folds[[i]],]
  fold.X.test <- X[folds[[i]],]
  
  fold.y.train <- y[-folds[[i]],]
  fold.y.test <- y[folds[[i]],]
  
  trgtRows <- c((foldIndex+1):(foldIndex+length(folds[[i]])))
  
  ##ET
  Y_eT[trgtRows, "Id"] <- fold.y.test[,"Id"]
  
  eT_fit <- extraTrees(fold.X.train
                       ,as.factor(fold.y.train[,2])
                       ,ntree=500
                       ,nodesize = 1
                       ,numRandomCuts = 5
                       ,evenCuts = FALSE
                       ,numThreads = 6
  )
  Y_eT[trgtRows, 2] <- predict(eT_fit, fold.X.test)
  
  foldIndex <- foldIndex + length(folds[[i]])  
}


Y_eT <- arrange(Y_eT, Id)

confusionMatrix(Y_eT[,2], y[,2])
sum(y[,2] == Y_eT[,2])/length(y[,2])
