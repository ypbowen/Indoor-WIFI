# Required
library(readr)
library(parallel)
library(foreach)
library(iterators)
library(lattice)
library(ggplot2)
library(doParallel)
library(caret)
library(dplyr)
library(tidyr)
library(C50)
library(randomForest)


# Find how many cores are on your machine
detectCores() # Result = Typically 4 to 6

# Create Cluster with desired number of cores. Don't use them all! Your computer is running other processes. 
cl <- makeCluster(2)

# Register Cluster
registerDoParallel(cl)

# Confirm how many cores are now "assigned" to R and RStudio
getDoParWorkers() # Result 2 

# Stop Cluster. After performing your tasks, stop your cluster. 
stopCluster(cl)

data<-read.csv('C:/Users/ypbow/Documents/C4T3 files/C4T3training.csv')
WIFI<- data
WIFI$LONGITUDE<-NULL
WIFI$LATITUDE<-NULL
WIFI$USERID <-NULL
WIFI$PHONEID<-NULL
WIFI$TIMESTAMP<-NULL
str(data)
attributes(data)


#WORKING WITH VARIABLES


WIFI0<- filter(WIFI,BUILDINGID==0)
str(WIFI0)
WIFI1<- filter(WIFI,BUILDINGID==1)
str(WIFI1)
WIFI2<- filter(WIFI,BUILDINGID==2)
 

WIFI0<-within(WIFI0,MASTERID<-paste(FLOOR,SPACEID,RELATIVEPOSITION, sep='-'))
WIFI1<-within(WIFI1,MASTERID<-paste(FLOOR,SPACEID,RELATIVEPOSITION, sep = '-'))
WIFI2<-within(WIFI2,MASTERID<-paste(FLOOR,SPACEID,RELATIVEPOSITION, sep = '-'))




WIFI0$MASTERID<-as.factor(WIFI0$MASTERID )
WIFI1$MASTERID<-as.factor(WIFI1$MASTERID )
WIFI2$MASTERID<-as.factor(WIFI2$MASTERID )


str(WIFI0$MASTERID)
str(WIFI1$MASTERID)

attributes(WIFI0$MASTERID)
attributes(WIFI1$MASTERID)


str(WIFI0)
str(WIFI1)






WIFI$BUILDINGID<-NULL
WIFI$FLOOR<-NULL
WIFI$SPACEID <-NULL
WIFI$RELATIVEPOSITION<-NULL


str(WIFI0$MASTERID)

attributes(WIFI0$MASTERID)

str(WIFI0)



nzv<-nearZeroVar(WIFI, saveMetrics = FALSE) 
nzv
summary(nzv)

# create a new data set and remove near zero variance features
WIFINZV0 <- WIFI0[,-nzv]
str(WIFINZV0)

str(WIFINZV0$MASTERID)
str(WIFI0)
str(WIFI1$MASTERID)
str(WIFI1)

set.seed(123)



#######################################################BUILDING 0####################################

#################################C5.0




#WIFI0$BUILDINGID<-NULL
#WIFI0$FLOOR<-NULL
#WIFI$SPACEID <-NULL
#WIFI0$RELATIVEPOSITION<-NULL



inTrain <- createDataPartition(WIFI0$MASTERID, p=.75, list = FALSE)
training <- WIFI0[ inTrain,]
testing  <- WIFI0[-inTrain,]


str(testing$MASTERID)
fitControl <- trainControl(method = "repeatedcv",  number = 10, repeats=1)

#LMFit1 <- train(train_b1$Location~., data = training, method = "lm", trControl=fitControl)
C5Fit0 <- train(MASTERID~., data = training, method = "C5.0", trControl=fitControl)


#check the results
C5Fit0





#unregister_dopar <- function() {
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}



#############################RF

inTrain <- createDataPartition(WIFI0$MASTERID, p=.75, list = FALSE)
training <- WIFI0[ inTrain,]
testing  <- WIFI0[-inTrain,]
#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)

#train Random Forest with a tuneLenght = 1 (trains with 1 mtry value for RandomForest)

rfFit0 <- train(MASTERID~., data = training, method = "rf", trControl=fitControl, tuneLength = 1)
#training results

rfFit0
#plot(rfFit0)




#########################KNN

inTrain <- createDataPartition(WIFI0$MASTERID, p=.75, list = FALSE)
training <- WIFI0[ inTrain,]
testing  <- WIFI0[-inTrain,]
#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)

#train Random Forest with a tuneLenght = 1 (trains with 1 mtry value for RandomForest)

knnFit0 <- train(MASTERID~., data = training, method = "knn", trControl=fitControl)
#training results

knnFit0
#plot(rfFit0)









####BLD0####
testPredc5.00<-predict(C5Fit0, testing)
#postResample
post<-postResample(testPredc5.00, testing$MASTERID)
post
                   
                
testPredrf0<-predict(rfFit0, testing)
                   #postResample
post<-postResample(testPredrf0, testing$MASTERID)
post
                   


testPredknn0<-predict(knnFit0, testing)
#postResample
post<-postResample(testPredknn0, testing$MASTERID) 
post





confusionMatrix(testPredc5.00,testing$MASTERID)
confusionMatrix(testPredrf0,testing$MASTERID)
confusionMatrix(testPredknn0,testing$MASTERID)

ModelData<- resamples(list(knn=knnFit0,c5.0=C5Fit0,  rf=rfFit0 ))
                   


summary(ModelData)
  
  
  #######################################BUILDING1#############################3

WIFI1$BUILDINGID<-NULL
WIFI1$FLOOR<-NULL
WIFI1$SPACEID <-NULL
WIFI1$RELATIVEPOSITION<-NULL



inTrain <- createDataPartition(WIFI1$MASTERID, p=.75, list = FALSE)
training <- WIFI1[ inTrain,]
testing  <- WIFI1[-inTrain,]


str(testing$MASTERID)
fitControl <- trainControl(method = "repeatedcv",  number = 10, repeats=1)

#LMFit1 <- train(train_b1$Location~., data = training, method = "lm", trControl=fitControl)
C5Fit1 <- train(MASTERID~., data = training, method = "C5.0", trControl=fitControl)


#check the results
C5Fit1
summary(C5Fit1)




unregister_dopar <- function() {
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}



#############################RF

inTrain <- createDataPartition(WIFI1$MASTERID, p=.75, list = FALSE)
training <- WIFI1[ inTrain,]
testing  <- WIFI1[-inTrain,]
#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)

#train Random Forest with a tuneLenght = 1 (trains with 1 mtry value for RandomForest)

rfFit1 <- train(MASTERID~., data = training, method = "rf", trControl=fitControl, tuneLength = 1)
#training results

rfFit1
#plot(rfFit0)




#########################KNN

inTrain <- createDataPartition(WIFI1$MASTERID, p=.75, list = FALSE)
training <- WIFI1[ inTrain,]
testing  <- WIFI1[-inTrain,]
#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)

#train Random Forest with a tuneLenght = 1 (trains with 1 mtry value for RandomForest)

knnFit1 <- train(MASTERID~., data = training, method = "knn", trControl=fitControl)
#training results

knnFit1
#plot(rfFit0)









testPredc5.01<-predict(C5Fit1, testing)
#postResample
post<-postResample(testPredc5.01, testing$MASTERID)
post


testPredrf1<-predict(rfFit1, testing)
#postResample
post<-postResample(testPredrf1, testing$MASTERID)
post



testPredknn1<-predict(knnFit1, testing)
#postResample
post<-postResample(testPredknn1, testing$MASTERID) 
post





confusionMatrix(testPredc5.01,testing$MASTERID)
confusionMatrix(testPredrf1,testing$MASTERID)
confusionMatrix(testPredknn1,testing$MASTERID)

Model<- resamples(list(knn=knnFit1,c5.0=C5Fit1,  rf=rfFit1 ))



summary(Model)






#######################################BUILDING2#############################3

WIFI2$BUILDINGID<-NULL
WIFI2$FLOOR<-NULL
WIFI2$SPACEID <-NULL
WIFI2$RELATIVEPOSITION<-NULL



inTrain <- createDataPartition(WIFI2$MASTERID, p=.75, list = FALSE)
training <- WIFI2[ inTrain,]
testing  <- WIFI2[-inTrain,]


str(testing$MASTERID)
fitControl <- trainControl(method = "repeatedcv",  number = 10, repeats=1)

#LMFit1 <- train(train_b1$Location~., data = training, method = "lm", trControl=fitControl)
C5Fit2 <- train(MASTERID~., data = training, method = "C5.0", trControl=fitControl)


#check the results
C5Fit2
summary(C5Fit2)




unregister_dopar <- function() {
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}



#############################RF

inTrain <- createDataPartition(WIFI2$MASTERID, p=.75, list = FALSE)
training <- WIFI2[ inTrain,]
testing  <- WIFI2[-inTrain,]
#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)

#train Random Forest with a tuneLenght = 1 (trains with 1 mtry value for RandomForest)

rfFit2 <- train(MASTERID~., data = training, method = "rf", trControl=fitControl, tuneLength = 1)
#training results

rfFit2
#plot(rfFit2)




#########################KNN

inTrain <- createDataPartition(WIFI2$MASTERID, p=.75, list = FALSE)
training <- WIFI2[ inTrain,]
testing  <- WIFI2[-inTrain,]
#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)

#train Random Forest with a tuneLenght = 1 (trains with 1 mtry value for RandomForest)

knnFit2<- train(MASTERID~., data = training, method = "knn", trControl=fitControl)
#training results

knnFit2
#plot(rfFit2)







testPredrf2<-predict(rfFit2, testing)
#postResample
post<-postResample(testPredrf2, testing$MASTERID)
post



testPredknn2<-predict(knnFit2, testing)
#postResampl2
post<-postResample(testPredknn2, testing$MASTERID) 
post





confusionMatrix(testPredc5.02,testing$MASTERID)
confusionMatrix(testPredrf2,testing$MASTERID)
confusionMatrix(testPredknn2,testing$MASTERID)

Model<- resamples(list(knn=knnFit2,c5.0=C5Fit2,  rf=rfFit2 ))



summary(Model)





