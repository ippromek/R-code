library(readr)
library(caret)
library(ggbiplot)
library(ggplot2)
library(dplyr)
library(rgl)
data_train <- read.csv("~/Documents/Datasets/R-code/Datasets/train.csv")
#------------- create 2 subsets ---------------------------------------
set.seed(111)
split <- createDataPartition(data_train$label, p = 0.6, list = FALSE)
train <- slice(data_train, split)
test <- slice(data_train, -split)
#---------- checl variables almost constant ---------------------------
zero_var_col <- nearZeroVar(train, saveMetrics = T)
train <- train[, !zero_var_col$nzv]
test <- test[, !zero_var_col$nzv]
dim(train)

#----------- PCA on 1000 records --------------------------------------
train_1000 <- train[sample(nrow(train), size = 1000),]
ggplot(data = train_1000, aes(x = pixel152, y = pixel153, color = factor(label))) + geom_point()
pc <- princomp(train_1000[, -1], cor=TRUE, scores=TRUE)
plot3d(pc$scores[,1:3], col= train_1000$label + 1, size = 0.7, type = "s")

#------------ PCA on all records ---------------------------------------
pc <- princomp(train[, -1], cor=TRUE, scores=TRUE)
variance <- pc$sdev^2/sum(pc$sdev^2)
cumvar <- cumsum(variance)
cumvar <- data.frame(PC = 1:252, CumVar = cumvar)
ggplot(data = cumvar, aes(x = PC, y = CumVar)) + geom_point()
#------------- PCA graph -----------------------------------------------
variance <- data.frame(PC = 1:252, Var = variance*100)
ggplot(data = variance[1:10,], aes(x = factor(PC), y = Var)) + geom_bar(stat = "identity")

sum(variance$Var[1:70])

#---------- Transform test and train into PCA space ------------------
train <- predict(pc) %>% cbind(train$label, .) %>% as.data.frame(.) %>% select(1:71)
colnames(train)[1]<- "label"
train$label <- as.factor(train$label)
test %<>% predict(pc, .) %>% cbind(test$label, .) %>% as.data.frame(.) %>% select(1:71)
colnames(test)[1]<- "label"
 #------------ 2 CPU in parallel -------------------------------------
#detectCores()
#cl <- makeCluster(detectCores())
#registerDoParallel(cl)
cl <- makePSOCKcluster(2)
registerDoParallel(cl)
#getDoParWorkers()
#------------ KNN ---------------------------------------------------
set.seed(111)
train_1000 <- train[sample(nrow(train), size = 1000),]
#------------10-fold cross-validation (CV) ------------------------
ctrl <- trainControl(method="repeatedcv",repeats = 3)
knnFit <- train(label ~ ., data = train_1000, method = "knn", trControl = ctrl,tuneLength = 20)
knnFit
#------------- final value for N ----------------------------------
grid <- expand.grid(k=2:5)
knnFit <- train(label ~ ., data = train_1000, method = "knn", trControl = ctrl, tuneGrid=grid)
knnFit
#-------------- predict and calculate accuracy---------------------
prediction_knn <- knn(train, test, train$label, k=5)
table(test$label, prediction_knn)
#-------- accuracy -----------------------------------------------
sum(diag(table(test$label, prediction_knn)))/nrow(test)


#------------------------------------------------------------------
#--------------- RANDOM FOREST ------------------------------------
# mtry - how many features to be used in every tree
# ------ example trainControl for binary classification ------------
#ensCtrl<- trainControl(method="cv",
#                       number=10,
#                       savePredictions=TRUE,
#                       allowParallel=TRUE,
#                       classProbs=TRUE,
#                       selectionFunction="best",
#                       summaryFunction=twoClassSummary)
rfFit <- train(label ~ ., data = train_1000, method = "rf", trControl = ctrl,tuneLength = 3)
rfFit
#---------------- train model -------------------------------------
grid <- expand.grid(mtry=2:6)
rfFit <- train(label ~ ., data = train_1000, method = "rf", trControl = ctrl,tuneGrid=grid)

#--------------------- Use model with mtry=4 ----------------------------------------
rfFit <- randomForest(label ~ ., data = train[sample(nrow(train), size = 15000),], mtry = 4)
prediction_rf<-predict(rfFit,test)
table(test$label, prediction_rf)
rfFit

importance(rfFit$finalModel, type=1)
varImp(rfFit)
#------------------- accuaracy---------------------------------------
sum(diag(table(test$label, prediction_rf)))/nrow(test)

#--------------------------------------------------------------------
#----------------- SVM ----------------------------------------------
#----- parameters - sigma (регуляризационный параметр) и C (параметр, определяющий форму ядра).
svmFit <- train(label ~ ., data = train_1000, method = "svmRadial", trControl = ctrl,tuneLength = 5)
svmFit
# ---------- train model --------------------------------------------
svmFit <- ksvm(label ~ ., data = train,type="C-svc",kernel="rbfdot",kpar=list(sigma=0.008),C=4)
prediction_svm <- predict(svmFit, newdata = test)
table(test$label, prediction_svm)
#----------- accuracy------------------------------------------------
sum(diag(table(test$label, prediction_svm)))/nrow(test)

#----------- ensemble models --------------------------------------
all_prediction <- cbind(as.numeric(levels(prediction_knn))[prediction_knn], 
                        as.numeric(levels(prediction_rf))[prediction_rf], 
                        as.numeric(levels(prediction_svm))[prediction_svm])

predictions_ensemble <- apply(all_prediction, 1, function(row) {
  row %>% table(.) %>% which.max(.) %>% names(.) %>% as.numeric(.)
})

table(test$label, predictions_ensemble)
#------------ accuracy ------------------------------------------
sum(diag(table(test$label, predictions_ensemble)))/nrow(test)

