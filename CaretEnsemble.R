#https://github.com/gdwangh/edxTheAnalyticsEdge/blob/master/kaggleCompetition/post-study/ensembleModel.R
#These function show information about models and 
#packages that are accessible via train
#modelLookup(model = NULL)
#getModelInfo(model = NULL, regex = TRUE, ...)
#checkInstall(pkg)
#modelLookup("gbm")
#getModelInfo("pls")

#https://github.com/gdwangh/edxTheAnalyticsEdge/blob/master/kaggleCompetition/otherShared/Mario%20Segal/Competition.R

#VERY GOOD - model compare
#https://github.com/gdwangh/edxTheAnalyticsEdge/blob/master/kaggleCompetition/otherShared/ebrucecfa/Kaggle_ModelCompare.R

library(doParallel)
cl<- makeCluster(detectCores()-1)  
registerDoParallel(cl)

library(caret)
ensCtrl<- trainControl(method="cv",
                       number=10,
                       savePredictions=TRUE,
                       allowParallel=TRUE,
                       classProbs=TRUE,
                       selectionFunction="best",
                       summaryFunction=twoClassSummary)
# RANDOM FOREST
rfGrid<- expand.grid(mtry=c(12))

# GLM
glmGrid<- expand.grid(alpha=c(1), lambda=8e-04)

# SVM RADIAL
svmGrid<- expand.grid(.sigma=c(0.001),.C=c(20))

#GBM
gbmGrid<- expand.grid(n.trees=c(4900), interaction.depth=c(26), shrinkage=c(.001),n.minobsinnode = 10)


# CREATE ENSEMBLE
library(caretEnsemble)
set.seed(1000)
model_list<- caretList(
  PopularFactor~NewsDesk+SectionName+SubsectionName+logWordCount+Weekday+Hour,
  data=newsTrain,
  trControl=ensCtrl,
  metric="ROC",
  tuneList=list(
    rf=caretModelSpec(method="rf", tuneGrid=rfGrid, nodesize=1, ntree=3000),
    glmnet=caretModelSpec(method="glmnet",tuneGrid=glmGrid, preProcess=c("center","scale")),
    gbm=caretModelSpec(method="gbm", tuneGrid=gbmGrid),
    svm=caretModelSpec(method="svmRadial",tuneGrid=svmGrid, preProcess=c("center","scale"))
  )
)

stopCluster(cl)

greedy_ensemble<- caretEnsemble(model_list)

library('caTools')
model_preds<- lapply(model_list, predict, newdata=newsTrain, type='prob')
model_preds<- lapply(model_preds, function(x) x[,'Yes'])
model_preds<- data.frame(model_preds)

ens_preds<- predict(greedy_ensemble, newdata=newsTrain)
model_preds$ensemble<- ens_preds

colAUC(model_preds, newsTrain$PopularFactor)


library("ROCR")
ROCR.Pred = prediction( newsTrain$Popular, ens_preds>0.5)
auc = as.numeric(performance(ROCR.Pred, "auc")@y.values)
auc  # 0.8956273

pred.test = predict(greedy_ensemble, newdata=newsTest)
MySubmission = data.frame(UniqueID = newsTest$UniqueID, Probability1 = pred.test)
write.csv(MySubmission, "post-study/ensembleModel.csv", row.names=FALSE)

# other method
cl<- makeCluster(detectCores()-1)  
registerDoParallel(cl)

gbm_stack<- caretStack(
  model_list,
  method='gbm',
  verbose=FALSE,
  # tuneGrid=expand.grid(n.trees=c(2500), interaction.depth=c(24), shrinkage=c(.001),n.minobsinnode=c(10)),
  metric='ROC',
  trControl=trainControl(
    method='cv',
    number=10,
    savePredictions=TRUE,
    classProbs=TRUE,
    allowParallel=TRUE,
    summaryFunction=twoClassSummary
  )
)


stopCluster(cl)

model_preds2 <- model_preds
model_preds2$ensemble <- predict(gbm_stack, newdata=newsTrain, type='prob')$Yes
colAUC(model_preds2, newsTrain$PopularFactor)

library("ROCR")
ROCR.Pred2 = prediction( newsTrain$Popular, model_preds2$ensemble>0.5)
auc = as.numeric(performance(ROCR.Pred2, "auc")@y.values)
auc  # 0.9036781


pred.test = predict(gbm_stack, newdata=newsTest, type='prob')$Yes
MySubmission = data.frame(UniqueID = newsTest$UniqueID, Probability1 = pred.test)
write.csv(MySubmission, "post-study/ensembleModel_gbm.csv", row.names=FALSE)
