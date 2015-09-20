
attach(netconn)

netconn.glm <- netconn %>% dplyr::select(Os_type,Host_type)
#datatable(head(netconn.glm,10))
#-------- dummy varibales ------------------
#dummies<-dummyVars( ~ Username+Process_name, data=netconn.glm)
#head(predict(dummies, newdata=netconn.glm))
#netconn.glm1<-data.frame(predict(dummies, newdata=netconn.glm))
#netconn.glm2<-cbind(netconn.glm[,1],netconn.glm1)
#colnames(netconn.glm2)[1]

colnames(netconn.glm2)[1]<-c("Os_type")
#head(netconn.glm2)
#str(netconn.glm1)

#colnames(netconn.glm2)
#netconn.glm<-netconn.glm[,1:5]
datatable(netconn.glm2)
mylogit <- glm(Os_type ~ .,data = netconn.glm, family = "binomial")
summary(mylogit)
#-----------------------------------------------------------------------

#------------ decision trees------------------------------------------

netconn.rpart <- netconn %>% dplyr::select(Host_type,Os_type,Username,Process_name)
target<-c("Os_type")
netconn.rpart[,target]<-as.integer(ifelse(netconn.rpart[,target]=="windows",1,0))
#head(netconn.rpart)
#--------------- inputs ----------------------------------------------
vars<-colnames(netconn.rpart)
inputs<-setdiff(vars,target) 
#------------ training and testing datasets ---------------------------
set.seed(998)
inTrain<-createDataPartition(y=netconn.rpart[,target], p=0.75, list=FALSE)
rpart.training<-netconn.rpart[inTrain,]
rpart.testing<-netconn.rpart[-inTrain,]
#---------- testing target ------------------------------------------
actual<-rpart.testing[,target]
#length(actual)
#dim(rpart.testing)
#--------------- formula and model ---------------------------------
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)
set.seed(123)

form<-sample(paste(target,"~Username+Process_name"))
myrpart <- rpart(formula=form,data=rpart.training ,control=rpart.control(maxdepth=2), method="class")

head(rpart.training)

#------------ conditional tree ---------------------------------
#myrpart_condition <- ctree(Os_type,data=rpart.training)
#----------------- graph -------------------------------------------
fancyRpartPlot(myrpart,main="OS type")
prp(myrpart,type=2,extra = 101,nn=TRUE,fallen.leaves = TRUE,faclen=0,varlen=0,
    shadow.col = "grey",branch.lty = 5)

summary(myrpart)
printcp(myrpart)
plotcp(myrpart)

#--------- prediction on testing results --------------------------
predicted<-predict(myrpart,rpart.testing,type="class")

#---------- confucion matrix -------------------------------------
round(100*table(actual,predicted,dnn=c("Actual","Predicted"))/length(predicted))
confusionMatrix(table(predicted,actual)) 

acc<-sum(predicted==rpart.testing[,target],na.rm=TRUE)/length(rpart.testing[,target])
acc

test<-as.party(myrpart)
plot(test)
########################################################
#------------ GBM --------------------------------------------
gbmTrain <- rpart.training
# only 1 or 0
gbmTrain[,target] <- as.integer(ifelse(gbmTrain[,target]=="windows",1,0))
table(gbmTrain[,target])

gbm.mod <- gbm(formula = Os_type~.,           # use all variables
               distribution = "bernoulli",       # for a classification problem
               data = gbmTrain,
               n.trees = 2000,                         # 2000 boosting iterations
               interaction.depth = 7,              # 7 splits for each tree
               shrinkage = 0.01,                       # the learning rate parameter
               verbose = FALSE)                        # Do not print the details
summary(gbm.mod) 
# ---------- use trainControl from caret ----------------------------------
ctrl <- trainControl(method="repeatedcv",               # use repeated 10fold cross validation
                     repeats=5,                          # do 5 repititions of 10-fold cv
                     summaryFunction=twoClassSummary,    # Use AUC to pick the best model
                     classProbs=TRUE)

#---Use the expand.grid to specify the search space Note that the default search grid 
#---selects 3 values of each tuning parameter
grid <- expand.grid(interaction.depth = seq(1,4,by=2), # look at tree depths from 1 to 4
                    n.trees=seq(10,100,by=10), # let iterations go from 10 to 100
                    shrinkage=c(0.01,0.1),
                    n.minobsinnode=1)         # Try 2 values of the learning rate parameter

set.seed(1)
registerDoParallel(4)       # Registrer a parallel backend for train
getDoParWorkers()
head(gbmTrain)

system.time(gbm.tune <- train(x=gbmTrain[,c(1,3,4)],y=gbmTrain$Os_type,
                              method = "gbm",
                         #     metric = "ROC",
                              trControl = ctrl,
                              tuneGrid=grid,
                              verbose=FALSE))
########################################################
#----------- SUPPORT VECTOR MACHINE --------------------------------
set.seed(1)
registerDoParallel(4,cores=4)
getDoParWorkers()
system.time(
  svm.tune <- train(x=trainX,
                    y= trainData$Class,
                    method = "svmRadial",
                    tuneLength = 9,                 # 9 values of the cost function
                    preProc = c("center","scale"),
                    metric="ROC",
                    trControl=ctrl) # same as for gbm above
)   

svm.tune

# Plot the SVM results
plot(svm.tune,
     metric="ROC",
     scales=list(x=list(log=2)))
#---------------------------------------------------
# SVM Predictions
svm.pred <- predict(svm.tune,testX)
head(svm.pred)

confusionMatrix(svm.pred,testData$Class)
########################################################

#--------- model comparison ------------------
resampls = resamples(list(RF = trf,
                          GBM = tgbm))

difValues = diff(resampls)
summary(difValues)

#-------------- model comparison graphs -----------
rValues <- resamples(list(svm=svm.tune,gbm=gbm.tune))
rValues$values
summary(rValues)
trellis.par.set(caretTheme())
xyplot(rValues,metric="ROC")        # scatter plot
bwplot(rValues,metric="ROC")            # boxplot
parallelplot(rValues,metric="ROC")  # parallel plot
dotplot(rValues,metric="ROC")           # dotplot
splom(rValues,metric="ROC")
#################################################


##########################################################
#--------- generate ROC curves for the methods -----------
# # Generate an ROC curve for the rf method
predRF <- prediction(rf_pred[,1], rm_test$PREGNANT)
perfRF <- performance(predRF, "tpr", "fpr")
#----- another option ----------------------------------
auc2 = as.numeric(performance(predRF, "auc")@y.values)

plot(perfRF, main = "ROC curves for randomForest, gbm and bag models")

# Generate an ROC curve for the gbm method
pred_gbm <- prediction(gbm_pred[,1], rm_test$PREGNANT)
perf_gbm <- performance(pred_gbm, "tpr", "fpr")
plot(perf_gbm, add = TRUE, col = "blue")

#Generate an ROC curve for the 'bag' method
pred_bag <- prediction(bag_pred[,1], rm_test$PREGNANT)
perf_bag <- performance(pred_bag, "tpr", "fpr")
plot(perf_bag, add = TRUE, col = "red")

# Add legends to the plot
legend("right", legend = c("randomForest", "gbm", "bag"), bty = "n", cex = 1, lty = 1,
       col = c("black", "blue", "red"))
#-----------------------------------------------------
########################################################



(factors<- which(sapply(netconn[vars], is.factor)))
(lvls <- sapply(factors, function(x) length(levels(netconn[[x]]))))
