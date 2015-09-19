
attach(netconn)

netconn.glm <- netconn %>% dplyr::select(Os_type,Host_type)
#datatable(head(netconn.glm,10))
#-------- dummy varibales ------------------
dummies<-dummyVars( ~ Username+Process_name, data=netconn.glm)
#head(predict(dummies, newdata=netconn.glm))
netconn.glm1<-data.frame(predict(dummies, newdata=netconn.glm))
netconn.glm2<-cbind(netconn.glm[,1],netconn.glm1)
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
inTrain<-createDataPartition(y=netconn.rpart[,target], p=0.75, list=FALSE)
rpart.training<-netconn.rpart[inTrain,]
rpart.testing<-netconn.rpart[-inTrain,]
#---------- testing target ------------------------------------------
actual<-rpart.testing[,target]
#length(actual)
#dim(rpart.testing)
#--------------- formula and model ---------------------------------
form<-sample(paste(target,"~Username+Process_name"))
myrpart <- rpart(formula=form,data=rpart.training , method="class")

#myrpart_condition <- ctree(Os_type,data=rpart.training)
#----------------- graph -------------------------------------------
fancyRpartPlot(myrpart,main="OS type")
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





(factors<- which(sapply(netconn[vars], is.factor)))
(lvls <- sapply(factors, function(x) length(levels(netconn[[x]]))))

model.matrix(~Host_type,head(netconn))
mainEffects <- dummyVars(~ Host_type+Os_type, data = netconn)
summary(mainEffects)

#-------------------------------
#Generate example dataframe with character column
example <- as.data.frame(c("A", "A", "B", "F", "C", "G", "C", "D", "E", "F"))
names(example) <- "strcol"
#For every unique value in the string column, create a new 1/0 column
#This is what Factors do "under-the-hood" automatically when passed to function requiring numeric data
for(level in unique(example$strcol)){
  example[paste("dummy", level, sep = "_")] <- ifelse(example$strcol == level, 1, 0)
}
#-------------------------------