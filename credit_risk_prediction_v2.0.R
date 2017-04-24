rm(list = ls())

setwd("D:/Work/credit/creditriskcode")
library(readr)
creditData <- read.csv("D:/Work/credit/creditriskcode/german_credit (1).csv")

row<- nrow(creditData)
col <- ncol(creditData)
cat("File is imported...\n No. of records=",row,"\n No.of Columns=",col)

creditData[,c(1,3,4,6,7,9,10,12,14,15,17,19,20,21)] <- as.data.frame(apply(creditData[,c(1,3,4,6,7,9,10,12,14,15,17,19,20,21)], 2 ,FUN = factor))
#creditData Exploration
str(creditData)
summary(creditData)



#about original Creditability varaiable
table(creditData$Creditability)
print(prop.table(table(creditData$Creditability)))


library(MASS)
md= glm(creditData$Creditability~., creditData, family=binomial(link = "logit"))
x<-stepAIC(md,direction="both")
selectedcols<-attr(terms(x),"term.labels")
cat("After feature selection, No.of important features=",length(selectedcols))
print(selectedcols)
write(selectedcols, "credit_important_features.txt")
newcreditData<-creditData[selectedcols]
newcreditData$Creditability<-creditData$Creditability

library(caret)
set.seed(1234)
splitIndex <- createDataPartition(newcreditData$Creditability, p = .70,
                                  list = FALSE,
                                  times = 1)
traincreditData <- newcreditData[ splitIndex,]
testcreditData <- newcreditData[-splitIndex,]

table(traincreditData$Creditability)
prop.table(table(traincreditData$Creditability))
table(testcreditData$Creditability)
prop.table(table(testcreditData$Creditability))


#1. Classification Tree(rpart)
library("C50")
library(partykit)
library(rpart)
rparttrain=rpart(traincreditData$Creditability~.,data=traincreditData)
write(capture.output(summary(rparttrain)), "creditRpartModelTrainSummary.txt")
modparty=as.party(rparttrain)
rls <- partykit:::.list.rules.party(modparty)
rval <- data.frame(response = predict(modparty, type = "response"))
rval$prob <- predict(modparty, type = "prob")
rval$rule <- rls[as.character(predict(modparty,type="node"))]
write.csv(rval,"creditallrulestrain.csv",row.names = F)
predict_rpart= predict(rparttrain,traincreditData[,-14],type="class")
conf.train_rpart<-table(traincreditData$Creditability,predict_rpart)
TP <- conf.train_rpart[1,1] 
TN <- conf.train_rpart[2,2]
FN <- conf.train_rpart[1,2]
FP <- conf.train_rpart[2,1]
accuracy <- ((TP+TN)/(TP+TN+FN+FP))*100
cat("accuracy is --->", accuracy)
sensitivity= (TP/(TP+FN))*100  
cat("sensitivity is --->", sensitivity) 
specificity = (TN/(TN+FP))*100  
cat("specificity is --->", specificity)
# MCC=(TP*TN-FP*FN)/sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN))
# cat("Matthew Correlation Cofficient=",MCC)
write.csv(rbind(c("Sensitivity=",sensitivity),
                c("Specificity=",specificity),
                c("Accuracy=",accuracy)),"rpart_credit_train_metrics.csv",row.names = FALSE)


rval_new <- data.frame(testcreditData,response = predict(modparty, newdata=testcreditData[,-14],type = "response"))
rval_new$prob<-predict(modparty,newdata = testcreditData[,-14],type = "prob")
rval_new$rule <- rls[as.character(predict(modparty,newdata = testcreditData[,-14],type="node"))]
write.csv(rval_new,"credit_test_allrules.csv",row.names = F)
predict_rpart_tst= predict(modparty,testcreditData[,-14],type="response", na.action = na.omit)
conf.test_rpart<-table(testcreditData$Creditability,predict_rpart_tst)
TP <- conf.test_rpart[1,1] 
TN <- conf.test_rpart[2,2]
FN <- conf.test_rpart[1,2]
FP <- conf.test_rpart[2,1]

accuracy <- ((TP+TN)/(TP+TN+FN+FP))*100
cat("accuracy is --->", accuracy)
sensitivity= (TP/(TP+FN))*100  
cat("sensitivity is --->", sensitivity) 
specificity = (TN/(TN+FP))*100  
cat("specificity is --->", specificity)
write.csv(rbind(c("Sensitivity=",sensitivity),
                c("Specificity=",specificity),
                c("Accuracy=",accuracy)),"rpart_credit_test_metrics.csv",row.names = FALSE)

w = table(as.factor(rval_new$rule))
t = as.data.frame(w)
colnames(t)[1]<-"Rule"
newdata1<-t[order(-t$Freq),]
write.csv(newdata1,"credit_test_both_rules_freq.csv",row.names=FALSE)

w=table(rval_new$rule[rval_new$Creditability=="Y"&rval_new$Creditability=="Y"])
t=as.data.frame(w)
colnames(t)[1]<-"Rule"
newdata1<-t[order(-t$Freq),]
write.csv(newdata1,"credit_test_Yonly_rules_freq.csv",row.names=FALSE)

w=table(rval_new$rule[rval_new$Creditability=="N"&rval_new$response=="N"])
t=as.data.frame(w)
colnames(t)[1]<-"Rule"
newdata1<-t[order(-t$Freq),]
write.csv(newdata1,"credit_test_Nonly_rules_freq.csv",row.names=FALSE)
#ayush
dota <- cbind(testcreditData,predict_rpart_tst)






###2. Logistic Regression
md= glm(traincreditData$Creditability~., data = traincreditData, family=binomial(link = "logit"))
probabilities=predict(md, testcreditData, type="response")
predictions= cut(probabilities, c(-Inf,0.43,Inf), labels=c("Risk","NoRisk"))
allpredictions=data.frame(testcreditData,predictions)
allpredictions$Creditability=c("Risk","NoRisk")
colnames(allpredictions)[length(allpredictions)]<-"pred.Creditability"
write.csv(allpredictions,file = "LR_credit_predictions.csv")
conf_LR = table(testcreditData$Creditability,allpredictions$pred.Creditability)
TP <- conf_LR[1,1] 
TN <- conf_LR[2,2]
FN <- conf_LR[1,2]
FP <- conf_LR[2,1]

accuracy <- ((TP+TN)/(TP+TN+FN+FP))*100
sensitivity= (TP/(TP+FN))*100  
specificity = (TN/(TN+FP))*100  
cat("Specificity=", specificity,"\n Sensitivity=",sensitivity,"\n Accuracy=",accuracy)


#3. SVM
library(e1071)
svm_moldel= svm(traincreditData$Creditability~.,data = traincreditData, kernel="radial", cost=10)
predict_svm= predict(svm_moldel,testcreditData)
allpredictions=data.frame(testcreditData,c("Risk","NoRisk")[svm_moldel])
allpredictions$Creditability=c("Risk","NoRisk")
colnames(allpredictions)[length(allpredictions)]<-"pred.Creditability"
write.csv(allpredictions,file = "SVM_credit_predictions.csv")
conf_SVM = table(testcreditData$Creditability,allpredictions$pred.Creditability)
TP <- conf_SVM[1,1] 
TN <- conf_SVM[2,2]
FN <- conf_SVM[1,2]
FP <- conf_SVM[2,1]

accuracy <- ((TP+TN)/(TP+TN+FN+FP))*100
sensitivity= (TP/(TP+FN))*100  
specificity = (TN/(TN+FP))*100  
cat("Specificity=", specificity,"\n Sensitivity=",sensitivity,"\n Accuracy=",accuracy)

#4.Naive Bayes
library(e1071)
model <- naiveBayes(traincreditData$Creditability~.,data = traincreditData)
predict_naive=predict(model, testcreditData[,-14])
allpredictions=data.frame(testcreditData,c("crediter","noncrediter")[predict_naive])
colnames(allpredictions)[length(allpredictions)]<-"pred.Creditability"
allpredictions$Creditability=c("N","Y")
write.csv(allpredictions,file = "naive_credit_predictions.csv")
conf_naive = table(testcreditData$Creditability,allpredictions$pred.Creditability)
TP <- conf_naive[1,1] 
TN <- conf_naive[2,2]
FN <- conf_naive[1,2]
FP <- conf_naive[2,1]

accuracy <- ((TP+TN)/(TP+TN+FN+FP))*100
sensitivity= (TP/(TP+FN))*100  
specificity = (TN/(TN+FP))*100  
cat("Specificity(Naive Bayes)=", specificity,"\n Sensitivity=",sensitivity,"\n Accuracy=",accuracy)


#5. Random Forest
library(randomForest)
credit_RF<- randomForest(traincreditData$Creditability~.,data = traincreditData,ntree=200,mtry = 4, nodesize = 1,maxnodes=20)
importance(credit_RF)
predict_RF=predict(credit_RF,testcreditData)
allpredictions=data.frame(testcreditData,c("crediter","noncrediter")[predict_RF])
allpredictions$Creditability=c("Risk","NoRisk")
colnames(allpredictions)[length(allpredictions)]<-"pred.Creditability"
write.csv(allpredictions,file = "RF_credit_predictions.csv")
conf_RF = table(testcreditData$Creditability,allpredictions$pred.Creditability)
TP <- conf_RF[1,1] 
TN <- conf_RF[2,2]
FN <- conf_RF[1,2]
FP <- conf_RF[2,1]

accuracy <- ((TP+TN)/(TP+TN+FN+FP))*100
sensitivity= (TP/(TP+FN))*100  
specificity = (TN/(TN+FP))*100  
cat("Specificity (RF)=", specificity,"\n Sensitivity=",sensitivity,"\n Accuracy=",accuracy)

