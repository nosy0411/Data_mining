library(rpart)
d.wine<-read.csv("wine.csv",header=TRUE, sep=",")

w.rp<-rpart(wine~.,data=d.wine, method="class")
plot(w.rp)
text(w.rp)
w.rp
pred.y<-predict(w.rp,d.wine[,-1],type="class")
cfm<-table(d.wine$wine, pred.y)
pred.y
cfm
misclassification_error=1-sum(diag(cfm))/sum(cfm)
misclassification_error
d.wine_test<-read.csv("wine_test.csv",header=TRUE, sep=",")
d.wine_test
pred.y_test<-predict(w.rp,d.wine_test,type="class")
pred.y_test

d.wineA<-d.wine[d.wine$wine=='A',]
d.wineB<-d.wine[d.wine$wine=='B',]
nr.A<-nrow(d.wineA)
nr.B<-nrow(d.wineB)
rd.wineA<-d.wineA[sample(nr.A),]
rd.wineB<-d.wineB[sample(nr.B),]
d.wineA.train<-rd.wineA[1:round(nr.A*0.8),]
d.wineA.test<-rd.wineA[(round(nr.A*0.8)+1):nr.A,]
d.wineB.train<-rd.wineB[1:round(nr.B*0.8),]
d.wineB.test<-rd.wineB[(round(nr.B*0.8)+1):nr.B,]
d.wine.train<-rbind(d.wineA.train, d.wineB.train)
d.wine.test<-rbind(d.wineA.test, d.wineB.test)

d.wine.train
d.wine.test

logr.model<-glm(wine~., data=d.wine.train, family=binomial("logit"))
summary(logr.model)
pred.train_logi<-predict(logr.model,d.wine.train[,-1],type="response")
pred.train_logi<-ifelse(pred.train_logi > 0.5,1,0)
train.logi_table<-table(d.wine.train$wine, pred.train_logi)
train.logi_table
logi_train_error=1-sum(diag(train.logi_table))/sum(train.logi_table)
logi_train_error

pred.test_logi<-predict(logr.model,d.wine.test[,-1],type="response")
pred.test_logi<-ifelse(pred.test_logi > 0.5,1,0)
test.logi_table<-table(d.wine.test$wine, pred.test_logi)
test.logi_table
logi_test_error=1-sum(diag(test.logi_table))/sum(test.logi_table)
logi_test_error

library(MASS)
lda.model<-lda(wine~.,d.wine.train)
lda.model
pred.train_lda<-predict(lda.model, d.wine.train)
lda_train_error=mean(d.wine.train$wine!=pred.train_lda$class)
lda_train_error

pred.test_lda<-predict(lda.model, d.wine.test)
lda_test_error=mean(d.wine.test$wine!=pred.test_lda$class)
lda_test_error

qda.model<-qda(wine~.,train)
qda.model
pred.train_qda<-predict(qda.model, d.wine.train)
qda_train_error=mean(d.wine.train$wine!=pred.train_qda$class)
qda_train_error

pred.test_qda<-predict(qda.model, d.wine.test)
qda_test_error=mean(d.wine.test$wine!=pred.test_qda$class)
qda_test_error

d.wine<-read.csv("wine.csv",header=TRUE, sep=",")
d.wine[,-1]<-scale(d.wine[,-1], center=TRUE, scale=TRUE)
d.wineA<-d.wine[d.wine$wine=='A',]
d.wineB<-d.wine[d.wine$wine=='B',]
nr.A<-nrow(d.wineA)
nr.B<-nrow(d.wineB)
rd.wineA<-d.wineA[sample(nr.A),]
rd.wineB<-d.wineB[sample(nr.B),]
d.wineA.train<-rd.wineA[1:round(nr.A*0.6),]
d.wineA.valid<-rd.wineA[(round(nr.A*0.6)+1):round(nr.A*0.8),]
d.wineA.test<-rd.wineA[(round(nr.A*0.8)+1):nr.A,]
d.wineB.train<-rd.wineB[1:round(nr.B*0.6),]
d.wineB.valid<-rd.wineB[(round(nr.B*0.6)+1):round(nr.B*0.8),]
d.wineB.test<-rd.wineB[(round(nr.B*0.8)+1):nr.B,]
scale_train<-rbind(d.wineA.train, d.wineB.train)
scale_valid<-rbind(d.wineA.valid, d.wineB.valid)
scale_test<-rbind(d.wineA.test, d.wineB.test)
train_label<-scale_train[,1]
valid_label<-scale_valid[,1]
test_label<-scale_test[,1]

library("class")
for(i in 1:length(valid_label)){
  knn_valid_pred<-knn(train=scale_train[,-1], test=scale_valid[,-1], cl=train_label,k=i)
  valid.knn_table<-table(scale_valid$wine, knn_valid_pred)
  knn_valid_error=1-sum(diag(valid.knn_table))/sum(valid.knn_table)
  print(knn_valid_error)
}

knn_train_pred<-knn(train=scale_train[,-1], test=scale_train[,-1], cl=train_label,k=5)
train.knn_table<-table(scale_train$wine, knn_train_pred)
knn_train_error=1-sum(diag(train.knn_table))/sum(train.knn_table)
knn_train_error

knn_test_pred<-knn(train=scale_train[,-1], test=scale_test[,-1], cl=train_label,k=5)
test.knn_table<-table(scale_test$wine, knn_test_pred)
test.knn_table
knn_test_error=1-sum(diag(test.knn_table))/sum(test.knn_table)
knn_test_error

library(nnet)
for(i in 1:9){
  nn.model<-nnet(wine~., data=scale_train ,size=i,maxit=100)
  pred.y.valid<-predict(nn.model, scale_valid, type="class")
  nn.valid.table<-table(scale_valid$wine, pred.y.valid)
  nn_valid_error=1-sum(diag(nn.valid.table))/sum(nn.valid.table)
  print(nn_valid_error)
}

nn.model<-nnet(wine~., data=scale_train ,size=3,maxit=1000)
pred.y.train<-predict(nn.model, scale_train, type="class")
nn.train.table<-table(scale_train$wine, pred.y.train)
nn_train_error=1-sum(diag(nn.train.table))/sum(nn.train.table)
nn_train_error

nn.model<-nnet(wine~., data=scale_train ,size=3,maxit=1000)
pred.y.test<-predict(nn.model, scale_test, type="class")
nn.test.table<-table(scale_test$wine, pred.y.test)
nn_test_error=1-sum(diag(nn.test.table))/sum(nn.test.table)
nn_test_error

library(e1071)
library(mlbench)
d.wine<-read.csv("wine.csv",header=TRUE, sep=",")
model<-naiveBayes(wine~.,data=d.wine)

pred.nB.train<-predict(model, d.wine.train[,-1], method="class")
nB.train.table<-table(d.wine.train[,1],pred.nB.train)
nB_train_error=1-sum(diag(nB.train.table))/sum(nB.train.table)
nB_train_error

pred.nB.test<-predict(model, d.wine.test[,-1], method="class")
nB.test.table<-table(d.wine.test[,1],pred.nB.test)
nB_test_error=1-sum(diag(nB.test.table))/sum(nB.test.table)
nB_test_error