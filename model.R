library(caret)
library(e1071)

data<-read.csv('sample.csv',row.names = 1)
train.control<- trainControl(method='LOOCV',number=1)
tuneGrid <- expand.grid(k=c(3,5,7,10,15,25))

modeldnn<-train(status~.,data=data,method='monmlp',trControl=train.control)

data6<-read.csv('spearmaneps3.5.csv',row.names = 1)
data<-read.csv('spearmantop.csv',row.names=1)

train.control<- trainControl(method='LOOCV',number=1)
tuneGrid <- expand.grid(k=c(3,5,7,10,15,25))

modelknn<-train(status~.,data=data,method='knn',trControl=train.control,tuneGrid=tuneGrid)
print(modelknn)
modellm<-train(status~.,data=data,method='lm',trControl=train.control)
print(modellm)

modelsvr<-train(status~.,data=data,method='svmLinear2',trControl=train.control)
print(modelsvr)
plot(modelsvr)

kgrid<-expand.grid(size=3:4,dropout=c(0,0.001,0.01),batch_size=138,lr=0.01,rho=0.9,decay=0.01,activation='relu')
modelkeras<-train(status~.,data=data,method='mlpKerasDropout',trControl=train.control,tuneGrid=kgrid)

modelkeras<-train(status~.,data=data,method='dnn',trControl=train.control)

tgrid <- expand.grid(layer1 = 1:5,
                     layer2 = 1:2, layer3 = 1:2,
                     hidden_dropout = c(0, .1,0.25), 
                     visible_dropout = 0)
modeldnn<-train(status~.,data=phgreg,method='dnn',trControl=train.control,tuneGrid = tgrid)

importance=varImp(modelsvr,scale=FALSE)
plot(importance)
