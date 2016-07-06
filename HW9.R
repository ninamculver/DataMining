install.packages("mlbench")
library(mlbench)
library(class)
library(MASS)
library(rpart)
library(rattle)
library(pROC)
library(kernlab)
library(e1071)

confmatrix=function(y,predy){
  matrix=table(y,predy)
  accuracy=sum(diag(matrix))/sum(matrix)
  return(list(matrix=matrix,accuracy=accuracy,error=1-accuracy))
}

### 3 ###
### A ###
germancredit <- read.csv("C:/Users/nculver/Downloads/germancredit.csv")
trainingCode=sample(nrow(germancredit),round(nrow(germancredit)*.7,0))
train =  germancredit[trainingCode,]
test = germancredit[-trainingCode,]

model=naiveBayes(as.factor(Default)~.,data=train)
model
pred = predict(model,newdata = test)
pred
cmatrix = confmatrix(test$Default,pred)$matrix


TP=cmatrix[1,1]
TN=cmatrix[2,2]
FP=cmatrix[2,1]
FN=cmatrix[1,2]

acc=(TP+TN)/(TP+TN+FP+FN)
acc #ACCURACY = 0.74
TPR=TP/(TP+FN)
TNR=TN/(TN+FP)
FPR=FP/(FP+TN)
FNR=FN/(TP+FN)
TNR #SPECIFICITY = 0.4565217

p=TP/(TP+FP)
r=TP/(TP+FN)
p # PRECISION = 0.7826087
r # SENSITIVITY =  0.8653846

F1=2*r*p/(r+p)
F1 # 0.8219178

### B ###
phat=predict(model,newdata=test,type="raw")[,1]
table(pred,(phat>=0.5))
trainphat=predict(model,newdata=train,type="raw")[,1]
trainprec=1:100
trainrecall=1:100
trainF1=1:100
range(trainphat)
p0vect=10^(-5+(1:100)/100*5)

for(i in 1:100){
  p0=p0vect[i]
  trainpred=(trainphat>=p0)*1
  
  TP=sum((trainpred==1)&(train$Default==0))
  FP=sum((trainpred==1)&(train$Default!=0))
  FN=sum((trainpred!=1)&(train$Default==0))
  
  trainprec[i]=TP/(TP+FP)
  trainrecall[i]=TP/(TP+FN)
  trainF1[i]=2*TP/(2*TP+FP+FN)}
which.max(trainF1) #93
trainF1[which.max(trainF1)] #0.8505747
trainprec[which.max(trainF1)] #0.8043478
trainrecall[which.max(trainF1)] #0.902439
p0vect[which.max(trainF1)] #0.4466836


### C ###
p0=p0vect[which.max(trainF1)]
newpred=phat
newpred[phat>=p0]=0
newpred[phat<p0]=1


confmatrix(test$Default,newpred)
cmatrix=confmatrix(test$Default,newpred)$matrix

TP=cmatrix[1,1]
TN=cmatrix[2,2]
FP=cmatrix[2,1]
FN=cmatrix[1,2]

acc=(TP+TN)/(TP+TN+FP+FN)
acc #ACCURACY 0.74
TPR=TP/(TP+FN)
TNR=TN/(TN+FP)
FPR=FP/(FP+TN)
FNR=FN/(TP+FN)
TNR # SPECIFICITY 0.4347826

p=TP/(TP+FP)
r=TP/(TP+FN)
p #SENSITIVITY 0.7777778
r #PRECISION 0.875

F1=2*r*p/(r+p)
F1 # 0.8235294

