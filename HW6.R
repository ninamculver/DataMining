library(rattle)
library(rpart)
library(rpart.plot)
library(exact2x2)
library(class)

wdbc <- read.csv("~/Fall 2015/Data Mining I/DATAMINING HW/wdbc.data", header=FALSE)

#1a)
x=wdbc[,3:32]
xbar=apply(x,2,mean)
xbarMatrix=cbind(rep(1,569))%*%xbar
s=apply(x,2,sd)
sMatrix=cbind(rep(1,569))%*%s

z=(x-xbarMatrix)/sMatrix
apply(z,2,mean)
apply(z,2,sd)

#1b)
train=sample(nrow(z),nrow(z)*.7)
z[train,]
z[-train,]

#1c)
confmatrix=function(y,predy){
  matrix=table(y,predy)
  accuracy=sum(diag(matrix))/sum(matrix)
  return(list(matrix=matrix,accuracy=accuracy,error=1-accuracy))
}

diagnosis=wdbc$V2
predDiagnosis=knn(train=z[train,],test=z[-train,],cl=diagnosis[train],k=3)

error = confmatrix(diagnosis[-train],predDiagnosis)$error
#I need to get the confidence interval to work.... hm try prop.test or t.test... 

#1d)
tree = rpart(wdbc$V2~., data=wdbc)
predTree = (predict(tree, newdata = wdbc, type = "class"))
confmatrix(wdbc$V2, predTree)

#accvectorKNN = ( == predDiagnosis)
accvectorTree = (wdbc$V2 ==  predTree)
mcnemartable = table(accvectorKNN, accvectorTree)

#2a)
predDiagnosis=knn.cv(train=z,cl=diagnosis,k=3)
confmatrix(diagnosis,predDiagnosis)

#2b)
accvect=1:10

for(k in 1:10){
  predSpecies=knn.cv(train=z,cl=diagnosis,k=k)
  accvect[k]=confmatrix(diagnosis,predDiagnosis)$accuracy
}

which.max(accvect)

#2c)
#Combine traindata and testdata.
Exdata=rbind(traindata,testdata)
folds=cvFolds(nrow(Exdata),K=10,type='random')
#createfolds function
createfolds=function(n,K){
  reps=ceiling(n/K)
  folds=sample(rep(1:K,reps))
  return(folds[1:n])
}
#Folds for Exdata
set.seed(5364)
folds=createfolds(nrow(Exdata),10)
#Accuracy for first fold
temptest=Exdata[folds==1,]
temptrain=Exdata[folds!=1,]
#2d)
#3a)
#3b)