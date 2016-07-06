library(rpart)
library(rattle)
library(party)
library(exact2x2)


splitdata = function(data, trainfrac) {  
  train = sample(nrow(data), round(trainfrac*nrow(data),0))
  traindata = data[train,]
  testdata = data[(-train),]
  dataset = list(traindata, testdata)
  return(dataset)
  }




wdbc <- read.csv("~/Fall 2015/Data Mining I/DATAMINING HW/wdbc.data", header=FALSE)
head(wdbc)

wdbc$V1 = NULL

splitlist = splitdata(wdbc, .7)
traindata = data.frame(splitlist[1])
testdata = data.frame(splitlist[2])



colSums(wdbc)
dim(wdbc)
colSums(traindata)
dim(traindata)
colSums(testdata)
dim(testdata)

tree1 = rpart(traindata$V2~., data = traindata)
fancyRpartPlot(tree1)

confmatrix=function(y,predy){
  matrix=table(y,predy)
  accuracy=sum(diag(matrix))/sum(matrix)
  return(list(matrix=matrix,accuracy=accuracy,error=1-accuracy))}


trainerror1 = confmatrix(tree1,predict(tree1,newdata=traindata,type='class'))$error
testerror1 = confmatrix(tree1, predict(tree1, newdata = testdata, type = 'class'))$error


tree2 = ctree(V2~., data = traindata)
plot(tree2, type = 'simple')


trainerror2 = confmatrix(tree2,predict(tree2,newdata=traindata,type='class'))$error
testerror2 = confmatrix(tree2, predict(tree2, newdata = testdata, type = 'class'))$error

accvector1=(testdata$class==predict(tree1,newdata=testdata,type='class'))
accvector2=(testdata$class==predict(tree2,newdata=testdata,type='class'))
mcnemartable = table(accvector1, accvector2)
  
createfolds=function(n,K){
  reps=ceiling(n/K)
  folds=sample(rep(1:K,reps))
  return(folds[1:n])
}

for(k in 1:10){
  temptest=wdbc[folds==k,]
  temptrain=wdbc[folds!=k,]
  
  temptree=rpart(class~.,data=temptrain)
  accvector[k]=confmatrix(temptest$class,
                          predict(temptree,newdata=temptest,type="class"))$accuracy
}

mean(accvector)

index=sample(nrow(Exdata))
index[1:20]
index[21:nrow(Exdata)]
