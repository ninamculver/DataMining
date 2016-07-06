library(rpart)
library(rattle)
library(kknn)
library(pROC)
library(e1071)

confmatrix=function(y,predy){
  matrix=table(y,predy)
  accuracy=sum(diag(matrix))/sum(matrix)
  return(list(matrix=matrix,accuracy=accuracy,error=1-accuracy))
}

germancredit <- read.csv("C:/Users/nculver/Downloads/germancredit.csv")

###1###
trainingCode=sample(nrow(germancredit),round(nrow(germancredit)*.7,0))
train =  germancredit[trainingCode,]
test = germancredit[-trainingCode,]
#a) Decision Tree 
germanTree = rpart(Default~., data = train)
fancyRpartPlot(germanTree)
treephat=predict(germanTree,newdata=test)
treeroc=roc(test$Default == 0,treephat)
plot(treeroc)

#b) Weighted K-nearest Neighbors
germanKKNN.fit=train.kknn(Default~.,data=train) #Says best kernel is optimal, best k is 11.
germanKKNNmodel=kknn(Default~.,train=train,test=test,k=11,kernel="optimal")
kknnphat=predict(germanKKNNmodel,newdata=test)
kknnroc=roc(test$Default == 0,kknnphat)
plot(kknnroc)

#c) Naive Bayes
germanBayes =naiveBayes(as.factor(Default)~.,data=train)
bayesphat = predict(germanBayes,newdata = test, type = 'raw')[,1]
bayesroc=roc(test$Default==0, bayesphat)
plot(bayesroc)

###2###
#a) p0
cminusplus = 20000
cplusminus = 5000
cminusminus = 0
cplusplus = 0
p0 = (cminusplus - cminusminus)/ (cminusplus+cplusminus-cminusminus -cplusplus)
p0

#b)
treepred=treephat
treepred[treephat>=p0]=0
treepred[treephat<p0]=1
treematrix = confmatrix(test$Default, treepred)$matrix
tTP=treematrix[1,1] 
tTN=treematrix[2,2]
tFP=treematrix[2,1]
tFN=treematrix[1,2]
treecost=cminusplus*tFP+cplusminus*tFN
treecost

kknnpred=kknnphat
kknnpred[kknnphat>=p0]=0
kknnpred[kknnphat<p0]=1
kknnmatrix = confmatrix(test$Default, kknnpred)$matrix
kTP=kknnmatrix[1,1]
kTN=kknnmatrix[2,2]
kFP=kknnmatrix[2,1]
kFN=kknnmatrix[1,2]
kknncost=cminusplus*kFP+cplusminus*kFN
kknncost

bayespred=bayesphat
bayespred[bayesphat>=p0]=0
bayespred[bayesphat<p0]=1
bayesmatrix = confmatrix(test$Default, bayesphat)$matrix
bTP=bayesmatrix[1,1]
bTN=bayesmatrix[2,2]
bFP=bayesmatrix[2,1]
bFN=bayesmatrix[1,2]
bayescost=cminusplus*bFP+cplusminus*bFN
bayescost


#c)
p0 = 0.5

treepred=treephat
treepred[treephat>=p0]=0
treepred[treephat<p0]=1
treematrix = confmatrix(test$Default, treepred)$matrix
tTP=treematrix[1,1]
tTN=treematrix[2,2]
tFP=treematrix[2,1]
tFN=treematrix[1,2]
treecost=cminusplus*tFP+cplusminus*tFN
treecost

kknnpred=kknnphat
kknnpred[kknnphat>=p0]=0
kknnpred[kknnphat<p0]=1
kknnmatrix = confmatrix(test$Default, kknnpred)$matrix
kTP=kknnmatrix[1,1]
kTN=kknnmatrix[2,2]
kFP=kknnmatrix[2,1]
kFN=kknnmatrix[1,2]
kknncost=cminusplus*kFP+cplusminus*kFN
kknncost

bayespred=bayesphat
bayespred[bayesphat>=p0]=0
bayespred[bayesphat<p0]=1
bayesmatrix = confmatrix(test$Default, bayesphat)$matrix
bTP=bayesmatrix[1,1]
bTN=bayesmatrix[2,2]
bFP=bayesmatrix[2,1]
bFN=bayesmatrix[1,2]
bayescost=cminusplus*bFP+cplusminus*bFN
bayescost

