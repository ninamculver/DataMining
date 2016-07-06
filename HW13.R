##### HOMEWORK 13 ####
#Due November 3, 2015#


##Necessary Things##
library(nnet)
library(pROC)

set.seed(5364)

#Note: the Plot.nnet Function is in another tab#

confmatrix=function(y,predy){
  matrix=table(y,predy)
  accuracy=sum(diag(matrix))/sum(matrix)
  return(list(matrix=matrix,accuracy=accuracy,error=1-accuracy))
}


createfolds=function(n,K){
    reps=ceiling(n/K)
    folds=sample(rep(1:K,reps))
    return(folds[1:n])
  }


### 1 ###

#a)
wdbc = wdbc[,2:32]
training=sample(nrow(wdbc),round(nrow(wdbc)*.7,0))
train = wdbc[training,]
test = wdbc[training,]
model = nnet(as.factor(V2)~.,data=train,size=1,maxit=1000)
plot.nnet(model) #This relies on plot.nnet
pred=predict(model,newdata=test)
confmatrix(pred,as.factor(test$V2))$accuracy # ans 0.6356784
plot(roc(response=(test$V2=="M")*1,predictor=pred[,1]))


#b)
folds=createfolds(nrow(wdbc),10)
accmatrix=matrix(nrow=100,ncol=10)
for(netsize in 1:100){
  for(k in 1:10){
    temptest=wdbc[folds==k,]
    temptrain=wdbc[folds!=k,]
    
    tempnnet=nnet(V2~.,data=temptrain,size=netsize)
    accmatrix[netsize,k]=confmatrix(temptest$V2,
                      predict(tempnnet,newdata=temptest))$accuracy
  }}
accmatrix
accvector=apply(accmatrix,1,mean)
plot(1:100,accvector)
which.max(accvector)


#c)
bestmodel = nnet(as.factor(V2)~.,data=train,size= 15, maxit=1000) #size is 15
plot(bestmodel)
bestpred=predict(bestmodel,newdata=test)
confmatrix(bestpred,as.factor(test$V2))$accuracy # ans 0.620603
plot(roc(response=as.factor(test$V2),predictor=bestpred[,1])) 

### 2 ###

#a)
xval=runif(100,0,2*pi)
yval=sin(xval)
sindata=data.frame(xval,yval)
sinmodel = nnet(yval~.,data=sindata, size = 2, linout = TRUE)
plot.nnet(sinmodel)

#b)
sinfolds=createfolds(nrow(sindata),10)
sinaccmatrix=matrix(nrow=100,ncol=10)
for(netsize in 1:100){
  for(k in 1:10){
    temptest=sindata[folds==k,]
    temptrain=sindata[folds!=k,]
    
    tempnnet=nnet(yval~.,data=temptrain,size=netsize)
    sinaccmatrix[netsize,k]=confmatrix(temptest$yval,
                                    predict(tempnnet,newdata=temptest))$accuracy
  }}
sinaccmatrix
sinaccvector=apply(sinaccmatrix,1,mean)
plot(1:100,sinaccvector)
which.max(sinaccvector)

#c)
bestsinmodel = nnet(yval~.,data=sindata, size = 24, linout = TRUE)
plot(bestsinmodel)

sinning=sample(nrow(sindata),round(nrow(sindata)*.7,0))
sintrain = sindata[sinning,]
sintest = sindata[-sinning,]

sin2model = nnet(yval~.,data=sintrain, size = 2, linout = TRUE)
sin2pred=predict(sin2model,newdata=sintest)
confmatrix(sin2pred,sintest$y)$accuracy #  0.6333333
roc(response=sintest$y, predictor=sin2pred[,1]) # 1

plot (c(0,30),c(-1.0,1.0),type="n",              
  xlab="X Value",ylab= "Probability")
  lines(yval,col='orange',lwd=2)
  lines(sin2pred,col="black",lwd=2)
  labels = c("sin(x)", "Predictions")
  legend("topright", inset = .05, title = "Legend", 
         labels, lwd = 2, col = c("orange", "black"))

bestsin2model = nnet(yval~.,data=sintrain, size = 24, linout = TRUE)
bestsin2pred=predict(bestsin2model,newdata=sintest)
confmatrix(bestsin2pred,sintest$y)$accuracy #1
roc(response=sintest$y, predictor=bestsin2pred[,1]) #1
  
plot (c(0,30),c(-1.0,1.0),type="n",              
  xlab="X Value",ylab= "Probability")
  lines(yval,col='orange',lwd=2)
  lines(bestsin2pred,col="black",lwd=2)
  labels = c("sin(x)", "Predictions")
  legend("topright", inset = .05, title = "Legend", 
         labels, lwd = 2, col = c("orange", "black"))
  
  