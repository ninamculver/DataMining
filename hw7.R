#### Necessary Things ####
library(kknn)
library(exact2x2)

confmatrix=function(y,predy){
  matrix=table(y,predy)
  accuracy=sum(diag(matrix))/sum(matrix)
  return(list(matrix=matrix,accuracy=accuracy,error=1-accuracy))
}

wdbc = read.csv("~/Fall 2015/Data Mining I/DATAMINING HW/wdbc.data", header=FALSE)
wdbc = wdbc[,2:32]

#### 1 ####
triweight = function(d){
  (35/32)*(1-d^2)^3 * (abs(d) <= 1)
}
cosine = function(d){
  (pi/4)* cos((pi*d)/2)*(abs(d) <=1)
}
xlim = c(-1.2,1.2)
ylim = c(0,1.2)
d = seq(from = -1.2,to = 1.2,by = .1)
 

#### 2 ####
optimalK = train.kknn(V2~., data = wdbc)
optimalK #Optimal Kernal is 9. 

#### 3 ####
train=sample((dim(wdbc)[1]*.7),(dim(wdbc)[1]*.3))
train = wdbc[train,]
test = wdbc[-train,]
testingF = train.kknn(V2 ~ ., data = wdbc, kmax = 15, 
           kernel = c("rectangular","triangular", "epanechnikov",  "biweight",
                     "triweight","cos","inv", "gaussian" ,"optimal"), 
           distance = 2) 
testingF #Optimal kernel is rectangular
predF = (kknn(V2~.,train=train,test=test,
              k=9,kernel="inv",distance=2))$fitted.values
confmatrix = confmatrix(test$V2, predF)
accF = confmatrix$accuracy
accF
confint = binom.exact(sum(diag(confmatrix$matrix)),sum(confmatrix$matrix), 
                      alternative = "two.sided", conf.level = .95)
confint

#### 4 ####
predHW6 = (kknn(V2~.,train=train,test = test,
                k=10,kernel="rectangular",distance=2))$fitted.values 
accHW6 = (confmatrix(test$V2, predHW6))$accuracy
accHW6

#### 5 ####
mcnemartable = table(predF, predHW6)
mcnemar.exact(mcnemartable)

