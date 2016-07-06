###Stuff I just Need###
library(e1071)
library(kernlab)

confmatrix=function(y,predy){
  matrix=table(y,predy)
  accuracy=sum(diag(matrix))/sum(matrix)
  return(list(matrix=matrix,accuracy=accuracy,error=1-accuracy))
}

##### 1 #####
data = iris
#1a)
species = data$Species
species = gsub("virginica", "nonsetosa", species)
species = gsub("versicolor", "nonsetosa", species)
dataIris = data.frame(data,species)
dataIris = dataIris[,-5]
train=sample(nrow(dataIris),round(nrow(dataIris)*.7,0))

#1b)
model=svm(species~.,data=dataIris[train,],kernel="linear",cost=1000)
plot(model,data=dataIris[train,], Petal.Length ~ Petal.Width) #two variables

#1c) Yes
#1d)
predspecies=predict(model,newdata=dataIris[-train,])
confmatrix(predspecies,dataIris$species[-train])$accuracy

#1e) 
model #3

#1f)
w=t(model$coefs)%*%(model$SV)
b=-model$rho

##### 2 #####
wdbc <- read.csv("~/Fall 2015/Data Mining I/DATAMINING HW/wdbc.data", header=FALSE)
wdbc = wdbc[,2:32]
trainwdbc=sample(nrow(wdbc),round(nrow(wdbc)*.7,0))
#2a)
wdbcmodelL=svm(V2~.,data=wdbc[trainwdbc,],kernel="linear")
wdbcpredL=predict(wdbcmodelL,newdata=wdbc[-trainwdbc,])
confmatrix(wdbcpredL,wdbc$V2[-trainwdbc])
wdbcmodelR=svm(V2~.,data=wdbc[trainwdbc,],kernel="radial")
wdbcpredR=predict(wdbcmodelR,newdata=wdbc[-trainwdbc,])
confmatrix(wdbcpredR,wdbc$V2[-trainwdbc])
#2b) Radial
#2c)
confmatrix(wdbcpredR,wdbc$V2[-trainwdbc])$accuracy
#2d)
wdbctrainmodel = tune.svm(V2~., data = wdbc[trainwdbc,], 
                          gamma = 10^(-6:-3), 
                          cost = 10^(1:2))
#2e)
wdbcmodelFIT=svm(V2~.,data=wdbc[trainwdbc,],kernel="radial",gamma = .001, cost=100)
#2f)
wdbcpredFIT=predict(wdbcmodelFIT,newdata=wdbc[-trainwdbc,])
confmatrix(wdbcpredFIT,wdbc$V2[-trainwdbc])$accuracy

##### 3 #####
#3a)
set.seed(5364)
x1 = rnorm(50, 0, 1.5)
y1 = rnorm(50, 0, 1.5)
x2 = rnorm(50, 6, 1.5)
y2 = rnorm(50, 0, 1.5)
x3 = rnorm(50, 0, 1.5)
y3 = rnorm(50, 6, 1.5)
x4 = rnorm(50, 6, 1.5)
y4 = rnorm(50, 6, 1.5)
class=c(rep("A",50),rep("B",50),rep("B",50),rep("A",50) )

mydata=data.frame(x=c(x1,x2,x3,x4),y=c(y1,y2,y3,y4),class)
mydata$class=as.factor(mydata$class)
plot(y~x,data=mydata,col=class)

#3b)
traindata=sample(nrow(mydata),round(nrow(mydata)*.7,0))
mydataSVM = svm(class~.,data= mydata[traindata,],kernel="radial")
mydatapred=predict(mydataSVM,newdata=mydata[-traindata,])
plot(mydataSVM,data=mydata[traindata,]) #two variables

confmatrix(mydatapred,mydata$class[-traindata])$accuracy

