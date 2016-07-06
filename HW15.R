####HOMEWORK 15####
###Due Nov. 12###
##Nina Culver##

install.packages("adabag")
library(adabag)
install.packages("randomForest")
library(randomForest)

wdbc = wdbc[,2:32]
train=sample(nrow(wdbc),round(nrow(wdbc)*.7,0))
trainWDBC = wdbc[train,]
testWDBC = wdbc[-train,]


###1###
#Compare the accuracies of bagging, boosting, and random forests on the wdbc data set,
#using 70% of the data as training data and 30% as testing data.

#Bagging:
bagModel=bagging(V2~.,data=trainWDBC)
bagPred=predict(bagModel,newdata=testWDBC,type="class")
bagPred
bagAccuracy = 1-bagPred$error
bagAccuracy #0.9298246

#Boosting:
boostModel=boosting(V2~.,data=trainWDBC)
boostPred=predict(boostModel,newdata=testWDBC,type="class")
boostPred
boostAccuracy = 1-boostPred$error
boostAccuracy #0.9590643

#Random Forests
forestModel=randomForest(V2~.,data=trainWDBC)
forestPred=predict(forestModel,newdata=testWDBC)
forestConf = confmatrix(testWDBC$V2,forestPred)
forestAccuracy = forestConf$accuracy
forestAccuracy #0.9473684
