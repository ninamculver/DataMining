### Necessary Things ###

install.packages("mlbench")
library(mlbench)
data(HouseVotes84)

install.packages("e1071")
library(e1071)

confmatrix=function(y,predy){
  matrix=table(y,predy)
  accuracy=sum(diag(matrix))/sum(matrix)
  return(list(matrix=matrix,accuracy=accuracy,error=1-accuracy))
}

### 1 ###

#a)
model = naiveBayes(Class ~ ., data = HouseVotes84)
probDem = model$apriori[1]/(sum(model$apriori))
probDem

#b) 
reptotal = sum(table(HouseVotes84$Class)[2])
reptotal
repV3n = table(HouseVotes84$Class, HouseVotes84$V3)[2]
repV3y = table(HouseVotes84$Class, HouseVotes84$V3)[4]
(repV3n + repV3y) / reptotal

#c)
table(HouseVotes84$Class, HouseVotes84$V4)[2]
table(HouseVotes84$Class, HouseVotes84$V4)[4]
table(HouseVotes84$Class, HouseVotes84$V5)[1]
table(HouseVotes84$Class, HouseVotes84$V5)[3]
table(HouseVotes84$Class, HouseVotes84$V16)[2]
table(HouseVotes84$Class, HouseVotes84$V16)[4]

#I'm not terribly certain how to do this currently. Bleh. 

### 2 ###
wdbc = read.csv("~/DataMining2015/wdbc.data", header=FALSE)
wdbc = wdbc[,2:32]
numeric = wdbc[,2:31]

hist(wdbc$V6)
shapiro.test(wdbc$V6)  
qqnorm(wdbc$V6)

hist(wdbc$V16)
shapiro.test(wdbc$V16)  
qqnorm(wdbc$V16)

hist(wdbc$V26)
shapiro.test(wdbc$V26)  
qqnorm(wdbc$V26)
#I wasn't sure if I should do it for every column, as there are thirty-one numeric valued columns. 
#So I just did it for the ones that were specified in HW6.

### 3 ### 
trains=sample(nrow(wdbc),nrow(wdbc)*.7)
train = wdbc[trains,]
test = wdbc[-trains,]
