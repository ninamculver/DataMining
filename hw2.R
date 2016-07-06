library (rpart)
library (rattle)

#1.
?kyphosis
#The kyphosis data frame contains the variables:
  #kyphosis, determining the type of deformation present after the operation;
  #age, age of the patient in months;
  #number, a count of the vertebrae involved;
  #start, the number of the first vertebrae operated on. 
#2. 
  #a.
    kyphosisTree = rpart(Kyphosis~., data = kyphosis)
    fancyRpartPlot(kyphosisTree)

  #b. 
    #confmatrix function
    confmatrix=function(y,predy){
      matrix=table(y,predy)
      accuracy=sum(diag(matrix))/sum(matrix)
      return(list(matrix=matrix,accuracy=accuracy,error=1-accuracy))
    }
  #c.   
    confmatrix(kyphosis$Kyphosis, predict(kyphosisTree, newdata = kyphosis, type = 'class'))
   
#3.
  plot(kyphosis$Age, kyphosis$Start, col= c('blue', 'red'))
#4.
  library (party)
  kyphosisTree2=ctree(Kyphosis~.,data=kyphosis)
  plot(kyphosisTree2, type = 'simple')
  predKyphosis=predict(kyphosisTree2,newdata=kyphosis)
  confusionmatrix=table(kyphosis$Kyphosis, predKyphosis)
  confusionmatrix
  accuracy=sum(diag(confusionmatrix))/sum(confusionmatrix)
  accuracy
#5.
  #The rpart function returned an accuracy of 83.9%,
  #while the ctree function returned an accuracy of 82.7%. 
  #For this reason I feel as though the rpart tree is better as it is more accurate.
  
#6. 
 table(kyphosis$Kyphosis)
 N = sum(kyphosis$Kyphosis == 'absent') + sum(kyphosis$Kyphosis == 'present')
 p0 = sum(kyphosis$Kyphosis == 'present')/N
 p1 = sum(kyphosis$Kyphosis == 'absent')/N
 
 entropy = -(p0*log2(p0) + p1*log2(p1))  
 entropy
 
 table(kyphosis$Kyphosis, kyphosis$Start)
 
node1 = 1.00
entropy1 = -(node1*log2(node1)) #I got rid of the other half because it's zero.
node2 = 1.00
entropy2 = -(node2*log2(node2))
node3 = .86
entropy3 = -(node3*log2(node3) + (1-node3)*log2(1-node3)) 
node4 = .43
entropy4 = -(node4*log2(node4) + (1-node4)*log2(1-node4)) 
node5 = .42
entropy5 = -(node5*log2(node5) + (1-node5)*log2(1-node5)) 



prob1 = .36
prob2 = .15
prob3 = .17
prob4 = .09
prob5 = .23

weightedEntropy = entropy1*prob1 + entropy2*prob2 + entropy3*prob3 + entropy4*prob4 + entropy5*prob5
weightedEntropy
  
#7.


