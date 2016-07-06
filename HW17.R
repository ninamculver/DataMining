###### Homework 17 ######
######Nina Culver######


####1####
set.seed(5364)
x1 = rnorm(100, 5, 1)
y1 = rnorm(100, 10, 1)
x2 = rnorm(100, 5, 1)
y2 = rnorm(100, 20, 1)
x3 = rnorm(100, 15, 1)
y3 = rnorm(100, 10, 1)
x4 = rnorm(100, 15, 1)
y4 = rnorm(100, 20, 1)

mydata=data.frame(x=c(x1,x2,x3,x4),y=c(y1,y2,y3,y4))
plot(y~x,data=mydata)


#1a
repeat.kmeans=function(data,centers,repetitions){
  best.kmeans=NULL
  best.ssw=Inf
  
  for(i in 1:repetitions){
    kmeans.temp=kmeans(x=data,centers=centers)
    if(kmeans.temp$tot.withinss<best.ssw){
      best.ssw=kmeans.temp$tot.withinss
      best.kmeans=kmeans.temp
    }
  }
  return(best.kmeans)
}


best=repeat.kmeans(mydata,4,1000)

#1b
plot(y~x,data=mydata,col=best$cluster,asp=1)
clusters=best$cluster

#1c
best$totss #20551.42
best$withinss #248.6826 176.7179 201.4723 158.9062
best$tot.withinss #785.779
best$betweenss #19765.64
best$tot.withinss+best$betweenss # 20551.42


#1d
centers=best$centers 
# (15.042800 19.88120)(15.097904 10.06792)(5.063149,10.10852)(4.972396 19.95161)
plot(y~x,data=mydata,col=colvect[clusters],asp=1)
points(centers,col='black',pch=24,bg='black')

#1e
minimumRepititions = function(x, error){
  ceiling(log(error)/log(1-factorial(x) / (x^x)))
}
minimumNumber = minimumRepititions(4, 0.01) #47 is the requirement

#1f
tryCenter1 = cbind(c(15,8,5,10), c(25,15,5,10))
tryCluster1 = kmeans(x = mydata, centers = tryCenter1)
plot(y~x,data=mydata,col=colvect[tryCluster1$cluster],asp=1)
points(tryCluster1$centers, col='black',pch=24,bg='black')

tryCenter2 = cbind(c(10,11,12,13), c(15,14,13,12))
tryCluster2 = kmeans(x = mydata, centers = tryCenter2)
plot(y~x,data=mydata,col=colvect[tryCluster2$cluster],asp=1)
points(tryCluster2$centers, col='black',pch=24,bg='black')

tryCenter3 = cbind(c(20,15,5,10), c(25,15,5,10))
tryCluster3 = kmeans(x = mydata, centers = tryCenter3)
plot(y~x,data=mydata,col=colvect[tryCluster3$cluster],asp=1)
points(tryCluster3$centers, col='black',pch=24,bg='black')

tryCenter4 = cbind(c(15,25,24,10), c(5,20,25,8))
tryCluster4 = kmeans(x = mydata, centers = tryCenter4)
plot(y~x,data=mydata,col=colvect[tryCluster4$cluster],asp=1)
points(tryCluster4$centers, col='black',pch=24,bg='black') #This one messed it up!


#testing number of centers 
tryCluster5 = kmeans(x = mydata, centers = 1)
plot(y~x,data=mydata,col=colvect[tryCluster5$cluster],asp=1)
points(tryCluster5$centers, col='black',pch=24,bg='black')

tryCluster6 = kmeans(x = mydata, centers = 6)
plot(y~x,data=mydata,col=colvect[tryCluster6$cluster],asp=1)
points(tryCluster6$centers, col='black',pch=24,bg='black')

tryCluster7 = kmeans(x = mydata, centers = 3)
plot(y~x,data=mydata,col=colvect[tryCluster7$cluster],asp=1)
points(tryCluster7$centers, col='black',pch=24,bg='black')

##### 2 #####
wdbc = wdbc[,2:32]
wdbcModel=repeat.kmeans(wdbc[,2:31],2,1000)
wdbctable = table(wdbcModel$cluster,wdbc$V2)
accuracy = ((wdbctable[1,1] + wdbctable[2,2]) - (wdbctable[2,1] + wdbctable[1,2])) / (dim(wdbc)[1])
accuracy
