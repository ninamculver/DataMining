###### Homework 18 ######
######Nina Culver######

library(fields)
library(cluster)

####Creating the Data####
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

#### 1 ####
#1a)

dmatrix=rdist(mydata)
plot.sil=function(data,max.K,max.iter,epsilon,dmatrix){
  sil.vect=1:max.K
  for(K in 2:max.K){
    iter=min(max.iter,min.rep(K,epsilon))
    kmeans.temp=repeat.kmeans(data,K,iter)
    sil.vect[K]=mysil(kmeans.temp$cluster,dmatrix)
  }
  sil.vect=sil.vect[2:max.K]
  plot(2:max.K,sil.vect,xlab="K",ylab="Silhouette Coefficient")
  return(max(sil.vect))
}
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
plot.sil(mydata,10,1000,.01,dmatrix) #best is 4 value given is 0.8187304

#1b)
mean(silhouette(x=best$cluster,dmatrix=dmatrix)[,3])  #0.8187304


#1c)
plot.ssw=function(data,max.K,max.iter,epsilon){
  ssw.vect=1:max.K
  for(K in 1:max.K){
    iter=min(max.iter,min.rep(K,epsilon))
    kmeans.temp=repeat.kmeans(data,K,iter)
    ssw.vect[K]=kmeans.temp$tot.withinss
  }
  plot(1:max.K,ssw.vect,xlab="K",ylab="SSW")
}

plot.ssw(mydata,10,1000,.01) # K = 4

#1d)
distance.matrix=function(data){
  n=nrow(data)
  dmatrix=matrix(nrow=n,ncol=n)
  for(i in 1:n){
    for(j in 1:n){
      dmatrix[i,j]=sum((data[i,]-data[j,])^2)
    }
  }
  return(sqrt(dmatrix))
}

mymatrix=distance.matrix(mydata)
plot.sil(mydata,10,1000,.01,mymatrix) #0.8187304



####2####
wdbc = wdbc[,2:32]
wdbc.x = wdbc[,2:31]
wdbcDmatrix=rdist(wdbc.x)
plot.sil(wdbc.x,20,1000,.01,wdbcDmatrix) #best is 2... value given is 0.6972646

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


best=repeat.kmeans(wdbc.x,2,1000)

###GETTING ERRORS HERE###
mean(silhouette(x=best$cluster,dmatrix=wdbcDmatrix)[,3])  #0.6972646

plot.ssw(wdbc.x,10,1000,.01) # K = 2
wdbcmatrix=distance.matrix(wdbc.x)
plot.sil(wdbc.x,10,1000,.01,wdbcmatrix) # 0.6972646

wdbc.kmeans=repeat.kmeans(wdbc.x,2,1000)

entropyterm=function(p){
  if(p==0){return(0)}
  return(-p*log(p,base=2))
}

entropy=function(p){
  return(sum(sapply(p,entropyterm)))
}

table.entropy=function(table){
  col.sums=apply(table,2,sum)
  col.props=col.sums/sum(col.sums)
  for(j in 1:ncol(table)){
    if(sum(table[,j]!=0)){
      table[,j]=table[,j]/sum(table[,j])
    }
  }
  table.entropies=apply(table,2,entropy)
  return(col.props%*%table.entropies)
}

wdbc.table=table(wdbc$V2,
                 wdbc.kmeans$cluster)
table.entropy(wdbc.table) #0.5503462