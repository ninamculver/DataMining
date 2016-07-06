giniscore = function(vector.g){
  g = rep(1,length(vector.g))
  for (i in 1:length(vector.g)){
    g[i] = vector.g[i]^2
  }
  (1 - sum(g[i]))
}
giniscore(vector.g)

entropyscore = function(vector.g){
  e = rep(1, length(vector.g))
  for (i in 1:length(vector.g)){
    if (vector.g != 0) 
    {e[i] = (vector.g[i] * log2(vector.g[i]))}
    else
    {e[i] = 0}
    
  }
  (-e[i])
}
entropyscore(vector.g)

classificationerror = function(vector.g){
  1-max(vector.g)
}
classificationerror(vector.g)

x=seq(from=-5,to=5,by=0.01) 
mysquare=function(a){ 
  return(a^2)}
y=sapply(x,mysquare) 
plot(x,y,type='l') 


vector.b = seq(from = 0, to = 1, by = .01)
twoclassgini = function(vector.b){
  p = rep (1,length(vector.b))
  for (i in 1:length(vector.b))
  { p0 = vector.b[i]
    p1 = 1-p0
    p[i] = 1- (p0^2 + p1^2)
  }
  plot(vector.b, p, type = 'l')
}
twoclassgini(vector.b)

twoclasserror = function(vector.b){
  pc = rep (1, length(vector.b))
  for (i in 1:length(vector.b)){
    p0 = vector.b[i]
    p1 = 1-p0
    pc[i] = 1-max(p0, p1)
  }
  plot(vector.b, pc, type = 'l')
}

twoclasserror(vector.b)

twoclassentropy = function(vector.b){
  ep = rep (1,length(vector.b))
  for (i in 1:length(vector.b)){
    p0 = vector.b[i]
    p1 = 1-p0
   if (vector.b[i] == 0 | vector.b[i] ==1 ){
    ep[i] = 0
   }
    else {
    ep[i] = -(p0 *log2(p0) + p1 * log2(p1))
    
    }
  }
 plot(vector.b, ep, type = 'l')
}

twoclassentropy(vector.b)


table(Hw2)
table(Hw2$Class) #there are 480 c0's and 520 c1's
c0 = 480
c1 = 520
n = 1000

p0 = c0/n
p1 = c1/n

p0
p1

HwEntropy = -(p0*log2(p0) + p1*log2(p1))  
HwEntropy


table(Hw2$Class, Hw2$Gender) 
gmN = 509
gfN = 491

fc0 = 190
mc0 = 290
fc1 = 301
mc1 = 219

fp0 = fc0/gfN
fp1 = fc1/gfN
mp0 = mc0/gmN
mp1 = mc1/gmN

MEntropy = -(mp0*log2(mp0) + mp1*log2(mp1))  
FEntropy = -(fp0*log2(fp0) + fp1*log2(fp1))


HwEntropy = (p0*FEntropy) + (p1*MEntropy)
HwEntropy


library(rpart)
library(rattle)

Hw2Tree = rpart(CarType~Gender+Class+ShirtSize, data = Hw2)
fancyRpartPlot(Hw2Tree)

CarTree = rpart(CarType~., data = Hw2)
fancyRpartPlot(CarTree)

GenderTree = rpart(Gender~., data = Hw2)
fancyRpartPlot(GenderTree)

ClassTree = rpart(Class~., data = Hw2)
fancyRpartPlot(ClassTree)

ShirtTree = rpart(ShirtSize ~., data = Hw2)
fancyRpartPlot(ShirtTree)
