##### Nina Culver #####
### Data Mining II ###
#### Feb. 2, 2016 ####
##### Homework 23 #####

install.packages("lawstat")
library("lawstat")

install.packages("MASS")
library("MASS")

Y = myData$Y

######## 1 ########
myData = math5305Lab6Data

colnames(myData) = c("Y", "X1", "X2", "X3")

#a) 
model = lm(Y~., data = myData)
summary(model)
betahat = coef(model)
yhat = predict(model)
e = residuals(model)

#b)
plot(myData$Y, yhat)
lines(0:2000000,0:2000000,col="red")
### If the model were valid, I would expect it to look like this for most of the graph, 
### however, there are a few outliers throughout it since it's not flat against the red line. 

#c) 
plot(e, yhat)
### The residuals seem fairly random, however there appears to be a small trend, therefore
### this may not be exactly what we want. 

#d)

#e)
qqnorm(e)

#f)
shapiro.test(e) #W = 0.94232, p-value = 0.0002678
### In shapiro you want a high p value, since ours is low the errors are not normalized.
### D=

#g) 
### No, they are not normal. D'=

#h)
plot(x,abs(e))

#i)
levene.test(e,as.factor(x<=median(x))) # p-value = 0.4095

#j)
###It appears from h that the error terms are not constant
###And by i we can infer by rejecting the null hypothesis that they are all equal.

#k)
#Yes, this does seem to be necessary.
#l)
esquared = sum((Y-yhat)^2) #6.085881e+16
###Multiple R-squared:  0.9066,	
###Adjusted R-squared:  0.9037 


######## 2 ########
boxcox.results=boxcox(model)
cbind(boxcox.results$x,boxcox.results$y)
which.max(boxcox.results$y)
lambda=boxcox.results$x[which.max(boxcox.results$y)]

#a) 
ytilde=Y^lambda
tmodel=lm(ytilde~ myData$X1+myData$X2+myData$X3)
tyhat = predict(tmodel)
te = residuals(tmodel)

#b)
plot(ytilde, tyhat)
plot(te, tyhat)
###Curvature appears to be much more normalized this time. 

#c) 
qqnorm(te)
shapiro.test(te) #W = 0.9862, p-value = 0.3862

#d)
plot(x,abs(te))
levene.test(te,as.factor(x<=median(x))) #Test Statistic = 0.099492, p-value = 0.7531

#e)
###It appears the resials are normal now which is good because we can use a p or f statistic that means!

######## 3 ########
#a) 
yhat3 = tyhat^(1/lambda)
e3 = (Y-yhat3)

#b)
plot(Y, yhat3)
plot(e3, yhat3)
#Yes, for the first plot it did, however there are some outliers in the second plot.

#c) 
esquared3 = sum((Y-yhat3)^2)
ybar3 = sum(Y)/length(Y)
ybar3 = rep(ybar3, 100)
calc3 = esquared/(sum(Y-ybar3)^2) #6.394571e+33 (it's really big...)
rsquared = 1-calc3 #-6.394571e+33
###Multiple R-squared:  0.9595,	Adjusted R-squared:  0.9582 

######## 4 ########
autoData = auto.mpg[-9]
colnames(autoData) = c("mpg", "cylinders", "displacement", "horsepower", "weight", "acceleration", "modelyear", "origin")
autoModel = lm(mpg~., data = autoData)
summary(autoModel) #Adjusted R-squared:  0.8694 F-statistic:  27.7 p-value: < 2.2e-16
autobetahat = coef(autoModel)
autoyhat = predict(autoModel)
autoe = residuals(autoModel)

plot(autoData$mpg, autoyhat)
plot(autoe, autoyhat)

qqnorm(autoe)
shapiro.test(autoe)#p-value = 1.002e-09 REALLY SMALL no good. D=

ax = c(1:length(autoe))
plot(ax,abs(autoe))
levene.test(autoe,as.factor(ax<=median(ax))) #p-value = 0.0001283

autoBoxcox=boxcox(autoModel)
cbind(autoBoxcox$x,autoBoxcox$y)
which.max(autoBoxcox$y)
autolambda=autoBoxcox$x[which.max(autoBoxcox$y)]

mpg = autoData$mpg
mpgtilde=mpg^autolambda
autotmodel=lm(mpgtilde~ autoData$cylinders+autoData$displacement+autoData$horsepower+autoData$weight+autoData$acceleration+autoData$modelyear+autoData$origin)
autotyhat = predict(autotmodel)
autote = residuals(autotmodel)

plot(mpgtilde, autotyhat)
plot(autote, autotyhat)

qqnorm(autote)
shapiro.test(autote) #W = 0.9862, p-value = 0.3862

plot(x,abs(te))
levene.test(te,as.factor(x<=median(x))) #p-value = 6.471e-06

autoyhat3 = autotyhat^(1/autolambda)
autoe3 = (mpg-autoyhat3)

plot(mpg, autoyhat3)
plot(autoe3, autoyhat3)


