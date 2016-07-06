
setwd("\\Users\\000680776\\Documents\\Nina's Computer\\Spring 2016\\Data Mining II\\sentiment labelled sentences")
temp = list.files(pattern="*.txt")
for (i in 1:length(temp))
  assign(temp[i], read.delim(file=temp[i], 
                             header = F, 
                             sep = "\n", 
                             quote = '',
                             stringsAsFactors = F))

afinn <- read.delim('http://faculty.tarleton.edu/crawford/documents/Math5364/AFINN.txt',
                    header = F, quote = '', stringsAsFactors = F)

source("http://faculty.tarleton.edu/crawford/documents/Math5364/MiscRFunctions.txt")

library(e1071)
library(pROC)
library(stringr)
library(randomForest)
library(tm)
library(twitteR)

setup_twitter_oauth(consumer_key="rf9Bym8zIRbxzwI8vEQTmLu5S",
                    consumer_secret ="BuN2d38QHcoH5PgxdUwIp6jps7poyQWxRxE2z2DbYf3N8oSISs",
                    access_token="4913426354-dWGlYAlEgr2xZVU0Z85P8xDLKUI4Guxq4RR9C2P",
                    access_secret="UTNWewqwm1kWc6OIEgscaJiKOJQRIx0wtHd8aRxqN5UFC")


afinn.frequencies=function(x){
  str_count(x,afinn$word.clean)
}
word.freq <- function(document.vector, sparsity = .999)
{
  temp.corpus <- Corpus(VectorSource(document.vector))
  temp.tf <- DocumentTermMatrix(temp.corpus,
                                control = list(stopwords = stopwords('english'),
                                               removeNumbers = T))
  temp.tf <- removeSparseTerms(temp.tf, sparsity)
  temp.tf <- as.matrix(temp.tf)
  
  freq.df <- colSums(temp.tf)
  freq.df <- data.frame(word = names(freq.df), freq = freq.df)
  rownames(freq.df) <- NULL
  return(freq.df)
}
ndsi.frequencies=function(x){
  str_count(x,freq.all$word)
}
####################CLEANING THE DATA###########################
amazon = amazon_cells_labelled.txt
imdb = imdb_labelled.txt
yelp = yelp_labelled.txt

amazon$V2 = amazon$V1
zeroonerows = sub(".*\t1|.*\t0", "1", amazon$V1)
meow = zeroonerows == 1
sandwich = which(meow, TRUE)
amazon.clean = amazon[sandwich,]

amazon.clean$V2 = gsub(".*\t0", "0", amazon.clean$V1)
amazon.clean$V2 = gsub(".*\t1", "1", amazon.clean$V2)

amazon.clean$V1 = gsub("[[:punct:]]", '', amazon.clean$V1)
amazon.clean$V1 = gsub("\t0|\t1", "", amazon.clean$V1)
amazon.clean$V1 = tolower(amazon.clean$V1)

yelp$V2 = yelp$V1 
zerooneyelp = sub(".*\t1|.*\t0", "1", yelp$V1)
this = (zerooneyelp == 1)
whichyelp = which(this, TRUE)
yelp.clean = yelp[whichyelp,]

yelp.clean$V2 = gsub(".*\t0", "0", yelp.clean$V1)
yelp.clean$V2 = gsub(".*\t1", "1", yelp.clean$V2)

yelp.clean$V1 = gsub("[[:punct:]]", '', yelp.clean$V1)
yelp.clean$V1 = gsub("\t0|\t1", "", yelp.clean$V1)
yelp.clean$V1 = tolower(yelp.clean$V1)

imdb.clean = imdb
imdb.clean$V2 = imdb.clean$V1
imdb.clean$V2 = gsub(".*\t0", "0", imdb.clean$V1)
imdb.clean$V2 = gsub(".*\t1", "1", imdb.clean$V2)

imdb.clean$V1 = gsub("[[:punct:]]", '', imdb.clean$V1)
imdb.clean$V1 = gsub("\t0|\t1", "", imdb.clean$V1)
imdb.clean$V1 = tolower(imdb.clean$V1)

yelp.clean$V3 = c("yelp")
amazon.clean$V3 = c("amazon")
imdb.clean$V3 = c("imdb")

siteData = rbind(yelp.clean, amazon.clean, imdb.clean)

siteData$V2=as.factor(siteData$V2)


names(afinn) <- c('word','score')
afinn$word.clean <- gsub('-',' ' , afinn$word)
afinn$word.clean <- gsub("[[:punct:]]", '', afinn$word.clean)
afinn$word.clean  = tolower(afinn$word.clean)

colnames(siteData) = c("review", "sentiment", "site")

term.freq <- t(apply(t(siteData$review), 2,
                     afinn.frequencies))
siteData$afinn = as.vector(term.freq %*% afinn$score)

###################TRAINING VS. TEST DATA#######################

training=sample(nrow(siteData),round(0.8*nrow(siteData),0))
train =  siteData[training,]
test = siteData[-training,]

########################## NAIVE BAYES ##################################
bayesModel=naiveBayes(sentiment~afinn,
                    data=train)

pred.sentiment=predict(bayesModel,
                       newdata=test)

confmatrix(pred.sentiment,test$sentiment)

phat=predict(bayesModel,
             newdata=test,
             type = "raw")
plot(roc(test$sentiment,phat[,2]))

##################BAG OF WORDS########################
bagsiteData=data.frame(sentiment=siteData$sentiment,term.freq)
bagmodel=randomForest(sentiment~.,data=bagsiteData[training,])

predbag=predict(bagmodel,
                       newdata=bagsiteData[-training,])
confmatrix(predbag,siteData$sentiment[-training])

phat=predict(bagmodel,
             newdata=bagsiteData[-training,],
             type="prob")
plot(roc(siteData$sentiment[-training],phat[,2]))

#################RANDOM FOREST TFIDF #####################
inversefreq = log(nrow(siteData)/colSums(sign(term.freq)))
inversefreq[is.infinite(inversefreq)] = 0
tfidf = term.freq %*% diag(inversefreq)
rf.siteData = data.frame(sentiment = siteData$sentiment, tfidf)
rf.model = randomForest(sentiment~., data = rf.siteData[training,])

predrf =predict(rf.model,
                       newdata=rf.siteData[-training,])
confmatrix(predrf,siteData$sentiment[-training])

phat=predict(rf.model,
              newdata=rf.siteData[-training,],
              type="prob")
plot(roc(siteData$sentiment[-training],phat[,2]))

###########NORMALIZED SENTIMENT DIFFERENCE INDEX####################
corpus=Corpus(VectorSource(siteData$review))

word.freq.pos = word.freq(train$review[train$sentiment == 1],
                          sparsity=0.99)

word.freq.neg = word.freq(train$review[train$sentiment == 0],
                          sparsity=0.99)
freq.all = merge(word.freq.neg, word.freq.pos, by = 'word', all = T)


freq.all$freq.x[is.na(freq.all$freq.x)] = 0
freq.all$freq.y[is.na(freq.all$freq.y)] = 0
alpha = 2^7
freq.all$ndsi = abs(freq.all$freq.x -
                      freq.all$freq.y)/(freq.all$freq.x +
                                          freq.all$freq.y +
                                          2 * alpha)
freq.all = freq.all[order(-freq.all$ndsi), ]
freq.all$word=as.character(freq.all$word)
siteData$review=as.character(siteData$review)

newtermfreq <- t(apply(t(siteData$review), 2,
                     ndsi.frequencies))

newinv.doc.freq=log(nrow(siteData)/colSums(sign(newtermfreq)))
range(newinv.doc.freq)

newinv.doc.freq[is.infinite(newinv.doc.freq)]=0

newtf.idf = newtermfreq %*% diag(newinv.doc.freq)
siteData$sentiment=as.factor(siteData$sentiment)
newrf.siteData=data.frame(sentiment=siteData$sentiment,newtf.idf)
newrf.model=randomForest(sentiment~.,data=newrf.siteData[training,])

newpred.sentiment=predict(newrf.model,
                       newdata=newrf.siteData[-training,])

confmatrix(newpred.sentiment,siteData$sentiment[-training])

phat=predict(newrf.model,
             newdata=newrf.siteData[-training,],
             type="prob")
plot(roc(siteData$sentiment[-training],phat[,2]))

######################################## PART 2 #################################################
verizon=searchTwitteR("Verizon",n=2000,lang="en")
verizon=twListToDF(verizon)

att=searchTwitteR("AT&T",n=2000,lang="en")
att=twListToDF(att)

verizon$clean = gsub("[[:punct:]]", '', verizon$text)
verizon$clean = gsub('<.*?>', '', verizon$clean)
verizon$clean = gsub('[[:cntrl:]]', '', verizon$clean) 
verizon$clean = sapply( verizon$clean, function (row) iconv (row, "latin1", "ASCII" , sub="" ))
verizon$clean = tolower(verizon$clean)

att$clean = gsub("[[:punct:]]", '', att$text)
att$clean = gsub('<.*?>', '', att$clean)
att$clean = gsub('[[:cntrl:]]', '', att$clean) 
att$clean = sapply( att$clean, function (row) iconv (row, "latin1", "ASCII" , sub="" ))
att$clean = tolower(att$clean)

verizonfreq = t(apply(t(verizon$clean), 2,
                      afinn.frequencies))
verizon$afinn = as.vector(verizonfreq %*% afinn$score)

attfreq = t(apply(t(att$clean), 2,
                  afinn.frequencies))
att$afinn = as.vector(attfreq %*% afinn$score)

verizonscore = sum(verizon$afinn)
attscore = sum(att$afinn)
