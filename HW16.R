library(MASS)
library(e1071)
data(fgl)
set.seed(5364)
####1 Preamble####
index=sample(nrow(fgl),round(nrow(fgl)*.7,0))
train = fgl[index,]
test = fgl[-index,]
###1a###
model = svm(type~., data = train)
pred=predict(model,newdata=test)
confmatrix(pred,test$type) # accuracy = 0.640625


tuneFGL=tune.svm(type~.,data=train,
                  gamma = 10^(-9:-1), 
                  cost = 10^(1:5))
tuneModel = svm(type~., data = train, gamma = tuneFGL$best.parameters[,1], cost = tuneFGL$best.parameters[,2])
tunePred=predict(tuneModel,newdata=test)
confmatrix(tunePred,test$type) # accuracy = 0.65625

###1b###
#WinF against the rest model
winF.train=traindata
winF.train$type[(winF.train$type!="WinF")]="other"

winF.model=svm(type~.,data=winF.train)
winF.pred=predict(winF.model,
                 newdata=fgl[-index,],
                 type="class")

table(winF.pred,fgl$type[-index])

#WinNF against the rest model
winNF.train=traindata
winNF.train$type[(winNF.train$type!="WinNF")]="other"


winNF.model=svm(type~.,data=winNF.train)
winNF.pred=predict(winNF.model,
                   newdata=fgl[-index,],
                   type="class")

table(winNF.pred,fgl$type[-index])

#Veh against the rest model
veh.train=traindata
veh.train$type[(veh.train$type!="Veh")]="other"
veh.model=svm(type~.,data=veh.train)
veh.pred=predict(veh.model,
                   newdata=fgl[-index,],
                   type="class")

table(veh.pred,fgl$type[-index])
#Con against the rest model
con.train=traindata
con.train$type[(con.train$type!="Con")]="other"

con.model=svm(type~.,data=con.train)
con.pred=predict(con.model,
                   newdata=fgl[-index,],
                   type="class")

table(con.pred,fgl$type[-index])
#Tabl against the rest model
tabl.train=traindata
tabl.train$type[(tabl.train$type!="Tabl")]="other"


tabl.model=svm(type~.,data=tabl.train)
tabl.pred=predict(tabl.model,
                   newdata=fgl[-index,],
                   type="class")

table(tabl.pred,fgl$type[-index])
#Head against the rest model
head.train=traindata
head.train$type[(head.train$type!="Head")]="other"


head.model=svm(type~.,data=head.train)
head.pred=predict(head.model,
                   newdata=fgl[-index,],
                   type="class")

table(head.pred,fgl$type[-index])

#Tallying votes
winF.votes2=(winF.pred=="WinF")+
  (winNF.pred=="other")+
  (veh.pred=="other")+
  (con.pred=="other")+
  (head.pred=="other")+
  (tabl.pred=="other")

winNF.votes2=(winNF.pred=="WinNF")+
  (winF.pred=="other")+
  (veh.pred=="other")+
  (con.pred=="other")+
  (head.pred=="other")+
  (tabl.pred=="other")

veh.votes2=(veh.pred=="Veh")+
  (winNF.pred=="other")+
  (winF.pred=="other")+
  (con.pred=="other")+
  (head.pred=="other")+
  (tabl.pred=="other")

con.votes2=(con.pred=="Con")+
  (winNF.pred=="other")+
  (veh.pred=="other")+
  (winF.pred=="other")+
  (head.pred=="other")+
  (tabl.pred=="other")

head.votes2=(head.pred=="Head")+
  (winNF.pred=="other")+
  (veh.pred=="other")+
  (con.pred=="other")+
  (winF.pred=="other")+
  (tabl.pred=="other")

tabl.votes2=(tabl.pred=="Tabl")+
  (winNF.pred=="other")+
  (veh.pred=="other")+
  (con.pred=="other")+
  (head.pred=="other")+
  (winF.pred=="other")
votes2=cbind(winF.votes2,winNF.votes2, veh.votes2, con.votes2, tabl.votes2, head.votes2)

one.against.rest.pred=c("WinF",
                        "WinNF",
                        "Veh",
                        "Con",
                        "Tabl",
                        "Head")[apply(votes2,
                                           1,
                                           which.max)]

table(pred,one.against.rest.pred)

confmatrix(fgl$type[-index],
           one.against.rest.pred) # accuracy =  0.078125
###1c###
#WinF v. WinNF
winF.winNF.train=traindata[(traindata$type=="WinF")|
                           (traindata$type=="WinNF"),]
winF.winNF.model=svm(type~.,data=winF.winNF.train)
winF.winNF.pred=predict(winF.winNF.model,
                      newdata=fgl[-index,],
                      type="class")
#WinF v. Con
winF.con.train=traindata[(traindata$type=="WinF")|
                             (traindata$type=="Con"),]
winF.con.model=svm(type~.,data=winF.con.train)
winF.con.pred=predict(winF.con.model,
                        newdata=fgl[-index,],
                        type="class")
#WinF v. Table
winF.tabl.train=traindata[(traindata$type=="WinF")|
                             (traindata$type=="Tabl"),]
winF.tabl.model=svm(type~.,data=winF.tabl.train)
winF.tabl.pred=predict(winF.tabl.model,
                        newdata=fgl[-index,],
                        type="class")
#WinF v. Veh
winF.veh.train=traindata[(traindata$type=="WinF")|
                             (traindata$type=="Veh"),]
winF.veh.model=svm(type~.,data=winF.veh.train)
winF.veh.pred=predict(winF.veh.model,
                        newdata=fgl[-index,],
                        type="class")
#WinF v. Head
winF.head.train=traindata[(traindata$type=="WinF")|
                             (traindata$type=="Head"),]
winF.head.model=svm(type~.,data=winF.head.train)
winF.head.pred=predict(winF.head.model,
                        newdata=fgl[-index,],
                        type="class")

#WinNF v. Con
winNF.con.train=traindata[(traindata$type=="WinNF")|
                           (traindata$type=="Con"),]
winNF.con.model=svm(type~.,data=winNF.con.train)
winNF.con.pred=predict(winNF.con.model,
                      newdata=fgl[-index,],
                      type="class")
#WinNF v. Tabl
winNF.tabl.train=traindata[(traindata$type=="WinNF")|
                            (traindata$type=="Tabl"),]
winNF.tabl.model=svm(type~.,data=winNF.tabl.train)
winNF.tabl.pred=predict(winNF.tabl.model,
                       newdata=fgl[-index,],
                       type="class")
#WinNF v. Veh
winNF.veh.train=traindata[(traindata$type=="WinNF")|
                           (traindata$type=="Veh"),]
winNF.veh.model=svm(type~.,data=winNF.veh.train)
winNF.veh.pred=predict(winNF.veh.model,
                      newdata=fgl[-index,],
                      type="class")
#WinNF v. Head
winNF.head.train=traindata[(traindata$type=="WinNF")|
                            (traindata$type=="Head"),]
winNF.head.model=svm(type~.,data=winNF.head.train)
winNF.head.pred=predict(winNF.head.model,
                       newdata=fgl[-index,],
                       type="class")
#Veh v. Head
veh.head.train=traindata[(traindata$type=="Veh")|
                             (traindata$type=="Head"),]
veh.head.model=svm(type~.,data=veh.head.train)
veh.head.pred=predict(veh.head.model,
                        newdata=fgl[-index,],
                        type="class")
#Veh v. Tabl
veh.tabl.train=traindata[(traindata$type=="Veh")|
                           (traindata$type=="Tabl"),]
veh.tabl.model=svm(type~.,data=veh.tabl.train)
veh.tabl.pred=predict(veh.tabl.model,
                      newdata=fgl[-index,],
                      type="class")
#Veh v. Con
veh.con.train=traindata[(traindata$type=="Veh")|
                           (traindata$type=="Con"),]
veh.con.model=svm(type~.,data=veh.con.train)
veh.con.pred=predict(veh.con.model,
                      newdata=fgl[-index,],
                      type="class")
#Con v. Head
con.head.train=traindata[(traindata$type=="Con")|
                           (traindata$type=="Head"),]
con.head.model=svm(type~.,data=con.head.train)
con.head.pred=predict(con.head.model,
                      newdata=fgl[-index,],
                      type="class")
#Con v. Table
con.tabl.train=traindata[(traindata$type=="Con")|
                           (traindata$type=="Tabl"),]
con.tabl.model=svm(type~.,data=con.tabl.train)
con.tabl.pred=predict(con.tabl.model,
                      newdata=fgl[-index,],
                      type="class")
#Head v. Table
head.tabl.train=traindata[(traindata$type=="Head")|
                           (traindata$type=="Tabl"),]
head.tabl.model=svm(type~.,data=head.tabl.train)
head.tabl.pred=predict(head.tabl.model,
                      newdata=fgl[-index,],
                      type="class")

winF.votes = (winF.winNF.pred=="WinF")+
  (winF.veh.pred=="WinF")+
  (winF.con.pred=="WinF")+
  (winF.tabl.pred=="WinF")+
  (winF.head.pred=="WinF")
winNF.votes = winF.votes = (winF.winNF.pred=="WinNF")+
  (winNF.veh.pred=="WinNF")+
  (winNF.con.pred=="WinNF")+
  (winNF.tabl.pred=="WinNF")+
  (winNF.head.pred=="WinNF")
veh.votes = (winF.veh.pred=="Veh")+
  (winNF.veh.pred=="Veh")+
  (veh.con.pred=="Veh")+
  (veh.tabl.pred=="Veh")+
  (veh.head.pred=="Veh")
con.votes = (winF.con.pred=="Con")+
  (winNF.con.pred=="Con")+
  (veh.con.pred=="Con")+
  (con.tabl.pred=="Con")+
  (con.head.pred=="Con")
tabl.votes = (winF.tabl.pred=="Tabl")+
  (winNF.tabl.pred=="Tabl")+
  (veh.tabl.pred=="Tabl")+
  (con.tabl.pred=="Tabl")+
  (head.tabl.pred=="Tabl")
head.votes = (winF.head.pred=="Head")+
  (winNF.head.pred=="Head")+
  (veh.head.pred=="Head")+
  (con.head.pred=="Head")+
  (head.tabl.pred=="Head")

votes=cbind(winF.votes,winNF.votes, veh.votes, con.votes, tabl.votes, head.votes)
apply(votes,1,which.max)

one.against.one.pred=c("WinF",
                       "WinNF",
                       "Veh", 
                       "Con",
                       "Tabl",
                       "Head")[apply(votes,
                                          1,
                                          which.max)]

table(pred,one.against.one.pred)
pred==one.against.one.pred
confmatrix(fgl$type[-index],
           one.against.one.pred) # accuracy = 0.078125

