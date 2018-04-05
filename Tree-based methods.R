rm(list=ls())

# Install packages
installIfAbsentAndLoad <- function(neededVector) {
  for(thispackage in neededVector) {
    if( ! require(thispackage, character.only = T) )
    { install.packages(thispackage)}
    require(thispackage, character.only = T)
  }
}

needed <- c("rpart",'tree',"randomForest","ada","e1071","gbm","xgboost","rattle")
installIfAbsentAndLoad(needed)

#Load data
mydata<-read.csv("Assignment2TrainingData.csv",sep=",",header=T)
mydata<-mydata[,-1]
mydata$SeniorCitizen<-as.factor(mydata$SeniorCitizen)
#Define cost function
at_risk<-0.26448
no_risk<-1-at_risk
leave<-0.55
stay<-0.45
f_cost<-function(x){x[1]*no_risk*1600+x[2]*at_risk*11500+x[3]*at_risk*(leave*13100+stay*1600)}

#Divide into training and test data
set.seed(5082)
train<-sample(1:nrow(mydata), .7*nrow(mydata))
test<-mydata[-train,]




cutoffs<-seq(0,0.99,0.01)
###############
#Decision Tree#
###############
dt <- rpart(Churn ~ .,data=mydata[train,], method="class",
                 parms=list(split="information"),control=rpart.control(usesurrogate=0,maxsurrogate=0,cp=0,minbucket=1,minsplit=2))

cprange<-seq(0,0.1,0.001)
dt_mincost_matrix<-matrix(0,nrow=length(cprange),ncol=7,dimnames = list(NULL,c("cutoff","FP","FN","TP","TN","OE","Exp.cost")))
for (i in 1:length(cprange)){
  dt.prune<-prune(dt,cp=cprange[i])
  dt.prune.prob <- predict(dt.prune, newdata=test, type="prob")
  
  error_matrix<-matrix(0,nrow=length(cutoffs),ncol = 6,dimnames = list(NULL,c("FP","FN","TP","TN","OE","Exp.cost")))
  for (j in 1:length(cutoffs)){
    dt.prune.pred <- rep("No", length(test))
    dt.prune.pred[dt.prune.prob[,2] >=cutoffs[j] ] <- "Yes"
    dt.prune.pred[is.na(dt.prune.pred)] <- "No"
    #mytable<-table(test$Churn, dt.prune.pred,dnn=c("Actual", "Predicted"))/length(dt.prune.pred)
    OE<-mean(dt.prune.pred!=test$Churn)
    TN<-sum(dt.prune.pred=="No"&test$Churn=="No")/sum(test$Churn=="No")
    TP<-sum(dt.prune.pred=="Yes"&test$Churn=="Yes")/sum(test$Churn=="Yes")
    FN<-1-TP
    FP<-1-TN
    error_rate<-c(FP,FN,TP,TN,OE)
    exp_cost<-f_cost(error_rate)
    error_matrix[j,]<-c(FP,FN,TP,TN,OE,exp_cost)
  }
  mymatrix<-cbind(cutoffs,error_matrix)
  dt_mincost_matrix[i,]<-mymatrix[which.min(mymatrix[,7]),]
}
dt_mincost_matrix<-cbind(cprange,dt_mincost_matrix)
dt_mincost_matrix[which.min(dt_mincost_matrix[,8]),]
#cprange       cutoff           FP           FN           TP           TN           OE     Exp.cost 
#0.0020000    0.1900000    0.2738806    0.2283298    0.7716702    0.7261194    0.2619967 2634.2049391 

dt_best.prune<-prune(dt,cp=dt_mincost_matrix[which.min(dt_mincost_matrix[,8]),][1])

###############
#Random Forest#
###############
set.seed(5082)
ntree_range<-seq(100,900,100)
mtry_range<-seq(1,18,1)
leng<-length(ntree_range)*length(mtry_range)
rf_mincost_matrix<-matrix(0,nrow=leng,ncol=9,dimnames = list(NULL,c("ntree","mtry","cutoff","FP","FN","TP","TN","OE","Exp.cost")))

for (a in 1:length(ntree_range)){
  for (b in 1:length(mtry_range)){
    rf <- randomForest(formula=Churn ~ .,data=mydata[train,],ntree= ntree_range[a], mtry=mtry_range[b],
                       importance=TRUE,localImp=TRUE,na.action=na.roughfix,replace=FALSE)
    rf.prob <- predict(rf, newdata=test, type="prob")

    error_matrix<-matrix(0,nrow=length(cutoffs),ncol = 6,dimnames = list(NULL,c("FP","FN","TP","TN","OE","Exp.cost")))
    for (i in 1:length(cutoffs)){
      rf.pred <- rep("No", length(test))
      rf.pred[rf.prob[,2] >= cutoffs[i]] <- "Yes"
      rf.pred[is.na(rf.pred)] <- "No"
      #mytable<-table(test$Churn, rf.pred,dnn=c("Actual", "Predicted"))/length(rf.pred)
      OE<-mean(rf.pred!=test$Churn)
      TN<-sum(rf.pred=="No"&test$Churn=="No")/sum(test$Churn=="No")
      TP<-sum(rf.pred=="Yes"&test$Churn=="Yes")/sum(test$Churn=="Yes")
      FN<-1-TP
      FP<-1-TN
      error_rate<-c(FP,FN,TP,TN,OE)
      exp_cost<-f_cost(error_rate)
      error_matrix[i,]<-c(FP,FN,TP,TN,OE,exp_cost)
    }
    mymatrix<-cbind(cutoffs,error_matrix)
    rf_mincost_matrix[(a-1)*length(mtry_range)+b,1:2]<-c(ntree_range[a],mtry_range[b])
    rf_mincost_matrix[(a-1)*length(mtry_range)+b,3:9]<-mymatrix[which.min(mymatrix[,7]),]
  }
}

rf_mincost_matrix[which.min(rf_mincost_matrix[,9]),]
#ntree         mtry       cutoff           FP           FN           TP           TN           OE     Exp.cost 
#700.0000000    2.0000000    0.2500000    0.2164179    0.2875264    0.7124736    0.7835821    0.2349697 2622.5523596


##############
#Boosting-ada#
##############
set.seed(5082)
bm<- ada(formula=Churn ~ .,data=mydata[train,],iter=50,bag.frac=0.5,
         control=rpart.control(maxdepth=30,cp=0.01,minsplit=20,xval=10))
bm.prob <- predict(bm, newdata=test, type="prob")

cutoffs<-seq(0,0.99,0.01)
error_matrix<-matrix(0,nrow=length(cutoffs),ncol = 6,dimnames = list(NULL,c("FP","FN","TP","TN","OE","Exp.cost")))
for (i in 1:length(cutoffs)){
  bm.pred <- rep("No", length(test))
  bm.pred[bm.prob[,2] >= cutoffs[i]] <- "Yes"
  bm.pred[is.na(bm.pred)]<-"No"
  #mytable<-table(test$Churn, bm.pred,dnn=c("Actual", "Predicted"))/length(bm.pred)
  OE<-mean(bm.pred!=test$Churn)
  TN<-sum(bm.pred=="No"&test$Churn=="No")/sum(test$Churn=="No")
  TP<-sum(bm.pred=="Yes"&test$Churn=="Yes")/sum(test$Churn=="Yes")
  FN<-1-TP
  FP<-1-TN
  error_rate<-c(FP,FN,TP,TN,OE)
  exp_cost<-f_cost(error_rate)
  error_matrix[i,]<-c(FP,FN,TP,TN,OE,exp_cost)
}
mymatrix<-cbind(cutoffs,error_matrix)

mymatrix[which.min(mymatrix[,7]),]

#cutoffs           FP           FN           TP           TN           OE     Exp.cost 
#0.2700000    0.2126866    0.2832981    0.7167019    0.7873134    0.2311087 2614.1632419

##############
#Boosting-gbm#
##############
set.seed(5082)
gbm.data<-mydata

gbm.data$Churn=as.character(gbm.data$Churn)
for (i in 1:nrow(gbm.data)){
  if (gbm.data$Churn[i]=="Yes"){
    gbm.data$Churn[i]=1
  } else {
    gbm.data$Churn[i]=0
  }
}
gbm.data$Churn=as.numeric(gbm.data$Churn)
gbmtest<-gbm.data[-train,]


# gbm bernoulli
gbmmodel<-gbm(Churn~.,distribution = 'bernoulli',data=gbm.data[train,],n.trees=50,shrinkage=0.3,bag.fraction = 0.5)

gbmmodel.prob <- predict(gbmmodel, newdata=test, type="response",n.trees=50)

cutoffs<-seq(0,0.99,0.01)
error_matrix<-matrix(0,nrow=length(cutoffs),ncol = 6,dimnames = list(NULL,c("FP","FN","TP","TN","OE","Exp.cost")))
for (i in 1:length(cutoffs)){
  gbmmodel.pred <- rep(0, length(test))
  gbmmodel.pred[gbmmodel.prob >= cutoffs[i]] <- 1
  gbmmodel.pred[is.na(gbmmodel.pred)] <- 0
  #mytable<-table(test$Churn, gbmmodel.pred,dnn=c("Actual", "Predicted"))/length(gbmmodel.pred)
  OE<-mean(gbmmodel.pred!=gbmtest$Churn)
  TN<-sum(gbmmodel.pred==0&gbmtest$Churn==0)/sum(gbmtest$Churn==0)
  TP<-sum(gbmmodel.pred==1&gbmtest$Churn==1)/sum(gbmtest$Churn==1)
  FN<-1-TP
  FP<-1-TN
  error_rate<-c(FP,FN,TP,TN,OE)
  exp_cost<-f_cost(error_rate)
  error_matrix[i,]<-c(FP,FN,TP,TN,OE,exp_cost)
}
mymatrix<-cbind(cutoffs,error_matrix)

mymatrix[which.min(mymatrix[,7]),]
#cutoffs           FP           FN           TP           TN           OE     Exp.cost 
#0.3300000    0.1970149    0.2917548    0.7082452    0.8029851    0.2217319 2603.7162594 
