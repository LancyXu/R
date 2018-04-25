rm(list=ls())
##############################
########## Functions##########
##############################
installIfAbsentAndLoad  <-  function(neededVector) {
  if(length(neededVector) > 0) {
    for(thispackage in neededVector) {
      if(! require(thispackage, character.only = T)) {
        install.packages(thispackage)}
      require(thispackage, character.only = T)
    }
  }
}
##############################
### Load required packages ###
##############################
needed <- c('e1071',"rpart",'tree',"randomForest","ada","gbm","xgboost","rattle","caret",'kernlab','doParallel', 'snow','ape','arules','reshape')      
installIfAbsentAndLoad(needed)

##############################
#########Import Data##########
##############################
dt<-read.csv(file = "train.csv",header=T)
# Make sure all the features are factors
dt$Occupation<-as.factor(dt$Occupation)
dt$Marital_Status<-as.factor(dt$Marital_Status)
dt$Product_Category<-as.factor(dt$Product_Category)

##############################
#########Classification#######
##############################
# #Each row contains a unique customer information
mydata<-dt[,c(-1,-9)]
mydata_info<-mydata[!duplicated(mydata[,1]),-8]
mydata_info<-mydata_info[order(mydata_info[,1]),]
#Sum purchase by User_ID
mydata_purchase<-aggregate(mydata$Purchase,by=list(UserID=mydata$User_ID),FUN=sum)
mydata_purchase<-mydata_purchase[order(mydata_purchase[,1]),]
#Combine each user's information with his/her total purchase
mydata<-cbind(mydata_info,Total_Purchase=mydata_purchase$x)

#Specify the threshold between High value customer and Low value customer
threshold<-mean(mydata$Total_Purchase)
mydata$Total_Purchase[mydata$Total_Purchase>=threshold]="High"
mydata$Total_Purchase[mydata$Total_Purchase!="High"]="Low"
mydata$Total_Purchase<-as.factor(mydata$Total_Purchase)

#Check missing value...FALSE...Good to go!
anyNA(mydata)

#Data Slicing
set.seed(5082)
train<-sample(1:nrow(mydata), .7*nrow(mydata))
test<-mydata[-train,]

registerDoParallel(cores=detectCores())

# #1. SVM
# set.seed(5082)
# seeds <- vector(mode = "list", length = 31)
# for(i in 1:30) seeds[[i]] <- sample.int(1000, 20)
# seeds[[31]]<-sample.int(1000, 1)
# sigma.dist <- sigest(Total_Purchase ~ ., data = mydata, frac = 1)
# svmTuneGrid <- data.frame(.sigma = c(rep(sigma.dist[1], 5),
#                                      rep(sigma.dist[2], 5),
#                                      rep(sigma.dist[3], 5)),
#                           .C = 2^(-1:3), row.names=NULL)
# 
# my.svm.model <- train(Total_Purchase ~ .,
#                       data =mydata[train,-1],
#                       method = "svmRadial",
#                       tuneGrid = svmTuneGrid,
#                       trControl = trainControl(method = "repeatedcv",
#                                                seeds =seeds,
#                                                number = 10,
#                                                repeats = 3,
#                                                classProbs =  TRUE,
#                                                summaryFunction = twoClassSummary))
# 
# #The final values used for the model were sigma = 0.07766811 and C = 0.5.
# svm.pred <- predict.train(my.svm.model, newdata=test[,-1],type = 'raw')
# table(test$Total_Purchase, svm.pred,dnn=c("Actual", "Predicted"))
# (OE_svm<-mean(svm.pred!=test$Total_Purchase))
# #0.313914

#2. Decision Tree
set.seed(5082)
my.tree.model<-train(Total_Purchase ~., data =mydata[train,-1], method = "rpart",
                     parms = list(split = "information"),
                     trControl=trainControl(method = "repeatedcv", number = 10, repeats = 10),
                     tuneLength = 10)
#The final value used for the model was cp = 0.002888087.
#fancyRpartPlot(my.tree.model$finalModel, main="Decision Tree")
tree.pred <- predict.train(my.tree.model, newdata=test[,-1],type = 'raw')
table(test$Total_Purchase, tree.pred,dnn=c("Actual", "Predicted"))
(OE_dt<-mean(tree.pred!=test$Total_Purchase))
#0.2997738

# #3. Random Forest
# set.seed(5082)
# seeds <- vector(mode = "list", length = 31)
# for(i in 1:30) seeds[[i]] <- sample.int(1000, 20)
# seeds[[31]]<-sample.int(1000, 1)
# #mtry <- floor(sqrt(ncol(mydata[train,-1])))
# metric = "Accuracy" #Default setting is Accuarcy"
# my.rf.model<- train(Total_Purchase~., data=mydata[train,-1], method="rf", metric=metric,
#                    tuneLength=length(mydata[train,-1]),
#                    trControl=trainControl(method="repeatedcv", number=10, repeats=3, search="random",seeds = seeds))
# #The final value used for the model was mtry = 5.
# rf.pred <- predict.train(my.rf.model, newdata=test[,-1], type="raw")
# table(test$Total_Purchase, rf.pred,dnn=c("Actual", "Predicted"))
# (OE_rf<-mean(rf.pred!=test$Total_Purchase))
# #0.3088235

# #4. Boosting-gbm
# set.seed(5082)
# seeds <- vector(mode = "list", length = 31)
# for(i in 1:30) seeds[[i]] <- sample.int(1000, 20)
# seeds[[31]]<-sample.int(1000, 1)
# my.gbm.model<-train(Total_Purchase~.,data=mydata[train,-1],method='gbm',
#                     trControl=trainControl(method="repeatedcv", number=10, repeats=3,seeds = seeds),
#                     tuneLength=10)
# #The final values used for the model were n.trees = 500, interaction.depth = 1, shrinkage = 0.1 and n.minobsinnode = 10.
# gbm.pred <- predict.train(my.gbm.model, newdata=test[,-1], type="raw")
# table(test$Total_Purchase, gbm.pred,dnn=c("Actual", "Predicted"))
# (OE_gbm<-mean(gbm.pred!=test$Total_Purchase))
# #0.290724


#The error rates for four models are close, so I pick the fastest model: decision tree
#Also tried gbm for the final validation , and it doesn't perform well as dt does.
best_model<-my.tree.model

##############################
############Cluster###########
##############################
vip_data<-mydata[which(mydata$Total_Purchase=="High"),]
vip_data<-vip_data[,1:7]
for (i in 2:ncol(vip_data)){
  vip_data[,i]<-as.numeric(vip_data[,i])
}

B<-100
nMaxCluster<-10
km_within<-matrix(0,nrow=B+1,ncol=nMaxCluster-1,dimnames = list(NULL,2:nMaxCluster))
for (i in 1:B){
  for (j in 2:nMaxCluster){
    km.out=kmeans(vip_data[,-1],j)
    km_within[i,j-1]<-km.out$tot.withinss
  }
}
for (k in 2:nMaxCluster){
  km_within[B+1,k-1]<-mean(km_within[1:B,k-1])
}

km_between<-matrix(0,nrow=B+1,ncol=nMaxCluster-1,dimnames = list(NULL,2:nMaxCluster))
for (i in 1:B){
  for (j in 2:nMaxCluster){
    km.out=kmeans(vip_data[,-1],j)
    km_between[i,j-1]<-km.out$betweenss
  }
}
for (k in 2:nMaxCluster){
  km_between[B+1,k-1]<-mean(km_between[1:B,k-1])
}

df <- data.frame(x=2:nMaxCluster,withinss=km_within[B+1,],betweenss=km_between[B+1,])
df.melted <- melt(df, id = "x")
ggplot(data = df.melted, aes(x = x, y = value, color = variable)) +
  geom_line()

#nCluster<-which.min(km_table[B+1,])+1
nCluster<-6
km<-kmeans(vip_data[,-1],nCluster)
km.cluster<-km$cluster

#############################
##classification on Cluster##
#############################

#Map cluster with customer
vip_data<-mydata[is.element(mydata$User_ID, vip_data$User_ID),1:7]
vip_data<-cbind(vip_data,km.cluster)
vip_data$km.cluster<-as.factor(vip_data$km.cluster)

#Data Slicing...Again
set.seed(5082)
train<-sample(1:nrow(vip_data), .7*nrow(vip_data))
test<-vip_data[-train,]

#Now do classification on cluster number...using decision tree model
my.tree.model<-train(km.cluster~., data =vip_data[train,-1], method = "rpart",
                     parms = list(split = "information"),
                     trControl=trainControl(method = "repeatedcv", number = 10, repeats = 10),
                     tuneLength = 10)
tree.pred <- predict.train(my.tree.model, newdata=test[,-1],type = 'raw')
table(test$km.cluster, tree.pred,dnn=c("Actual", "Predicted"))
(OE_dt<-mean(tree.pred!=test$km.cluster))
#0.07179487

registerDoSEQ()

##############################
#####Association Analysis#####
##############################
nRec<-10
rec_table<-data.frame(matrix(0,nrow=nRec*nCluster,ncol=7))
Cluster<-rep(1:nCluster,each=10)
rec_table<-cbind(rec_table,Cluster)

for (i in 1:nCluster){
  clusterdata<-dt[is.element(dt$User_ID, vip_data$User_ID[which(vip_data$km.cluster==i)]),]
  buy<- as(split(clusterdata$Product_Category, clusterdata$Transaction_ID),"transactions")
  #inspect(buy)
  #summary(buy)
  #image(buy) 
  myarules<- apriori(buy,parameter=list(support=0.01,confidence=0.01))
  rules<-inspect(sort(myarules,by="confidence"))
  rules<-rules[1:nRec,]
  for (j in c(1,3)){
    rules[,j]<-gsub("\\{|\\}", "", rules[,j])
  }
  rec_table[(1+(i-1)*nRec):(nRec+(i-1)*nRec),1:7]<-rules[1:nRec,]
}
colnames(rec_table)[1:7]<-colnames(rules)
rec_table[,2]<-'=>'

##############################
##Validating using test.csv###
##############################
#Load test.csv
dt_test<-read.csv(file = "test.csv",header=T)
dt_test$Occupation<-as.factor(dt_test$Occupation)
dt_test$Marital_Status<-as.factor(dt_test$Marital_Status)

mydata<-dt_test[,c(-1,-9)]
mydata<-mydata[!duplicated(mydata[,1]),]

#Classify High Purchase customers
Pred<-predict(best_model,mydata[,-1])
mydata<-cbind(mydata,Pred)
vip_data<-mydata[which(mydata$Pred=="High"),]

#Predict the cluster number
cluster.pred <- predict.train(my.tree.model, newdata=vip_data[,-1],type = 'raw')
vip_data<-cbind(vip_data[,-8],cluster.pred)
vip_data<-vip_data[order(vip_data[,1]),]
product<-dt_test[is.element(dt_test$User_ID, vip_data$User_ID),-1]
product<-aggregate(product[,8],by=list(product[,1]),FUN=c)
product<-product[order(product[,1]),]
vip_data<-cbind(vip_data,product)
vip_data<-vip_data[,-9]
colnames(vip_data)[9]<-"Purchase"


#Look up the reccomendation by cluster
rec<-rec_table[,c(1,3,8)]
for (i in 1: nrow(rec)){
  rec$Recommend[i]<-paste(rec[i,1])
}
rec<-rec[,3:4]

interm<-aggregate(rec$Recommend,list(rep(1:(NROW(rec$Recommend)%/%nCluster+1),each=nCluster,len=NROW(rec$Recommend))),paste)
#df_split
#str(interm)
#interm[[2]][5,]
df_splot<-sapply(1:nCluster,function(i) interm[[2]][i,])
hh<-t(df_splot)
final<-sapply(1:nCluster,function(i) paste(hh[i,], sep = ',', collapse = ','))
final<-gsub(" ", "", final, fixed = TRUE)
#vip_data$Purchase[1]
numright<-data.frame(matrix(0, nrow = nCluster, ncol = 1))
numwrong<-data.frame(matrix(0, nrow = nCluster, ncol = 1))
for (i in 1:nrow(vip_data)){
  x<-unique(as.vector(unlist(vip_data$Purchase[i])))
  y<-unique(as.numeric(unlist(strsplit(final[as.numeric(vip_data$cluster.pred[i])], split=","))))
  res <- outer(x, y, '==')
  colnames(res) <- paste0("y=", y)
  rownames(res) <- paste0("x=", x)
  res[is.na(res)]<-FALSE
  #trues<-sum(res, na.rm=TRUE)
  #wrongs<-length(x)*length(y)
  numright[as.numeric(vip_data$cluster.pred[i]),]=numright[as.numeric(vip_data$cluster.pred[i]),]+sum(rowSums(res))
  numwrong[as.numeric(vip_data$cluster.pred[i]),]=numwrong[as.numeric(vip_data$cluster.pred[i]),]+nrow(res)
}
#numright
#numwrong
validations<-numright/numwrong
barplot(validations[,1],names.arg=1:nCluster,xlab='Group',ylab='validation')
