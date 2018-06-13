setwd()
#q1
read_digit<-function(n = 'train'){
  df<-read.table(paste(sep=".",n,"txt"))
  df[ ,1]<-factor(df[ ,1])#Change the class of the first column to factor.
  df
}
train <- read_digit('train')
test <- read_digit('test')
#q2
view_digit=function(n = 'train',d){
  df<-read_digit(n)
  df<-df[ ,-1]#Drop the first column with are the labels for every observation.
  df1<-as.vector(as.numeric(df[d, ]))
  df1<-matrix(df1,16,16)#Convert the data frame to matrix for image.
  image(df1)
}
view_digit('train', 7)

#q3
#a
par(mfrow=c(2,5))
data1<-train
colnames(data1)<-c('label',as.character(1:(ncol(data1)-1)))
data2<-split(data1,data1$label)#Split the data according to label.
label_mean<-data.frame()
for(i in 1:10){#Calculate the mean pixels for every class and image them.
  df<-as.data.frame(data2[i])
  df_mean<-apply(df[ ,-1],2,mean)
  label_mean<-rbind(label_mean,df_mean)
  df_mean<-matrix(df_mean,16,16)
  image(df_mean)
}
#b
names(label_mean)<-as.character(c(1:(ncol(data1)-1)))
variance_pixel<-apply(label_mean,2,var)#Calculate the variance for every mean of pixel for every class.
variance_max<-which.max(variance_pixel)
variance_max
variance_min<-which.min(variance_pixel)
variance_min
par(mfrow=c(2,1))
boxplot(data1$`230`~data1$label,outline=FALSE,col=c("blue","black","grey","yellow","purple","green","red","pink","purple","light blue"),main="Distribution of #230 pixel by class",col.main="red",font.main=4)
boxplot(data1$`1`~data1$label,outline=FALSE,col=c("blue","black","grey","yellow","purple","green","red","pink","purple","light blue"),main="Distribution of #1 pixel by class",col.main="red",font.main=4)

#q4
##index.t represent the number of the row in test data, train_sample represents the sample volume
##the users want to use in function
predict_knn<-function(test_data=train[c(3:6,18:87),-1],train_sample = 1000,dist_method = "euclidean",k = 10){
  prediction <- test_data#select the predict point
  length_test<-nrow(test_data)
  train <- read_digit('train')
  length_train<-nrow(train)
  train0 <- train[sample(1:length_train, train_sample), ]#select the training data
  df_train<-train0[ ,-1]
  colnames(df_train)<-as.character(c(1:ncol(df_train)))
  colnames(prediction)<-as.character(c(1:ncol(prediction)))
  df_matrix <- rbind(prediction,df_train)#combine the train data and the test data
  df_dist <-dist(df_matrix,method=dist_method)#calculate the distance between each row
  dist_mat <- as.matrix(df_dist)
  dist_mat <- dist_mat[-c(1:length_test), c(1:length_test)]#extract the dist between test points and train points
  dist_mat<-cbind(dist_mat,train0[ ,1])
  dist_mat<-as.data.frame(dist_mat)
  poi <- c()#create a vector to store the nearest class
  #find the nearest possible class
  for (i in 1:length_test){
    df1<-dist_mat[order(dist_mat[ ,i])[1:k],length(dist_mat[1, ])]
    poi[i] <- names(sort(table(df1),decreasing = T))[1]
  }
  poi
}
predict_knn()


#q5
df_train<-train[,-1]
eu_distance<-dist(df_train,method = 'euclidean')#Calculate the distance for the whole data with the method euclidean.
eu_distance<-as.matrix(eu_distance)
ma_distance<-dist(df_train,method = 'manhattan')#Calculate the distance for the whole data with the method manhattan.
ma_distance<-as.matrix(ma_distance)
cv_error_knn<-function(k=10,dist_method = 'euclidean'){
  if(dist_method=='euclidean')#Select the distance accrording to methods.
    distance<-eu_distance
  if(dist_method=='manhattan')
    distance<-ma_distance
  distance1<-cbind(distance,1:nrow(df_train))
  train1<-train
  train1$class<-rep_len(1:10,nrow(train1))
  error_rate=c()
  for(i in 1:10){
    df_seq<-seq(i,nrow(train1),by=10)#Devide the data into 10 groups
    df_dist<-distance1[-df_seq,c(df_seq,length(distance1[1,]))]
    #Get the useful distances
    label1<-c()
    for (j in 1:length(df_seq)){#Find the predicted label accoding to knn.
      df1<-train$V1[df_dist[,length(df_dist[1,])][order(df_dist[,j])][1:k]]   
      df2<-sort(table(df1),decreasing = T)[1]  
      label1[j]<-names(df2)
    }
    error_rate[i]<-1-sum(label1==as.character(train1$V1[which(train1$class==i)]))/length(df_seq)
    #Calculate the error rate.
  }
  mean(error_rate)
}
cv_error_knn()

#q6
method<-c('euclidean','manhattan')
error<-data.frame(c(1:15),c(1:15))
for(i in 1:2){#Get the mean error rates for k=1,2.....15 with two methods.
  error1<-c()
  for(j in 1:15){
    error1[j]<-cv_error_knn(j,method[i])
  }
  error[ ,i]<-error1
}
names(error)<-c("euclidean","manhattan")
error_range<-range(error)
par(mfrow=c(1,1))
plot(as.vector(error$manhattan),pch=22,type="o",lty=1,col="red",ylab="error rate",xlab="k",ylim=error_range,xaxt='n')
lines(as.vector(error$euclidean),type="o",pch=21,lty=1,col="blue")
axis(1,at=1:15,labels = as.character(c(1:15)))
legend("topleft",c("euclidean","manhattan"),cex=0.6,col=c("blue","red"),lty=1,pch=c(21,22))
title(main="CV error rate for k",col.main="red",font.main=4)

#q7
distance<-eu_distance
distance1<-cbind(distance,1:nrow(train))
train1<-train
train1$class<-rep_len(1:10,nrow(train1))
train1$predict <- c(rep(12, times=nrow(train1)))

predict_label<-c()
cl_error_knn <- function(k=1){#Get the predicted label for every observation.
        for(i in 1:10){
                df_seq<-seq(i,nrow(train1),by=10)
                df_dist<-distance1[-df_seq,c(df_seq,length(distance1[i,]))]
                label1<-c()
                for (j in 1:length(df_seq)){
                        df1<-train$V1[df_dist[,length(df_dist[1,])][order(df_dist[,j])][1:k]]   
                        df2<-sort(table(df1),decreasing = T)[1]  
                        label1[j]<-names(df2)
                 }
                predict_label[df_seq]=label1     
        }
  predict_label
}
cm_error_knn<-function(k=1){#Calculate the number of every actual label matched with every predicted label.        
        train1$predict<-cl_error_knn(k)
        train2 <- train1[, c(1, ncol(train1))]
        con_mat <- data.frame()
        cf_mat <- c(0:9)
        cf_mat = as.data.frame(cf_mat)
        colnames(cf_mat) <- 'df_sub'
        for (i in 0:9){
                df_sub <- train2[which(train2$V1 == i), 2]
                pred <- as.data.frame(table(df_sub))
                cf_mat <- merge(cf_mat, pred, by = 'df_sub', all.x = T)
          
            }
        cf_mat1 <- cf_mat
        cf_mat1[is.na(cf_mat)] <- 0
        cf_mat1 <- cf_mat1[, -1]
        colnames(cf_mat1) <- as.character(c(0:9))
        rownames(cf_mat1) <- as.character(c(0:9))
        cf_mat1
}
cm_error_knn()

x <- as.matrix(c(1, 3, 4))
apply(x, 1, cm_error_knn)

#q8
train3<-train
train3$predict<-cl_error_knn(1)
classified<-which(train3$V1==train3$predict)#Match the predicted lable with actual label.
train4<-train3[-c(classified), ]#Drop the data which autual label is equal to predicted label.
mis_num<-length(train4[ ,1])
mis_label<-train4$V1
mis_label<-table(mis_label)
total_num<-tapply(as.character(rep_len(0:9,nrow(train3))),train$V1,length)#Calculate the misclassified rate.
mis_per<-as.data.frame(as.vector(mis_label)/as.vector(total_num))
rownames(mis_per)<-as.character(c(0:9))
colnames(mis_per)<-c('misclassified rate')
plot(as.vector(mis_per$`misclassified rate`),type = "o",lty=1,pch=21,main="Misclassified rate for every label",col.main="red",font.main=4,xlab="label",ylab="misclassified rate",col="blue",xaxt='n')
axis(1,at=1:10,labels = as.character(c(0:9)))

#q9
train_test <- rbind(test, train)
train_test1 <- train_test[, -1]
eu_dist_traintest <- dist(train_test1, method = 'euclidean')
ma_dist_traintest <- dist(train_test1, method = 'manhattan')
length_test<-nrow(test)
#mi_dist_traintest <- dist(train_test1, method = 'minkowski')
test_knn <- function(dist_method = 'euclidean',k = 10){
  if(dist_method=='euclidean')
    distance=eu_dist_traintest
  if(dist_method=='manhattan')
    distance=ma_dist_traintest
  #if(dist_method=='minkowski')
  #distance=mi_dist_traintest
  dist_mat <- as.matrix(distance)
  dist_mat <- cbind(dist_mat, c(1:nrow(test), 1:nrow(train)))
  dist_mat <- dist_mat[-c(1:length_test), c(1:length_test,length(dist_mat[1, ]))]#extract the dist between test points and train points
  poi <- c()#create a vector to store the nearest class
  #find the nearest possible class
  for (i in 1:length_test){
    df1 <- train$V1[order(dist_mat[, i])][1:k]
    poi[i] <- names(which.max(table(df1)))
  }
  err_rate <- 1 - (sum(poi == as.character(test$V1)))/nrow(test)
}

method<-c('euclidean','manhattan')
error2<-data.frame(c(1:15),c(1:15))
for(i in 1:2){
  error1<-c()
  for(j in 1:15){
    error1[j]<-test_knn(dist_method = method[i], k = j)
  }
  error2[ ,i]<-error1
}
total_error<-cbind(error,error2)
names(total_error)<-c("CV-euclidean","CV-manhattan","test-euclidean","test-manhattan")
error_range<-range(total_error)
plot(as.vector(total_error$`test-manhattan`),pch=22,type="o",lty=1,col="red",ylab="error rate",xlab="k",
     ylim=error_range,xaxt='n')
lines(as.vector(total_error$`test-euclidean`),type="o",pch=21,lty=1,col="blue")
lines(as.vector(total_error$`CV-euclidean`),type="o",pch=23,lty=2,col="green")
lines(as.vector(total_error$`CV-manhattan`),pch=24,type="o",lty=2,col="yellow")
axis(1,at=1:15,labels = as.character(c(1:15)))
legend("topleft",c("test-euclidean","test-manhattan","CV-euclidean","CV-manhattan"),cex=0.5,col=c("blue","red","green","yellow"),lty=c(1,1,2,2),pch=c(21,22,23,24))
title(main="Error rate for k",col.main="red",font.main=4)




