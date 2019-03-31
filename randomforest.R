#Random forest on breast cancer data
cancer_data<- read.csv(file.choose(),header = TRUE)
#Dividing the data into test data and training data
pd<-sample(2,nrow(cancer_data),replace = TRUE,prob = c(0.8,0.2))
cancer_train = cancer_data[pd==1,-1]
cancer_test = cancer_data[pd==2,-1]
cancer_test2 = cancer_test
library(randomForest)
n_tree = seq(from = 100,to=500,by=50)
mtry = seq(4,to=32,by=2)
accuracy=c()
num_tree = c()
m_try = c()
for(i in n_tree){
  for(j in mtry){
    target = diagnosis~.
    model <- randomForest(target,data=cancer_train,ntree = i,mtry = 4)

    
    result<-predict(model,cancer_test)

    pred=0
    for(i in 1:nrow(cancer_test)){
      if(cancer_test[i,length(cancer_test)]==result[i]){
        pred = pred + 1
      }
    }
    accuracy = c(accuracy,(pred/nrow(cancer_test))*100)
    m_try = c(m_try,j)
    num_tree = c(num_tree,i)
  }
}

final<-data.frame(num_tree,m_try,accuracy)

f<-max(final[,3])
