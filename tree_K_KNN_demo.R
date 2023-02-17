#-------------------------------TREE-----------------------------------------#
library(rpart)
library(rpart.plot)
s_iris <- sample(150,100)
help(sample)
iris_train <- iris[s_iris,]
iris_test <- iris[-s_iris,]
dim(iris_test)
dim(iris_train)
decisionTreeModel <- rpart(Species~., iris_train,method = "class")
decisionTreeModel
rpart.plot(decisionTreeModel)

#--------------------------------KNN-------------------------------------------#
library(class)
abalone <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data"), header = FALSE,sep = ",")
summary(abalone)
colnames(abalone) <- c("sex", "length", 'diameter', 'height', 'whole_weight', 'shucked_wieght', 'viscera_wieght', 'shell_weight','rings')
str(abalone)
summary(abalone$rings)

abalone$rings <- as.numeric(abalone$rings)
abalone$rings <- cut(abalone$rings, br=c(-1,8,11,35), labels = c("young", 'adult', 'old'))
abalone$rings <- as.factor(abalone$rings)
summary(abalone$rings)

aba <- abalone
aba$sex <- NULL

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

aba[1:7] <- as.data.frame(lapply(aba[1:7], normalize))
summary(aba$shucked_wieght)

ind<-sample(2, nrow(aba), replace=TRUE, prob=c(0.7, 0.3))
KNNtrain <- aba[ind==1,]
KNNtest <- aba[ind==2,]
sqrt(2918)
help(knn)
KNNpred <- knn(train = KNNtrain[1:7], test = KNNtest[1:7], cl = KNNtrain$rings, k = 55)
KNNpred
table(KNNpred)

#--------------------------------KMEAN-------------------------------------#

library(ggplot2)
head(iris)
str(iris)
summary(iris)
help(sapply)
sapply(iris[,-5],var)
summary(iris)
#Sepal.Length Vs Sepal.Width
ggplot(iris,aes(x = Sepal.Length, y = Sepal.Width, col= Species)) + geom_point()
#Petal.Length Vs Sepal.Width
ggplot(iris,aes(x = Petal.Length, y = Petal.Width, col= Species)) + geom_point()

help(kmeans)
set.seed(300)
k.max<-12
wss<- sapply(1:k.max,function(k){kmeans(iris[,3:4],k,nstart = 20,iter.max = 20)$tot.withinss})

plot(1:k.max,wss, type= "b", xlab = "Number of clusters(k)", ylab = "Within cluster sum of squares")
icluster <- kmeans(iris[,3:4],3,nstart = 20)
table(icluster$cluster,iris$Species)
