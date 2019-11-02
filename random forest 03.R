data <- read.csv("D:/latihanskoring.csv")
head(data)

#menghitung error rate
#cara 1: menggunakan OOB
#cara 2: menggunakan data test


#CARA 1:
#membuat model random forest
library(randomForest)
y = data[,8]
x = data[,-8]
model.rf <- randomForest(x=x,y=as.factor(y), ntree=500) 

#menampilkan error rate berdasarkan OOB
model.rf$err.rate[500,]


#CARA 2:
#membagi data menjadi dua bagian
#training 70%, testing 30%
set.seed(100)
library(caret)
acak <- createDataPartition(data$status, p=0.7, list=FALSE)
data.tr <- data[acak,]
data.ts <- data[-acak,]

#membuat model random forest dan memasukkan data test untuk diprediksi
library(randomForest)
y = data.tr[,8]
x = data.tr[,-8]
ytest = data.ts[,8]
xtest = data.ts[,-8]
model.rf <- randomForest(x=x,y=as.factor(y), ntree=500,
                         xtest=xtest, ytest=as.factor(ytest)) 
model.rf$test$err.rate[500,]
model.rf$test$confusion
