data <- read.csv("D:/latihanskoring.csv")
head(data)

#membagi data menjadi dua bagian
#training 70%, testing 30%
set.seed(100)
library(caret)
acak <- createDataPartition(data$status, p=0.7, list=FALSE)
data.tr <- data[acak,]
data.ts <- data[-acak,]

#membuat model random forest
library(randomForest)
y = data.tr[,8]
x = data.tr[,-8]
model.rf <- randomForest(x=x,y=as.factor(y), ntree=500) 

#mengukur akurasi model pohon
prediksi.rf <- predict(model.rf, data.ts, type="class")
akurasi.rf <- mean(data.ts$status == prediksi.rf)

