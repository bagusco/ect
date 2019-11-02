data <- read.csv("D:/latihanskoring.csv")
head(data)

#membagi data menjadi dua bagian
#training 70%, testing 30%
set.seed(100)
library(caret)
acak <- createDataPartition(data$status, p=0.7, list=FALSE)
data.tr <- data[acak,]
data.ts <- data[-acak,]

#membuat model pohon klasifikasi
library(rpart)
pohon = rpart(status ~ ., data=data.tr, method="class")

#mengukur akurasi model pohon
prediksi.pohon <- predict(pohon, data.ts, type="class")
akurasi.pohon <- mean(data.ts$status == prediksi.pohon)

#membuat model bagging
library(ipred)
y = data.tr[,8]
x = data.tr[,-8]
model.bagging <- ipredbagg(as.factor(y), x, nbagg=25)
#rpart.plot(model.bagging$mtrees[[1]]$btree)
#rpart.plot(model.bagging$mtrees[[2]]$btree)

#mengukur akurasi model bagging
prediksi.bagging <- predict(model.bagging, data.ts, type="class")
akurasi.bagging <- mean(data.ts$status == prediksi.bagging)


akurasi.pohon
akurasi.bagging