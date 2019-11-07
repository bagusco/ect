#membaca data dan membuang kolom pertama
#kolom pertama hanya berisi nomor urut
data <- read.csv("D:/tree03.csv")
data <- data[,-1]

#membagi data menjadi dua bagian
#training 70%, testing 30%
#set.seed(100)
library(caret)
acak <- createDataPartition(data$Tertarik.Beli., p=0.7, list=FALSE)
data.tr <- data[acak,]
data.ts <- data[-acak,]

#model pohon klasifikasi tunggal
pohon <- rpart(Tertarik.Beli. ~ ., data=data.tr,
               method="class",
               control=rpart.control(cp=0, minsplit=20))
#model bagging
y = data.tr[,8]
x = data.tr[,-8]
model.bagging <- ipredbagg(as.factor(y), x, nbagg=50)

#model random forest
model.rf <- randomForest(x=x,y=as.factor(y), ntree=1000) 


#prediksi dan akurasi TESTING dari ketiga model
prediksi.pohon <- predict(pohon, data.ts, type="class")
akurasi.pohon <- mean(data.ts$Tertarik.Beli. == prediksi.pohon)

prediksi.bagging <- predict(model.bagging, data.ts, type="class")
akurasi.bagging <- mean(data.ts$Tertarik.Beli. == prediksi.bagging)

prediksi.rf <- predict(model.rf, data.ts, type="class")
akurasi.rf <- mean(data.ts$Tertarik.Beli. == prediksi.rf)

#perbandingan akurasi ketiga-nya
cbind(akurasi.pohon, akurasi.bagging, akurasi.rf)


#variable importance
model.rf <- randomForest(x=x,y=as.factor(y), ntree=1000,
                         importance=TRUE) 
kepentingan <- importance(model.rf, type=1)
kepentingan
urutan <- order(kepentingan, decreasing=TRUE)
data.frame(kepentingan[urutan,])
