data <- read.csv("D:/latihanskoring.csv")
akurasi.pohon <- NULL
akurasi.bagging <- NULL
akurasi.rf <- NULL
for (ulangan in 1:100){
  
  acak <- createDataPartition(data$status, p=0.7, list=FALSE)
  data.tr <- data[acak,]
  data.ts <- data[-acak,]
  
  #membuat model pohon klasifikasi
  pohon = rpart(status ~ ., data=data.tr, method="class")
  
  #mengukur akurasi model pohon
  prediksi.pohon <- predict(pohon, data.ts, type="class")
  akurasi.pohon[ulangan] <- mean(data.ts$status == prediksi.pohon)
  
  #membuat model bagging
  y = data.tr[,8]
  x = data.tr[,-8]
  model.bagging <- ipredbagg(as.factor(y), x, nbagg=25)
  
  #mengukur akurasi model bagging
  prediksi.bagging <- predict(model.bagging, data.ts, type="class")
  akurasi.bagging[ulangan] <- mean(data.ts$status == prediksi.bagging)
  
  #membuat model random forest
  model.rf <- randomForest(x=x,y=as.factor(y), ntree=500) 
  
  #mengukur akurasi model pohon
  prediksi.rf <- predict(model.rf, data.ts, type="class")
  akurasi.rf[ulangan] <- mean(data.ts$status == prediksi.rf)
  
  }


minimum <- min(akurasi.pohon, akurasi.bagging, akurasi.rf)
maksimum <- max(akurasi.pohon, akurasi.bagging, akurasi.rf)
plot(1:100, akurasi.pohon, type="b", ylim=c(minimum, maksimum))
points(1:100, akurasi.bagging, type="b", col="red")
points(1:100, akurasi.rf, type="b", col="blue")

boxplot(cbind(akurasi.pohon, akurasi.bagging, akurasi.rf))
