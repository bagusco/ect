data <- read.csv("D:/latihanskoring.csv")
akurasi.pohon <- NULL
akurasi.bagging <- NULL
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
}


minimum <- min(akurasi.pohon, akurasi.bagging)
maksimum <- max(akurasi.pohon, akurasi.bagging)
plot(1:100, akurasi.pohon, type="b", ylim=c(minimum, maksimum))
points(1:100, akurasi.bagging, type="b", col="red")

plot(akurasi.pohon, akurasi.bagging, pch=19, cex=2, 
     xlim=c(minimum,maksimum), ylim=c(minimum,maksimum))
abline(0,1, col="red")
