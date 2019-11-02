data <- read.csv("D:/latihanskoring.csv")
x.kategorik <- data[,c(3,4,6,7)]
x.numerik <- data[,c(1,2,5)]
dummies = predict(dummyVars(~ jeniskelamin + marital_status + 
                              pendidikan+ pekerjaan, data=x.kategorik), x.kategorik)
x <- cbind(x.numerik, dummies)

y <- data[,8]


akurasi.pohon <- NULL
akurasi.boosting <- NULL
for (ulangan in 1:100){
  
  acak <- createDataPartition(data$status, p=0.7, list=FALSE)
  x.tr <- x[acak,]
  x.ts <- x[-acak,]
  y.tr <- y[acak]
  y.ts <- y[-acak]
  
  #membuat model pohon klasifikasi
  data.tr = cbind(y.tr, x.tr)
  pohon = rpart(y.tr ~ ., data=data.tr, method="class")
  
  #mengukur akurasi model pohon
  prediksi.pohon <- predict(pohon, x.ts, type="class")
  akurasi.pohon[ulangan] <- mean(y.ts == prediksi.pohon)
  
  
  #membuat model boosting
  y.tr <- 2*y.tr - 1
  model.boost <- adaboost(as.matrix(x.tr), y.tr,
                          n_rounds = 200)
  
  #menilai akurasi model boosting
  prediksi.boost = predict(model.boost, x.ts)
  prediksi.boost = ifelse(prediksi.boost == 1, 1, 0)
  akurasi.boosting[ulangan] = mean(y.ts == prediksi.boost)
  
}


minimum <- min(akurasi.pohon, akurasi.boosting)
maksimum <- max(akurasi.pohon, akurasi.boosting)
plot(1:100, akurasi.pohon, type="b", ylim=c(minimum, maksimum))
points(1:100, akurasi.boosting, type="b", col="red")

boxplot(cbind(akurasi.pohon, akurasi.boosting))
        