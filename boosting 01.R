data <- read.csv("D:/latihanskoring.csv")
library(caret)
library(JOUSBoost)

x.kategorik <- data[,c(3,4,6,7)]
x.numerik <- data[,c(1,2,5)]
dummies = predict(dummyVars(~ jeniskelamin + marital_status + 
                              pendidikan+ pekerjaan, data=x.kategorik), x.kategorik)
x <- cbind(x.numerik, dummies)

y <- data[,8]

#membagi data menjadi dua bagian
#training 70%, testing 30%
set.seed(100)
acak <- createDataPartition(data$status, p=0.7, list=FALSE)
x.tr <- x[acak,]
x.ts <- x[-acak,]
y.tr <- y[acak]
y.ts <- y[-acak]

#menjalankan algoritma adaboost
#catatan: y bernilai -1 dan 1
y.tr <- 2*y.tr - 1
model.boost <- adaboost(as.matrix(x.tr), y.tr,
               n_rounds = 200)

#menilai akurasi model
prediksi.boost = predict(model.boost, x.ts)
prediksi.boost = ifelse(prediksi.boost == 1, 1, 0)
akurasi.boost = mean(y.ts == prediksi.boost)
akurasi.boost
