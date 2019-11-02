#membaca data
cnth<-read.csv("D:/bank-additional-full.csv",sep=";")
library(caret)
library(rpart)
library(ebmc)

nrow(cnth) #melihat banyaknya observasi

#melihat distribusi kelas variabel target
table(cnth$y)
prop.table(table(cnth$y))


#mempartisi data
set.seed(100)
acak <- createDataPartition(cnth$y, p=0.7, list=FALSE)

c.trn <- cnth[acak,]  #membuat data training
c.tst <- cnth[-acak,]  #membuat data testing

#pemodelan TREE pada data training
#tidak ada perlakuan apa-apa terhadap data training
pohon <- rpart(y~., data=c.trn, method="class")

#memprediksi data testing
pred.peluang <- predict(pohon, c.tst)[,2]
pred.pohon <-as.factor(ifelse(pred.peluang < 0.5,"no","yes"))

#mengevaluasi ketepatan prediksi dari model
eval.pohon <-confusionMatrix(pred.pohon, c.tst$y, positive="yes")

eval.pohon

c.trn$y <- factor(c.trn$y, levels = c("no", "yes"), labels = c("0", "1"))

rus.boost <- rus(y ~ ., data = c.trn, size = 10, 
                     alg = "c50", ir = 1)

peluang.rusboost <-predict(rus.boost, newdata=c.tst, type="prob")
pred.rusboost <-as.factor(ifelse(peluang.rusboost<0.5,"no","yes"))
eval.rusboost<-confusionMatrix(pred.rusboost, c.tst$y, positive="yes")
eval.rusboost

