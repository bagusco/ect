#membangkitkan data
#n=30
#5 prediktor
set.seed(1000)
sigma = matrix(0.95, 5, 5)
diag(sigma) = c(1,1,1,1,1)
mean = c(0,0,0,0,0)
library(MASS)
data = data.frame(mvrnorm(30, mean, sigma))
colnames(data)=c("x1", "x2", "x3", "x4", "x5")
peluang = ifelse(data$x1<0.5, 0.2, 0.8)
data$class <- rbinom(30, 1, peluang)

#bentuk pohon data asli
library(rpart)
pohon = rpart(class ~ . , data=data, method="class",
              control=rpart.control(cp=0, minsplit=3))
library(rpart.plot)
rpart.plot(pohon)

#bentuk pohon data sampel pertama
acak = sample(1:30, 25)
sampel = data[acak,]
pohon.a = rpart(class ~ . , data=sampel, method="class",
              control=rpart.control(cp=0, minsplit=3))
rpart.plot(pohon.a)

#bentuk pohon data sampel kedua
acak = sample(1:30, 25)
sampel = data[acak,]
pohon.b = rpart(class ~ . , data=sampel, method="class",
                control=rpart.control(cp=0, minsplit=3))
rpart.plot(pohon.b)
