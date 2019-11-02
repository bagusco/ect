data <- read.csv("D:/latihanskoring.csv")
head(data)

#membuat model random forest
library(randomForest)
y = data[,8]
x = data[,-8]
model.rf <- randomForest(x=x,y=as.factor(y), ntree=50,
                         importance=TRUE) 
kepentingan <- importance(model.rf, type=1)
kepentingan
urutan <- order(kepentingan, decreasing=TRUE)
data.frame(kepentingan[urutan,])
