#Used package neuralnet for implementing Neural Networks
library(neuralnet)
#Read Training and Testing Dataset from the directory
testdata<-read.csv('testdatafinal.csv')
traindata<-read.csv('traindatafinal.csv')
#Excluded drill bit column from the datasets beacausse its just an identification
traindata<-traindata[-c(1)]
testdata<-testdata[-c(1)]
#Calculated maximum and minimum feature in the Column 2 of the Training Set
maxs <- apply(traindata, 2, max) 
mins <- apply(traindata, 2, min)
#From all data points mean of all data points was subtracted
scaled <- as.data.frame(scale(traindata, center = mins, scale = maxs - mins))
train_ <- scaled
#Calculated maximum and minimum feature in the Column 2 of the Testing Set
maxs <- apply(testdata, 2, max) 
mins <- apply(testdata, 2, min)
#From all data points mean of all data points was subtracted
scaled <- as.data.frame(scale(testdata, center = mins, scale = maxs - mins))
test_ <- scaled
n <- names(train_)
f <- as.formula(paste("RUL ~", paste(n[!n %in% "RUL"], collapse = " + ")))
#Implemented Neural Networks
nn <- neuralnet(f,data=train_,hidden=c(5,3),linear.output=F)
plot(nn)
pr.nn <- compute(nn,test_[,1:9])
pr.nn_ <- pr.nn$net.result*(max(traindata$RUL)-min(traindata$RUL))+min(traindata$RUL)
test.r <- (test_$RUL)*(max(traindata$RUL)-min(traindata$RUL))+min(traindata$RUL)
#Calculated RMSE
RMSE.nn <- sqrt(sum((test.r - pr.nn_)^2)/nrow(test_))
print(paste(RMSE.nn))
predictions <- cbind(testdata$RUL,pr.nn_)
predictions_last5 <- rbind(predictions[19:23,],predictions[40:44,],predictions[85:89,],predictions[129:133,])

write.csv(predictions_last5,"last5holes(neural_network).csv")

d11 = 0
for (i in 1:5){
  a = (predictions_last5[i,1] - predictions_last5[i,2])*(predictions_last5[i,1] - predictions_last5[i,2])
  d11 = a + d11
}
d11rms = sqrt(d11/5)
d12 = 0
for (i in 6:10){
  a = (predictions_last5[i,1] - predictions_last5[i,2])*(predictions_last5[i,1] - predictions_last5[i,2])
  d12 = a + d12
}
d12rms = sqrt(d12/5)
d13 = 0
for (i in 11:15){
  a = (predictions_last5[i,1] - predictions_last5[i,2])*(predictions_last5[i,1] - predictions_last5[i,2])
  d13 = a + d13
}
d13rms = sqrt(d13/5)
d14 = 0
for (i in 16:20){
  a = (predictions_last5[i,1] - predictions_last5[i,2])*(predictions_last5[i,1] - predictions_last5[i,2])
  d14 = a + d14
}
d14rms = sqrt(d14/5)
rms <- cbind(d11rms,d12rms,d13rms,d14rms)
median(rms)
write.csv(predictions_last5,"last5holes.csv")
