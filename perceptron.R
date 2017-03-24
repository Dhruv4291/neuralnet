#ibrary package for implementing Perceptron 
library(monmlp)
## importing trainingdata
traindata <- read.csv("traindatafinal.csv")
## importing testdata
testdata <- read.csv("testdatafinal.csv")
## removing the not required columns
traindata1 <- traindata[-c(1,11)]
testdata1 <- testdata[-c(1,11)]
## making the label dataset for training data
labels <- traindata[c(11)]
## making the label dataset for testdata to find rmse
labelstest <- c(22:0,20:0,44:0,43:0)
## making matrix so as to met the requirements of the function
x <- cbind(traindata1$mean.Thrust,traindata1$mean.torque,traindata1$thrust_non_zero,traindata1$Torque_non_zero,traindata1$degradation,traindata1$Torque_zero,traindata$Area,traindata$max.Thrust,traindata1$max.Torque)
y <- cbind(labels$RUL)
y2 <- cbind(labelstest)
x1 <- cbind(testdata1$mean.Thrust,testdata1$mean.torque,testdata1$thrust_non_zero,testdata1$Torque_non_zero,testdata1$degradation,testdata1$Torque_zero,testdata1$Area,testdata1$max.Thrust,testdata1$max.Torque)

## Applying the model
r <- monmlp.fit(x, y, hidden1=1, n.ensemble=11, monotone=1, bag=TRUE)
z <- monmlp.predict(x = x1, weights = r)

## plotting the predictions
plot(z,y2)
## calculating the rmse of the whole testdata
s=0
for(i in 1:133)
{
  a=(z[i]-y2[i])*(z[i]-y2[i])
  s=s+a
}
sqrt(s/134)


##Calculating the rmse of the last 5 holes.
sf=0
for(i in 19:23)
{
  a=(z[i]-y2[i])*(z[i]-y2[i])
  sf=sf+a
}
for(i in 40:44)
{
  a=(z[i]-y2[i])*(z[i]-y2[i])
  sf=sf+a
}
for(i in 85:89)
{
  a=(z[i]-y2[i])*(z[i]-y2[i])
  sf=sf+a
}
for(i in 129:133)
{
  a=(z[i]-y2[i])*(z[i]-y2[i])
  sf=sf+a
}
sqrt(sf/20)

predictions <- cbind(z,y2)

predictions_last5 <- rbind(predictions[19:23,],predictions[40:44,],predictions[85:89,],predictions[129:133,])
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
