setwd("C:/Users/dell/Desktop/fwdlinkformachinelearning")
temp = list.files(pattern="*.csv")
library(AUC)
list2env(
  dataset<-lapply(setNames(temp, make.names(gsub("*.csv$", "", temp))), 
         read.csv), envir = .GlobalEnv)
finaldata<-matrix(ncol=7,nrow=334)
sum1=0
sum2=0
sum3=0
sum4=0
sum5=0
sum6=0
sum7=0
sum8=0
sum9=0
sum10=0
sum11=0
sum12=0
sum13=0
sum14=0
sum=0

RULcalc <- function(holenumber,drillval){
  switch(drillval,
         "1" = 21-holenumber,
         "2" = 20-holenumber,
         "3" = 20-holenumber,
         "4" = 16-holenumber,
         "5" = 19-holenumber,
         "6" = 18-holenumber,
         "7" = 21-holenumber,
         "8" = 19-holenumber,
         "9" = 23-holenumber,
         "10" = 23-holenumber,
         "11" = 23-holenumber,
         "12" = 21-holenumber,
         "13" = 45-holenumber,
         "14" = 45-holenumber
  )
}


for(i in 1:length(dataset))
{
  variable<-as.data.frame(dataset[i])
  name<-names(dataset[i])
  pos<-regexpr('h', name)
  drillbit<-as.numeric(substr(name,2,pos-1))
  drillcharval<-as.character(drillbit)
  switch(drillcharval,
         "1" = sum1<-sum1+nrow(variable),
         "2" = sum2<-sum2+nrow(variable),
         "3" = sum3<-sum3+nrow(variable),
         "4" = sum4<-sum4+nrow(variable),
         "5" = sum5<-sum5+nrow(variable),
         "6" = sum6<-sum6+nrow(variable),
         "7" = sum7<-sum7+nrow(variable),
         "8" = sum8<-sum8+nrow(variable),
         "9" = sum9<-sum9+nrow(variable),
         "10" = sum10<-sum10+nrow(variable),
         "11" = sum11<-sum11+nrow(variable),
         "12" = sum12<-sum12+nrow(variable),
         "13" = sum13<-sum13+nrow(variable),
         "14" = sum14<-sum14+nrow(variable)
  )
}

degden_cal <- function(drillval){
  switch(drillval,
         "1" = sum1,
         "2" = sum2,
         "3" = sum3,
         "4" = sum4,
         "5" = sum5,
         "6" = sum6,
         "7" = sum7,
         "8" = sum8,
         "9" = sum9,
         "10" = sum10,
         "11" = sum11,
         "12" = sum12,
         "13" = sum13,
         "14" = sum14
  )
}

  
  
for(i in 1:length(dataset))
{
  variable<-as.data.frame(dataset[i])
  colnames(variable)<-c("Thrust","Torque")
  meanthrust<-mean(variable$Thrust)
  maxthrust<-max(variable$Thrust)
  meantor<-mean(variable$Torque)
  maxtor<-max(variable$Torque)
  deg_num<-nrow(variable)
  name<-names(dataset[i])
  pos<-regexpr('h', name)
  holenumber<-as.numeric(substr(name,pos+1,nchar(name)))
  drillbit<-as.numeric(substr(name,2,pos-1))
  drillcharval<-as.character(drillbit)
  RUL<-RULcalc(holenumber,drillcharval)
  deg_den<-degden_cal(drillcharval)
  degradation<-deg_num/deg_den
  finaldata[i, ]<-c(name,meanthrust,degradation,meantor,maxthrust,maxtor,RUL)
}

finaldata<-as.data.frame(finaldata)
colnames(finaldata)<-c("drill bit","mean Thrust","mean torque","degradation","max Thrust","max Torque","RUL")

testdata<-subset(finaldata,'d11'%in%finaldata$`drill bit`)
testdata1<-finaldata[grep("d11", finaldata$`drill bit`), ]
testdata2<-finaldata[grep("d12", finaldata$`drill bit`), ]
testdata3<-finaldata[grep("d13", finaldata$`drill bit`), ]
testdata4<-finaldata[grep("d14", finaldata$`drill bit`), ]
testdata<-rbind(testdata1,testdata2,testdata3,testdata4)
finaldata$Row=1:334
traindata=subset(finaldata,finaldata$Row<=23|finaldata$Row>=158)
traindata$Row=NULL
testdata$Row=NULL
write.csv(traindata,"traindatafinal.csv",row.names = FALSE)
write.csv(testdata,"testdatafinal.csv",row.names = FALSE)


