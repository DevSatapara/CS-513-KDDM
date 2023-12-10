# Course ID:- CS-513A
# Course Instructor:- Khashayar Dehnad
# Name:- Dev Satapara
# CWID:- 20021369
# Assignment Breif:- Using the C5.0 methodology to develop a classification model for the Diagnosis

rm(list=ls())

filename<-file.choose()

colcls=c("Sample"="character",
         "F1"="factor","F2"="factor","F3"="factor",
         "F4"="factor","F5"="factor","F6"="factor",
         "F7"="factor","F8"="factor","F9"="factor",
         "Class"="factor")

df <-  read.csv(filename,
                na.strings = "?",
                colClasses=colcls)

library('C50')

index<-sort(sample(nrow(df),round(.30*nrow(df)))) #Splitting train and test data
train_data<-df[-index,]
test_data<-df[index,]


C50_model <- C5.0( Class~.,data=train_data[,-1] )

summary(C50_model )
dev.off()
plot(C50_model) #plotting C50 decision tree
C50_predict<-predict( C50_model ,test_data , type="class" )
table(actual=test_data[,"Class"],C50=C50_predict)
false_result<- (test_data[,"Class"]!=C50_predict)
Accuracy<-sum(false_result)/length(test_data[,"Class"])
Accuracy
