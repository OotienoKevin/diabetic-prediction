#we are dealing with a data set diabet,i got it from kaggle ,sprcial thanks to the kaggle family
#our aim is building a predictive model that predicts the type of diabetes given some specific variables
#the two types of diabetes we are trying to predict are 1.inspidus 2.melitus
library(caTools)
diabet<-read.csv(file.choose())# upload the dataset
head(diabet)#view sample of the data
#we are already dealing with a clean dataset
#divide the dataset into training data and test data,for training model and testing model
##some data visualization
png(file="blood sugar.png")
boxplot(diabet$blood.sugar~diabet$Sex,data=diabet)##shows that female have the highest amount of blood sugar level ,
dev.off()
png(file="higestbloodugarlevel.png")
boxplot(diabet$blood.sugar~diabet$type.of.diabetes)##this boxplot shows that the number of people havin diabetes inpidu and melitus are almost the same but we should confirm by finding the mean
abline(h=mean(diabet$blood.sugar))
dev.off()
mean(diabet$blood.sugar[diabet$type.of.diabetes=="1"])
mean(diabet$blood.sugar[diabet$type.of.diabetes=="2"])#those with diabetes melitus have higher bloog sugar level
split<-sample.split(diabet,SplitRatio = 0.8)
train<-subset(diabet,split==TRUE)
test<-subset(diabet,split==FALSE)
#lets build our predictive model using random forest
library(randomForest)
train$type.of.diabetes<-as.factor(train$type.of.diabetes)
train$Sex<-as.factor(train$Sex)
train$Family.history<-as.factor(train$Family.history)
train$Complications.of.diabetes<-as.character(train$Complications.of.diabetes)
model<-randomForest(train$type.of.diabetes~.,data=train)
model ##we got an OOB  estimate of error to be 20.97%% meaning our model is roughly 80% accurate ,which is a good model
summary(model)
#now lets check the predictive capability of our model using the test data
test$type.of.diabetes<-as.factor(test$type.of.diabetes)
test$Sex<-as.factor(test$Sex)
test$Family.history<-as.factor(test$Family.history)
test$Complications.of.diabetes<-as.character(test$Complications.of.diabetes)
predicted<-predict(model,newdata = test)
predicted
head(test)
compare<-data.frame(test$type.of.diabetes,predicted)
compare##construct a table of predicted values and actual values
length(test$type.of.diabetes)
