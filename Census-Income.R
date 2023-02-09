read.csv("C:\\Users\\SV077490\\OneDrive - Cerner Corporation\\Desktop\\D-O\\Projects-R\\Census_datacleaning\\census-income_.csv")->censusdata
head(censusdata)


library(dplyr)
library(caTools)
library(stringr)   # import the package
library(tree)
#data preprocessing

#replace missing values
censusdata[censusdata==" ?" |censusdata=="" ]<-NA
View(censusdata)
 #remove NA rows
censusdata<-na.omit(censusdata)
View(censusdata)
#remove whitespaces from data set
censusdata<-mutate_if(censusdata,is.character,str_trim)

censusdata$workclass<-as.factor(censusdata$workclass)
censusdata$X<-as.factor(censusdata$X)
str(censusdata)


#Data Manipulation

#a
censusdata$education->census_ed

View(census_ed)

as.data.frame(census_ed)->census_ed
View(census_ed)

#b
censusdata[,c(1:8)]->census_seq
View(census_seq)

#c
censusdata[,c(5,8,11)]->census_sol
View(census_sol)

#d
male_gov<-filter(censusdata,sex=="Male"& workclass=="State-gov")
View(male_gov)

#e

census_us<-filter(censusdata,age==39 & (education=="Bachelors"|native.country=="United-States"))
View(census_us)

#f

census_200<-sample_n(censusdata,200)
View(census_200)

#g

countcls<-count(censusdata,workclass)
countcls

table(censusdata$workclass)

#h

censusdata %>% group_by(workclass) %>% summarise(mean(capital.gain))


#Data Visualization

#a

library(ggplot2)
ggplot(censusdata,aes(x=relationship,fill=race))+geom_bar()+labs(x='Categories of relationship',y='Count of categories',title = 'Distribution Relationship')

#b

ggplot(censusdata,aes(x=age,fill=X))+geom_histogram(bins = 50)+labs(title="Distribution of age",fill="Yearly Income")+theme_bw()

#c

ggplot(censusdata,aes(x=capital.gain,y=hours.per.week,col=X))+geom_point(alpha=0.6,size=2)+labs(x="Capital gain",y="Hours per Week Invested",col="Yearly income")

#d

ggplot(censusdata,aes(x=education,y=age, fill=sex))+geom_boxplot()+labs(x='Education',y='Age',title ='Box-Plot of Age by Education and Sex')


# linear regression

sample.split(censusdata$education.num,SplitRatio = 0.70)->split
train <- subset(censusdata,split==TRUE)
View(train)
test<- subset(censusdata,split==FALSE)
lm(education.num~hours.per.week,data=train) -> lm


predict(lm,newdata=test) ->predicted
as.data.frame(predicted) -> predicted
View(predicted)


cbind(Actual=test$education.num,predicted=predicted)->Result
View(Result)
as.data.frame(Result)->Result
Result$Actual-Result$predicted->Result$error_pred


#RMSE
sqrt(mean((Result$error_pred)^2))





#logistic regression
censusdata$occupation <- as.factor(censusdata$occupation)
sample.split(censusdata$X,SplitRatio = 0.65)->split
train <- subset(censusdata,split==TRUE)
View(train)
test<- subset(censusdata,split==FALSE)
glm(X~occupation,data=train,family="binomial") -> glmod
predict(glmod,newdata=test,type="response")->result
range(result,na.rm = TRUE)


summary(glmod)
library(pROC)
library(ROCR)
prediction(result,test$X) ->predict_log
performance(predict_log,"acc")->acc
plot(acc)
table(test$X,result>0.3)

accuracy3 <- (6290+1389)/nrow(test)
accuracy3

roc_object <- roc( test$X, result)
auc( roc_object )

#2 -logistic regression multiple
sample.split(censusdata$X,SplitRatio = 0.80)->split
train <- subset(censusdata,split==TRUE)
View(train)
test<- subset(censusdata,split==FALSE)
glm(X~age+workclass+education,data=train,family="binomial") -> glmod
predict(glmod,newdata=test,type="response")->result
range(result,na.rm = TRUE)


summary(glmod)
library(pROC)
library(ROCR)
prediction(result,test$X) ->predict_log
performance(predict_log,"acc")->acc
plot(acc)
table(test$X,result>0.35)

accuracy3 <- (3842+772)/nrow(test)
accuracy3

roc_object <- roc( test$X, result)
auc( roc_object )



#decision tree
censusdata$X <- as.factor(censusdata$X)
sample.split(censusdata$education.num,SplitRatio = 0.70)->split
train <- subset(censusdata,split==TRUE)
View(train)
test<- subset(censusdata,split==FALSE)
tree(X~.,data=train)->dec_mod
plot(dec_mod)

text(dec_mod,cex=0.5)
summary(dec_mod)

predict(dec_mod,newdata=test,type="class")->result
head(result,10)
table(test$X,result)
nrow(test)
accuracy = (6776 + 502)/9048
accuracy


#Random forest
library(randomForest)
sample.split(censusdata$X,SplitRatio = 0.80)->split
train <- subset(censusdata,split==TRUE)

test<- subset(censusdata,split==FALSE)
randomForest(X~.,data=train,mtry=3,ntree=300) ->rf
importance(rf)
varImpPlot(rf)
predict(rf,newdata = test,type="class") -> result
table(test$X,result)
acc=(4263+956)/nrow(test)
#86%