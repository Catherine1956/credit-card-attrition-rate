rm(list=ls())

library(dplyr) 
library(ggplot2)
df <- read.csv("BankChurners.csv", stringsAsFactors = FALSE)
a <- df[,c(1:21)]

#changing variables  to factor
a$Attrition_Flag <- as.factor(a$Attrition_Flag)
a$Gender <- as.factor(a$Gender)
a$Education_Level <- as.factor(a$Education_Level)
a$Marital_Status<- as.factor(a$Marital_Status)
a$Income_Category <- as.factor(a$Income_Category)
a$Card_Category <- as.factor(a$Card_Category)

##### EDA ######

#Pie- Customer status 
n=nrow(a)
count<-table(a$Attrition_Flag);
piepercent<-round(count/n*100, 1);
pie(piepercent,
    labels=paste(piepercent,"%",sep=""),
    main="Customer Status",
    col = c("Yellow","Orange"))
legend("topleft",
       c('Attrited Customer','Existing Customer'),
       cex=0.8,
       fill=c("Yellow","Orange")
)

#Total_Trans_Ct 
ggplot(a, aes(Total_Trans_Ct, fill = Attrition_Flag ))+
  geom_density()


#Customer_Age   
ggplot(a, aes(x=Attrition_Flag, y=Customer_Age, fill=Attrition_Flag)) + 
  geom_boxplot(alpha=0.3) 


# Customer Status on Gender
a %>% count(Attrition_Flag, Gender) %>% 
  group_by(Attrition_Flag) %>% 
  mutate(pct=n/sum(n))
mosaicplot(Attrition_Flag~Gender, data=a, color=TRUE)
a %>% 
  ggplot(aes(x=factor(Attrition_Flag),
             y=prop.table(stat(count)),
             fill = factor(Gender),
             label = scales::percent(prop.table(stat(count))))) +
  geom_bar(position = 'dodge') +
  geom_text(stat = 'count',
            position = position_dodge(.9),
            vjust = -0.5,
            size = 3) +
  scale_y_continuous(labels = scales::percent) +
  labs(x= 'Attrition_Flag', y='pct', fill = 'Gender')

#Cutomer status on Marital Status 
ggplot(a, aes(x=Marital_Status))+
  geom_bar(aes(fill =Attrition_Flag),position = "dodge")+
  labs(title="Customer status on Marital Status ", x="Count", y="Marital Status")+
  scale_fill_brewer(palette="Blues")

#Cutomer status on Income 
a$Income_Category= factor(a$Income_Category, ordered=TRUE, levels=c("$120K +", "$80K - $120K", "$60K - $80K", "$40K - $60K", "Less than $40K", "Unknown"))
ggplot(a , aes(y = Income_Category))+
  geom_bar(aes(fill = Attrition_Flag),position='dodge')+
  labs(title="Customer status on Income", x="Count", y="Income_Category")

#Cutomer status on Education level
a$Education_Level= factor(a$Education_Level, ordered=TRUE, levels=c("Uneducated","High School","College","Graduate","Post-Graduate","Doctorate", "Unknown"))
ggplot(a , aes(y = Education_Level))+
  geom_bar(aes(fill = Attrition_Flag),position='dodge')+
  labs(title="Customer status on Education level", x="Count", y="Education level")

d=table(a$Education_Level)
d=as.data.frame(d)
x = d$Freq
piepercent<- round(100*x/sum(x), 1)

pie(x, labels = piepercent, main = "Existing People Education Level",col = terrain.colors(length(x)))
legend("topright", c("College","Doctorate","Graduate","High School","Post-Graduate","	Uneducated","Unkown"),
       fill = terrain.colors(length(x)))


# data cleaning
a %>% filter(duplicated(.))

#checking for null values
sapply(a, function(x) sum(is.na(x)))

##Months_on_book Outlier Removal
boxplot(a$Months_on_book,
        ylab = "Months_on_book",
        main = "Boxplot of Months_on_book."
)
summary(a$Months_on_book)
x <- a$Months_on_book
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
a$Months_on_book <- x

##Total_Amt_Chng_Q4_Q1 Outlier Removal
boxplot(a$Total_Amt_Chng_Q4_Q1,
        ylab = "Total_Amt_Chng_Q4_Q1",
        main = "Boxplot of Total_Amt_Chng_Q4_Q1"
)

summary(a$Total_Amt_Chng_Q4_Q1)
x <- a$Total_Amt_Chng_Q4_Q1
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
a$Total_Amt_Chng_Q4_Q1 <- x


##Total_Trans_Amt Outlier Removal
boxplot(a$Total_Trans_Amt,
        ylab = "Total_Trans_Amt",
        main = "Boxplot of Total_Trans_Amt"
)
summary(a$Total_Trans_Amt)
x <- a$Total_Trans_Amt
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
a$Total_Trans_Amt <- x


##Total_Ct_Chng_Q4_Q1 Outlier Removal
boxplot(a$Total_Ct_Chng_Q4_Q1,
        ylab = "Total_Ct_Chng_Q4_Q1",
        main = "Boxplot of Total_Ct_Chng_Q4_Q1"
)
summary(a$Total_Ct_Chng_Q4_Q1)
x <- a$Total_Ct_Chng_Q4_Q1
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
a$Total_Ct_Chng_Q4_Q1 <- x


##Total_Trans_Ct Outlier Removal
boxplot(a$Total_Trans_Ct,
        ylab = "Total_Trans_Ct",
        main = "Boxplot of Total_Trans_Ct"
)
summary(a$Total_Trans_Ct)
x <- a$Total_Trans_Ct
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
a$Total_Trans_Ct <- x


##Months_Inactive_12_mon Outlier Removal
boxplot(a$Months_Inactive_12_mon,
        ylab = "Distance.mi.",
        main = "Boxplot of Distance.mi."
)
summary(a$Months_Inactive_12_mon)
x <- a$Months_Inactive_12_mon
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
a$Months_Inactive_12_mon <- x


##Avg_Open_To_Buy Outlier Removal
boxplot(a$Avg_Open_To_Buy,
        ylab = "Avg_Open_To_Buy",
        main = "Boxplot of Avg_Open_To_Buy"
)

summary(a$Avg_Open_To_Buy)
x <- a$Avg_Open_To_Buy
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
a$Avg_Open_To_Buy <- x


##Customer_Age Outlier Removal
boxplot(a$Customer_Age,
        ylab = "Customer_Age",
        main = "Boxplot of Customer_Age"
)
summary(a$Customer_Age)
x <- a$Customer_Age
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
a$Customer_Age <- x


##Credit_Limit Outlier Removal
boxplot(a$Credit_Limit,
        ylab = "Credit_Limit",
        main = "Boxplot of Credit_Limit"
)
summary(a$Credit_Limit)
x <- a$Credit_Limit
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
a$Credit_Limit <- x


##Contacts_Count_12_mon Outlier Removal
boxplot(a$Contacts_Count_12_mon,
        ylab = "Contacts_Count_12_mon",
        main = "Boxplot of Contacts_Count_12_mon"
)
summary(a$Contacts_Count_12_mon)
x <- a$Contacts_Count_12_mon
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
a$Contacts_Count_12_mon <- x

#splitting Data
set.seed(345) 
train <- sample(1:nrow(a), nrow(a)*(2/3)) 
a.train <- a[train,]   
a.test <- a[-train,]  

# Decision Tree
library(rpart)
fit <- rpart(Attrition_Flag ~ ., 
             data=a.train, 
             method="class",  
             control=rpart.control(xval=0, minsplit=50), 
             parms=list(split="gini"))  

fit  
plot(fit, uniform=TRUE,  
     branch=0.5,         
     main="Classification Tree for Attrition Prediction",   
     margin=0.1)         
text(fit,  use.n=TRUE,   
     all=TRUE,          
     fancy=FALSE,           
     pretty=TRUE,         
     cex=0.8)

install.packages("rpart.plot")
library(rpart.plot)

rpart.plot(fit, type = 1, extra = 1, main="Classification Tree for Attrition Prediction")  

Attrition_Flag.pred <- predict(fit, a.train, type="class")
Attrition_Flag.actual <- a.train$Attrition_Flag
confusion.matrix <- table(Attrition_Flag.pred ,Attrition_Flag.actual)  
confusion.matrix
addmargins(confusion.matrix)
pt <- prop.table(confusion.matrix)  
pt
#accuracy
pt[1,1] + pt[2,2]   #0.935417
prop.table(confusion.matrix, 2)

#testing data
Attrition_Flag.pred <- predict(fit, a.test, type="class")
Attrition_Flag.actual <- a.test$Attrition_Flag
confusion.matrix <- table(Attrition_Flag.pred ,Attrition_Flag.actual)
confusion.matrix
addmargins(confusion.matrix)
pt <- prop.table(confusion.matrix)
pt
tp <- confusion.matrix[2,2];tp
tn <- confusion.matrix[1,1];tn
fp <- confusion.matrix[2,1];fp
fn <- confusion.matrix[1,2];fn
# accuracy
(tp + tn)/(tp + tn + fp + fn)
# TPR = Recall = Sensitivity
tp/(fn+tp)
# TNR = Specificity
tn/(fp+tn)
# FPR
fp/(fp+tn)
# FNR
fn/(fn+tp)
# precision
tp/(tp+fp)
# prevalance
(fn+tp)/(tp + tn + fp + fn)
#accuracy
pt[1,1] + pt[2,2]   #0.932761

###########Logistic Regression ########### 
logit.reg <- glm(Attrition_Flag ~., 
                 data = a.train, family = binomial(link = 'logit')) 
summary(logit.reg)

logitPredict <- predict(logit.reg, a.test, type = "response")
logitPredict
logitPredictClass <- (ifelse(logitPredict > 0.5, 1, 0))
logitPredictClass
table(logitPredictClass)

op<-data.frame(logitPredictClass)
output<-cbind(a.test,op)
index1<-output$logitPredictClass==0;index1
index2<-output$logitPredictClass>0;index2
output$logitPredictClass[index1]<-'Existing'
output$logitPredictClass[index2]<-'Attrited'
Output1<-output

n=nrow(Output1)
count<-table(Output1$logitPredictClass);
piepercent<-round(count/n*100, 1);
pie(piepercent,
    labels=paste(piepercent,"%",sep=""),
    main="Customer Status",
    col = c("Pink","Yellow"))
legend("topright",
       c('Existing Customer','Attrited Customer'),
       cex=0.8,
       fill=c("Pink","Yellow")
)


# evaluate classifier on test.df
actual <- a.test$Attrition_Flag
predict <- logitPredictClass
cm <- table(predict, actual)
cm
tp <- cm[2,2];tp
tn <- cm[1,1];tn
fp <- cm[2,1];fp
fn <- cm[1,2];fn
# accuracy
(tp + tn)/(tp + tn + fp + fn)
# TPR = Recall = Sensitivity
tp/(fn+tp)
# TNR = Specificity
tn/(fp+tn)
# FPR
fp/(fp+tn)
# FNR
fn/(fn+tp)
# precision
tp/(tp+fp)
# prevalance
(fn+tp)/(tp + tn + fp + fn)
addmargins(cm)
pt1 <- prop.table(cm)
pt1
pt1[1,1] + pt1[2,2] 



########### 3. Naive Bayes Classifier ########### 

library(e1071)

# run naive bayes
fit.nb <- naiveBayes(Attrition_Flag ~., data = a.train)
fit.nb

actual <- a.test$Attrition_Flag
nbPredict <- predict(fit.nb, a.test, type = "raw")
nbPredictClass <- predict(fit.nb, a.test, type = "class")
cm1 <- table(nbPredictClass, actual)
cm1

tp <- cm1[2,2];tp
tn <- cm1[1,1];tn
fp <- cm1[2,1];fp
fn <- cm1[1,2];fn
# accuracy
(tp + tn)/(tp + tn + fp + fn)
# TPR = Recall = Sensitivity
tp/(fn+tp)
# TNR = Specificity
tn/(fp+tn)
# FPR
fp/(fp+tn)
# FNR
fn/(fn+tp)
# precision
tp/(tp+fp)
# prevalance
(fn+tp)/(tp+tn+fp+fn)