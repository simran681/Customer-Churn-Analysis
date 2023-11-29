churn=read.csv(file.choose())
attach(churn)
names(churn)

#for visualizing outliers
boxplot(CustServ.Calls)
#skewness
hist(Day.Mins)
#z score standardized
y=(Day.Mins-mean(Day.Mins))/sd(Day.Mins) 
#skewness of z score standardized
hist(y)


#finding summary only for target variable i.e Churn 
x=summary(as.factor(Churn));x  #we have used as.factor as it is categorical column


#proportions of churners and non churners
p=prop.table(table(Churn));p
  #or
prop.table(x)


#table for counts of churn and international plan
c=table(Churn,Int.l..Plan,dnn =c("Churn","International Plan")) ;c

#table for proportions over rows
rm=round(prop.table(c,margin=1),4)*100 ;rm #margin=1 means row wise and margin=2 for columnwise
##28.4% of churners belong to international plan compared to 6.5% of non churners

#table for proportions over columns
cm=round(prop.table(c,margin=2),4)*100 ;cm 
##42.41% of international plan holders have churned as compared to 11.5 % of those without international plan

#barchart of variable churn
barplot(x,ylim=c(0,3000),main="BarGraph of churners and non-churners")
 #or
barplot(p,main="BarGraph of churners and non-churners")

#overlayed barchart
barplot(c,legend=rownames(c),col=c("pink","purple"),ylim=c(0,3300),ylab="counts",xlab="international plan",main="comparison BarChart : churn proportions by international plan")
box(which="plot",lty="solid",col="black")

#clustered barchart
barplot(c,col=c("red","orange"),ylim=c(0,3300),beside=TRUE,ylab="counts",xlab="international plan",main="comparison BarChart : churn proportions by international plan")
legend("topright",rownames(c),col=c("red","orange"),pch=15,title="Churn")
box(which="plot",lty="solid",col="black")

#exploring numerical variable
#histogram of non overlayed customer service calls
hist(CustServ.Calls,xlim=c(0,10),col="lightblue",ylab="counts",xlab="customer service calls",main="Histogram of customer service calls")

#overlayed barchart
library(ggplot2)
#ggplot is used when we have one categorical variable and variable is either discrete  or have larger values
ggplot()+geom_bar(data=churn,aes(x=factor(CustServ.Calls),fill=factor(Churn)),position="stack")+scale_x_discrete("Customer service calls")+scale_y_continuous("count")+guides(fill=guide_legend(title='churn'))+scale_fill_manual(values=c("lightblue","purple"))

#normalized histogram
ggplot()+geom_bar(data=churn,aes(x=factor(CustServ.Calls),fill=factor(Churn)),position="fill")+scale_x_discrete("Customer service calls")+scale_y_continuous("percent")+guides(fill=guide_legend(title='churn'))+scale_fill_manual(values=c("purple","orange"))


#Binning based on predictive value
range(Day.Mins)
breaks=c(0,50,100,150,200,250,300,350,400) 
tags=c("[0-50)","[50-100)","[100-150)","[150-200)","[200-250)","[250-300)","[300-350)","[350-400)")  #specifying interval/bin labels
group_tags=cut(Day.Mins,breaks=breaks,include.lowest = TRUE,right=FALSE,labels=tags) #bucketing values into bins
summary(group_tags)

#non-standardized histogram of day minutes
daymins_grp= factor(group_tags,ordered=TRUE)
ggplot()+geom_bar(data=churn,aes(x=factor(group_tags),fill=factor(Churn)),position="stack")+scale_x_discrete("Day minutes")+scale_y_continuous("count")+guides(fill=guide_legend(title='churn'))+scale_fill_manual(values=c("orange","purple"))
#standardized histogram of day minutes
ggplot()+geom_bar(data=churn,aes(x=factor(group_tags),fill=factor(Churn)),position="fill")+scale_x_discrete("Day minutes")+scale_y_continuous("percent")+guides(fill=guide_legend(title='churn'))+scale_fill_manual(values=c("orange","purple"))


#exploring multivariate relationships
plot(Eve.Mins,Day.Mins,col=ifelse(Churn=="True.","red","blue"))
legend("topright",c("TRUE","FALSE"),col=c("red","blue"),pch=1,title="churn")

#scatter plot matrix to investigate correlated predictor variables
pairs(~Day.Mins+Day.Calls+Day.Charge)

#correlation values with p values
days=cbind(Day.Mins,Day.Calls,Day.Charge)
#H0:There is no correlation between two variables
#H1:There is a correlation between two variables
mincall=cor.test(Day.Mins,Day.Calls);mincall #pvalue >0.05 ,do not reject H0
mincharge=cor.test(Day.Mins,Day.Charge);mincharge #pvalue <0.05 ,reject H0
callcharge=cor.test(Day.Calls,Day.Charge);callcharge #pvalue >0.05 ,do not reject H0

round(cor(days),4) #correlation matrix
 

#table for counts of churn and voice mail plan
v=table(Churn,VMail.Plan,dnn=c("churn","voice mail plan"));v
prop.table(v)
rv=round(prop.table(v,margin=1),4)*100;rv
cv=round(prop.table(v,margin=2),4)*100;cv
#conclusion : International plan users churn more as compared to voice mail plan users.

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------

#logistic regression with voice mail plan(dichotomous predictor)
#H0:VMP is not significant predictor(beta=0)
#H1:VMP is  significant predictor(beta!=0)
fit=glm(as.factor(Churn)~as.factor(VMail.Plan),family=binomial)
summary(fit)
  #pvalue<0.05,we reject H0. Hence VMP is significant predictor for finding churned behaviour of customer

#estimated logit g(x)= -1.60596-0.74780x
#estimated probability of churning of customer belonging to plan(x=1) and not belonging to plan(x=0)
#g(1)= -1.60596-0.74780(1)=-2.35376 and prob= e^g(1)/(1+e^g(1))=0.0868
#g(0)= -1.60596-0.74780(0)=-1.60596 and  prob= e^g(0)/(1+e^g(0))=0.16715
#interpretation: prob of customers without VMP is more as compared to with VMP ,so company should promote more for VMP among customers to reduce churning.

or=round(exp(coef(fit)),3);or
#confidence interval for odds ratio for churning among VMP member and non members=(0.37,0.61)
exp(confint(fit))

#logistic regression for customer service calls(polychotomous predictor)
#replacing the numeric inputs of cust service calls into qaulitative categories low(0-1),medium(2-3),high(4-9)
csc=factor(CustServ.Calls)
levels(csc)
levels(csc)[1:2]="low" #0 and 1
levels(csc)[2:3]="medium" #2 and 3
levels(csc)[3:8]="high" #4 to 9
csc
#dummy variables with base as low
csc_med=ifelse(csc=="medium",1,0);csc_med 
csc_high=ifelse(csc=="high",1,0);csc_high
table(Churn,csc)

#logistic regression with levels med,high
#H0:levels are not significant predictor(beta=0)
#H1:levels are  significant predictor(beta!=0)
fit=glm(factor(Churn)~csc_med+csc_high,family=binomial)
summary(fit)
#pvalue of med>0.05,we do not reject H0. Hence medium customer service call is not significant predictor for finding churned behaviour of customer
#pvalue of high<0.05,we  reject H0. Hence  high customer service calls is significant predictor for finding churned behaviour of customer

#estimated logit g(x)= -2.051+0.03699(csc_med)+2.11844(csc_high)

#----------------------------------------------------------------------------------------------
#partioning the training and testing 
n=floor(0.75*nrow(churn))
set.seed(124)
trainingdata=sample(seq_len(nrow(churn)),size=n)
training=churn[trainingdata,]
testing=churn[-trainingdata,]
t1=table(training$Churn);t1
t2=table(testing$Churn);t2
write.csv(training,"Z:\\22pbd029\\pbd-3803(B)\\training.csv")
prop.table(t1)
prop.table(t2)

#check for multicollinearity(we can check for each variable by keeping it as target variable and rest as dependent)
library(car)
#here we are using account length as target and checking if it has any multicollinearity with others
vif(lm(training$Account.Length~training$CustServ.Calls+training$Intl.Calls+training$Day.Mins+training$Day.Calls+training$Day.Charge+training$Eve.Mins+training$Eve.Charge+training$Eve.Calls+training$Night.Calls+training$Night.Charge+training$Night.Mins+training$VMail.Message+training$VMail.Plan))

#logistic regression for multiple predictors
fit2=glm(as.factor(training$Churn)~as.factor(training$Int.l..Plan)+training$Intl.Mins+training$Intl.Charge+training$Account.Length+training$CustServ.Calls+training$Intl.Calls+training$Day.Mins+training$Day.Calls+training$Day.Charge+training$Eve.Mins+training$Eve.Charge+training$Eve.Calls+training$Night.Calls+training$Night.Charge+training$Night.Mins+training$VMail.Message+as.factor(training$VMail.Plan),family=binomial())
fit2
summary(fit2)
#by comparing pvalue of all with 0.05, we found that international plan,international mins ,international calls,customer service calls and voice mail plan are significant variables.

#using backward stepwise regression (removing insignificant varibale with highest pvalue)
#by removing account length as it has highest p value among all insignificant varibles
fit3=glm(as.factor(training$Churn)~as.factor(training$Int.l..Plan)+training$Intl.Mins+training$Intl.Charge+training$CustServ.Calls+training$Intl.Calls+training$Day.Mins+training$Day.Calls+training$Day.Charge+training$Eve.Mins+training$Eve.Charge+training$Night.Charge+training$Night.Mins+training$VMail.Message+as.factor(training$VMail.Plan),family=binomial())
summary(fit3)
#removing day mins
fit4=glm(as.factor(training$Churn)~as.factor(training$Int.l..Plan)+training$Intl.Mins+training$Intl.Charge+training$CustServ.Calls+training$Intl.Calls+training$Day.Calls+training$Day.Charge+training$Eve.Mins+training$Eve.Charge+training$Night.Charge+training$Night.Mins+training$VMail.Message+as.factor(training$VMail.Plan),family=binomial())
summary(fit4)
#removing international mins
fit5=glm(as.factor(training$Churn)~as.factor(training$Int.l..Plan)+training$Intl.Charge+training$CustServ.Calls+training$Intl.Calls+training$Day.Calls+training$Day.Charge+training$Eve.Mins+training$Eve.Charge+training$Night.Charge+training$Night.Mins+training$VMail.Message+as.factor(training$VMail.Plan),family=binomial())
summary(fit5)
#removing night mins
fit6=glm(as.factor(training$Churn)~as.factor(training$Int.l..Plan)+training$Intl.Charge+training$CustServ.Calls+training$Intl.Calls+training$Day.Calls+training$Day.Charge+training$Eve.Mins+training$Eve.Charge+training$Night.Charge+training$VMail.Message+as.factor(training$VMail.Plan),family=binomial())
summary(fit6)
#removing evening charge
fit7=glm(as.factor(training$Churn)~as.factor(training$Int.l..Plan)+training$Intl.Charge+training$CustServ.Calls+training$Intl.Calls+training$Day.Calls+training$Day.Charge+training$Eve.Mins+training$Night.Charge+training$VMail.Message+as.factor(training$VMail.Plan),family=binomial())
summary(fit7)
#removing day calls
fit8=glm(as.factor(training$Churn)~as.factor(training$Int.l..Plan)+training$Intl.Charge+training$CustServ.Calls+training$Intl.Calls+training$Day.Charge+training$Eve.Mins+training$Night.Charge+training$VMail.Message+as.factor(training$VMail.Plan),family=binomial())
summary(fit8)
#removing vmail msg
fit9=glm(as.factor(training$Churn)~as.factor(training$Int.l..Plan)+training$Intl.Charge+training$CustServ.Calls+training$Intl.Calls+training$Day.Charge+training$Eve.Mins+training$Night.Charge+as.factor(training$VMail.Plan),family=binomial())
summary(fit9)

#model diagonostics: overall model significance
#H0:beta1=beta2=...=betap=0(insignificant)
#H1: Atleast one of them differs(significant)
with(fit9,pchisq(null.deviance-deviance,df.null-df.residual,lower.tail=FALSE))
    #or
1-pchisq(2056.9-1624.0,df=(2498-2490)) #here, null deviance-residual deviance

#pvalue=0<0.05,we reject H0 i.e Atleast one of them is non zero. Hence model building can be done and model is significant

#prediction
p=predict(fit9,type='response')
head(p)

#classification table(confusion matrix)
pred=ifelse(p>0.5,'True','False');pred
tab=table(predicted=pred,actual=training[,21]) ;tab
sumtable=addmargins(tab,FUN=sum);sumtable

TAP=sumtable[3,2];TAP #TRUE ACTUAL POSITIVE
TAN=sumtable[3,1];TAN #TRUE ACTUAL NEGATIVE
TP=sumtable[2,2];TP #TRUE  POSITIVE
TN=sumtable[1,1];TN #TRUE NEGATIVE
FP=sumtable[2,1];FP #FALSE POSITIVE
FN=sumtable[1,2];FN #FALSE NEGATIVE

#sensitivity (RECALL)
TPR=TP/TAP;TPR #TRUE POSITIVE RATE
#since sensitivity is 19% that means our model fails to predict the churners well.

#specificity
TRN=TN/TAN ;TRN #TRUE NEGATIVE RATE
#since specificity is >90%,our model is good for non churners

#1-specificity
FPR=FP/TAN;FPR #FALSE POSITIVE RATE


#The proportion of obs. correctly classified is(accuracy):
sum(diag(tab))/sum(tab) 
 #or
acc=((2090+70)/2499)*100; acc
#since accuracy is 87% but sensitivity is less,we cant rely on prediction for churners

library(ROCR)
ROCR_pred=prediction(p,training[,21]) ;ROCR_pred
ROCR_pref=performance(ROCR_pred,'tpr','fpr') ;ROCR_pref
plot(ROCR_pref,colorize=T,main="ROC CURVE",ylab="Sensitivity",xlab="1-Specificity")
abline(a=0,b=1)

auc=performance(ROCR_pred,measure="auc")
auc=auc@y.values[[1]] ;auc

#-------------------------------------------------------------------
#Resampling the data since data is unbalanced 

library(ROSE)
#Randomly oversampling examples :ROSE
over=ovun.sample(Churn~Int.l..Plan+Intl.Charge+CustServ.Calls+Intl.Calls+Day.Charge+Eve.Mins+Night.Charge+VMail.Plan, data=training, method="over", N=4280,
            subset=options("subset")$subset,
            na.action=options("na.action")$na.action, seed=2)$data
table(over$Churn)
View(over)
summary(over)

lr=glm(as.factor(Churn)~as.factor(Int.l..Plan)+Intl.Charge+CustServ.Calls+Intl.Calls+Day.Charge+Eve.Mins+Night.Charge+as.factor(VMail.Plan),data=over,family="binomial",maxit=500)
summary(lr)
#Individual variables are significant
with(lr,pchisq(null.deviance-deviance,df.null-df.residual,lower.tail = FALSE))
#overall model is also significant as pvalue is approx 0

#prediction based on over sample data
pover=predict(lr,type='response')
head(pover)

#classification table(confusion matrix) for over data sample
pred_over=ifelse(pover>0.5,'True','False');pred_over
tab_o=table(predicted=pred_over,actual=over[,9]) ;tab_o
sumtable_o=addmargins(tab_o,FUN=sum);sumtable_o

TAP=sumtable_o[3,2];TAP #TRUE ACTUAL POSITIVE
TAN=sumtable_o[3,1];TAN #TRUE ACTUAL NEGATIVE
TP=sumtable_o[2,2];TP #TRUE  POSITIVE
TN=sumtable_o[1,1];TN #TRUE NEGATIVE
FP=sumtable_o[2,1];FP #FALSE POSITIVE
FN=sumtable_o[1,2];FN #FALSE NEGATIVE

#sensitivity (RECALL)
TPR_o=TP/TAP;TPR_o #TRUE POSITIVE RATE
#since sensitivity is 76% that means our model based on resampled data(over) has improved to predict the churners well.

#specificity
TRN_o=TN/TAN ;TRN_o #TRUE NEGATIVE RATE

#1-specificity
FPR_o=FP/TAN;FPR_o #FALSE POSITIVE RATE

#The proportion of obs. correctly classified is(accuracy):
sum(diag(tab_o))/sum(tab_o) 


#Fitting Testing data
View(testing)
new_test=testing[,-21]
View(new_test)
lr1=glm(as.factor(testing$Churn)~as.factor(Int.l..Plan)+Intl.Charge+CustServ.Calls+Intl.Calls+Day.Charge+Eve.Mins+Night.Charge+as.factor(VMail.Plan),data=testing,family="binomial",maxit=500)
summary(lr1)
pt=predict(lr1,new_test,type='response');pt

