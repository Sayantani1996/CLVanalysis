#importing data
CustomerLifetimeValue=read.csv('C:/Users/LENOVO/Downloads/Final R Project IVY/Final R Project IVY/Fn-UseC_-Marketing-Customer-Value-Analysis.csv',na.string=c(""," ","NA","NULL"), stringsAsFactors = T)
str(CustomerLifetimeValue)

#converting few columns inside the data as a categorical column
CustomerLifetimeValue$Type.of.Open.Complaints=as.factor(CustomerLifetimeValue$Type.of.Open.Complaints)
CustomerLifetimeValue$Type.of.Policies=as.factor(CustomerLifetimeValue$Type.of.Policies)
table(CustomerLifetimeValue$Type.of.Open.Complaints)
table(CustomerLifetimeValue$Type.of.Policies)
str(CustomerLifetimeValue)

#treatment of the useless columns
UselessColumns=c('Customer','Effective.To.Date')
CustomerLifetimeValue[,UselessColumns]=NULL
head(CustomerLifetimeValue)
str(CustomerLifetimeValue)

#outliers treatment
boxplot(CustomerLifetimeValue$Income, horizontal = T)
boxplot(CustomerLifetimeValue$Monthly.Premium.Auto, horizontal = T)  #outliers
boxplot(CustomerLifetimeValue$Months.Since.Last.Claim, horizontal = T)
boxplot(CustomerLifetimeValue$Months.Since.Policy.Inception, horizontal = T)
boxplot(CustomerLifetimeValue$Total.Claim.Amount, horizontal = T)  #outliers

#Treating outliers:
####Treatment for Monthly.Premium.Auto
qt=quantile(CustomerLifetimeValue$Monthly.Premium.Auto,c(0.95,0.96,0.963,0.965,0.97,0.98,0.99,0.995,0.996,0.997,0.998,0.999),na.rm = TRUE)
qt
qt_final=quantile(CustomerLifetimeValue$Monthly.Premium.Auto,0.995,na.rm = TRUE)
qt_final[1]
max(CustomerLifetimeValue$Monthly.Premium.Auto)
CustomerLifetimeValue[,"Monthly.Premium.Auto"] = ifelse(CustomerLifetimeValue[,"Monthly.Premium.Auto"] > qt_final[1] , qt_final[1], CustomerLifetimeValue[,"Monthly.Premium.Auto"])
boxplot(CustomerLifetimeValue$Monthly.Premium.Auto, horizontal = T)
max(CustomerLifetimeValue$Monthly.Premium.Auto)

####Treatment for Total.Claim.Amount
qt2=quantile(CustomerLifetimeValue$Total.Claim.Amount,c(0.95,0.96,0.963,0.965,0.97,0.98,0.99,0.995,0.996,0.997,0.998,0.999),na.rm = TRUE)
qt2
qt_final2=quantile(CustomerLifetimeValue$Total.Claim.Amount,0.995,na.rm = TRUE)
qt_final2[1]
max(CustomerLifetimeValue$Total.Claim.Amount)
CustomerLifetimeValue[,"Total.Claim.Amount"] = ifelse(CustomerLifetimeValue[,"Total.Claim.Amount"] > qt_final2[1] , qt_final2[1], CustomerLifetimeValue[,"Total.Claim.Amount"])
boxplot(CustomerLifetimeValue$Total.Claim.Amount, horizontal = T)
max(CustomerLifetimeValue$Total.Claim.Amount)

#exploring the multiple continuous features
ColsForHist=c('Customer.Lifetime.Value','Income','Monthly.Premium.Auto','Months.Since.Last.Claim','Months.Since.Policy.Inception','Total.Claim.Amount')

#splitting the windows
par(mfrow=c(2,3))

#for color palatte
library(RColorBrewer)

#for loop function
for (hist_cols in ColsForHist) {
  hist(table(CustomerLifetimeValue[,c(hist_cols)]),main = paste("Histogram of:",hist_cols),col=brewer.pal(8,"Paired"))
}

#exploring the multiple categorical features
ColsForBar1=c('State','Response','Coverage','Education','EmploymentStatus','Gender','Location.Code')
ColsForBar2=c('Marital.Status','Type.of.Open.Complaints','Type.of.Policies','Policy.Type','Policy','Renew.Offer.Type','Sales.Channel','Vehicle.Class','Vehicle.Size')

#splitting the window for set 1
par(mfrow=c(2,4))

#for using color palatte
library(RColorBrewer)

#loop function foe set 1
for (bar_cols in ColsForBar1) {
  barplot(table(CustomerLifetimeValue[,c(bar_cols)]),main = paste("Barplot of :", bar_cols), col = brewer.pal(8,"Paired"))
}

#splitting the window for set 2
par(mfrow=c(2,5))

#loop function for set 2
for (bar_cols in ColsForBar2) {
  barplot(table(CustomerLifetimeValue[,c(bar_cols)]),main = paste("Barplot of :", bar_cols), col = brewer.pal(8,"Paired"))
}

#Statistical relationship between target variable and the predictor
## continuous vs continuous : Correlation Test
## continuous vs categorical : Anova Test

#Visual Relationship
## Continuous vs Continuous : Scatter plot
## Continuous vs Categorical : Bar Plot

# Continuous vs Continuous : Scatter plot
##For multiple columns at once
ContinuousCols= c('Customer.Lifetime.Value','Income','Monthly.Premium.Auto','Months.Since.Last.Claim','Months.Since.Policy.Inception','Total.Claim.Amount')
plot(CustomerLifetimeValue[,ContinuousCols],col='blue')

# Correlation test
## for multiple columns
CorData= cor(CustomerLifetimeValue[,ContinuousCols],use='complete.obs')
class(CorData)

CorData[,'Customer.Lifetime.Value']
names(CorData[,'Customer.Lifetime.Value'])

#setting the threshold level
abs(CorData[,'Customer.Lifetime.Value'])>0.5
# as we can see we cannot include any of the columns, so we decreased the threshold level
abs(CorData[,'Customer.Lifetime.Value'])>0.2

names(CorData[,'Customer.Lifetime.Value'][abs(CorData[,'Customer.Lifetime.Value'])>0.2])
## so here we sorted out the name of those columns which satisfied the threshold level.


#Continuous Vs Categorical----- Box Plot
catVar=c('State','Response','Coverage','Education','EmploymentStatus','Gender','Location.Code','Marital.Status','Type.of.Open.Complaints','Type.of.Policies','Policy.Type','Policy','Renew.Offer.Type','Sales.Channel','Vehicle.Class','Vehicle.Size')
for(bar_cols in catVar ){
   boxplot(Customer.Lifetime.Value ~ (CustomerLifetimeValue[,c(bar_cols)]), data = CustomerLifetimeValue, main = paste('Boxplot of:',bar_cols), col = brewer.pal(8,"Paired"))
}


## Anova Test
###Null Hypothesis: the variables are not correlated

#creating a loop
for(i in catVar){
  test_summary= summary(aov(Customer.Lifetime.Value ~ CustomerLifetimeValue[,c(i)], data = CustomerLifetimeValue))
  print(paste("Anova test of :",i))
  print(test_summary)
}

#creating a separate database
ImpData= CustomerLifetimeValue
TargetVariableName= 'Customer.Lifetime.Value'
BestPredictorName= c('Monthly.Premium.Auto','Total.Claim.Amount','Coverage','Education','EmploymentStatus','Marital.Status','Type.of.Open.Complaints','Type.of.Policies','Renew.Offer.Type','Vehicle.Class','Vehicle.Size')
TargetVariable= ImpData[,c(TargetVariableName)]
PredictorVariable=ImpData[,BestPredictorName]
str(PredictorVariable)

#creating final data for ML
DataForML=data.frame(TargetVariable,PredictorVariable)
str(DataForML)
head(DataForML)

#splitting data into 70% for training and 30% for testing
TrainingSampleIndex= sample(1:nrow(DataForML),size = 0.7*nrow(DataForML))
length(TrainingSampleIndex)

#to get the records of the training set
DataForMLTrain=DataForML[TrainingSampleIndex,]
DataForMLTrain

#index for test set
DataForMLTest=DataForML[-TrainingSampleIndex,]
DataForMLTest

dim(DataForMLTrain)
dim(DataForMLTest)

#linear regression algorithm
StartTime=Sys.time()
Model_Reg=lm(TargetVariable~.,data= DataForMLTrain)
EndTime= Sys.time()
TimeTaken= EndTime-StartTime

summary(Model_Reg)

#multicolinearity test
library(car)
Multi=vif(Model_Reg)
last_col=Multi[,3]
FinalValues=last_col**2
Multi_Final=data.frame(FinalValues)
Multi_Final

#removal of column
Model_Reg=lm(TargetVariable~.-Monthly.Premium.Auto,data=DataForMLTrain)
Model_Reg
summary(Model_Reg)

#homoscadasticity test
library(lmtest)
bptest(Model_Reg)

#Durbin_Watson test
dwtest(Model_Reg)

#Anderson_Darling Normality test
library(nortest)
ad.test(Model_Reg$residuals)

#based on the model using train data, we will predict values on test data
DataForMLTest$Pred_LM=predict(Model_Reg, DataForMLTest)
head(DataForMLTest)

#calculating the absolute error % for each prediction
DataForMLTest$LM_APE=100*(abs(DataForMLTest$TargetVariable-DataForMLTest$Pred_LM)/DataForMLTest$TargetVariable)
head(DataForMLTest)

#checking the accuracy of the model on test data
print(paste('Mean Accuracy of Linear Regression Model is: ', 100 - mean(DataForMLTest$LM_APE)))
print(paste('Median Accuracy of Linear Regression Model is: ', 100 - median(DataForMLTest$LM_APE)))

