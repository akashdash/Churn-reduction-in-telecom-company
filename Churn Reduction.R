## Load Libraries

x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees')

# install.packages(x)
lapply(x, require, character.only = TRUE)
rm(x)

## Set Working Directory
setwd("/Users/ad/Desktop/Project 1")
getwd()

## Importing dataset to R
train<- read.csv("Train_data.csv", header = TRUE, stringsAsFactors = TRUE)
test<- read.csv("Test_data.csv", header= TRUE, stringsAsFactors = TRUE)

## Combining train and test data
df<- rbind(train, test)

## Renaming Variables
#names(df)[1]= "intl.plan"
#names(df)[16]= "calls.to.cus.service"

## Copy original dataframe
df_copy<- df
#df<- df_copy

## Exploratory Data Analysis
names(df)
head(df, 5)
colnames(df)
dim(df)
str(df)
summary(df)

## Removing first four variables
df<- df[,-(:4)]
#df2<- subset(train, select = c("international.plan", "voice.mail.plan", "number.vmail.messages", "total.day.minutes", "total.day.calls", "total.day.charge", "total.eve.minutes", "total.eve.calls", "total.eve.charge", "total.night.minutes", "total.night.calls", "total.night.charge", "total.intl.minutes", "total.intl.calls", "total.intl.charge", "number.customer.service.calls", "Churn"))

# Data Manipulation: Convert String category into factor numeric
for (i in 1:ncol(df)){
  if(class(df[,i])== 'factor'){
    df[,i]= factor(df[,i], labels = (1: length(levels(factor(df[,i])))))
  }
}


### Mising Value Analysis

# Create Dataframe with missing percentage
missing_val<- data.frame(apply(df, 2, function(x){sum(is.na(x))} ))

# Renaming Column
#names(missing_val)[1]= "MissingValue"

# Creating new column with index values
#missing_val$columns<- row.names(missing_val)
#row.names(missing_val)<- NULL

# Converting to percent
#missing_val$percent_missing<- (missing_val$`Missing Value`/ nrow(df))*100
# Rearranging Columns
#missing_val<- missing_val[,c(2,1)]
# Write the output back to disk
#write.csv(missing_val,"Missing_Value_Analysis", row.names = F)

###################################### Outlier Analysis ######################################

# BoxPlots- Distribution and Outlier Check

numeric_index <- sapply(df, is.numeric) #Selecting only numeric columns

numeric_columns <- df[,numeric_index]

cnames <- colnames(numeric_columns)

## Plotting the BoxPlot
for (i in 1:length(cnames))
  {
  assign(paste0("gn",i), ggplot(aes_string(y= cnames[i], x= "Churn"), data = subset(df))+
           stat_boxplot(geom = "errorbar", width = 0.5)+
           geom_boxplot(outlier.color = "red", fill= "grey", outlier.shape = 18,
                        outlier.size = 1, notch = FALSE)+
           theme(legend.position = "bottom")+
           labs(y=cnames[i], x= "Churn")+
           ggtitle("BoxPlot of Churn for", cnames[i]))
}

## Plotting the plots
gridExtra::grid.arrange(gn1,gn2,gn3,ncol=3)
gridExtra::grid.arrange(gn4,gn5,gn6,ncol=3)
gridExtra::grid.arrange(gn7,gn8,gn9,ncol=3)
gridExtra::grid.arrange(gn10,gn11,gn12,ncol=3)
gridExtra::grid.arrange(gn13,gn14,ncol=2)

## Loop to remove outliers from all variables
#for (i in cnames){
#  print(i)
#  val= df[,i][df[,i] %in% boxplot.stats(df[,i])$out]
#  print(length(val))
#  df= df[which(!df[,i] %in% val),]
#}

## Replace all outliers with NA and impute
for (i in cnames) {
  val= df[,i][df[,i] %in% boxplot.stats(df[,i])$out]
  print(length(val))
  df[,i][df[,i] %in% val]= NA
}

df$number.vmail.messages[is.na(df$number.vmail.messages)]= mean(df$number.vmail.messages, na.rm = T)
df$total.day.minutes[is.na(df$total.day.minutes)]= mean(df$total.day.minutes, na.rm = T)
df$total.day.calls[is.na(df$total.day.calls)]= mean(df$total.day.calls, na.rm = T)
df$total.day.charge[is.na(df$total.day.charge)]= mean(df$total.day.charge, na.rm = T)
df$total.eve.minutes[is.na(df$total.eve.minutes)]= mean(df$total.eve.minutes, na.rm = T)
df$total.eve.calls[is.na(df$total.eve.calls)]= mean(df$total.eve.calls, na.rm = T)
df$total.eve.charge[is.na(df$total.eve.charge)]= mean(df$total.eve.charge, na.rm = T)
df$total.night.minutes[is.na(df$total.night.minutes)]= mean(df$total.night.minutes, na.rm = T)
df$total.night.calls[is.na(df$total.night.calls)]= mean(df$total.night.calls, na.rm = T)
df$total.night.charge[is.na(df$total.night.charge)]= mean(df$total.night.charge, na.rm = T)
df$total.intl.minutes[is.na(df$total.intl.minutes)]= mean(df$total.intl.minutes, na.rm = T)
df$total.intl.calls[is.na(df$total.intl.calls)]= mean(df$total.intl.calls, na.rm = T)
df$total.intl.charge[is.na(df$total.intl.charge)]= mean(df$total.intl.charge, na.rm = T)
df$number.customer.service.calls[is.na(df$number.customer.service.calls)]= mean(df$number.customer.service.calls, na.rm = T)

## Looping to impute NA
for (i in cnames) {
  print(i)
  df[,i][is.na(df[,i])]= mean(df[,i], na.rm = T)
  print(sum(is.na(df[i])))
}


## Checking for Missing Values after outlier removal
#cnames<- colnames(df)
for (i in cnames) {
  print(i)
  print(sum(is.na(df[,i])))  
}

##################### FEATURE SELECTION #########################

## Correlation Plot

numeric_index <- sapply(df, is.numeric) #Selecting only numeric columns

corrgram(df[, numeric_index], order= T, lower.panel=panel.shade, 
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Correlation Plot")

## Chi Square Test of Independence
factor_index= sapply(df, is.factor)
factor_data= df[,factor_index]

for(i in 1:4)
  {
  print(names(factor_data)[i])
  print(chisq.test(table(factor_data$Churn, factor_data[,i])))
  }
  
## Dimension Reduction
df_deleted<- subset(df,select = -c(total.day.minutes, total.eve.minutes, total.intl.minutes, total.night.minutes, phone.number))

###################### Feature Scaling #################################

# Normality Check

# Normally Distributed
qqnorm(df_deleted$account.length)
hist(df_deleted$account.length)

qqnorm(df_deleted$total.day.calls)
hist(df_deleted$total.day.calls)

qqnorm(df_deleted$total.day.charge)
hist(df_deleted$total.day.charge)

qqnorm(df_deleted$total.eve.calls)
hist(df_deleted$total.eve.calls)

qqnorm(df_deleted$total.eve.charge)
hist(df_deleted$total.eve.charge)

qqnorm(df_deleted$total.night.calls)
hist(df_deleted$total.night.calls)

qqnorm(df_deleted$total.night.charge)
hist(df_deleted$total.night.charge)

qqnorm(df_deleted$total.intl.charge)
hist(df_deleted$total.intl.charge)


## Not Normally Distributed
qqnorm(df_deleted$number.vmail.messages)
hist(df_deleted$number.vmail.messages) #Not normally distributed

qqnorm(df_deleted$total.intl.calls) #Not normally distributed
hist(df_deleted$total.intl.calls)

qqnorm(df_deleted$number.customer.service.calls) #Not normally distributed
hist(df_deleted$number.customer.service.calls)

qqnorm(df_deleted$area.code)
hist(df_deleted$area.code)

# Normalization
cnames= c("number.vmail.messages", "total.intl.calls", "number.customer.service.calls")
for (i in cnames) {
  print(i)
  df_deleted[,i]= (df_deleted[,i]- min(df_deleted[,i]))/
    (max(df_deleted[,i]-min(df_deleted[,i])))
  
}

# Standardization
cnames= c("total.day.calls","total.day.charge","total.eve.calls", "total.eve.charge", "total.night.calls", "total.night.charge", "total.intl.charge")
for (i in cnames) {
  print(i)
  df_deleted[,i]= (df_deleted[,i]- mean(df_deleted[,i]))/
                    sd(df_deleted[,i])
}

########################### Sampling #############################
##Stratified Sampling
stratas = strata(df_deleted, c("Churn"), size = c(2400,400), method = "srswor")
sample = getdata(df_deleted, stratas)

## Simple Random Sampling
#data_sample = df_deleted[sample(nrow(df_deleted), 3000, replace = F), ]





########################### Modelling #############################

###################################################################
library(DataCombine)
rmExcept("df_deleted")
df<- df_deleted
## Divide the dataset into train and test data
set.seed(1234)
train.index = createDataPartition(df$Churn, p= 0.80 , list = FALSE)
train = df[train.index,]
test = df[-train.index,]
## Decision Tree Classification

# Develop model on training data
C50_model <- C5.0(Churn~. , train, trials= 100, rules= TRUE )
summary(C50_model)

# Write rules into disk
write(capture.output(summary(C50_model)), "c50Rules.txt")

# Predict for test cases
C50_Predictions = predict(C50_model, test[,-13], type = "class")

# Evaluate the performance of Decision Tree classification model
ConfMatrix_C50 = table(test$Churn, C50_Predictions)
confusionMatrix(ConfMatrix_C50)

## Accuracy: 95.5 %
## TPR: 24.1%
## TNR: 99.5%
## FPR: 0.46%
## FNR: 4.77%
####################################################################
## Logistic Regression (Classification Model)

logit_model = glm(Churn~., data = train, family = "binomial")
summary(logit_model)

# Predict using logistic regression
logit_prediction = predict(logit_model, newdata = test, type= "response")

# Convert Probablity
logit_prediction = ifelse(logit_prediction > 0.5, 1, 0)

# Evaluate performance of Logistic classification model
ConfMatrix_RF= table(test$Churn, logit_prediction)
## Accuracy: 86.8 %
## Error rate: 13.2%
## TPR: 15.6%
## TNR: 98.6%
## FPR: 1.3%
## FNR: 13.8%
#######################################################################

## Random Forest Classification

## Divide the dataset into train and test data
set.seed(1234)
train.index = createDataPartition(df$Churn, p= 0.66 , list = FALSE)
train = df[train.index,]
test = df[-train.index,]

RF_model = randomForest(Churn ~ ., train, importance = TRUE, ntree = 100)

# Extract rules fromn random forest
# Transform rf object to an inTrees format
treeList = RF2List(RF_model)

# Extract rules
exec = extractRules(treeList, train[,-13])

# Visualize some rules
exec[1:2,]

# Make rules more readable
readableRules = presentRules(exec, colnames(train))
readableRules[1:2,]

# Get rule metrics
rulemetric = getRuleMetric(exec, train[,-13], train$Churn)

# Evaulate few rules
rulemetric[1:2,]

#Predict test data using random forest model
RF_prediction = predict(RF_model, test[,-13])

#Evaluate the performance of classification model
ConfMatrix_RF= table(test$Churn, RF_prediction)
confusionMatrix(ConfMatrix_RF)

### With 100 Trees
## Accuracy: 94.53%
## FNR: 37.08%
## Error rate: 6 %
## TNR: 92.64%
## FPR: 0.82%

### With 200 Trees
## Accuracy: 94.17 %
## FNR: 39.5%
## Error rate: 6%
## TNR: 
## FPR: 0.54%


### With 300 Trees
## Accuracy: 94.47%
## FNR: 37.08%
############################################################################################

## KNN (Lazy Learning Algorithm)
library(class)

# Predict the test cases
KNN_Prediction = knn(train[,1:12], test[,1:12], train$Churn, k= 13)

# Confusion Matrix
ConfMatrix_KNN = table(KNN_Prediction, test$Churn)

#K=5
# Accuracy: 88.22%
# FNR: 23.6
#K=7
# Accuracy: 88%
# FNR: 20.3%
#K= 9
# Accuracy: 87.8%
# FNR: 9.5%
#K=11
# Accuracy: 88.11%
# FNR: 2.5

## Naive Bayes Classification Model
library(e1071)

# Develop Model
NB_model = naiveBayes(Churn~., data= train)

# Predict on the test cases
NB_Predictions = predict(NB_model, test[,1:12], type = "class")

# Confusion matrix
ConfMatrix_NB = table(observed= test[,13], predicted = NB_Predictions)
confusionMatrix(ConfMatrix_NB)

## Accuracy: 87.88%
## FNR: 76.66%

## Statistical way of finding accuracy
mean(NB_Predictions == test$Churn)
## Accuracy= 87.87%
########################### Error Metrics ###########################






