#---------------------------------------------------------------------------------------------------------------------------
#https://www.kaggle.com/code/adrynh/stroke-prediction
#https://www.kaggle.com/code/adrynh/stroke-prediction
library(DiagrammeR)

library(DiagrammeR)

DiagrammeR::grViz("digraph {

graph [layout = dot, rankdir = TB, label = 'Project Flowchart',labelloc= ta]
node [shape = rectangle, style = filled, fillcolor = lightpink]

'Calling libraries' ->'Cleansing'-> 'EDA analysis' -> 'Processing data' -> 'None Parametric'
'Processing data' -> 'Parametric'
'Parametric'-> 'Logistic Regression'
'Parametric'-> 'Discrimination models'
'None Parametric' -> 'Regression Tree model'
'None Parametric' -> 'KNN model'
'KNN model'-> 'Visualization and testing'
'Regression Tree model'-> 'Visualization and testing'
'Logistic Regression'-> 'Visualization and testing'
'Discrimination models'-> 'Visualization and testing' 
'Visualization and testing'->'fitting and testing Candid model'->'Over Smapling and Under Sampling'
}")
#needed libraries
library(readxl)
library(tibble)   # data frame printing
library(dplyr)      # data manipulation
library(caret)     # fitting knn
library(rpart)    # fitting trees
library(rpart.plot) # plotting trees
library(knitr)
library(Hmisc)
library(cowplot)
library(ggplot2)
library(MASS)
library(tidyverse)
library(knitr)
library(kableExtra)
library(ROSE) 
#specific
library(e1071)
library(nnet)
library(pROC)
library(ellipse)
library(klaR)
#library(WVPlots)
#EDA needed library
library(ggplot2)


#reading the file
heart_data=read_excel("healthcare-dataset-stroke-data.xlsx")
heart_data
#attach(heart_data)
#hypertension is a high blood pressure ,(higher than normal or not).
#-------------------------------------------------------------------------------------------------------------------------
#Cleansing
describe(heart_data)
sum(is.na(heart_data))
#first:BMI
# Check data type
class(heart_data$bmi)
# Convert BMI to numeric
heart_data$bmi=as.numeric(heart_data$bmi)
#since we got a Warning: NAs introduced by coercion we should replace the NAs.
# Check data type
class(heart_data$bmi)
#View Summary Statistics for data
summary(heart_data$bmi)
#we have 201 data consist of NA as BMI so we replace them with mean of BMI.
#Replace N/A's in BMI column with mean
heart_data$bmi[is.na(heart_data$bmi)] =mean(heart_data$bmi,na.rm=TRUE)
sum(is.na(heart_data$bmi))
#View New Summary Statistics for data
summary(heart_data$bmi)

#second : Gender
table(heart_data$gender)
#we replace the "Other" category with "Female" since it's the majority of records.
heart_data$gender=ifelse(heart_data$gender == "Other", "Female", heart_data$gender)
table(heart_data$gender)

#third: Smoke_status
#Since there is an "unknown" category, we put the data in that column in other categories based on their probability.
#Calculate the probability of formerly smoker, current smokers and non-smokers given that there's 
#only this three categories in the smoking_status column
prob.FS = 885 / (885 + 1892 + 789)
prob.NS = 1892 / (885 + 1892 + 789)
prob.S =789 / (885 + 1892 + 789)
#Duplicate the data
data1=heart_data
#Replacing 'Unknown' in smoking_status by the other 3 variables according to their weightage
data1$rand <- runif(nrow(data1))
data1 = data1%>%mutate(Probability = ifelse(rand <= prob.FS, "formerly smoked", ifelse(rand <= (prob.FS+prob.NS), "never smoked", ifelse(rand <= 1, "smokes", "Check"))))
data1 = data1%>%mutate(smoking.status = ifelse(smoking_status == "Unknown", Probability, smoking_status))
table(data1$smoking.status)
#Remove columns that are not needed
heart_data=subset(data1, select = -c(rand,Probability,smoking_status))
# revise the column name of smoking status
colnames(heart_data)[12] = "smoking_status"


#-------------------------------------------------------------------------------------------------------
#EDA visualization analysis
#Exploratory Data Analysis
#bar chart(for Quality features:stroke, heart_disease,sex,residence,smoke,work_type,hypertension)
Yes=subset(heart_data, stroke == '1')
No=subset(heart_data, stroke == '0')
strokecount=as.data.frame(table(heart_data$stroke))
ggplot(strokecount, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") + theme(legend.position="none") +
  geom_text(aes(label = Freq), vjust = 1.5) +
  labs(title="Stroke Status of Patients",x ="Stroke", y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5))
#as it is illustrated , the majority of record have not experienced stroke , only 249 people did so.
#SO we can see that we have an imbalance data.

# Create hypertension counts table

hypercount=as.data.frame(table(heart_data$hypertension,heart_data$stroke))
# Replace headers
colnames(hypercount)[1] ='Hypertension'
colnames(hypercount)[2] = 'Stroke'

hypercount$Hypertension =ifelse(hypercounts$Hypertension == 0, "No", 'Yes')
hypercount$Stroke =ifelse(hypercounts$Stroke == 0, "No", 'Yes')
# Bar Chart of Hypertension : No vs. Yes     
ggplot(hypercount, aes(x = Hypertension, y = Freq, fill = Stroke)) +
  geom_bar(stat = "identity")+
  geom_text(aes(label = Freq), vjust = 0) +
  labs(title="Hypertension Status of Patients",x ="Hypertension", y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5))
#we can have conclusion that people with blood pressure less than normal form the majority of our data, how ever the gap between 
#the people who has hypertension and those who have not is slightly less than the plot with stroke patients. we can see that 
# 15% of people with hypertension have experienced stoke and only 4 % of people with no hypertension had an stoke before.
#we can conclude the "hypertension" is one of important features effecting the stroke chance.
# Create heart disease counts table
heartcount =as.data.frame(table(heart_data$heart_disease, heart_data$stroke))
# Replace headers
colnames(heartcount)[1] <- 'Heart_Disease'
colnames(heartcount)[2] <- 'Stroke'
# Replace num to char
heartcount$Heart_Disease <- ifelse(heartcount$Heart_Disease== 0, "No", 'Yes')
heartcount$Stroke <- ifelse(heartcount$Stroke == 0, "No", 'Yes')

# Bar Chart of Heart Disease : No vs. Yes     
ggplot(heartcount, aes(x = Heart_Disease, y = Freq, fill = Stroke)) +
  geom_bar(stat = "identity") + 
  geom_text(aes(label = Freq), vjust = 1) +
  labs(title="Heart Disease Status of Patients",x ="Heart Disease", y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5))
#Again as it can be seen, the vast number of record do not have heart disease , 20 % of people who Have Heart disease have had
#stroke and only  4 % of people without heart disease experienced stroke, which illustrates that Heart disease have effect 
#on number of stoke
#Heart disease have a relatively important effect on chance of stroke.
# Create gender counts table
gendercount=as.data.frame(table(heart_data$gender,heart_data$stroke))
colnames(gendercount)[2] ='Stroke'
# Bar Chart of Gender   
ggplot(gendercount, aes(x = Var1, y = Freq, fill = Stroke)) +
  geom_bar(stat = "identity") + 
  geom_text(aes(label = Freq), vjust = 1.5) +
  labs(title="Gender of Patients",x ="Gender", y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5))
#we can see the number of patients with stroke experience in male is slightly less than females which is logical due
#to more number of females in general, so we can conclude the gender has no significant effect on stroke.

# Create work type counts table
workcount = as.data.frame(table(heart_data$work_type))
# Bar Chart of Patient Work Type   
ggplot(workcount, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +  theme(legend.position="none") +
  geom_text(aes(label = Freq), vjust = 0) +
  labs(title="Patient Work Type",x ="Work Type", y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5))
#the majority of data, had private work type, the least type is 22 people who have never worked.
#There are approximately even amounts of patients that are working government jobs, are self-employed, and are children
#effect of work type on stroke
work_stroke_count = as.data.frame(table(heart_data$work_type,heart_data$stroke))
# Bar Chart of Patient Work Type   
ggplot(work_stroke_count, aes(x = Var1, y = Freq, fill = Var2)) +
  geom_bar(stat = "identity") + 
  geom_text(aes(label = Freq), vjust = 0) +
  labs(title="Patient Work Type",x ="Work Type", y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5))
#as the chart illustrates, the most patients with stoke experience, have the Private type of work, 
#we can see 2 patients being under age had experience of stoke as well, 
#not so much of a significant effect on stroke chance.

# Create ever married counts table
marriedcount <- as.data.frame(table(heart_data$ever_married,heart_data$stroke))
colnames(marriedcount)[2] <- 'Stroke'
# Bar Chart of Patients Who Have Been Married  
ggplot(marriedcount, aes(x = Var1, y = Freq, fill = Stroke)) +
  geom_bar(stat = "identity")  +
  geom_text(aes(label = Freq), vjust = 1) +
  labs(title="Bar Chart of Patients Who Have Been Married",x ="Have the Patient been Married", y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5))

#the majority of record have been married before.Roughly double the amount of patients have been married before 
#than those who have not.
#as shown, almost 7% of married people experienced stroke whereas only 1% of single people experienced the stroke, so 
#we can conclude there is a slightly  effect of married on stroke chance.
# Create residence type counts table
rescount=as.data.frame(table(heart_data$Residence_type,heart_data$stroke))
colnames(rescount)[2] <- 'Stroke'

# Bar Chart of Patients base on residency 
ggplot(rescount, aes(x = Var1, y = Freq, fill = Stroke)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Freq), vjust = 0) +
  labs(title="Residence Type of the Patients",x ="Residence Type", y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5))
#there is virtually an even distribution between records in Rural residence and urban ones.
#there is no significat effect of residency on stroke_experienced number of patients.
# Create smoking status counts table
smokecount= as.data.frame(table(heart_data$smoking_status,heart_data$stroke))
colnames(smokecount)[2] <- 'Stroke'
# Bar Chart of Patients based on smoking status
ggplot(smokecount, aes(x = Var1, y = Freq, fill = Stroke)) +
  geom_bar(stat = "identity")+
  geom_text(aes(label = Freq), vjust = 1.5) +
  labs(title="Smoking Status of Patients",x ="Smoking Status", y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5))
#the majority of data have never smoked.the data for formerly and currently smokers are similar.
#as illustrated,surprisingly,there is no significant effect on people smoking status and stroke.

#histograms(for quantifiable features: bmi,age,avg_glucose)
#Histogram of Age with normal distribution overlay
histage=hist(heart_data$age,xlim=c(0,95),
                main="Histogram of Age with Normal Distribution Overlay",
                xlab="Age",las=1,col="lightblue")
xfit=seq(min(heart_data$age),max(heart_data$age))
yfit=dnorm(xfit,mean=mean(heart_data$age),sd=sd(heart_data$age))
#scaling
yfit=yfit*diff(histage$mids[1:2])*length(heart_data$age)
lines(xfit,yfit,col="red",lwd=2)
mean(heart_data$age)
median(heart_data$age)
#As illustrated, the age of patients in this study is close to normal distribution of age  which got the mean of43.22661
#and based on this information we can conclude the majority of record are around 40 years old.

#Histogram of Average Glucose Level with normal distribution overlay
histglucose=hist(heart_data$avg_glucose_level,xlim=c(0,300),
                    main="Histogram of Avg. Glucose with Normal Distribution Overlay",
                    xlab="Avg. Glucose",las=1,col="pink")
xfit =seq(min(heart_data$avg_glucose_level),max(heart_data$avg_glucose_level))
yfit =dnorm(xfit,mean=mean(heart_data$avg_glucose_level),sd=sd(heart_data$avg_glucose_level))
#scaling
yfit = yfit*diff(histglucose$mids[1:2])*length(heart_data$avg_glucose_level)
lines(xfit,yfit,col="red",lwd=2)
mean(heart_data$avg_glucose_level)
median(heart_data$avg_glucose_level)
#The average glucose levels of the patients in the study are right skewed,with mean of 106.1477 which is greater than median of 91.885.


#Boxplot( for comparing one numeric and one quality features: stroke and avg_glucose and BMI)
# Box and Whisker Plot of Average Glucose in Patients With and Without Strokes
boxplot(Yes$avg_glucose_level,No$avg_glucose_level,
        main="Boxplot of Average Glucose Level by Stroke Status",
        ylab="Average Glucose Level",las=1,names=c("Stroke","No Stroke"))

#The boxplot shows a relatively similar mean average glucose level
#in patients who suffered strokes and patients who have not, with lots of high outliers among non-stroke victims.
#we can conclude the average glucose level is not an effective feature on stroke.

#Box and Whisker Plot of Body Mass Index(BMI)in Patients With and Without Strokes
boxplot(Yes$bmi,No$bmi,main="Boxplot of Body Mass Index by Stroke Status",
        ylab="BMI",las=1,names=c("Stroke","No Stroke"))

#the mean of both patient with stroke and without it is relatively similar, we can see the BMI 
#is not the most effective feature on stroke.
boxplot(Yes$age,No$age,main="Boxplot of Age Index by Stroke Status",
        ylab="age",las=1,names=c("Stroke","No Stroke"))
#as it is shown, the older the patient is, the mean of stoke is higher than no stoke, so we can say age is a 
#significant factor on stroke.
ggplot(data=heart_data,aes(age,stroke))+
  geom_jitter(aes(color=factor(stroke)),alpha=0.7)+theme_light()
#as the Barcharts above illustrated, the majority of patients did not have stoke, but among those who had
#they are mostly from the age 40 to older.(mostly between 60 to 80)
#--------------------------------------------------------------------------------
#processing data
#AS we saw in above visualizations, we have several not significant variables: "gender","residence type","bmi","woke type,"id".
#so I decided to remove them from the main data.
heart_data=subset(heart_data,select=-c(id,gender,Residence_type,bmi,work_type))
heart_data = heart_data %>% 
  select(-stroke, everything())

heart_data$stroke=ifelse(heart_data$stroke=="1","Y","N")
heart_data$stroke=factor(heart_data$stroke)
#split Train and test

set.seed(42)
heart_trn_idx = sample(nrow(heart_data), size = 0.8 * nrow(heart_data))
heart_trn = heart_data[heart_trn_idx, ]
heart_tst = heart_data[-heart_trn_idx, ]

#split estimation and validation 
heart_est_idx = sample(nrow(heart_trn), size = 0.8 * nrow(heart_trn))
heart_est = heart_trn[heart_est_idx, ]
heart_val = heart_trn[-heart_est_idx, ]

#check data
head(heart_trn, n = 10)

levels(heart_data$stroke)
prop.table(table(heart_data$stroke))

#GGally::ggpairs(heart_data, progress = FALSE)
#----------------------------------------------------------------------------------
#criteria function to calculate only Sensitivity and accuracy(two important criteria)

calc_criteria = function(pred1) {
  
  TP = sum(heart_val$stroke == "Y" & pred1 == "Y")
  TN = sum(heart_val$stroke== "N" & pred1 == "N")
  FP = sum(heart_val$stroke == "N" & pred1 == "Y")
  FN = sum(heart_val$stroke == "Y" & pred1 == "N")
  P = TP + FN
  N = TN + FP
  
  c(acc = (TP + TN) / (P+N),
    sens = TP/P)
  
}
#-------------------------------------------------------------------------------
#NONE PARAMETRIC METHODES
#KNN model
heart_knn_est=heart_est
heart_knn_val=heart_val
#normalization
#Before we start with fitting candidate models of KNN we have to use Scale function to normalize the quantifiable features such
#as age and glucose level.
#age
heart_knn_est$age.s=scale(heart_knn_est$age)
age.center = attr(heart_knn_est$age.s,"scaled:center")
age.scale = attr(heart_knn_est$age.s,"scaled:scale")
#average glucose level
heart_knn_est$avg_glucose_level.s=scale(heart_knn_est$avg_glucose_level)
avg_glucose_level.center = attr(heart_knn_est$avg_glucose_level.s,"scaled:center")
avg_glucose_level.scale = attr(heart_knn_est$avg_glucose_level.s,"scaled:scale")

# normalization of validation data
#age
heart_knn_val$age.s = scale(heart_knn_val$age, center = age.center, scale = age.scale)
#average glucose level
heart_knn_val$avg_glucose_level.s = scale(heart_knn_val$avg_glucose_level, center = avg_glucose_level.center, scale = avg_glucose_level.scale)

#tuning K 
#now after we normalized our features, I created a function to generate K ranging from 1 to 100 and after comparing the 
#misclassifications error,we fit the final model.


#misclassification error function
calc_misclass = function(actual, predicted) {
  mean(actual != predicted)
}

set.seed(42)
knn_mod_list=list()
creat_knn_normal=function(i){
  knn3(stroke~ hypertension+age.s+avg_glucose_level.s+heart_disease+ever_married+smoking_status, data = heart_knn_est, k =i)
}
for(i in 1:20){
  knn_mod_list[i]=list(creat_knn_normal(i))
}
length(knn_mod_list)
#validation prediction
knn_val_pred=lapply(knn_mod_list,predict,heart_knn_val,type="class")
knn_val_mis=sapply(knn_val_pred,calc_criteria)

# plot results
barplot(knn_val_mis,beside = T,legend.text = T,col=c("lightpink","green"))

#In this question ,it is important for us to predict the positive(1) factors correctly, since it's important to know
#whether a person is likely to have stroke or not in order to be able to prevent it. so the criteria we use is based on
#the model with least False positive because we do not want the patient 
#likely to have stroke be left with no alert with wrong prediction.
#As the result shows, the KNN with k=1 with the 90% accuracy and 12% sensitivity is the candid model.
#-------------------------------------------------------------------------------------------------

#Regression Tree
#tree
tree_mod_list = list(
  tree_1 = rpart(stroke~ ., data = heart_est, cp = 0.000,minsplit=5),
  tree_2 = rpart(stroke~ ., data = heart_est, cp = 0.001,minsplit=5),
  tree_3 = rpart(stroke~ ., data = heart_est, cp = 0.01,minsplit=5),
  tree_4 = rpart(stroke~ ., data = heart_est, cp = 0.1,minsplit=5),
  tree_5 = rpart(stroke~ ., data = heart_est, cp = 1,minsplit=5),
  tree_6 = rpart(stroke~ ., data = heart_est, cp = 0.000,minsplit=20),
  tree_7 = rpart(stroke~ ., data = heart_est, cp = 0.001,minsplit=20),
  tree_8 = rpart(stroke~ ., data = heart_est, cp = 0.01,minsplit=20),
  tree_9 = rpart(stroke~ ., data = heart_est, cp = 0.1,minsplit=20),
  tree_10 =rpart(stroke~ ., data = heart_est, cp = 1,minsplit=20)
)

tree_val_pred=lapply(tree_mod_list,predict,heart_val,type="class")
tree_val_mis=sapply(tree_val_pred,calc_criteria)
barplot(tree_val_mis,beside = T,legend.text = T,col=c("purple","yellow"))
#As illustration shows, the model 2 with accuracy of 91% which is slighlty higher than model 1 and sensitivity of 8% is the candid one.
rpart.plot(tree_mod_list$tree_2)
#-------------------------------------------------------------------------------------------------------------
#PARAMETRIC METHODES
#Logistic Regression

#Stroke=1 is positive for us because it's the important factor in this problem.
# fit model

set.seed(42)
calc_metrics_cutoff = function(probs, cutoff) {
  
  pred1 = factor(ifelse(probs > cutoff, "Y", "N"))
  
  TP = sum(heart_val$stroke == "Y" & pred1 == "Y")
  TN = sum(heart_val$stroke== "N" & pred1 == "N")
  FP = sum(heart_val$stroke == "N" & pred1 == "Y")
  FN = sum(heart_val$stroke == "Y" & pred1 == "N")
  P = TP + FN
  N = TN + FP
  Ps = TP + FP
  Ns = TN + FN
  
  c(acc = (TP + TN) / (P+N),
    sens = TP/P,
    spec = TN/N)
  
}

# trying cutoffs
#model1
reg_mod1= glm(stroke ~ ., data = heart_est, family = binomial)
reg_probs1 = predict(reg_mod1, heart_val, type = "response")
cutoffs = seq(from = 0.1, to = 1, by = 0.2)
results1 = sapply(cutoffs, calc_metrics_cutoff, probs = reg_probs1)
results1 = as.data.frame(t(rbind(cutoffs, results1)))
results1
pred1 = factor(ifelse(reg_probs1 > 0.1, "Y", "N"))
CM1 = table(pred1,heart_val$stroke)

#trying other models
reg_mod2 =glm(stroke ~ age+avg_glucose_level, data = heart_est, family = binomial)
reg_probs2 = predict(reg_mod2, heart_val, type = "response")
cutoffs = seq(from = 0.1, to = 1, by = 0.2)
results2 = sapply(cutoffs, calc_metrics_cutoff, probs = reg_probs2)
results2 = as.data.frame(t(rbind(cutoffs, results2)))
results2
pred2 = factor(ifelse(reg_probs2 > 0.1, "Y", "N"))
CM2 = table(pred2,heart_val$stroke)


reg_mod3 =glm(stroke ~ age+avg_glucose_level+hypertension+heart_disease, data = heart_est ,family = binomial)
reg_probs3 = predict(reg_mod3, heart_val, type = "response")
cutoffs = seq(from = 0.1, to = 1, by = 0.2)
results3 = sapply(cutoffs, calc_metrics_cutoff, probs = reg_probs3)
results3 = as.data.frame(t(rbind(cutoffs, results3)))
results3
pred3 = factor(ifelse(reg_probs3 > 0.1, "Y", "N"))
CM3 = table(pred3,heart_val$stroke)


reg_mod4 =step(glm(stroke ~ .^2, data = heart_est ,family = binomial),trace=FALSE)
reg_probs4 = predict(reg_mod4, heart_val, type = "response")
cutoffs = seq(from = 0.1, to = 1, by = 0.2)
results4 = sapply(cutoffs, calc_metrics_cutoff, probs = reg_probs4)
results4 = as.data.frame(t(rbind(cutoffs, results4)))
results4
pred4 = factor(ifelse(reg_probs4 > 0.1, "Y", "N"))
CM4 = table(pred4,heart_val$stroke)

#As a result of these 4 confusion matrix illustrates base on highest accuracy, the fourth model consist of all features 
#with their binary interaction has the most Accuracy in 
#cutoff 0.5. this cut off also has the sensitivity of 0.04 which means the number of True positives(stroke=1, pred=1)
#are not so much high. out of all the 50 positive(stroke=1), we only have predicted 2 of them right.(1-48/50)=0.04!
# on the other hand , the important factor, specificity is high. specificity indicates on (stroke=0 and pred=0) TN which
#is 768 out of 768, so the specificity is 1 and it means that we make no mistake in prediction of people unlikely to have stroke.
#accuracy is 94% which means that model is doing Alright specially predicting the healthy patients being healthy.
#what matters  most in this modeling is that we should not have high FN. which means we do not say a patients who is likely to have 
#stroke that they are healthy. in this model out of 50 people likely to have stoke, we wrongly predicted 49 of them that they
#are not going to have a stroke, so the model is not good at all.
#so we should switch from the criteria of choosing the models based on the accuracy to highest Sensitivity.
log_table=cbind(
  calc_criteria(pred1),
  calc_criteria(pred2),
  calc_criteria(pred3),
  calc_criteria(pred4)
)
barplot(log_table,beside = T,col=c("lightblue","lightpink"),legend.text = T)
#as we can see the models with most sensitivity is model 2 and 3 with 0.54%. how ever our candid Logistic model is model 3, since the 
#second important factor , accuracy is slightly higher.
#-----------------------------------------------------------------------------------------

#DISCRIMINATE

#LDA
lda_mod1=lda(stroke~. , data=heart_est)
lda_mod2=lda(stroke~poly(age+avg_glucose_level ,degree=2),data=heart_est)
lda_mod3=lda(stroke~ . , data=heart_est,prior=c(1,1)/2)
lda_mod4=lda(stroke~hypertension+heart_disease+age+avg_glucose_level,data=heart_est)
lda_list=list(
  pred_lda1=predict(lda_mod1,heart_val)$class,
  pred_lda2=predict(lda_mod2,heart_val)$class,
  pred_lda3=predict(lda_mod3,heart_val)$class,
  pred_lda4=predict(lda_mod4,heart_val)$class
  
)
attach(lda_list)
miss_val=sapply(lda_list,calc_misclass,actual=heart_val$stroke)

table_lda=rbind(calc_criteria(pred_lda1),
      calc_criteria(pred_lda2),
      calc_criteria(pred_lda3),
      calc_criteria(pred_lda4)
)
barplot(t(table_lda),besid=T,legend.text = T,col=c("Yellow","Blue"))
#As we can see the best model out of lda model is the one with uniform prior probabilities  , since our main criteria here
#is not misclassifiaction or accuracy, we are looking for the model with highest sensitivity because it is 
#important to predict right in order to raise survival rate of patients.but since the model is not perfect,we chose the third LDA model 
#with uniform prior probabilities which had 77% accuracy and 68% sensitivity and doing better than other models in this criteria.
#QDA

qda_mod1=qda(stroke~. , data=heart_est)
qda_mod2=qda(stroke~poly(age+avg_glucose_level ,degree=2),data=heart_est)
qda_mod3=qda(stroke~ . , data=heart_est,prior=c(1,1)/2)
qda_mod4=qda(stroke~hypertension+heart_disease+age+avg_glucose_level,data=heart_est)
qda_list=list(
  pred_qda1=predict(qda_mod1,heart_val)$class,
  pred_qda2=predict(qda_mod2,heart_val)$class,
  pred_qda3=predict(qda_mod3,heart_val)$class,
  pred_qda4=predict(qda_mod4,heart_val)$class
  
)
attach(qda_list)
miss_val=sapply(qda_list,calc_misclass,actual=heart_val$stroke)

table_qda=rbind(calc_criteria(pred_qda1),
                calc_criteria(pred_qda2),
                calc_criteria(pred_qda3),
                calc_criteria(pred_qda4)
)
barplot(t(table_qda),besid=T,legend.text = T,col=c("green","pink"))

#since our best model must have the highest sensitivity, The third model with accuracy of 76% and sensitivity of 70% is our candid model.

#Naive Bayes

nb_mod1=naiveBayes(stroke~. , data=heart_est)
nb_mod2=naiveBayes(stroke~ . , data=heart_est,prior=c(1,1)/2)
nb_mod3=naiveBayes(stroke~hypertension+heart_disease+age+avg_glucose_level,data=heart_est)
nb_list=list(
  pred_nb1=predict(nb_mod1,heart_val),
  pred_nb2=predict(nb_mod2,heart_val),
  pred_nb3=predict(nb_mod3,heart_val)
  
)
attach(nb_list)
miss_val=sapply(nb_list,calc_misclass,actual=heart_val$stroke)

table_nb= rbind(calc_criteria(pred_nb1),
                calc_criteria(pred_nb2),
                calc_criteria(pred_nb3)
)
barplot(t(table_nb),besid=T,legend.text = T,col=c("purple","gold"))

#the three models of Naive Bayes are all slightly different, the result shows that all of them have accuracy of nearly 87% and 
#sensitivity around 35%. between three, the third model is with the most accuracy and sensitivity,
#so we pick the third model in this method.
#-------------------------------------------------------------------------------------------------------------
#Compare All

#creating a list of candid models of all methods

Candid_list=list(
  KNN=knn3(stroke~ hypertension+age.s+avg_glucose_level.s+heart_disease+ever_married+smoking_status, data = heart_knn_est, k =1),
  Tree=rpart(stroke~ ., data = heart_est, cp = 0.001,minsplit=5),
  GLM=glm(stroke ~ age+avg_glucose_level+hypertension+heart_disease, data = heart_est ,family = binomial),
  LDA=lda(stroke~ . , data=heart_est,prior=c(1,1)/2),
  QDA=qda(stroke~ . , data=heart_est,prior=c(1,1)/2),
  NB=naiveBayes(stroke~hypertension+heart_disease+age+avg_glucose_level,data=heart_est)
)
attach(Candid_list)
Pred_list=list(
  pred_knn=predict(KNN,heart_knn_val,type="class"),
  pred_tree=predict(Tree,heart_val,type="class"),
  pred_glm=factor(ifelse(predict(GLM, heart_val, type = "response") > 0.1, "Y", "N")),
  pred_lda=predict(LDA,heart_val)$class,
  pred_qda=predict(QDA,heart_val)$class,
  pred_nb=predict(NB,heart_val)
)
all_sens_val=sapply(Pred_list,calc_criteria)

#visualization
x=c("KNN","Tree","GLM","LDA","QDA","NB")
table1=as.data.frame(cbind(x,all_sens_val[1,]))
plot1=ggplot(data=table1,aes(x=x,y=V2,group=1))+geom_line(color="purple",size=1)+geom_point(col="red",size=3)+theme_light()+
  xlab("Models")+ylab("Accuracy")
table2=as.data.frame(cbind(x,all_sens_val[2,]))
plot2=ggplot(data=table2,aes(x=x,y=V2,group=1))+geom_line(color="lightblue",size=1)+geom_point(col="darkgreen",size=3)+theme_light()+
  xlab("Models")+ylab("sensitivity")
plot3=plot_grid(plot1,plot2)
title3=ggdraw()+draw_label("Accuracy and Sensitivity of each Model")
plot4=plot_grid(title3,plot3,ncol=1,rel_heights=c(0.1, 1))

#AS both result of tables and chart illustrates, the highest sensitivity belongs to model LDA andQDa , since the accuracy of
#model QDA is 1% higher than LDA , we chose QDA model as our final Candid Model.

#fitting the final model on train
Final_mod=qda(stroke~ . , data=heart_trn,prior=c(1,1)/2)
#predict on test
Final_pred=predict(Final_mod,heart_tst,type="response")$class
confusionMatrix(as.factor(Final_pred), heart_tst$stroke, positive = "Y")
# We can see the final Model has the sensitivity of 70% and accuracy of 74% , in general the model is doing alright.

table(Final_pred,heart_tst$stroke)
#------------------------------------------------------------------------------------------------------------------------
#Final remark
#we guess the problem of not accurately predicting the patients with stroke is due to this problem so we use Over Sampling.
#Over Sampling
set.seed(42)
over_data = ovun.sample(stroke~ . , data=heart_trn, method = "over")$data
table(over_data$stroke)

qda.fit.over = qda( stroke ~ . , data = over_data,prior=c(1,1)/2, family = binomial)

pred.over =predict(qda.fit.over, heart_tst , type = "response")$class

confusionMatrix(as.factor(pred.over), heart_tst$stroke, positive = "Y")

#with this change, we can see that sensitivity,specificity and accuracy did not change significantly.

#Under Sampling
under_data = ovun.sample(stroke~ . , data=heart_trn, method = "under")$data
table(under_data$stroke)

qda.fit.under = qda( stroke ~ . , data = under_data,prior=c(1,1)/2, family = binomial)

pred.under =predict(qda.fit.under, heart_tst , type = "response")$class

confusionMatrix(as.factor(pred.under), heart_tst$stroke, positive = "Y")


#with this change, we can see that specificity and accuracy did not change significantly , but the sensitvity went up 2%.