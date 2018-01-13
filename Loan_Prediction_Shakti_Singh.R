install.packages('party')
library(party)
library(caret)


library(ggplot2)
library(MASS)
library(car)
library(logit)
library(sqldf)
library(Hmisc)
library(dplyr)
library(HH)
library(gmodels)
library(rms)
#LOAD THE DATA
Dec_tree <- read.csv('Av.csv')
summary(Dec_tree)
#FIND OUT THE NA'S PRESENT IN THE DATA
sapply(Dec_tree,function(x) sum(is.na(x))
#CONVERT FACTORS INTO CHARACTER VARIABLES,CONVERT BLANK SPACES INTO NA'S AND THE AGAIN
#MAKE THE VARIABLES FACTOR AGAIN.
    
  
Dec_tree$Married<- as.character(Dec_tree$Married)
Dec_tree$Married[Dec_tree$Married ==''] <- NA
Dec_tree$Married <- as.factor(Dec_tree$Married)
Dec_tree$Gender[Dec_tree$Gender == ""] <- NA
Dec_tree$Gender <- as.character(Dec_tree$Gender)
Dec_tree$Gender[Dec_tree$Gender==''] <- NA
Dec_tree$Gender <- as.factor(Dec_tree$Gender)
Dec_tree$Dependents <- as.character(Dec_tree$Dependents)
Dec_tree$Dependents[Dec_tree$Dependents==""] <- NA
Dec_tree$Dependents <- as.factor(Dec_tree$Dependents)
Dec_tree$Self_Employed <- as.character(Dec_tree$Self_Employed)
Dec_tree$Self_Employed[Dec_tree$Self_Employed==""] <- NA
Dec_tree$Self_Employed <- as.factor(Dec_tree$Self_Employed)
summary(Dec_tree)
#INSTALL MICE FOR DATA IMPUTATION
install.packages('mice')
library(mice)
imputed_Data <- mice(Dec_tree, m=5, maxit = 50, method = 'pmm', seed = 500)
summary(imputed_data)
#USE ANY ONE OF THE 5 TO COMPLETE THE DATA
Dec_tree <- complete(imputed_data,1)
summary(Dec_tree)
categ_vars <- c("Gender", "Married","Dependents","Education","Self_Employed",
                "Loan_Amount_Term","Credit_History","Property_Area")
cont_vars <- c("ApplicantIncome","CoapplicantIncome","LoanAmount")
#UNIVARIATE ANALYSIS OF CATEGORICAL VARS
str(Dec_tree)
Dec_tree$Credit_History <- as.factor(Dec_tree$Credit_History)
table(Dec_tree$Gender)
table(Dec_tree$Married)
table(Dec_tree$Dependents)
table(Dec_tree$Education)
table(Dec_tree$Self_Employed)
table(Dec_tree$Loan_Amount_Term)
table(Dec_tree$Credit_History)
table(Dec_tree$Loan_Status)
Dec_tree_bkup <- Dec_tree
Dec_tree$Loan_Amount_Term <- ifelse(Dec_tree$Loan_Amount_Term<84,84,Dec_tree$Loan_Amount_Term)
table(Dec_tree$Property_Area)

#CONTINUOUS VARIABLE ANALYSIS

plot(Dec_tree$ApplicantIncome)
hist(Dec_tree$ApplicantIncome)
quant_up <- quantile(Dec_tree$ApplicantIncome,c(.90,.92,.94,.96,.98,1))
quant_val <- c('P90','P92','P94','P96','P98','P100')
library(ggplot2)
qplot(y=quant_up,x=quant_val,label= quant_up,geom = c('text','point','line'),hjust=-.25)

hist(Dec_tree[Dec_tree$ApplicantIncome>30000,'ApplicantIncome'])
sum(Dec_tree$ApplicantIncome>14000)
View(Dec_tree[Dec_tree$ApplicantIncome>40000,])

Dec_tree$ApplicantIncome <- ifelse(Dec_tree$ApplicantIncome>15000,15000,Dec_tree$ApplicantIncome)
plot(Dec_tree$ApplicantIncome)
hist(Dec_tree$ApplicantIncome)
hist(log10(Dec_tree$ApplicantIncome))

quant_down <- quantile(Dec_tree$ApplicantIncome,c(0,.01,.03,.05,.07,.09,.1))
quant_val2 <- c('P0','P01','P03','P05','P07','P09','P10')
qplot(x=quant_val2,y=quant_down,label=quant_down,geom = c('text','line','point'))
hist(Dec_tree[Dec_tree$ApplicantIncome<1800,'ApplicantIncome'])
sum(Dec_tree$ApplicantIncome<2000)


Dec_tree$ApplicantIncome <- ifelse(Dec_tree$ApplicantIncome<1500,1500,Dec_tree$ApplicantIncome)
plot(Dec_tree$ApplicantIncome)
hist(Dec_tree$ApplicantIncome)
hist(log10(Dec_tree$ApplicantIncome))
Dec_tree$ApplicantIncome_log <- log10(Dec_tree$ApplicantIncome)
qqnorm(Dec_tree$ApplicantIncome_log)
qqline(Dec_tree$ApplicantIncome_log)

plot(Dec_tree$CoapplicantIncome)
hist(Dec_tree$CoapplicantIncome)


quant_up <- quantile(Dec_tree$CoapplicantIncome,c(.90,.92,.94,.96,.98,1))
quant_val <- c('P90','P92','P94','P96','P98','P100')
qplot(y=quant_up,x=quant_val,label= quant_up,geom = c('text','point','line'),hjust=-.25)
hist(Dec_tree[Dec_tree$CoapplicantIncome>5000,'CoapplicantIncome'])
sum(Dec_tree$CoapplicantIncome>7000)
Dec_tree$CoapplicantIncome <- ifelse(Dec_tree$CoapplicantIncome>0,1,Dec_tree$CoapplicantIncome)
Dec_tree$CoapplicantIncome <- as.factor(Dec_tree$CoapplicantIncome)
hist(Dec_tree$CoapplicantIncome)
quant_up <- quantile(Dec_tree$LoanAmount,c(.90,.92,.94,.96,.98,1))
quant_val <- c('P90','P92','P94','P96','P98','P100')
qplot(y=quant_up,x=quant_val,label= quant_up,geom = c('text','point','line'),hjust=-.25)
hist(Dec_tree[Dec_tree$LoanAmount>320,'LoanAmount'])
sum(Dec_tree$LoanAmount>480)
hist(Dec_tree$CoapplicantIncome)
quant_down <- quantile(Dec_tree$LoanAmount,c(0,.01,.03,.05,.07,.09,.1))
quant_val2 <- c('P0','P01','P03','P05','P07','P09','P10')
qplot(x=quant_val2,y=quant_down,label=quant_down,geom = c('text','line','point'),hjust=-.25)
sum(Dec_tree$LoanAmount<30)
Dec_tree$LoanAmount <- ifelse(Dec_tree$LoanAmount<30,30,Dec_tree$LoanAmount)
hist(Dec_tree$LoanAmount)
hist(log10(Dec_tree$LoanAmount))
qqline(log10(Dec_tree$LoanAmount))
qqnorm(log10(Dec_tree$LoanAmount))
Dec_tree$LoanAmount_log <- log10(Dec_tree$LoanAmount)

#AUTOMATED UNIVARIATE ANALYSIS

#UNIVARIATE CATEGORICAL
univar_fn <- function(var)
{
  unidata <- data.frame(table(Dec_tree[,var]))
  unidata$variable <- var
  colnames(unidata) <- c('values','freq','variable')
  return(unidata)
}
empty_data <- data.frame(values=character(),freq=numeric(),variable=character())
for(i in 1:length(categ_vars))
  {
  empty_data <- rbind(empty_data,univar_fn(categ_vars[i]))
}
empty_data$perc <- 100*empty_data$freq/nrow(Dec_tree)
colnames(Dec_tree)

View(empty_data)

library(ggplot2)
ggplot(empty_data,aes(x=values,y= perc))+geom_bar(stat = 'identity')+facet_wrap(~variable,scales ='free')
# UNIVARIATE CONTINUOUS VARS
uni_data_fn <- function(var)
  {
  pctl_tbl <- as.vector(quantile(Dec_tree[,var],c(0,.01,.03,.05,.07,.09,.1,.90,.92,.94,.96,.98,1)))
  pctl_tbl <- data.frame(c('P00','P01','P03','P05','P07','P09','P10','P90','P92','P94','P96','P98','P100'),pctl_tbl)
  pctl_tbl$variable <- var
  colnames(pctl_tbl) <- c('values','freq','variable')
  return(pctl_tbl)
}
empty_data2 <- data.frame(values=character(),freq=numeric(),variable=character())
for(i in 1:length(cont_vars))
{
  empty_data2 <- rbind(empty_data2,uni_data_fn(cont_vars[i]))
}
View(empty_data2)
 empty_data2$perc <- NULL
ggplot(empty_data2,aes(x=values,y=freq))+geom_bar(stat = 'identity')+facet_wrap(~variable,scales = 'free')

#BIVARIATE ANALYSIS CATEGORICAL
library(gmodels)
var <- Dec_tree$Gender
bidata_categ <- function(var,var2)
{
  bidata <- data.frame(CrossTable(var,Dec_tree$Loan_Status))
  bidata <- bidata[bidata$t.y=='Y',c('t.x','prop.row.Freq')]
  bidata$varaible <- var2
  colnames(bidata) <- c('values','freq','variable')
  return(bidata)
}
empty_data3 <- data.frame(values=character(),freq=numeric(),variable=character())
for(i in 1:length(categ_vars)){
 empty_data3 <-  rbind(empty_data3,bidata_categ(Dec_tree[,categ_vars[i]],categ_vars[i]))
}
ggplot(empty_data3,aes(x=values,y=freq))+geom_bar(stat = 'identity')+facet_wrap(~variable,scales = 'free')


#BIVARIAVTE ANALYSIS CONTINUOUS
bidata_fn <- function(var)
{
  bidata2 <- data.frame(aggregate(Dec_tree[,var]~Loan_Status,Dec_tree,mean))
  bidata2$variable <- var
  colnames(bidata2) <- c('values','avg_val','variable')
  return(bidata2)
}
cont_vars
empty_data4 <- data.frame(values=character(),avg_val=numeric(),variable=character())

for(i in 1:length(cont_vars))
{
  empty_data4 <- rbind(empty_data4,bidata_fn(cont_vars[i]))
}
 ggplot(empty_data4,aes(x=values,y=avg_val))+geom_bar(stat = 'identity')+facet_wrap(~variable,scales='free')

#CREATE DERIVED VARIABLES
str(Dec_tree_model)
Dec_tree$D_Gender_Male <- NULL
Dec_tree$D_Dependents_zero <- ifelse(Dec_tree$Dependents==0,1,0)
Dec_tree$D_Dependents_one <- ifelse(Dec_tree$Dependents==1,1,0)
Dec_tree$D_Dependents_two <- ifelse(Dec_tree$Dependents==2,1,0)

Dec_tree$D_Proerty_Area_Rural <- ifelse(Dec_tree$Property_Area=='Rural',1,0)
Dec_tree$D_Proerty_Area_Semiurban <- ifelse(Dec_tree$Property_Area=='Semiurban',1,0)

#VIF VARIABLE REDUCTION

colnames(Dec_tree)
Dec_tree_model <- Dec_tree[,-c(1,4,7,9,12)]
colnames(Dec_tree_model)
fit <- lm(Loan_Status~.,Dec_tree_model)
sort(vif(fit))
colnames(Dec_tree)
fit <- lm(Loan_Status~ Gender+Married+Education+Self_Employed+CoapplicantIncome+
            Credit_History+LoanAmount_log+ApplicantIncome_log+Loan_Amount_Term+
            D_Dependents_zero+D_Dependents_one+D_Dependents_two+D_Loan_Amount_Term_+D_Proerty_Area_RuralD_Proerty_Area_Semiurban,Dec_tree)

fit
str(Dec_tree)

str(Dec_tree)
Dec_tree$Credit_History <- as.factor(Dec_tree$Credit_History)
fit
fit <- lm(Loan_Status~ Gender+Married+Education+Self_Employed+CoapplicantIncome+
            Credit_History+LoanAmount_log+ApplicantIncome_log+Loan_Amount_Term_bnd+
            D_Dependents_zero+D_Dependents_one+D_Dependents_two+D_Loan_Amount_Term_84,Dec_tree)
fit
fit <- lm(Loan_Status~ Gender+Married+Education+Self_Employed+CoapplicantIncome+
            Credit_History+LoanAmount_log+ApplicantIncome_log+Loan_Amount_Term_bnd+
            D_Dependents_zero+D_Dependents_one+D_Dependents_two,Dec_tree)

#INFORMATION VALUE CALCULATION
library(woe)
ivinf_data <- data.frame(iv.mult(Dec_tree_model,"Loan_Status",summary = TRUE))
 

#TRAINING AND TESTING DATA SEPARATION
training_data <- Dec_tree_model
testing_data <- read.csv("av_test.csv")

#LOGISTIC REGRESSION ON FULL DATA

model <- glm(Loan_Status~.,training_data,family=binomial())
summary(model)
colnames(training_data)
model <- glm(Loan_Status~Gender+Education+Self_Employed+
               CoapplicantIncome+Loan_Amount_Term+Credit_History+ApplicantIncome_log+
               LoanAmount_log+D_Dependents_zero+D_Dependents_one+D_Dependents_two+
               D_Proerty_Area_Rural+D_Proerty_Area_Semiurban,training_data,family = binomial())
summary(model)
model <- glm(Loan_Status~Education+
    CoapplicantIncome+Loan_Amount_Term+Credit_History+ApplicantIncome_log+D_Dependents_one+D_Dependents_two+
  LoanAmount_log+D_Proerty_Area_Rural+D_Proerty_Area_Semiurban,training_data,family = binomial())
summary(model)
#DIFF. -2LL OF NULL MODEL AND MODEL WITH VARIABLES
modelChi <- model$null.deviance-model$deviance
modelChi
##Let L0 be the value of the likelihood function for a model
Lo <- model$null.deviance
#with no predictors,

#and let LM be the likelihood for the model being estimated
Lm <- model$deviance
#McFADDEN R2
R2.MCF <- 1-Lm/Lo
1-pchisq(762.89,613)#pvalue for null model
1-pchisq(591.88,603)#pvalue for actual model
1-pchisq(762.89-591.88,613-603)
# a low value means that null model and actual model are

#different from each other



#GET THE PROBABILITY SCORES
 training_data$Scores <- predict(model,training_data,type='response')
View(training_data)

#CONCORDANCE FUNCTION

bino <- model
model$y
model$fitted.values
Concordance <- function(bino)
{
  fitted <- data.frame(cbind(model$y,model$fitted.values))
  colnames(fitted) <- c('loan_status','scores')
  ones <- fitted[fitted$loan_status==1,]
  zeros <- fitted[fitted$loan_status==0,]
  pairstested <- 0
  conc <- 0
  diconc <- 0
  tied <- 0
  for(i in 1:nrow(ones))
  {
    for (j in 1:nrow(zeros))
    {
      pairstested <- pairstested+1
      if(ones[i,2]>zeros[j,2]){conc <- conc+1}
      else if (ones[i,2]==zeros[j,2]){ tied <- tied+1}
      else{diconc <- diconc+1}
    }
  }
  concordance <- conc/pairstested
  discordance <- diconc/pairstested
  tied <- tied/pairstested
  return(list("Concordance"=concordance,
              "Discordance"=discordance,
              "Tied"=tied,
              "Pairs"=pairstested))
}
Concordance(model)

#MODEL VALIDATION
y <- training_data$Loan_Status
yhat <- training_data$Scores
g <- 10

hosmerlem <- function (y, yhat, g = 10)
  
{
  
  cutyhat <- cut(yhat, breaks = quantile(yhat, probs = seq(0,1,1/g)), include.lowest = T)
  obs <- xtabs(cbind(1-y, y) ~ cutyhat)
  expect <- xtabs(cbind(1 - yhat, yhat) ~ cutyhat)
  chisq <- sum((obs - expect)^2/expect)
  P <- 1 - pchisq(chisq, g - 2) 
  c("X^2" = chisq, Df = g - 2, "P(>Chi)" = P)
}
hosmerlem(training_data$Loan_Status,training_data$Scores)
install.packages('pROC')
library(pROC)
rocCurve <- roc(response = training_data$Loan_Status, predictor=training_data$Scores, levels = c("N","Y"))

#plot ROC CURVE

plot(rocCurve$)

#BEST THRESHOLD

coords(rocCurve,'best')
install.packages('ROCR')
library(ROCR)


#CONFUSION MATRIX

predclass1 <- ifelse(training_data$Scores>coords(rocCurve,'best')[1],'Y','N')

confusion <- table(predicted=predclass1,actual=training_data$Loan_Status)

Accuracy <- sum(diag(confusion))/sum(confusion)
auc(rocCurve)
# AUC MATRIX
aucmatrix <- data.frame(c(coords(rocCurve,'best'),AUC=auc(rocCurve),Acc.=Accuracy))
colnames(aucmatrix) <- c('values')

#lIFTCHART
pct_tbl <- NULL
pct_tbl <- as.vector(quantile(training_data$Scores,c(.10,.20,.30,.40,.50,.60,.70,.80,.90)))
pct_tbl <- data.frame(c('P10','P20','P30','P40','P50','P60','P70','P80','P90'),pct_tbl)
colnames(pct_tbl) <- c('quantiles','values')
training_data$Segment <- ifelse(training_data$Scores >
                                 pct_tbl[pct_tbl$quantiles=='P90',2],10,
                                ifelse(training_data$Scores>=
                                         pct_tbl[pct_tbl$quantiles=='P80',2],9,
                                       ifelse(training_data$Scores >=
                                                pct_tbl[pct_tbl$quantiles=='P70',2],8,
                                              ifelse(training_data$Scores >=
                                                       pct_tbl[pct_tbl$quantiles=='P60',2],7,
                                                     ifelse(training_data$Scores >=
                                                              pct_tbl[pct_tbl$quantiles=='P50',2],6,
                                                            ifelse(training_data$Scores >
                                                                     pct_tbl[pct_tbl$quantiles=='P40',2],5,
                                                                   ifelse(training_data$Scores >=
                                                                            pct_tbl[pct_tbl$quantiles=='P30',2],4,
                                                                          ifelse(training_data$Scores >=
                                                                                   pct_tbl[pct_tbl$quantiles=='P20',2],3,
                                                                                 ifelse(training_data$Scores >=
                                                                                          pct_tbl[pct_tbl$quantiles=='P10',2],2,1)))))))))
                                
Liftchart <- data.frame(CrossTable(training_data$Loan_Status,training_data$Segment))
Liftchart <- Liftchart[Liftchart$prop.col.x=='Y',c('t.Freq','prop.col.y')]
colnames(Liftchart) <- c('Freq_of_Y','Bin')

testing_data <- read.csv('av_test.csv')
summary(testing_data)



testing_data$Gender <- as.character(testing_data$Gender)
testing_data$Gender[testing_data$Gender==''] <- NA
testing_data$Gender <- as.factor(testing_data$Gender)
testing_data$Dependents <- as.character(testing_data$Dependents)
testing_data$Dependents[testing_data$Dependents==""] <- NA
testing_data$Dependents <- as.factor(testing_data$Dependents)
testing_data$Self_Employed <- as.character(testing_data$Self_Employed)
testing_data$Self_Employed[testing_data$Self_Employed==""] <- NA
testing_data$Self_Employed <- as.factor(testing_data$Self_Employed)
summary(testing_data)

#IMPUTATION WITH 'MICE'
imputed_Data_1 <- mice(testing_data, m=1, maxit = 1, method = 'pmm', seed = 500)

testing_data <- complete(imputed_Data_1,1)
summary(testing_data)
categ_vars <- c("Gender", "Married","Dependents","Education","Self_Employed",
                "Loan_Amount_Term","Credit_History","Property_Area")
cont_vars <- c("ApplicantIncome","CoapplicantIncome","LoanAmount")
#UNIVARIATE ANALYSIS OF CATEGORICAL VARS
table(testing_data$Gender)
table(testing_data$Married)
table(testing_data$Dependents)
table(testing_data$Education)
table(testing_data$Self_Employed)
table(testing_data$Loan_Amount_Term)
testing_data$Loan_Amount_Term <- ifelse(testing_data$Loan_Amount_Term<84,84,testing_data$Loan_Amount_Term)
table(testing_data$Property_Area)

table(testing_data$Credit_History)

#CONTINUOUS VARIABLE ANALYSIS

plot(testing_data$ApplicantIncome)
hist(testing_data$ApplicantIncome)
quant_up <- quantile(testing_data$ApplicantIncome,c(.90,.92,.94,.96,.98,1))
quant_val <- c('P90','P92','P94','P96','P98','P100')
library(ggplot2)
qplot(y=quant_up,x=quant_val,label= quant_up,geom = c('text','point','line'),hjust=-.25)

hist(testing_data[testing_data$ApplicantIncome>30000,'ApplicantIncome'])
sum(testing_data$ApplicantIncome>14000)

testing_data$ApplicantIncome <- ifelse(testing_data$ApplicantIncome>15000,15000,testing_data$ApplicantIncome)
plot(testing_data$ApplicantIncome)
hist(testing_data$ApplicantIncome)
hist(log10(testing_data$ApplicantIncome))

quant_down <- quantile(testing_data$ApplicantIncome,c(0,.01,.03,.05,.07,.09,.1))
quant_val2 <- c('P0','P01','P03','P05','P07','P09','P10')
qplot(x=quant_val2,y=quant_down,label=quant_down,geom = c('text','line','point'))
hist(testing_data[testing_data$ApplicantIncome<1800,'ApplicantIncome'])
sum(testing_data$ApplicantIncome<1600)


testing_data$ApplicantIncome <- ifelse(testing_data$ApplicantIncome<1500,1500,testing_data$ApplicantIncome)
plot(testing_data$ApplicantIncome)
hist(testing_data$ApplicantIncome)
hist(log10(testing_data$ApplicantIncome))
testing_data$ApplicantIncome_log <- log10(testing_data$ApplicantIncome)
qqnorm(testing_data$ApplicantIncome_log)
qqline(testing_data$ApplicantIncome_log)

plot(testing_data$CoapplicantIncome)
hist(testing_data$CoapplicantIncome)

sum(testing_data$CoapplicantIncome==0)
quant_up <- quantile(testing_data$CoapplicantIncome,c(.90,.92,.94,.96,.98,1))
quant_val <- c('P90','P92','P94','P96','P98','P100')
qplot(y=quant_up,x=quant_val,label= quant_up,geom = c('text','point','line'),hjust=-.25)
hist(testing_data[testing_data$CoapplicantIncome>5000,'CoapplicantIncome'])
sum(testing_data$CoapplicantIncome>6000)
testing_data$CoapplicantIncome <- ifelse(testing_data$CoapplicantIncome>0,1,testing_data$CoapplicantIncome)
testing_data$CoapplicantIncome <- as.factor(testing_data$CoapplicantIncome)
quant_up <- quantile(testing_data$LoanAmount,c(.90,.92,.94,.96,.98,1))
quant_val <- c('P90','P92','P94','P96','P98','P100')
qplot(y=quant_up,x=quant_val,label= quant_up,geom = c('text','point','line'),hjust=-.25)
hist(testing_data[testing_data$LoanAmount>320,'LoanAmount'])
sum(testing_data$LoanAmount>300)
testing_data$LoanAmount <- ifelse(testing_data$LoanAmount>300,300,testing_data$LoanAmount)
hist(testing_data$CoapplicantIncome)
quant_down <- quantile(testing_data$LoanAmount,c(0,.01,.03,.05,.07,.09,.1))
quant_val2 <- c('P0','P01','P03','P05','P07','P09','P10')
qplot(x=quant_val2,y=quant_down,label=quant_down,geom = c('text','line','point'),hjust=-.25)
sum(testing_data$LoanAmount<38)
testing_data$LoanAmount <- ifelse(testing_data$LoanAmount<38,38,testing_data$LoanAmount)
hist(testing_data$LoanAmount)
hist(log10(testing_data$LoanAmount))
qqline(log10(testing_data$LoanAmount))
qqnorm(log10(testing_data$LoanAmount))
testing_data$LoanAmount_log <- log10(testing_data$LoanAmount)

#CREATE DERIVED VARIABLES
str(testing_data_model)
testing_data$D_Dependents_zero <- ifelse(testing_data$Dependents==0,1,0)
testing_data$D_Dependents_one <- ifelse(testing_data$Dependents==1,1,0)
testing_data$D_Dependents_two <- ifelse(testing_data$Dependents==2,1,0)

testing_data$D_Proerty_Area_Rural <- ifelse(testing_data$Property_Area=='Rural',1,0)
testing_data$D_Proerty_Area_Semiurban <- ifelse(testing_data$Property_Area=='Semiurban',1,0)
#CHOOSING DATA FOR PREDICTION
colnames(testing_data)
Loanid <- testing_data$Loan_ID
View(Loanid)
str(testing_data)
testing_data <- testing_data[,-c(1,4,7,9,12)]


#GETTING THE PROBABILITY SCORES
testing_data$Score <- predict(model,testing_data,type="response")
testing_data$Loan_Status <- ifelse(testing_data$Score>=coords(rocCurve,'best')[1],'Y','N')
testing_data$Loan_ID <- Loanid
submission_file <- testing_data[,c('Loan_ID','Loan_Status')]
Loan_prediction <- write.csv(submission_file,'Loan_Prediction.csv',col.names = TRUE)
sum(testing_data$Loan_Status=='Y')/nrow(testing_data)
