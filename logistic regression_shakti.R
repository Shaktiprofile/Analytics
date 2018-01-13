getwd()
telecom_data <- read.csv('C:/Users/acer/Downloads/telecom.csv')
summary(telecom_data)


#missing values
apply(telecom_data,2,function(x) sum(is.na(x)))
#remove missing values
telecom_data <- na.omit(telecom_data)
table(telecom_data[,'gender'])
all_continuous_col <- telecom_data[,c('tenure','MonthlyCharges','TotalCharges')]
all_categorical_col <- telecom_data[,-c('tenure','MonthlyCharges','TotalCharges')]
cont_col <- c('tenure','MonthlyCharges','TotalCharges')
categ_col <- c('gender','SeniorCitizen','Partner','Dependents','PhoneService',
               'MultipleLines','InternetService','OnlineSecurity','OnlineBackup',
               'DeviceProtection','TechSupport','StreamingTV','StreamingMovies',
               'Contract','PaperlessBilling','PaymentMethod','Churn')

#univariate analysis categ
table(telecom_data$gender)

#univariate analysis continuous
plot(telecom_data$tenure)
hist(telecom_data$tenure)
plot(telecom_data$MonthlyCharges)
hist(telecom_data$MonthlyCharges)

#bivariate analysis

#  1. continuous
aggregate(tenure~Churn,telecom_data,mean)

#2. categ
table(telecom_data$SeniorCitizen,telecom_data$Churn)

#AUTOMATEd ANALYSIS


unianalysis <- function(var) 
  {
  freq_tbl <- data.frame(table(telecom_data[,var]))
  freq_tbl$variable <- var
  colnames(freq_tbl) <- c('values','freq','variable')
  return(freq_tbl)
}
  
unianalysis(gender)
unidatacateg <- data.frame(values=character(),freq=numeric(),variable=character())
for i in 1:length(categ_col)
{
  rbind(unidatacateg,uniAnalysisCateg(categ_col[i]))
}


uniAnalysisCateg<-function(var)
  
{
  
  Freq_tbl<-data.frame(table(telecom_data[,var]))
  Freq_tbl$var_name <-var
  
  colnames(Freq_tbl)<-c("values","freq","variable")
  
  return(Freq_tbl)
}
uniAnalysisCateg(gender)
library(caret)

Shakti Singh

HOME DATA SCIENCE COURSES ANALYTICS TOOLS COURSES BIG DATA COURSES

library(ggplot2)
library(MASS)
library(car)
library(mlogit)
library(sqldf)
library(Hmisc)
library(dplyr)
library(HH)
library(gmodels)
library(rms)
var <- 'gender'
unianalysis_fn <- function(var)
  {
  freq_tbl <- data.frame(table(telecom_data[,var]))
  freq_tbl$variable <- var
  colnames(freq_tbl) <- c('values','freq','variable')
  return(freq_tbl)
}
unidatacateg <- data.frame(values=character(),freq=numeric(),variable=character())
for (i in 1:length(categ_col))
{
 unidatacateg <-  rbind(unidatacateg,unianalysis_fn(categ_col[i]))
}
uniAnalysisCateg<-function(var)
  
{
  
  Freq_tbl<-data.frame(table(telecom_data[,var]))
  Freq_tbl$var_name <-var
  
  colnames(Freq_tbl)<-c("values","freq","variable")
  
  return(Freq_tbl)
}

uniDataCateg <- data.frame(values = character(),freq =
                             
                             numeric(),variable = character())
for(i in 1 : length(categ_columns)){
  uniDataCateg<-
    
    rbind(uniDataCateg,uniAnalysisCateg(categ_columns[i]))
  
}


uniDataCateg$PERC<- uniDataCateg$freq*100/nrow(telecom_data)

ggplot(uniDataCateg,aes(x=values,y=PERC))+geom_bar(stat = 'identity')+facet_wrap(~variable,scales = 'free')


unianalysiscont <- function(var)
{
pct_tbl <- as.vector(quantile(telecom_data[,var],c(.01,.02,.03,.05,.07,.09,.1,.90,.92,.95,.97,.99,1)))
pct_tbl <- data.frame(c('P01','P02','P03','P05','P07','P09','P10','P90','P92','P95','P97','P99','P100'),pct_tbl)
  pct_tbl$variable <- var
colnames(pct_tbl) <- c('quantile','values','variable')
  return(pct_tbl)
}
unidatacontinuous <- data.frame(quantile=character(),values=numeric(),variable=character())
for(i in 1:length(cont_col)){
    rbind(unidatacontinuous,unianalysiscont(cont_col[i]))
}


ggplot(uni)





uniAnalysisCont<-function(var)
  
{
  
  Pctl_tbl<-as.vector(quantile(telecom_data[,var], probs=c(.01, .10, .20,
                                                   
                                                   .50, .80, .90, .99, 1.0)))
  Pctl_tbl<-
    
    data.frame(c("P001","P010","P020","P050","P080","P090","P099","P100"),Pctl_tbl)
  Pctl_tbl<-data.frame(c(var,var,var,var),Pctl_tbl)
  colnames(Pctl_tbl)<-c("variable","quantiles","values")
  
  return(Pctl_tbl)
}

uniDataCont <- data.frame(variable = character(), quantiles =
                            
                            character(), values = numeric())
for(i in 1 : length(cont_columns)){
  uniDataCont<-
    
    rbind(uniDataCont,uniAnalysisCont(cont_columns[i]))
  
}


ggplot(uniDataCont,aes(x=quantiles,y=values))+geom_bar(stat = 'identity')+facet_wrap(~variable,scales = 'free')

cross <- data.frame(CrossTable(telecom_data$gender,telecom_data$Churn))



#BIVARIATE ANALYSIS
# categorical
bivariatecateg <-function(var) 
  {
  bivaridata <- data.frame(CrossTable(telecom_data[,var],telecom_data$Churn))
bivaridata <- bivaridata[bivaridata$prop.row.y==1,c('t.x','prop.row.Freq')]
bivaridata$variable <- var
colnames(bivaridata) <-c('values','freq','variable') 
return(bivaridata)
}

bivaridataframe <- data.frame(values=character(),freq=numeric(),variable=character())
for(i in 1:length(categ_col))
{
 bivaridataframe <-  rbind(bivaridataframe,bivariatecateg(categ_col[i]))
}


#continuous
bivarcont <- function(var)
{
dataframe <- aggregate(var~Churn,telecom_data,mean)
dataframe2 <- aggregate(MonthlyCharges~Churn,telecom_data,mean)
dataframe <- cbind(dataframe,dataframe2$MonthlyCharges)
}
for(i in 1:length(cont_col))
{

}
#dummy variable creation

 
