#install.packages('PackageName')
#install.packages("lubridate")
library(lubridate)

#Set working directory
getwd()
setwd("D:/Google Drive/Work/SA/Training/Resources/R Training/LinearRegression/Step 5 R case/Housing case")

#1. Read data file
LinRegData <-housing_case
dim(LinRegData)
View(LinRegData)
head(LinRegData)
tail(LinRegData)
str(LinRegData)


#2. Change variable format

#3. dependant variable exploration
#change the name of DV from SalePrice to Price
colnames(LinRegData)[81] <- "price"
plot(LinRegData$price)
hist(LinRegData$price)
plot(quantile(LinRegData$price, c(.90, .95, .96, .97, .98, .99, 1)), type="l")
#now using qplot
library(ggplot2)
x_data <- c(.90, .95, .96, .97, .98, .99, 1)
y_data <- round(quantile(LinRegData$price, c(.90, .95, .96, .97, .98, .99, 1)),0)
qplot(x = x_data, y = y_data,label = round(y_data/1000000,1), geom=c("text","point"), hjust=-0.25)
#performing outlier treatment now
hist(LinRegData[LinRegData$price>400000,'price'])
plot(LinRegData[LinRegData$price>400000,'price'])
getOption("scipen")
opt <- options("scipen" = 20)
sum(LinRegData$price>700000)

x_data <- c(0,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1)
y_data <- round(quantile(LinRegData$price, c(0,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1)),1)
qplot(x = x_data, y = y_data,label = y_data, geom=c("text","point"), hjust=-0.25)
hist(LinRegData[LinRegData$price<79000,'price'])
plot(LinRegData[LinRegData$price<79000,'price'])
sum(LinRegData$price<50000)
LinRegData <- LinRegData[LinRegData$price>50000 & LinRegData$price<700000,]

hist(log10(LinRegData$price))
plot(density(log10(LinRegData$price)))

qqnorm(LinRegData$price)
qqline(LinRegData$price)

qqnorm(log10(LinRegData$price))
qqline(log10(LinRegData$price))

LinRegData$price_log <- log10(LinRegData$price)

sum(is.na(LinRegData$price))
View(LinRegData)
#4. remove NAs
fn_chk_col_wit_na <- function(){
  columns_with_na <- sapply(LinRegData,function(x) sum(is.na(x)))
  columns_with_na[columns_with_na>0]
}

fn_chk_col_wit_na()
table(LinRegData$LotFrontage)
 LinRegData$LotFrontage <- ifelse(is.na(LinRegData$LotFrontage),0,LinRegData$LotFrontage)

table(LinRegData$Alley)
LinRegData$Alley <- ifelse(is.na(LinRegData$Alley),"None",LinRegData$Alley)

table(LinRegData$MasVnrType)
LinRegData$MasVnrType <- ifelse(is.na(LinRegData$MasVnrType),"None",LinRegData$MasVnrType)
LinRegData$MasVnrArea <- ifelse(is.na(LinRegData$MasVnrArea),0,LinRegData$MasVnrArea)

table(LinRegData$BsmtQual)
LinRegData$BsmtQual <- ifelse(is.na(LinRegData$BsmtQual),"NA",LinRegData$BsmtQual)

table(LinRegData$BsmtCond)
LinRegData$BsmtCond <- ifelse(is.na(LinRegData$BsmtCond),"NA",LinRegData$BsmtCond)

table(LinRegData$BsmtExposure)
LinRegData$BsmtExposure <- ifelse(is.na(LinRegData$BsmtExposure),"NA",LinRegData$BsmtExposure)

table(LinRegData$BsmtFinType1)
LinRegData$BsmtFinType1 <- ifelse(is.na(LinRegData$BsmtFinType1),"NA",LinRegData$BsmtFinType1)

table(LinRegData$BsmtFinType2)
LinRegData$BsmtFinType2 <- ifelse(is.na(LinRegData$BsmtFinType2),"NA",LinRegData$BsmtFinType2)

fn_chk_col_wit_na()
table(LinRegData$Electrical)
LinRegData$Electrical <- ifelse(is.na(LinRegData$Electrical),"NA",LinRegData$Electrical)

fn_chk_col_wit_na()
table(LinRegData$FireplaceQu)
LinRegData$FireplaceQu <- ifelse(is.na(LinRegData$FireplaceQu),"NA",LinRegData$FireplaceQu)

table(LinRegData$GarageType)
LinRegData$GarageType <- ifelse(is.na(LinRegData$GarageType),"NA",LinRegData$GarageType)

# when garage is not there year built is na, we are replacing it with Yr Sold right now so that later on when we 
# calculate the age of garage from yr sold it become 0
table(LinRegData$GarageYrBlt)
LinRegData$GarageYrBlt <- ifelse(is.na(LinRegData$GarageYrBlt),LinRegData$YrSold,LinRegData$GarageYrBlt)

fn_chk_col_wit_na()
table(LinRegData$GarageQual)
LinRegData$GarageQual <- ifelse(is.na(LinRegData$GarageQual),"NA",LinRegData$GarageQual)
LinRegData$GarageCond <- ifelse(is.na(LinRegData$GarageCond),"NA",LinRegData$GarageCond)

table(LinRegData$PoolQC)
LinRegData$PoolQC <- NULL
fn_chk_col_wit_na()

table(LinRegData$Fence)
LinRegData$Fence <- ifelse(is.na(LinRegData$Fence),"NA",LinRegData$Fence)

table(LinRegData$MiscFeature)
LinRegData$MiscFeature <- ifelse(is.na(LinRegData$MiscFeature),"NA",LinRegData$MiscFeature)

fn_chk_col_wit_na()


#5. Create derived variables
LinRegData$Age <- LinRegData$YrSold - LinRegData$YearBuilt
LinRegData$RenovateAge <- LinRegData$YrSold - LinRegData$YearRemodAdd
LinRegData$RenovateAge <- ifelse(LinRegData$RenovateAge<0,0,LinRegData$RenovateAge)
LinRegData$GarageAge <- LinRegData$YrSold - LinRegData$GarageYrBlt


#6. Removing the vars not required
LinRegData$YrSold <- NULL
LinRegData$YearBuilt <- NULL
LinRegData$GarageYrBlt <- NULL
LinRegData$Id <- NULL
LinRegData$YearRemodAdd <- NULL
LinRegData$price <- NULL

#6. Identifying cont & categ
columns <- colnames(LinRegData)
columns_categ <- columns[-c(1,3,4,24,32,34:36,41:44,59,63:67, 71,75:78)]
columns_cont <- columns[c(1,3,4,24,32,34:36,41:44,59,63:67, 71,75:78)]

str(LinRegData[,columns_categ])
View(LinRegData[,columns_categ])
str(LinRegData[,columns_cont])
View(LinRegData[,columns_cont])

length(columns_categ)
length(columns_cont)


#6. Univariate analysis - categorical variable

uniAnalysisCateg<-function(var)
{
  Freq_tbl<-data.frame(table(LinRegData[,var]))
  Freq_tbl$var_name <-var
  colnames(Freq_tbl)<-c("values","freq","variable")
  return(Freq_tbl)
}
uniDataCateg <- data.frame(values = character(),freq = numeric(),variable = character())
for(i in 1 : length(columns_categ)){
  uniDataCateg<-rbind(uniDataCateg,uniAnalysisCateg(columns_categ[i]))  
}

tot<-nrow(LinRegData)
uniDataCateg$perc<-round(100*uniDataCateg$freq/tot,0)
ggplot(uniDataCateg, aes(x = values, y = perc)) + geom_bar(stat = "identity")+ facet_wrap(~ variable, scales = "free")


uniAnalysisCont<-function(var)
{
  Pctl_tbl<-as.vector(quantile(LinRegData[,var], probs=c(.01, .10, .20, .50, .80, .90, .99, 1.0)))
  Pctl_tbl<-data.frame(c("P001","P010","P020","P050","P080","P090","P099","P100"),Pctl_tbl)
  Pctl_tbl<-data.frame(c(var,var,var,var),Pctl_tbl)
  colnames(Pctl_tbl)<-c("variable","quantiles","values")
  return(Pctl_tbl)
}
uniDataCont <- data.frame(variable = character(), quantiles = character(), values = numeric())
for(i in 1 : length(columns_cont)){
  uniDataCont<-rbind(uniDataCont,uniAnalysisCont(columns_cont[i]))  
}
ggplot(uniDataCont, aes(x = quantiles, y = values)) + geom_bar(stat = "identity")+ facet_wrap(~ variable, scales = "free")


#7. outlier treatment

fn_high_outlier_view <- function(var){
  x_data <- c(0.90, 0.91, 0.92, 0.93, 0.94, 0.95, 0.96, 0.97, 0.98, 0.99, 1)
  y_data <- round(quantile(LinRegData[,var], c(0.90, 0.91, 0.92, 0.93, 0.94, 0.95, 0.96, 0.97, 0.98, 0.99, 1)),1)
  qplot(x = x_data, y = y_data,label = y_data, geom=c("text","point"), hjust=-0.25)
}

fn_low_outlier_view <- function(var){
  x_data <- c(0,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1)
  y_data <- round(quantile(LinRegData[,var], c(0,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1)),1)
  qplot(x = x_data, y = y_data,label = y_data, geom=c("text","point"), hjust=-0.25)
}

fn_high_outlier_view("LotFrontage")
sum(LinRegData$LotFrontage>150)
hist(LinRegData[LinRegData$LotFrontage>150,'LotFrontage'])
LinRegData <- LinRegData[LinRegData$LotFrontage<=200,]
hist(LinRegData$LotFrontage)

fn_high_outlier_view("LotArea")
hist(LinRegData[LinRegData$LotArea>36130,'LotArea'])
sum(LinRegData$LotArea>100000)
LinRegData <- LinRegData[LinRegData$LotArea<=100000,]
hist(LinRegData$LotArea)
hist(log10(LinRegData$LotArea))
LinRegData$LotArea <- log10(LinRegData$LotArea)

# you can check for the others as well
LinRegDataBkp <- LinRegData


#8. Bivariate analysis - continous  
Corr_data<- LinRegData[,columns_cont]
View(Corr_data)
#install.packages("gclus")
library(gclus)
corr_abs <- abs(cor(Corr_data)) # get correlations
corr_color <- dmat.color(corr_abs) # get colors
# reorder variables so those with highest correlation
# are closest to the diagonal
corr_ordered <- order.single(corr_color) 
cpairs(Corr_data, corr_abs, panel.colors=corr_color, gap=.5,
       main="Variables Ordered and Colored by Correlation" )


dta <- Corr_data[,c(20,1:10)] # get data 
dta.r <- abs(cor(dta)) # get correlations
dta.col <- dmat.color(dta.r) # get colors
# reorder variables so those with highest correlation
# are closest to the diagonal
dta.o <- order.single(dta.r) 
cpairs(dta, dta.o, panel.colors=dta.col, gap=.5,main="Variables Ordered and Colored by Correlation" )

dta <- Corr_data[,c(11:23)] # get data 
dta.r <- abs(cor(dta)) # get correlations
dta.col <- dmat.color(dta.r) # get colors
# reorder variables so those with highest correlation
# are closest to the diagonal
dta.o <- order.single(dta.r) 
cpairs(dta, dta.o, panel.colors=dta.col, gap=.5,main="Variables Ordered and Colored by Correlation" )


#Variance Inflation Factor (VIF) check for multi-collinearity. VIF>2 indicates multi-collinearity. Remove one variable at a time.
#install.packages("car")
library("car")
vif_data <- lm(price_log~.,data=Corr_data)
vif(vif_data)
alias(vif_data)
#next iteration
vif_data <- lm(price_log~.,data=Corr_data[,!(colnames(Corr_data) %in% c("TotalBsmtSF", "GrLivArea"))])
sort(vif(vif_data))
#next iteration
vif_data <- lm(price_log~.,data=Corr_data[,!(colnames(Corr_data) %in% c("TotalBsmtSF", "GrLivArea","BsmtFinSF1"))])
sort(vif(vif_data))
#next iteration - final set 1
vif_data <- lm(price_log~.,data=Corr_data[,!(colnames(Corr_data) %in% c("TotalBsmtSF", "GrLivArea","BsmtFinSF1","Age"))])
sort(vif(vif_data))
#next iteration - final set 2
vif_data <- lm(price_log~.,vif_data <- lm(SalePrice_log~MSSubClass+LotFrontage+LotArea+MasVnrArea+BsmtFinSF1+BsmtFinSF2+BsmtUnfSF+X1stFlrSF+X2ndFlrSF+LowQualFinSF+
                                            GarageArea +WoodDeckSF+OpenPorchSF+EnclosedPorch+X3SsnPorch+ScreenPorch+MiscVal+Age+RenovateAge,cor_dat)
               


#Bivariate analysis - categorical

biAnalysisCateg<-function(var)
{
  temp<-data.frame(aggregate(x=LinRegData$price_log, by=LinRegData[var], FUN=mean))
  temp$var_name <-var
  colnames(temp)<-c("Values","Mean_DV","Variable")
  return(temp)
}

biDataCateg <- data.frame(Values = character(), Mean_DV = numeric(),Variable = character())
for(i in 1 : length(columns_categ)){
  biDataCateg<-rbind(biDataCateg,biAnalysisCateg(columns_categ[i]))  
}

ggplot(biDataCateg, aes(x = Values, y = Mean_DV)) + geom_bar(stat = "identity")+ facet_wrap(~ Variable, scales = "free")

training_data <- LinRegData
training_data[,columns_categ] <- lapply(training_data[,columns_categ],as.factor)
str(LinRegData)

#Run model - first iteration
FitLinReg <- lm(price_log~.,data=training_data)
summary(FitLinReg)
# create the dummy vars for the significant category vars. remove the insignificant cont var
# follow abbove steps to build the final model
training_data <- training_data[,!(colnames(training_data) %in% c("LotFrontage", "Alley","LotShape",
                                                                 "LandContour","LotConfig","BldgType",
                                                                 "HouseStyle1","HouseStyle2","RoofMatl",
                                                                 "Exterior1st","Exterior2nd","MasVnrArea",
                                                                 "ExterQual","ExterCond","Foundation","GarageType",
                                                                 "Utilities","RoofStyle","MasVnrType","BsmtExposure",
                                                                 "BsmtFinType1","BsmtFinType2","TotalBsmtSF","Electrical",
                                                                 "LowQualFinSF","GrLivArea","BsmtFullBath","FullBath",
                                                                 "BedroomAbvGr","TotRmsAbvGrd","Fireplaces","FireplaceQu",
                                                                 "GarageFinish","GarageArea","GarageCond","PavedDrive",
                                                                 "X3SsnPorch","Fence","MiscFeature","MoSold","GarageAge","MSSubClass"
                                                                 ))]
# here is how you will create the dummies
table(training_data$MSZoning)
training_data$MSZoningRL <- ifelse(training_data$MSZoning == "FV",1,0)
training_data <- training_data[,!(colnames(training_data) %in% c("MSZoning"))]

FitLinReg <- lm(price_log~.,data=training_data)
summary(FitLinReg)
View(training_data)

#right now building only on the choice of cont vars obtained after vif
training_data <- Corr_data[,!(colnames(Corr_data) %in% c("X1stFlrSF","X2ndFlrSF","LowQualFinSF", "BsmtUnfSF", "BsmtFinSF2","BsmtFinSF1","Age"))]
FitLinReg <- lm(price_log~.,data=training_data)
summary(FitLinReg)

#Get the fitted values & residual on training data
predval <- data.frame(predict(FitLinReg, newdata=training_data))
colnames(predval) <-c("predval")
training_data$predval <- predval$predval
training_data$residual <- training_data$predval - training_data$SalePrice_log

# check for residual normal and mean = 0 assumptions
hist(training_data$residual)
mean(training_data$residual)

#Generate residual metrics on training data
rmse_train <- sqrt(mean((training_data$residual)^2))
mape_train <- mean(abs(training_data$residual)/training_data$price_log)*100
rmse_train
 mape_train

#check for hetroscadasticity
plot(training_data$predval,training_data$residual)

LinRegData_bkp <- LinRegData

#apply the same steps on test data
LinRegData <- read.csv(file = "test.csv", stringsAsFactors = FALSE)
LinRegData$LotFrontage <- ifelse(is.na(LinRegData$LotFrontage),0,LinRegData$LotFrontage)
LinRegData$Alley <- ifelse(is.na(LinRegData$Alley),"None",LinRegData$Alley)
LinRegData$MasVnrType <- ifelse(is.na(LinRegData$MasVnrType),"None",LinRegData$MasVnrType)
LinRegData$MasVnrArea <- ifelse(is.na(LinRegData$MasVnrArea),0,LinRegData$MasVnrArea)
LinRegData$BsmtQual <- ifelse(is.na(LinRegData$BsmtQual),"NA",LinRegData$BsmtQual)
LinRegData$BsmtCond <- ifelse(is.na(LinRegData$BsmtCond),"NA",LinRegData$BsmtCond)
LinRegData$BsmtExposure <- ifelse(is.na(LinRegData$BsmtExposure),"NA",LinRegData$BsmtExposure)
LinRegData$BsmtFinType1 <- ifelse(is.na(LinRegData$BsmtFinType1),"NA",LinRegData$BsmtFinType1)
LinRegData$BsmtFinType2 <- ifelse(is.na(LinRegData$BsmtFinType2),"NA",LinRegData$BsmtFinType2)
LinRegData$Electrical <- ifelse(is.na(LinRegData$Electrical),"NA",LinRegData$Electrical)
LinRegData$FireplaceQu <- ifelse(is.na(LinRegData$FireplaceQu),"NA",LinRegData$FireplaceQu)
LinRegData$GarageType <- ifelse(is.na(LinRegData$GarageType),"NA",LinRegData$GarageType)
LinRegData$GarageYrBlt <- ifelse(is.na(LinRegData$GarageYrBlt),LinRegData$YrSold,LinRegData$GarageYrBlt)
LinRegData$GarageQual <- ifelse(is.na(LinRegData$GarageQual),"NA",LinRegData$GarageQual)
LinRegData$GarageCond <- ifelse(is.na(LinRegData$GarageCond),"NA",LinRegData$GarageCond)
LinRegData$PoolQC <- NULL
LinRegData$Fence <- ifelse(is.na(LinRegData$Fence),"NA",LinRegData$Fence)
LinRegData$MiscFeature <- ifelse(is.na(LinRegData$MiscFeature),"NA",LinRegData$MiscFeature)
LinRegData$Age <- LinRegData$YrSold - LinRegData$YearBuilt
LinRegData$RenovateAge <- LinRegData$YrSold - LinRegData$YearRemodAdd
LinRegData$RenovateAge <- ifelse(LinRegData$RenovateAge<0,0,LinRegData$RenovateAge)
LinRegData$GarageAge <- LinRegData$YrSold - LinRegData$GarageYrBlt
LinRegData$YrSold <- NULL
LinRegData$YearBuilt <- NULL
LinRegData$GarageYrBlt <- NULL
LinRegData$Id <- NULL
LinRegData$YearRemodAdd <- NULL

testing_data <- LinRegData
# performiing prediction on test data 
predval <- data.frame(predict(FitLinReg, newdata=testing_data))
colnames(predval) <-c("predval")
testing_data$predval <- predval$predval
