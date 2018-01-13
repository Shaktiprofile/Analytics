# Analytics
housing_case <- read.csv('C:/Users/acer/Desktop/Housing case/train.csv')
table(housing_case$Alley)
sum(is.na(housing_case$Alley))
#dependant variable exploration
str(housing_case)
summary(housing_case)
#Saleprice
library(ggplot2)
hist(housing_case$SalePrice)
plot(housing_case$SalePrice)
v <- round(quantile(housing_case$SalePrice,c(.90,.92,.95,.96,.97,.98,.99,1),0)
r <- c(.90,.92,.95,.96,.97,.98,.99,1)
d <- c(.01,.02,.03,.05,.06,.07,.08,.09,.1)
t <- round(quantile(housing_case$SalePrice,c(.01,.02,.03,.05,.06,.07,.08,.09,.1)))
qplot(r,v,label=v,geom =c('line','point','text'),hjust=-.25)
qplot(d,t,label=t,geom=c('text','point','line'),hjust=-.25)
plot(housing_case[housing_case$SalePrice>400000,'SalePrice'])
sum(housing_case$SalePrice>400000)
sum(housing_case$SalePrice<70000)
which(housing_case$SalePrice>700000)
sum
housing_case <- housing_case[housing_case$SalePrice<700000&housing_case$SalePrice>50000,]
hist(log10(housing_case$SalePrice))
plot(density(housing_case$SalePrice))
getOption("scipen")
  <- options("scipen" = 20)
table(housing_case$MSSubClass)
sum(is.na(housing_case$MSSubClass))
hist(log10(housing_case$MSSubClass))
plot(housing_case$MSSubClass)
housing_case$SalePrice_log <- log10(housing_case$SalePrice)
#removing nas

table(housing_case$MSSubClass)
sum(is.na(housing_case$MSSubClass))
plot(housing_case$MSSubClass)
hist(housing_case$MSSubClass)
housing_case$LotFrontage <-   ifelse(is.na(housing_case$LotFrontage),0,housing_case$LotFrontage)
housing_case$Alley <- ifelse(is.na(housing_case$Alley),'None',housing_case$Alley)
housing_case$MasVnrType <- ifelse(is.na(housing_case$MasVnrType),'None',housing_case$MasVnrType)
housing_case$MasVnrType <- ifelse(is.na(housing_case$MasVnrType),"None",housing_case$MasVnrType)
housing_case$MasVnrArea <- ifelse(is.na(housing_case$MasVnrArea),0,housing_case$MasVnrArea)


table(housing_case$BsmtQual)
housing_case$BsmtQual <- ifelse(is.na(housing_case$BsmtQual),"NA",housing_case$BsmtQual)
sum(is.na(housing_case$BsmtQual))

table(housing_case$BsmtCond)
housing_case$BsmtCond <- ifelse(is.na(housing_case$BsmtCond),"NA",housing_case$BsmtCond)
sum(is.na(housing_case$BsmtCond))

table(housing_case$BsmtExposure)
housing_case$BsmtExposure <- ifelse(is.na(housing_case$BsmtExposure),"NA",housing_case$BsmtExposure)
sum(is.na(housing_case$BsmtExposure))

table(housing_case$BsmtFinType1)
housing_case$BsmtFinType1 <- ifelse(is.na(housing_case$BsmtFinType1),"NA",housing_case$BsmtFinType1)
sum(is.na(housing_case$BsmtFinType1))

table(housing_case$BsmtFinType2)
housing_case$BsmtFinType2 <- ifelse(is.na(housing_case$BsmtFinType2),"NA",housing_case$BsmtFinType2)
sum(is.na(housing_case$BsmtFinType2))

table(housing_case$Electrical)
housing_case$Electrical <- ifelse(is.na(housing_case$Electrical),"NA",housing_case$Electrical)
sum(is.na(housing_case$Electrical))

table(housing_case$FireplaceQu)
housing_case$FireplaceQu <- ifelse(is.na(housing_case$FireplaceQu),"NA",housing_case$FireplaceQu)
sum(is.na(housing_case$FireplaceQu))

table(housing_case$GarageType)
housing_case$GarageType <- ifelse(is.na(housing_case$GarageType),"NA",housing_case$GarageType)


housing_case$GarageYrBlt <- ifelse(is.na(housing_case$GarageYrBlt),housing_case$YrSold,housing_case$GarageYrBlt)

table(housing_case$GarageQual)
housing_case$GarageQual <- ifelse(is.na(housing_case$GarageQual),"NA",housing_case$GarageQual)$GarageCond <- ifelse(is.na(LinRegData$GarageCond),"NA",LinRegData$GarageCond)
housing_case$GarageCond <- ifelse(is.na(housing_case$GarageCond),"NA",housing_case$GarageCond)

table(LinRegData$PoolQC)
housing_case$PoolQC <- NULL
fn_chk_col_wit_na()

table(housing_case$Fence)
housing_case$Fence <- ifelse(is.na(housing_case$Fence),"NA",housing_case$Fence)
sum(is.na(housing_case$Fence))

table(housing_case$MiscFeature)
housing_case$MiscFeature <- ifelse(is.na(housing_case$MiscFeature),"NA",housing_case$MiscFeature)


#5. Create derived variables
housing_case$Age <- housing_case$YrSold - housing_case$YearBuilt
housing_case$RenovateAge <- housing_case$YrSold - housing_case$YearRemodAdd
housing_case$RenovateAge <- ifelse(housing_case$RenovateAge<0,0,housing_case$RenovateAge)
housing_case$GarageAge <- housing_case$YrSold - housing_case$GarageYrBlt


#6. Removing the vars not required
housing_case$YrSold <- NULL
housing_case$YearBuilt <- NULL
housing_case$GarageYrBlt <- NULL
housing_case$Id <- NULL
housing_case$YearRemodAdd <- NULL
housing_case$price <- NULL

#6. Identifying cont & categ
columns <- colnames(housing_case)
columns_categ <- columns[-c(1,3,4,24,32,34:36,41:44,59,63:67, 71,75:78)]
columns_cont <- columns[c(1,3,4,24,32,34:36,41:44,59,63:67, 71,75:78)]

str(LinRegData[,columns_categ])
View(housing_case[,columns_categ])
str(LinRegData[,columns_cont])
View(housing_case[,columns_cont])
#6. Univariate analysis - categorical variable
unicategfn <- function(var)
  {
    freq_tbl <- data.frame(table(housing_case[,var]))
    freq_tbl$variable <- var
    colnames(freq_tbl) <- c('values','freq','variable')
    return(freq_tbl)
}
unidtacateg <- data.frame(values=character(),freq=numeric(),variable=character())
for(i in 1:length(columns_categ)){
 unidtacateg <- rbind(unidtacateg,unicategfn(columns_categ[i])) 
}
unidtacateg$prec <- round(100*unidtacateg$freq)/nrow(housing_case)
ggplot(unidtacateg,aes(x=values,y=prec))+geom_bar(stat='identity')+facet_wrap(~variable,scales = 'free')


#univariate continuous
unicontfn <- function(var)
{
  pct_tbl <- as.vector(quantile(housing_case[,var],c(.01,.03,.05,.07,.09,.1,.90,.92,.95,.98,1)))
  pct_tbl <- data.frame(c('P01','P03','P05','P07','P09','P10','P90','P92','P95','P98','P100'),pct_tbl )
  pct_tbl$variable <- var
  colnames(pct_tbl) <- c('values','quantile','variable')
  return(pct_tbl)
}
unidtacont <- data.frame(values=character(),quantile=numeric(),variable=character())
for(i in 1:length(columns_cont)){
unidtacont <- rbind(unidtacont,unicontfn(columns_cont[i]))
}



ggplot(unidtacont,aes(x=values,y=quantile))+geom_bar(stat='identity')+facet_wrap(~variable,scales='free')
#bivariate analysis categ
var <- 'MSZoning'
bivariatecontfn <- function(var)
{
  bidatacont <- data.frame(aggregate(SalePrice~housing_case[,var],housing_case,mean)) 
bidatacont$variable <- var
colnames(bidatacont) <- c('values','salesprice','variable')
}
bidata <- data.frame(values=character(),salesprice=numeric(),variable=character())
  
rbind(bidata,bivariatecontfn(MSZoning))
bivariatecontfn(Street)
#8. Bivariate analysis - continous 
cor_dat <- housing_case[,columns_cont]
View(cor_dat)
cor_csv <- cor(cor_dat,cor_dat)
View(cor_csv)
cor_csv <- abs(cor_csv)
install.packages("gclus")
library(gclus)
cor_color <- dmat.color(cor_csv)
View(cor_color)
cor_csv <- write.csv(cor_csv,'cor.csv')
library(car)
vif_data <- lm(SalePrice_log~.,cor_dat)
vif(vif_data)
cor_dat$SalePrice <- NULL
alias(vif_data)
vif_data <- lm(SalePrice_log~MSSubClass+LotFrontage+LotArea+MasVnrArea+BsmtFinSF1+BsmtFinSF2+BsmtUnfSF+X1stFlrSF+X2ndFlrSF+LowQualFinSF+
                 GarageArea +WoodDeckSF+OpenPorchSF+EnclosedPorch+X3SsnPorch+ScreenPorch+MiscVal+Age+RenovateAge,cor_dat)


vif_data <- lm(SalePrice_log~MSSubClass+LotFrontage+LotArea+MasVnrArea+BsmtFinSF2+BsmtUnfSF+X1stFlrSF+X2ndFlrSF+LowQualFinSF+
                 GarageArea +WoodDeckSF+OpenPorchSF+EnclosedPorch+X3SsnPorch+ScreenPorch+MiscVal+Age+RenovateAge,cor_dat)


vif_data <- lm(SalePrice_log~MSSubClass+LotFrontage+LotArea+MasVnrArea+BsmtFinSF1+BsmtFinSF2+BsmtUnfSF+X1stFlrSF+X2ndFlrSF+LowQualFinSF+
                 GarageArea +WoodDeckSF+OpenPorchSF+EnclosedPorch+X3SsnPorch+ScreenPorch+MiscVal+RenovateAge,cor_dat)
training_data <- housing_case


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

FitLinReg <- lm(SalePrice_log~.,data=training_data)
summary(FitLinReg)
View(predval) <- data.frame(predict(FitLinReg, newdata=training_data))
hist(training_data$residual)
mean(training_data$residual)
plot(training_data$predval,training_data$residual)
rmse <- sqrt(mean((training_data$residual)^2))
mape <- mean(abs(training_data$residual)/training_data$price_log)*100
