###
setwd("D:/Work/SA Training/Decion Trees/Chaid n cart/Chaid n cart/Chaid")
library(party)
##Read the data in the file
cust_data<-read.csv("Default_On_Payment_CHAID.csv")

# Conditional Inference Tree for Default_On_Payment
fit <- ctree(Default_On_Payment~Status_Checking_Acc+Credit_History, 
             data=cust_data )
plot(fit, main="Conditional Inference Tree for Default_On_Payment ")

###detailed results including splits
summary(fit)

