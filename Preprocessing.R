rm(list=ls())
setwd("D:/INSOFE/Insofe Lab/CUTe/20180901_Batch_45_CSE_7305c_CUTe_PartB")
 
####Data reading and preparation

raw_data <- read.csv("bankdata.csv",header = T)

## 64 indepenadent variable and one target variable 
colnames(raw_data)  #column names 

str(raw_data)

sum(is.na(raw_data))


#lets check missing values in target.
sum(is.na(raw_data$target))  #NO missing values.

library(DMwR)
omitrow<-manyNAs(raw_data,nORp = 0.2)   #rows with 20% missing values
raw_data<-raw_data[-omitrow,]  #remove rows with 20% missing values
summary(raw_data)
sum(is.na(raw_data))  #nows having missing values

#lets check columns with missing values

raw_data<-raw_data[,-which(colMeans(is.na(raw_data)) > 0.4)]
#removed columns with >40% data missing i.e attr37 column removed
names(raw_data)

sum(is.na(raw_data))

summary(raw_data)



####   Central Imputation
library(DMwR)
clean_data <- centralImputation(raw_data[,!names(raw_data) %in% "target"])
clean_data$target<-raw_data$target
sum(is.na(clean_data))

#### Factor conversion from string to boolean

levels(clean_data$target) <- list('0'="No", '1'="yes")
levels(clean_data$target)
clean_data$target<-as.logical(as.factor(clean_data$target))
typeof(clean_data$target)


####   Standardisation

library(vegan)
std_clean<-decostand(clean_data[,!names(clean_data) %in% "target"],method = "range")
std_clean$target<-clean_data$target


#### create clean standardised data and save for  visualizations

write.csv(std_clean,"Clean_Data.csv",row.names = FALSE)

