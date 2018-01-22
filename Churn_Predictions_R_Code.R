
# ID: 1430 BATCH 31 SREENIVASA ETLA
#=====================================================================

getwd()

# LIBRARY Initialization
library(MASS) 
library(ROCR)
library(Metrics)
library(car)
library(caret)
library(Amelia)
library(mice)
library(DMwR)
library(dummies)
library(vegan)
library(glmnet)
library(dplyr)
library(reshape2)
library(gdata)
library(randomForest)

#======================================================
# DATA PREPERATION
#======================================================


# -------------------TRAIN_DATA-----------------

#Data Import
Train_AccountInfo <- read.csv("Train_AccountInfo.csv",na.strings = c("MISSINGVAL","NA","?","","MISSINGVAL"))
Train_Demographics <- read.csv("Train_Demographics.csv",na.strings = c("MISSINGVAL","NA","?","","MISSINGVAL"))
Train_trainresults <- read.csv("Train.csv",na.strings = c("MISSINGVAL","NA","?","","MISSINGVAL"))
Train_ServicesOptedFor <- read.csv("Train_ServicesOptedFor.csv",na.strings = c("MISSINGVAL","NA","?","","MISSINGVAL"))

# understanding the Data
dim(Train_Demographics); dim(Train_AccountInfo); dim(Train_ServicesOptedFor)
summary(Train_Demographics)
summary(Train_AccountInfo)
summary(Train_ServicesOptedFor)
summary(Train_trainresults)
str(Train_Demographics)
str(Train_AccountInfo)
str(Train_ServicesOptedFor)
str(Train_trainresults)
sum(is.na(Train_Demographics))
sum(is.na(Train_AccountInfo))
sum(is.na(Train_ServicesOptedFor))
sum(is.na(Train_trainresults))
missmap(Train_Demographics)
missmap(Train_AccountInfo)
missmap(Train_ServicesOptedFor)
missmap(Train_trainresults)

# Rename householdID to CustomerID in Demographics
Train_Demographics <- rename.vars(Train_Demographics, from = "HouseholdID", to = "CustomerID")

# Using reshape2 and dcast function to modift Servicesopted because there are 47682 rows 
Train_ServicesOptedFor_dcast <- dcast(Train_ServicesOptedFor, CustomerID ~ Train_ServicesOptedFor$TypeOfService)

# Missing NA values imputation thru Knn method
Train_AccountInfo <- knnImputation(Train_AccountInfo,k=3)
Train_Demographics <- knnImputation(Train_Demographics,k=3)
sum(is.na(Train_Demographics))
sum(is.na(Train_AccountInfo))

#merge() - To merge the datasets using columns

Train_merge_1 <- merge(Train_AccountInfo,Train_trainresults,by ="CustomerID")
Train_merge_2 <- merge(Train_merge_1,Train_ServicesOptedFor_dcast,by ="CustomerID")
Train_final_DF <- merge(Train_merge_2,Train_Demographics,by ="CustomerID")

sum(is.na(Train_final_DF))
summary(Train_final_DF)
missmap(Train_final_DF)


# visualizations 

Train_final_DF_Plots <- Train_final_DF
library(ggplot2)
qplot(Train_final_DF_Plots$ContractType, Train_final_DF_Plots$TotalCharges, data=Train_final_DF_Plots)
ggplot(Train_final_DF_Plots, aes(x=ContractType)) + ggtitle("ContractType") + xlab("ContractType") +geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
ggplot(Train_final_DF_Plots, aes(x=Churn)) + ggtitle("Churn") + xlab("Churn") +  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()


#Removing Country,State,DOC from DF
Train_final_DF <- subset(Train_final_DF,select = -c(DOC,State,Country))
Train_final_DF$DOE<-as.Date(Train_final_DF$DOE,format= "%d-%b-%y")

#Converting Numeric Column HasPhoneService,Retired, HasDependant,HasPartner to Categorical
Train_final_DF$HasPhoneService <- as.factor(Train_final_DF$HasPhoneService)
Train_final_DF$Retired <- as.factor(Train_final_DF$Retired)
Train_final_DF$HasDependents <- as.factor(Train_final_DF$HasDependents)
Train_final_DF$HasPartner <- as.factor(Train_final_DF$HasPartner)
Train_final_DF$DeviceProtection <- as.factor(Train_final_DF$DeviceProtection)
Train_final_DF$InternetServiceCategory <- as.factor(Train_final_DF$InternetServiceCategory)
Train_final_DF$OnlineBackup <- as.factor(Train_final_DF$OnlineBackup)
Train_final_DF$OnlineSecurity <- as.factor(Train_final_DF$OnlineSecurity)
Train_final_DF$StreamingMovies <- as.factor(Train_final_DF$StreamingMovies)
Train_final_DF$StreamingTelevision <- as.factor(Train_final_DF$StreamingTelevision)
Train_final_DF$TechnicalSupport <- as.factor(Train_final_DF$TechnicalSupport)
Train_final_DF$MultipleLines <- as.factor(Train_final_DF$MultipleLines)
str(Train_final_DF)


# ---------------------TEST_DATA -----------------------------

#Data Import
Test_AccountInfo <- read.csv("Test_AccountInfo.csv",na.strings = c("MISSINGVAL","NA","?","","MISSINGVAL"))
Test_Demographics <- read.csv("Test_Demographics.csv",na.strings = c("MISSINGVAL","NA","?","","MISSINGVAL"))
Test_ServicesOptedFor <- read.csv("Test_ServicesOptedFor.csv",na.strings = c("MISSINGVAL","NA","?","","MISSINGVAL"))

# Rename householdID to CustomerID in Demographics
Test_Demographics <- rename.vars(Test_Demographics, from = "HouseholdID", to = "CustomerID")

# Using reshape2 and dcast function to modift Servicesopted because there are 47682 rows 
Test_ServicesOptedFor_dcast <- dcast(Test_ServicesOptedFor, CustomerID ~ Test_ServicesOptedFor$TypeOfService)

# Missing NA values imputation thru Knn method
Test_AccountInfo <- knnImputation(Test_AccountInfo,k=3)
Test_Demographics <- knnImputation(Test_Demographics,k=3)
sum(is.na(Train_Demographics))
sum(is.na(Train_AccountInfo))

#merge() - To merge the datasets using columns
Test_merge_1 <- merge(Test_AccountInfo,Test_ServicesOptedFor_dcast,by ="CustomerID")
Test_final_DF <- merge(Test_merge_1,Test_Demographics,by ="CustomerID")

#Removing Country,State,DOC from DF
Test_final_DF <- subset(Test_final_DF,select = -c(DOC,State,Country))
Test_final_DF$DOE<-as.Date(Test_final_DF$DOE,format= "%d-%b-%y")

#Converting Numeric Column HasPhoneService,Retired, HasDependant,HasPartner to Categorical
Test_final_DF$HasPhoneService <- as.factor(Test_final_DF$HasPhoneService)
Test_final_DF$Retired <- as.factor(Test_final_DF$Retired)
Test_final_DF$HasDependents <- as.factor(Test_final_DF$HasDependents)
Test_final_DF$HasPartner <- as.factor(Test_final_DF$HasPartner)
Test_final_DF$DeviceProtection <- as.factor(Test_final_DF$DeviceProtection)
Test_final_DF$InternetServiceCategory <- as.factor(Test_final_DF$InternetServiceCategory)
Test_final_DF$OnlineBackup <- as.factor(Test_final_DF$OnlineBackup)
Test_final_DF$OnlineSecurity <- as.factor(Test_final_DF$OnlineSecurity)
Test_final_DF$StreamingMovies <- as.factor(Test_final_DF$StreamingMovies)
Test_final_DF$StreamingTelevision <- as.factor(Test_final_DF$StreamingTelevision)
Test_final_DF$TechnicalSupport <- as.factor(Test_final_DF$TechnicalSupport)
Test_final_DF$MultipleLines <- as.factor(Test_final_DF$MultipleLines)


# Churn column is stored in new dataframe in order to add after standardzation/dummification
# Column 'Customer ID' is also removed

Train_Churn_Column <- Train_final_DF$Churn
Train_final_DF <- subset(Train_final_DF,select = -c(Churn,CustomerID))
Test_final_DF <- subset(Test_final_DF,select = -c(CustomerID))
Train_Test_Merged <- rbind(Train_final_DF,Test_final_DF)
Train_Test_Merged_dummy <- dummy.data.frame(Train_Test_Merged) #<- Combining Train & Test
Train_final_temp<- Train_Test_Merged_dummy[1:5298,]
Test_final_temp <- Train_Test_Merged_dummy[5299:7067,]
Standardisation_data_preprocess <- preProcess(x=Train_Test_Merged_dummy,method=c('center','scale')) 
Train_final_DF_STD <- predict(Standardisation_data_preprocess ,Train_final_temp)
Train_final_DF_STD <- cbind(Train_final_DF_STD,Train_Churn_Column)

Test_final_DF_STD <- predict(Standardisation_data_preprocess ,Test_final_temp)
Train_final_DF_STD$Train_Churn_Column
Train_final_DF_STD <- rename.vars(Train_final_DF_STD, from = "Train_Churn_Column", to = "Churn")

# The col names need to have NO special chars and spaces. Hence they are renamed

Train_final_DF_STD <- rename.vars(Train_final_DF_STD, from = "ContractTypeMonth-to-month", to = "ContractTypeMonthtomonth")
Train_final_DF_STD <- rename.vars(Train_final_DF_STD, from = "ContractTypeOne year", to = "ContractTypeOneyear")
Train_final_DF_STD <- rename.vars(Train_final_DF_STD, from = "ContractTypeTwo year", to = "ContractTypeTwoyear")
Train_final_DF_STD <- rename.vars(Train_final_DF_STD, from = "PaymentMethodBank transfer (automatic)", to = "PaymentMethodBanktransferautomatic")
Train_final_DF_STD <- rename.vars(Train_final_DF_STD, from = "PaymentMethodCredit card (automatic)", to = "PaymentMethodCreditcardautomatic")
Train_final_DF_STD <- rename.vars(Train_final_DF_STD, from = "PaymentMethodElectronic check", to = "PaymentMethodElectroniccheck")
Train_final_DF_STD <- rename.vars(Train_final_DF_STD, from = "PaymentMethodMailed check", to = "PaymentMethodMailedcheck")
Train_final_DF_STD <- rename.vars(Train_final_DF_STD, from = "DeviceProtectionNo internet service", to = "DeviceProtectionNointernetservice")
Train_final_DF_STD <- rename.vars(Train_final_DF_STD, from = "MultipleLinesNo phone service", to = "MultipleLinesNophoneservice")
Train_final_DF_STD <- rename.vars(Train_final_DF_STD, from = "OnlineBackupNo internet service", to = "OnlineBackupNointernetservice")
Train_final_DF_STD <- rename.vars(Train_final_DF_STD, from = "OnlineSecurityNo internet service", to = "OnlineSecurityNointernetservice")
Train_final_DF_STD <- rename.vars(Train_final_DF_STD, from = "StreamingMoviesNo internet service", to = "StreamingMoviesNointernetservice")
Train_final_DF_STD <- rename.vars(Train_final_DF_STD, from = "StreamingTelevisionNo internet service", to = "StreamingTelevisionNointernetservice")
Train_final_DF_STD <- rename.vars(Train_final_DF_STD, from = "TechnicalSupportNo internet service", to = "TechnicalSupportNointernetservice")
Train_final_DF_STD <- rename.vars(Train_final_DF_STD, from = "EducationHighschool or below", to = "EducationHighschoolorbelow")
Train_final_DF_STD <- rename.vars(Train_final_DF_STD, from = "InternetServiceCategoryFiber optic", to = "InternetServiceCategoryFiberoptic")
Test_final_DF_STD <- rename.vars(Test_final_DF_STD, from = "ContractTypeMonth-to-month", to = "ContractTypeMonthtomonth")
Test_final_DF_STD <- rename.vars(Test_final_DF_STD, from = "ContractTypeOne year", to = "ContractTypeOneyear")
Test_final_DF_STD <- rename.vars(Test_final_DF_STD, from = "ContractTypeTwo year", to = "ContractTypeTwoyear")
Test_final_DF_STD <- rename.vars(Test_final_DF_STD, from = "PaymentMethodBank transfer (automatic)", to = "PaymentMethodBanktransferautomatic")
Test_final_DF_STD <- rename.vars(Test_final_DF_STD, from = "PaymentMethodCredit card (automatic)", to = "PaymentMethodCreditcardautomatic")
Test_final_DF_STD <- rename.vars(Test_final_DF_STD, from = "PaymentMethodElectronic check", to = "PaymentMethodElectroniccheck")
Test_final_DF_STD <- rename.vars(Test_final_DF_STD, from = "PaymentMethodMailed check", to = "PaymentMethodMailedcheck")
Test_final_DF_STD <- rename.vars(Test_final_DF_STD, from = "DeviceProtectionNo internet service", to = "DeviceProtectionNointernetservice")
Test_final_DF_STD <- rename.vars(Test_final_DF_STD, from = "MultipleLinesNo phone service", to = "MultipleLinesNophoneservice")
Test_final_DF_STD <- rename.vars(Test_final_DF_STD, from = "OnlineBackupNo internet service", to = "OnlineBackupNointernetservice")
Test_final_DF_STD <- rename.vars(Test_final_DF_STD, from = "OnlineSecurityNo internet service", to = "OnlineSecurityNointernetservice")
Test_final_DF_STD <- rename.vars(Test_final_DF_STD, from = "StreamingMoviesNo internet service", to = "StreamingMoviesNointernetservice")
Test_final_DF_STD <- rename.vars(Test_final_DF_STD, from = "StreamingTelevisionNo internet service", to = "StreamingTelevisionNointernetservice")
Test_final_DF_STD <- rename.vars(Test_final_DF_STD, from = "TechnicalSupportNo internet service", to = "TechnicalSupportNointernetservice")
Test_final_DF_STD <- rename.vars(Test_final_DF_STD, from = "EducationHighschool or below", to = "EducationHighschoolorbelow")
Test_final_DF_STD <- rename.vars(Test_final_DF_STD, from = "InternetServiceCategoryFiber optic", to = "InternetServiceCategoryFiberoptic")


#======================================================
# MODEL BUILDING
#======================================================


# ---------------------SPLIT TRAIN into TRAINing and VALidation sets----------------------------

set.seed(1234)
indices <- createDataPartition(y= Train_final_DF_STD$Churn, p = .8, list = F)
Train_final_DF_STD_TRAINSET <- Train_final_DF_STD[indices,]
Train_final_DF_STD_VALSET <- Train_final_DF_STD[-indices,]



#======================================================================
#                MODEL1 : Decision Tree RPART 
#======================================================================

library(rpart)
library(rpart.plot)
model_rpart <- rpart(Churn~., data=Train_final_DF_STD_TRAINSET, method = "class")
summary(model_rpart)
plotcp(model_rpart)
rpart.plot(model_rpart)
predictions <- predict(model_rpart, Train_final_DF_STD_VALSET,type = "class") 
summary(predictions)
confusionMatrix(predictions,Train_final_DF_STD_VALSET[,52],positive = "Yes")
#Predicting on Test
Rpart_TEST_predict <- predict(model_rpart, Test_final_DF_STD,type = "class") 
summary(Rpart_TEST_predict)
#Writing to A File
#write.csv(data.frame(Rpart_TEST_predict), 'predictions.csv', quote=F, row.names = F)

#======================================================================
#               MODEL2 : RANDOM FOREST 
#======================================================================
#Build on Train Train Set
model_RF_fit <- randomForest(Train_final_DF_STD_TRAINSET$Churn ~ ., data=Train_final_DF_STD_TRAINSET, keep.forest=TRUE, ntree=500, replace=TRUE, type = "classification",mtry=10)
model_RF_fit$predicted
varImpPlot(model_RF_fit)
#predict on validation
predict_RF <-predict(model_RF_fit,Train_final_DF_STD_VALSET[,-52])
table(predict_RF,Train_final_DF_STD_VALSET[,52])
confusionMatrix(predict_RF,Train_final_DF_STD_VALSET[,52]) #(c) METRICS
#Predicting on Test data
RF_TEST_predict <- predict(model_RF_fit, Test_final_DF_STD,type = "class") 
summary(RF_TEST_predict)
#Writing to A File
#write.csv(data.frame(RF_TEST_predict), 'predictions.csv', quote=F, row.names = F)

#======================================================================
#               MODEL3 : LOGISTIC REGRESSION 
#======================================================================
library(ROCR)
log_model <- glm(Churn~.,data = Train_final_DF_STD_TRAINSET,family = "binomial")
stepAIC = stepAIC(log_model,direction = "both")
log_Model_AIC <- glm(formula = Churn ~ BaseCharges + TotalCharges + DOE + ElectronicBillingNo + 
                       ContractTypeMonthtomonth + ContractTypeOneyear + PaymentMethodElectroniccheck + 
                       DeviceProtectionNointernetservice + HasPhoneService0 + OnlineBackupNo + 
                       OnlineSecurityNo + TechnicalSupportNo + Retired0 + EducationGraduation + 
                       EducationHighschoolorbelow + EducationMasters + EducationOther + 
                       InternetServiceCategoryDSL, family = "binomial", data = Train_final_DF_STD_TRAINSET)
Predicted_AIC <- predict(log_Model_AIC, Train_final_DF_STD_VALSET,type = "response") 
Predict_Train <- prediction(Predicted_AIC,Train_final_DF_STD_VALSET$Churn)
Train_Performance <- performance(Predict_Train, measure="tpr", x.measure="fpr")
plot(Train_Performance, col=rainbow(10), colorize=T, print.cutoffs.at=seq(0,1,0.05)) # AOC Curve
Predicted_YesNo <- ifelse(Predicted_AIC > 0.25, "Yes", "No")
summary(Predicted_YesNo)
confusionMatrix(Predicted_YesNo,Train_final_DF_STD_VALSET$Churn,positive = "Yes")

#Predicting on Test data
Predicted_AIC_Test <- predict(log_Model_AIC, Test_final_DF_STD,type = "response") 
Predicted_YesNo_Test <- ifelse(Predicted_AIC_Test > 0.25, "Yes", "No")
summary(Predicted_YesNo_Test)
#Writing to A File
write.csv(data.frame(Predicted_YesNo_Test), 'predictions_1.csv', quote=F, row.names = F)


#======================================================================
#               MODEL4 : C50 decision tree  
#======================================================================

library(C50)
# based on stepAIC in Model3 taking only the relevant variables for c50
Train_Train_c50 <- subset(Train_final_DF_STD_TRAINSET,select = c(Churn,BaseCharges,TotalCharges,ElectronicBillingNo,ContractTypeMonthtomonth,ContractTypeOneyear,PaymentMethodElectroniccheck,DeviceProtectionNointernetservice,HasPhoneService0,OnlineBackupNo,OnlineSecurityNo,TechnicalSupportNo,Retired0,EducationGraduation,EducationHighschoolorbelow,EducationMasters,EducationOther,InternetServiceCategoryDSL))
Train_Val_c50   <- subset(Train_final_DF_STD_VALSET,  select = c(Churn,BaseCharges,TotalCharges,ElectronicBillingNo,ContractTypeMonthtomonth,ContractTypeOneyear,PaymentMethodElectroniccheck,DeviceProtectionNointernetservice,HasPhoneService0,OnlineBackupNo,OnlineSecurityNo,TechnicalSupportNo,Retired0,EducationGraduation,EducationHighschoolorbelow,EducationMasters,EducationOther,InternetServiceCategoryDSL))
Test_c50<-subset(Test_final_DF_STD,select = c(BaseCharges,TotalCharges,ElectronicBillingNo,ContractTypeMonthtomonth,ContractTypeOneyear,PaymentMethodElectroniccheck,DeviceProtectionNointernetservice,HasPhoneService0,OnlineBackupNo,OnlineSecurityNo,TechnicalSupportNo,Retired0,EducationGraduation,EducationHighschoolorbelow,EducationMasters,EducationOther,InternetServiceCategoryDSL))

model_c50 <- C5.0(x=Train_Train_c50[,-1], y=Train_Train_c50[,1], rules = T)
model_c50_tree <- C5.0(x=Train_Train_c50[,-1], y=Train_Train_c50[,1])
summary(model_c50)
plot(model_c50_tree)
# Predicting on Validation data
c50_Predict_Val <- predict(model_c50, Train_Val_c50[,-1])
summary(c50_Predict_Val)
table(c50_Predict_Val,Train_Val_c50$Churn)
confusionMatrix(c50_Predict_Val,Train_Val_c50$Churn,positive = "Yes")
# Predicting on test data
c50_Predict_Test <- predict(model_c50, Test_c50)
summary(c50_Predict_Test)
#Writing to A File
#write.csv(data.frame(c50_Predict_Test), 'predictions_1.csv', quote=F, row.names = F)

