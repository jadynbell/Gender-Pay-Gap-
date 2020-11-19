Analytics_Data1$Seniority2<-Analytics_Data1$Seniority^2 #QUADRATIC TRANSFORMATION (2nd ORDER)
Analytics_Data1$Seniority3<-Analytics_Data1$Seniority^3 #CUBIC TRANSFORMATION (3rd ORDER)
Analytics_Data1$ln_Seniority<-log(Analytics_Data1$Seniority) #LOGARITHMIC TRANSFORMATION

#fraction of sample to be used for training
p<-.7

#number of observations (rows) in the dataframe
obs_count<-dim(Analytics_Data1)[1]

#number of observations to be selected for the training partition
#the floor() function rounds down to the nearest integer
training_size <- floor(p * obs_count)
training_size
#set the seed to make your partition reproducible
set.seed(1234)
#create a vector with the shuffled row numbers of the original dataset
train_ind <- sample(obs_count, size = training_size)

Training <- Analytics_Data1[train_ind, ] #pulls random rows for training
Testing <- Analytics_Data1[-train_ind, ] #pulls random rows for testing

#Regression Model 1# 
model1 <- lm(BasePay ~ gender, data = Testing) 
summary(model1)

PRED_1_IN <- predict(model1, Training) #generate predictions on the (in-sample) training data
View(PRED_1_IN)
View(model1$fitted.values) #these are the same as the fitted values

PRED_1_OUT <- predict(model1, Testing) #generate predictions on the (out-of-sample) testing data

RMSE_1_IN<-sqrt(sum((PRED_1_IN-Training$BasePay)^2)/length(PRED_1_IN))  #computes in-sample error
RMSE_1_OUT<-sqrt(sum((PRED_1_OUT-Testing$BasePay)^2)/length(PRED_1_OUT)) #computes out-of-sample 

RMSE_1_IN #IN-SAMPLE ERROR
RMSE_1_OUT #OUT-OF-SAMPLE ERROR

jarque.bera.test(model1$residuals) #TEST FOR NORMLAITY!

#Regression Model 2#
model2 <- lm(BasePay ~ Seniority, data = Testing)
summary(model2)
plot(BasePay ~ Seniority, Analytics_Data1, xlim=c(0,5), ylim=c(34208,179726)) #plot entire dataset 
plot(BasePay ~ Seniority, Training, xlim=c(0,5), ylim=c(34208,179726), col="blue") #plot in-sample training partitio 
plot(BasePay ~ Seniority, Testing, xlim=c(0,5), ylim=c(34208,179726), col="red") #plot out of sample testing partition

PRED_2_IN <- predict(model2, Training) #generate predictions on the (in-sample) training data
View(PRED_2_IN)
View(model2$fitted.values) #these are the same as the fitted values

PRED_2_OUT <- predict(model2, Testing) #generate predictions on the (out-of-sample) testing data

RMSE_2_IN<-sqrt(sum((PRED_2_IN-Training$BasePay)^2)/length(PRED_2_IN))  #computes in-sample error
RMSE_2_OUT<-sqrt(sum((PRED_2_OUT-Testing$BasePay)^2)/length(PRED_2_OUT)) #computes out-of-sample 

RMSE_2_IN #IN-SAMPLE ERROR
RMSE_2_OUT #OUT-OF-SAMPLE ERROR

jarque.bera.test(model2$residuals) #TEST FOR NORMLAITY!

#Regression Model 3#
model3 <- lm(BasePay ~ gender + Seniority + PerfEval + age_bin, data = Testing)
summary(model3)

PRED_3_IN <- predict(model3, Training) #generate predictions on the (in-sample) training data
View(PRED_3_IN)
View(model3$fitted.values) #these are the same as the fitted values

PRED_3_OUT <- predict(model3, Testing) #generate predictions on the (out-of-sample) testing data

RMSE_3_IN<-sqrt(sum((PRED_3_IN-Training$BasePay)^2)/length(PRED_3_IN))  #computes in-sample error
RMSE_3_OUT<-sqrt(sum((PRED_3_OUT-Testing$BasePay)^2)/length(PRED_3_OUT)) #computes out-of-sample 

RMSE_3_IN #IN-SAMPLE ERROR
RMSE_3_OUT #OUT-OF-SAMPLE ERROR

jarque.bera.test(model3$residuals) #TEST FOR NORMLAITY!

#Regression Model 4# 
model4 <- lm(BasePay ~ gender + Seniority + PerfEval + age_bin + Education + JobTitle, data = Testing)
summary(model4)

PRED_4_IN <- predict(model4, Training) #generate predictions on the (in-sample) training data
View(PRED_4_IN)
View(model3$fitted.values) #these are the same as the fitted values

PRED_4_OUT <- predict(model4, Testing) #generate predictions on the (out-of-sample) testing data

RMSE_4_IN<-sqrt(sum((PRED_4_IN-Training$BasePay)^2)/length(PRED_4_IN))  #computes in-sample error
RMSE_4_OUT<-sqrt(sum((PRED_4_OUT-Testing$BasePay)^2)/length(PRED_4_OUT)) #computes out-of-sample 

RMSE_4_IN #IN-SAMPLE ERROR
RMSE_4_OUT #OUT-OF-SAMPLE ERROR

jarque.bera.test(model4$residuals) #TEST FOR NORMLAITY!

#COMPARISON OF IN-SAMPLE MODEL PERFORMANCE BY RMSE
RMSE_1_IN #MODEL WITH ONLY LINEAR TERM
RMSE_2_IN #MODEL WITH LINEAR AND QUADRATIC TERM
RMSE_3_IN #MODEL WITH LINEAR, QUADRATIC, AND CUBIC TERM
RMSE_4_IN #LOGARITHMIC MODEL

#COMPARISON OF OUT-OF-SAMPLE MODEL PERFORMANCE BY RMSE
RMSE_1_OUT #MODEL WITH ONLY LINEAR TERM
RMSE_2_OUT #MODEL WITH LINEAR AND QUADRATIC TERM
RMSE_3_OUT #MODEL WITH LINEAR, QUADRATIC, AND CUBIC TERM
RMSE_4_OUT #LOGARITHMIC MODEL



