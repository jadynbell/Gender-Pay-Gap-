Analytics_Data <- read.csv("Desktop/Analytics Data.csv")
View(Analytics_Data)
class(Analytics_Data)
dim(Analytics_Data)
names(Analytics_Data)
summary(Analytics_Data) # Summary Statistics of the numerical data
hist(Analytics_Data$BasePay)
plot(Analytics_Data$Age, Analytics_Data$BasePay)
Analytics_Data1 <- Analytics_Data[,-c(6,9)] # Deleting columns 6 and 9
View(Analytics_Data1)
age <- Analytics_Data1$Age
age
cut(age, 5)
cut(age, 5, labels = c("Below Age 25", "Age 25-34", "Age 35-44", "Age 45-54","Age 55+") # Setting up age bins 
Analytics_Data1$age_bin <- cut(age, 5, labels = c("Below Age 25", "Age 25-34", "Age 35-44", "Age 45-54","Age 55+"))
sort(age)
Analytics_Data$gender <- ifelse(Analytics_Data$Gender == "Male",1,0) # Using dummy data to make gender a numerical variable
summary_base <- group_by(Analytics_Data1, Gender)
summary_base <- summarise(summary_base, meanBasePay = mean(BasePay, na.rm = TRUE),medBasePay = median(BasePay, na.rm = TRUE),cnt = sum(!(is.na(BasePay))))
View(summary_base) # Difference in mean base pay 
summary_job <- group_by(Analytics_Data1, JobTitle, gender)
summary_job <- summarise(summary_job, meanBasePay = mean(BasePay, na.rm = TRUE),cnt = sum(!(is.na(JobTitle)))) %>% arrange(desc(JobTitle, gender))
View(summary_job) 
table(Analytics_Data1$gender)
counts <- table(Analytics_Data1$gender)
barplot(counts, xlab = "gender", ylab = "Frequency")
Data_female <- filter(Analytics_Data1, gender == 0) # Creating a dataset for only females
View(Data_female)
Data_male <- filter(Analytics_Data1, gender == 1) # Creating a dataset for only males
View(Data_male)
summary(Data_female)
summary(Data_male)
hist(Data_female$BasePay)
hist(Data_male$BasePay)
Data_female %>% ggplot(aes(x = Education, fill = BasePay)) + geom_bar(position = "dodge") + xlab("Education Female")
Data_male %>% ggplot(aes(x = Education, fill = BasePay)) + geom_bar(position = "dodge") + xlab("Education Male")
table(Data_female$Education) 
table(Data_male$Education)
Analytics_Data1$log_base <- log(Analytics_Data1$BasePay, base = exp(1))
model1 <- lm(log_base ~ gender, data = Analytics_Data1) 
model2 <- lm(log_base ~ gender + PerfEval + age_bin + Education, data = Analytics_Data1)
model3 <- lm(log_base ~ gender + PerfEval + age_bin + Education + JobTitle, data = Analytics_Data1)
stargazer(model1, model2, model3, type = "text", out = "results.txt")
job_results <- lm(log_base ~ gender*JobTitle + PerfEval + age_bin + Education, data = Analytics_Data1)
stargazer(job_results, type = "text", out = "job.txt")
