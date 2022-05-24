setwd("C:/Users/new/OneDrive - University of Salford/Desktop/ASDM")
getwd()


# read the data file into a data frame
TELECOM_DATA <-read.csv("Telco_Customer_Churn.csv",header = TRUE)

#inspect my dataset
names(TELECOM_DATA)
head(TELECOM_DATA)
tail(TELECOM_DATA)
dim(TELECOM_DATA)
summary(TELECOM_DATA)
str(TELECOM_DATA)

#creating a new column and converting it to a factor
TELECOM_DATA$ChurnC <- as.factor(TELECOM_DATA$Churn)

#inspecting the new column that has been created
summary(TELECOM_DATA)

#inspecting the new column that has been created and the character variable
#has change from a character with yes and no to 1 and 2 with 2 levels

str(TELECOM_DATA)

#set a seed to reproduce the same vector everytime the function is performed

 set.seed(1234)
TCD<-sample(2, nrow(TELECOM_DATA),replace = TRUE,prob = c(0.8,0.2))

TCD
# training the data set
TRAIN_TCD <- TELECOM_DATA[TCD==1,]
VALIDATE_TCD <- TELECOM_DATA[TCD==2,]

# check the dimension of the training and the validating dataset
dim(TRAIN_TCD)
dim(VALIDATE_TCD)

#installing packages party for the training 
install.packages("party")
library(party)

TELCO_dctree <- ctree(ChurnC~ SeniorCitizen+tenure+MonthlyCharges+TotalCharges, TRAIN_TCD,)

#plot the decision tree
plot(TELCO_dctree)

#Plotting with a simple decision tree

plot(TELCO_dctree,type="simple")
 
#Crating a table to predict the dataset

TELCO_TABLE<- (table(predict(TELCO_dctree),TRAIN_TCD$ChurnC))

print(TELCO_TABLE)

#calculating the accuracy of the trained dataset

sum(diag(TELCO_TABLE))/sum(TELCO_TABLE)

#predict the validate dataframe
PREDICT_VALIDATE<-table(predict(TELCO_dctree,newdata=VALIDATE_TCD),VALIDATE_TCD$ChurnC)
print(PREDICT_VALIDATE)

#Calculate the accuracy of the validate dataset
sum(diag(PREDICT_VALIDATE))/sum(PREDICT_VALIDATE)

#error rate
1-sum(diag(PREDICT_VALIDATE))/sum(PREDICT_VALIDATE)