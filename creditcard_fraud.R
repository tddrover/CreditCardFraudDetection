# ---------------- INSTALLING LIBRARIES REQUIRED ---------------------------
#install.packages("caret")
#install.packages("rpart")
#install.packages("xgboost")
#install.packages("dplyr")
#install.packages("randomForest")
#install.packages("rpart.plot")
#install.packages("pROC")

# ---------------- LOADING LIBRARIES REQUIRED ------------------------------
library(caret)
library(rpart)
library(dplyr)
library(ROSE)
library(randomForest)
library(rpart.plot)
library(xgboost)
library(ggplot2)

# --------------------------------------------------------------------------

# --------------------- Reading the Data set --------------------------------
#credit_card = read.delim(file.choose())
file_name<- paste(getwd(),"/fraudTrain.csv",sep='') 
credit_card<-read.csv(file_name)

# --------------------- Viewing the data set --------------------------------
#View(credit_card)

# - Converting the Class to factor as it has 0 (non-frauds) and 1 (frauds) --
credit_card$is_fraud = as.factor(credit_card$is_fraud)

# -------------- Summarizing the count of the Frauds and Non-Frauds --------
#summary(credit_card$is_fraud)

# --------------------- Checking for any NA values -------------------------
#sum(is.na(credit_card))

# -------------- Separating the frauds and non-frauds into new dfs ---------
credit_card.true = credit_card[credit_card$is_fraud == 0,]
credit_card.false = credit_card[credit_card$is_fraud == 1,]

# --------- Data Visualization on the basis of physically imp features -----
ggplot()+geom_density(data = credit_card.true,aes(x = Time),color="blue",
                      fill="blue",alpha=0.12)+
  geom_density(data = credit_card.false,aes(x = Time),color="red",fill="red",
               alpha=0.12)

ggplot()+geom_density(data = credit_card.true,aes(x = Amount),color="blue",
                      fill="blue",alpha=0.12)+
  geom_density(data = credit_card.false,aes(x = Amount),color="red",fill="red",
               alpha=0.12)

# --------- PIE CHART for comparing no.of frauds and non-frauds ------------

#labels = c("NON_FRAUD","FRAUD")
#labels = paste(labels,round(prop.table(table(credit_card$is_fraud))*100,2))
#labels = paste0(labels,"%")
#pie(table(credit_card$is_fraud),labels,col = c("blue","red"),
 #   main = "Pie Chart of Credit Card Transactions")

# ---------------------- DATA SPLITTING -------------------------------------
rows = nrow(credit_card)
cols = ncol(credit_card)

set.seed(123)
credit_card = credit_card[sample(rows),1:cols]
ntr = as.integer(round(0.8*rows))

credit_card.train = credit_card[1:ntr,1:cols] # for train
credit_card.test = credit_card[(ntr+1):rows,-cols] # for test input
credit_card.testc = credit_card[(ntr+1):rows,cols] # for test data CLass

credit_card.testc = as.data.frame(credit_card.testc)
colnames(credit_card.testc)[1] = c("is_fraud")



# -------------------- RANDOM FOREST ALGORITHM -----------------------------
samp = as.integer(0.49*ntr)
rF = randomForest(is_fraud ~ . ,data =credit_card.train,ntree = 39,
                  samplesize = samp,maxnodes=44)
rF_pred = predict(rF,credit_card.test)
credit_card.testc$Pred = rF_pred

print(confusionMatrix(credit_card.testc$Pred,credit_card.testc$is_fraud))

#roc.curve(credit_card.testc$is_fraud,credit_card.testc$Pred,plotit = TRUE,
 #         col="green",main = "ROC curve for Random Forest Algorithm",
  #        col.main="darkgreen")

# ------------------------- THE END ----------------------------------------