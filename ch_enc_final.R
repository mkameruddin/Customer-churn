


train <-read.csv("/home/mkameruddin/churn/train.csv")




cols_missing_data <-c()
for (i in 1:ncol(train))
{ cols_missing_data <- c(cols_missing_data,class(train[,i])=="logical") }
w <- which (cols_missing_data == TRUE) #268 271 276 285
train <- train[,-w]


for ( x in 1:ncol(train))
{ m <- which(train[,x]=="")
train[m,x]<-NA}



dtrain <- train[,-c(7,9)] #removing zip and city 
dtrain <- dtrain[,-c(359:364)] # removing req, query, and complaint vars


cat <- c()
num <- c()
for (x in 1:ncol(dtrain))
{if (class(dtrain[,x])=="factor")
  cat <- c(cat,x)
else num <- c(num,x)}

# raplace NA of category with Mode
dtrain$OCCUP_ALL_NEW [is.na(dtrain$OCCUP_ALL_NEW)] <- "SELF_EMPLOYED"
dtrain$FINAL_WORTH_prev1 [is.na(dtrain$FINAL_WORTH_prev1)]<- "HIGH"
dtrain$ENGAGEMENT_TAG_prev1 [is.na(dtrain$ENGAGEMENT_TAG_prev1)] <- "LOW"
dtrain$gender_bin [dtrain$gender_bin=="Missin"] <- "Male"



# converting vars to categorical which are meant to be
cat_tobe <- c(262:288,311:324)
for (i in 1:length(cat_tobe))
  dtrain[,cat_tobe[i]] <- as.factor(dtrain[,cat_tobe[i]] )
summary(dtrain[,cat_tobe])

dtrain$FRX_PrevQ1 <- as.factor(dtrain$FRX_PrevQ1)
dtrain$Billpay_Active_PrevQ1 <- as.factor(dtrain$Billpay_Active_PrevQ1)
dtrain$Billpay_Reg_ason_Prev1 <- as.factor(dtrain$Billpay_Reg_ason_Prev1)

# Remving duplicates [Billpay_Active_PrevQ1_N,Billpay_Reg_ason_Prev1_N,Charges_cnt_PrevQ1_N,FRX_PrevQ1_N,AL_DATE]
dtrain <- dtrain [,-c(330:333,313)]

cat <- c()
num <- c()
for (x in 1:ncol(dtrain))
{if (class(dtrain[,x])=="factor")
  cat <- c(cat,x)
else num <- c(num,x)}

# one hot encode the factor levels
dtrain_all <-dtrain
library(ade4)
library(data.table)

cat_vars <- names(dtrain[,cat])
for (f in cat_vars){
  dtrain_dummy = acm.disjonctif(dtrain_all[f])
  dtrain_all[f] = NULL
  dtrain_all = cbind(dtrain_all, dtrain_dummy)
}

dtrain <-dtrain_all

write.csv(dtrain,"train_ch_final_cleaned.csv",row.names=F)
dtrain <- read.csv("train_ch_final_cleaned.csv")


mea <- apply(dtrain,2,mean)
zero_var<- which(mea==0)
summary(dtrain[,zero_var])

dtrain <- dtrain [,-zero_var]

dtrain <- data.frame(dtrain[,-245],Responders=dtrain[,245])

write.csv(dtrain,"train_ch_enc_final.csv",row.names=F)

############# For test data

test <- read.csv("/home/mkameruddin/churn/test.csv")

cols_missing_data1 <-c()
for (i in 1:ncol(test))
{ cols_missing_data1 <- c(cols_missing_data1,class(test[,i])=="logical") }
w1 <- which (cols_missing_data1 == TRUE) #268 271 276 285 [308,326]
test<- test[,-w1]


for ( x in 1:ncol(test))
{ m1 <- which(test[,x]=="")
test[m1,x]<-NA}



dtest<- test[,-c(7,9)] #removing zip and city 
dtest <- dtest[,-c(357:362)] # removing req, query, and complaint vars


cat1 <- c()
num1 <- c()
for (x in 1:ncol(dtest))
{if (class(dtest[,x])=="factor")
  cat1 <- c(cat1,x)
else num1 <- c(num1,x)}

# raplace NA of category with Mode
dtest$OCCUP_ALL_NEW [is.na(dtest$OCCUP_ALL_NEW)] <- "SELF_EMPLOYED"
dtest$FINAL_WORTH_prev1 [is.na(dtest$FINAL_WORTH_prev1)]<- "HIGH"
dtest$ENGAGEMENT_TAG_prev1 [is.na(dtest$ENGAGEMENT_TAG_prev1)] <- "LOW"
dtest$gender_bin [dtest$gender_bin=="Missin"] <- "Male"



# converting vars to categorical which are meant to be
cat_tobe1 <- c(262:288,310:322)
for (i in 1:length(cat_tobe1))
  dtest[,cat_tobe1[i]] <- as.factor(dtest[,cat_tobe1[i]] )
summary(dtest[,cat_tobe1])

dtest$FRX_PrevQ1 <- as.factor(dtest$FRX_PrevQ1)
dtest$Billpay_Active_PrevQ1 <- as.factor(dtest$Billpay_Active_PrevQ1)
dtest$Billpay_Reg_ason_Prev1 <- as.factor(dtest$Billpay_Reg_ason_Prev1)

# Remving duplicates [Billpay_Active_PrevQ1_N,Billpay_Reg_ason_Prev1_N,Charges_cnt_PrevQ1_N,FRX_PrevQ1_N,AL_DATE]
dtest <- dtest [,-c(328:331,312)]

cat1 <- c()
num1 <- c()
for (x in 1:ncol(dtest))
{if (class(dtest[,x])=="factor")
  cat1 <- c(cat1,x)
else num1 <- c(num1,x)}


# one hot encode the factor levels
dtest_all <-dtest 
library(ade4)
library(data.table)

cat_vars1 <- names(dtest[,cat1])
for (f in cat_vars1){
  dtest_dummy = acm.disjonctif(dtest_all[f])
  dtest_all[f] = NULL
  dtest_all = cbind(dtest_all, dtest_dummy)
}

dtest <-dtest_all

write.csv(dtrain,"train_ch_final_cleaned.csv",row.names=F)
dtrain <- read.csv("train_ch_final_cleaned.csv")


mea1 <- apply(dtest,2,mean)
zero_var1<- which(mea1==0)
summary(dtest[,zero_var1])

dtest <- dtest [,-zero_var1]


n <- which(names(dtrain)%in%names(dtest))
names(dtrain)[-n]
dtrain_d <- data.frame(dtrain[,n],Responders=dtrain$Responders)
table((names(dtrain_d)%in%names(dtest)))

dtrain <-dtrain_d



write.csv(dtrain,"train_ch_enc_final1.csv",row.names=F)
write.csv(dtest,"test_ch_enc_final1.csv",row.names=F)

write.csv(dtrain, gzfile("train_ch_enc_final1.csv.gz"),row.names=F)
write.csv(dtest, gzfile("test_ch_enc_final1.csv.gz"),row.names=F)


