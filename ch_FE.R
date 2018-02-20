


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

# Remving duplicates [Billpay_Active_PrevQ1_N, Billpay_Reg_ason_Prev1_N, Charges_cnt_PrevQ1_N, FRX_PrevQ1_N]
dtrain <- dtrain [,-c(330:333)]


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




mea <- apply(dtrain[-ncol(dtrain)],2,mean)
zero_var<- which(mea==0)
na_mean <- which(is.na(mea))
summary(dtrain[,zero_var])
summary(dtrain[,na_mean ])
dtrain <- dtrain [,-c(zero_var,239,240,241,256:282,390)]

dtrain <- data.frame(dtrain[,-331],Responders=dtrain[,331])





#######

test <- read.csv("/home/mkameruddin/churn/test.csv")


cols_missing_data <-c()
for (i in 1:ncol(test))
{ cols_missing_data <- c(cols_missing_data,class(test[,i])=="logical") }
w <- which (cols_missing_data == TRUE) #268 271 276 285 [,308,326 -- extra in test]
test <- test[,-w]

for ( x in 1:ncol(test))
{ m <- which(test[,x]=="")
test[m,x]<-NA}



test <- test[,-c(7,9)] #removing zip and city 

dtest <- test [,-c(357:362)] # removing req, query, and complaint vars

cat1 <- c()
num1 <- c()
for (x in 1:ncol(dtest))
{if (class(dtest[,x])=="factor")
  cat1 <- c(cat1,x)
else num1 <- c(num1,x)}

cat_na1 <- cat1[-c(1,3,4,5,6,28,29,30,31,32,33)] 
for (i in 1:length(cat_na1)) {
  dtest[,cat_na1[i]] <- as.character(dtest[,cat_na1[i]])
  dtest[which(is.na(dtest[cat_na1[i]])==TRUE),cat_na1[i]] <- "N"
  dtest[,cat_na1[i]] <- as.factor(dtest[,cat_na1[i]])
}
summary(dtest[cat_na1])


dtest_all <-dtest
library(ade4)
library(data.table)

cat_vars1 <- names(dtest[,cat1])
for (f in cat_vars1){
  dtest_dummy = acm.disjonctif(dtest_all[f])
  dtest_all[f] = NULL
  dtest_all = cbind(dtest_all, dtest_dummy)
}

dtest <- dtest_all



n <- which(names(dtrain)%in%names(dtest))
names(dtrain)[-n]
dtrain_d <- data.frame(dtrain[,n],Responders=dtrain$Responders)
table((names(dtrain_d)%in%names(dtest)))

dtrain <-dtrain_d


####

write.csv(dtrain,"/home/mkameruddin/churn/train_334v.csv",row.names=F)
write.csv(dtest,"/home/mkameruddin/churn/test_334v.csv",row.names=F)

### modelling using XGB of caret

dtrain <- read.csv("train_334v.csv")
dtest <- read.csv("test_334v.csv")


train <-read.csv("/home/mkameruddin/churn/train.csv")
test <-read.csv("/home/mkameruddin/churn/test.csv")

# creating column for region in train set
substrRight <- function(x){
  substr(train[x,9],1,1)
}
s <- substrRight(1:nrow(train))

tr <- data.frame(Region = s)

library(ade4)
library(data.table)
dummy = acm.disjonctif(tr["Region"])
tr[,1] = NULL
tr = cbind(tr,dummy)

### for test
substrRight1 <- function(x){
  substr(test[x,9],1,1)
}
s1 <- substrRight1(1:nrow(test))

tr1 <- data.frame(Region = s1)

dummy1 = acm.disjonctif(tr1["Region"])
tr1[,1] = NULL
tr_test = cbind(tr1,dummy1)



##### FE 
FE <- read.csv("/home/mkameruddin/mljar-importance-RpOmPogLJ92D.csv",header=T)
FE_316v <- FE[1:316,1]
FE_316v <- as.character (FE_316v)

dtrain <-data.frame(UCIC_ID=dtrain[,1],dtrain[FE_316v], Responders=dtrain[,ncol(dtrain)])
dtest <-data.frame(UCIC_ID=dtest[,1],dtest[FE_316v])


dtrain <- data.frame(dtrain[,-ncol(dtrain)],tr, Responders = dtrain$Responders)
dtest <- data.frame(dtest, tr_test)



# create index for train/test split
library(caret)
set.seed(7)
train.index <- createDataPartition(dtrain$Responders, p = 0.8, list = FALSE)



dtrain$Responders[dtrain$Responders==1] <-"Y"
dtrain$Responders[dtrain$Responders==0] <-"N"

# perform x/y ,train/test split.
x_train <- dtrain[train.index, -c(1,ncol(dtrain))]
y_train <- as.factor(dtrain[train.index , ncol(dtrain)])

x_test <- dtrain[-train.index, -c(1,ncol(dtrain))]
y_test <- as.factor(dtrain[-train.index, ncol(dtrain)])




# create the training control object. Two-fold CV to keep the execution time under the kaggle
# limit. You can up this as your compute resources allow. 
trControl = trainControl(
  method = 'cv',
  number= 2,
  summaryFunction = defaultSummary,
  classProbs = TRUE,
  verboseIter = TRUE,
  allowParallel = TRUE)

# create the tuning grid. Again keeping this small to avoid exceeding kernel memory limits.
# You can expand as your compute resources allow. 
set.seed(239)
tuneGridXGB <- expand.grid(
  nrounds= c(150),#150,100,250
  max_depth = c(16),#4,6,8,10
  eta = c(0.05),#0.05,0.075, 0.01,0.1
  gamma = c(0),#
  colsample_bytree = c(0.9), #
  subsample = c(0.5), #
  min_child_weight = c(10) #
)

#The final values used for the model were nrounds = 250, max_depth = 8, eta
#= 0.05, gamma = 0, colsample_bytree = 0.5, min_child_weight = 10
#and subsample = 0.5.


library(e1071)
# train the xgboost learner
xgbmod <- train(
  x = x_train,
  y = y_train,
  method = 'xgbTree',
  metric = 'Accuracy',
  trControl = trControl,
  tuneGrid = tuneGridXGB)


xgbmod
#print(Sys.time() - start)

# Diagnostics
print(xgbmod$results)
print(xgbmod$resample)

# make predictions
preds <- predict(xgbmod, newdata = x_test, type = "prob")
preds_final <- predict(xgbmod, newdata = dtest, type = "prob")
preds1 <- predict(xgbmod, newdata = x_test, type = "raw")
preds_final1 <- predict(xgbmod, newdata = dtest, type = "raw")
table(preds1)
table(y_test,preds1)
table(preds_final1)




# prep the predictions for submissions
sub <- data.frame(UCIC_ID= as.integer(dtest[,1]), Responders = preds_final$Y)
w<- which(sub[,2]>0.5)
summary(sub[,2])


# write to csv
write.csv(sub,gzfile("/home/mkameruddin/churn/xgb_t_344v_150_16_0.05_0.9_10.csv.gz"),
          row.names = FALSE)



###ensemble
x_test <- data.frame(x_test,prob_Y = preds$Y)
dtest <- data.frame(dtest,prob_Y = preds_final$Y)

xgbmod1 <- train(
  x = x_test,
  y = y_test,
  method = 'xgbTree',
  metric = 'Accuracy',
  trControl = trControl,
  tuneGrid = tuneGridXGB)


xgbmod1
#print(Sys.time() - start)

# Diagnostics
print(xgbmod1$results)
print(xgbmod1$resample)

# make predictions
pred <- predict(xgbmod1, newdata = x_test, type = "prob")
pred_final <- predict(xgbmod1, newdata = dtest, type = "prob")
pred1 <- predict(xgbmod1, newdata = x_test, type = "raw")
pred_final1 <- predict(xgbmod1, newdata = dtest, type = "raw")
table(pred1)
table(y_test,pred1)
table(pred_final1)










# plot the ROC curve
#roc.plot(y_test_raw, preds$Yes, plot.thres = c(0.02, 0.03, 0.04, 0.05))


# prep the predictions for submissions
sub1 <- data.frame(UCIC_ID= as.integer(dtest[,1]), Responders = pred_final$Y)
w1<- which(sub1[,2]>0.5)
summary(sub1[,2])


# write to csv
write.csv(sub1,gzfile("/home/mkameruddin/churn/xgb_t_ens_334v_150_10_0.05_10.csv.gz"),
          row.names = FALSE)



saveRDS(xgbmod,"xgb_t_all_250_8_0.05_10_0.684.rds")
# my_model <- readRDS("xgb_t_all_250_8_0.05_10_0.684.rds")



#####


