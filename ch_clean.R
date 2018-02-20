

library(data.table)
train <-read.csv("/home/mkameruddin/churn/train.csv")



cols_missing_data <-c()
for (i in 1:ncol(train))
{ cols_missing_data <- c(cols_missing_data,class(train[,i])=="logical") }
w <- which (cols_missing_data == TRUE) #268 271 276 285
train <- train[,-w]

for ( x in 1:ncol(train))
{ m <- which(train[,x]=="")
train[m,x]<-NA}

cat <- c()
num <- c()
for (x in 1:ncol(train))
{if (class(train[,x])=="factor")
  cat <- c(cat,x)
else num <- c(num,x)}

train <- train[,-c(7,9)] #removing zip and city 


cat_ch <- c()
for (x in 1:ncol(dtrain))
{cat_ch <-c(cat_ch,class(dtrain[,x]))}


write.csv(train,"/home/mkameruddin/churn/train_ch_cleaned.csv",row.names=F)
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

cat1 <- c()
num1 <- c()
for (x in 1:ncol(test))
{if (class(test[,x])=="factor")
  cat1 <- c(cat1,x)
else num1 <- c(num1,x)}

test <- test[,-c(7,9)] #removing zip and city 

write.csv(test,"/home/mkameruddin/churn/test_ch_cleaned.csv",row.names=F)


########

library(data.table)
train <- read.csv("train_ch_cleaned.csv")


dtrain <- train[,-c(7,9)] #removing zip and city 

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
dtrain <- data.frame(dtrain[,-334],Responders=dtrain[,334])
####

test <- read.csv("test_ch_cleaned.csv")

# collecting categorical vars names
cat1 <- c()
num1 <- c()
for (x in 1:ncol(test))
{if (class(test[,x])=="factor")
  cat1 <- c(cat1,x)
else num1 <- c(num1,x)}

# One-Hot encoding
dtest_all <-test
library(ade4)
library(data.table)

cat_vars1 <- names(test[,cat1])
for (f in cat_vars1){
  dtest_dummy = acm.disjonctif(dtest_all[f])
  dtest_all[f] = NULL
  dtest_all = cbind(dtest_all, dtest_dummy)
}

dtest <- dtest_all


# Making sure all vars of train in test
n <- which(names(dtrain)%in%names(dtest))
names(dtrain)[-n]
dtrain_d <- data.frame(dtrain[,n],Responders=dtrain[,450])
table((names(dtrain_d)%in%names(dtest)))
dtrain <-dtrain_d
####

write.csv(dtrain,"/home/mkameruddin/churn/train_ch_encoded.csv",row.names=F)
write.csv(dtest,"/home/mkameruddin/churn/test_ch_encoded.csv",row.names=F)

### modelling using XGB of caret

train <- read.csv("train_ch_encoded.csv")
test <- read.csv("test_ch_encoded.csv")

#FE <- read.csv("mljar-importance-2PV4XAgjXeLr-gbm.csv",header=T) # LB score = 0.6857
#FE_313v <- FE[1:313,1]
#FE_313v <- as.character (FE_313v)
#dtrain1 <-data.frame(UCIC_ID=train[,1],train[FE_313v ], Responders=train[,ncol(train)])
#dtest <-data.frame(UCIC_ID=test[,1],test[FE_313v ])



# create index for train/test split
library(caret)
set.seed(7)
train.index <- createDataPartition(dtrain$Responders, p = 0.8, list = FALSE)



dtrain$Responders[dtrain$Responders==1] <-"Y"
dtrain$Responders[dtrain$Responders==0] <-"N"

# perform x/y ,train/test split.
x_train <- dtrain[train.index, -c(1,ncol(dtrain))]
y_train <- as.factor(dtrain[ train.index, ncol(dtrain)])

x_test <- dtrain[-train.index, -c(1,ncol(dtrain))]
y_test <- as.factor(dtrain[-train.index, ncol(dtrain)])




# create the training control object. Two-fold CV to keep the execution time under the kaggle
# limit. You can up this as your compute resources allow. 
trControl = trainControl(
  method = 'cv',
  number=10,
  summaryFunction = defaultSummary,
  classProbs = TRUE,
  verboseIter = TRUE,
  allowParallel = TRUE)

# create the tuning grid. Again keeping this small to avoid exceeding kernel memory limits.
# You can expand as your compute resources allow. 
set.seed(239)
tuneGridXGB <- expand.grid(
  nrounds= c(150),#150,100,250
  max_depth = c(10),#4,6,8,10
  eta = c(0.05),#0.05,0.075, 0.01,0.1
  gamma = c(0),#
  colsample_bytree = c(0.5), #
  subsample = c(0.5), #
  min_child_weight = c(15) #
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


xgbmod[[1]]
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

# plot the ROC curve
#roc.plot(y_test_raw, preds$Yes, plot.thres = c(0.02, 0.03, 0.04, 0.05))


# prep the predictions for submissions
sub <- data.frame(UCIC_ID= as.integer(dtest[,1]), Responders = preds_final$Y)
w<- which(sub[,2]>0.5)
summary(sub[,2])


# write to csv
write.csv(sub,gzfile("/home/mkameruddin/churn/xgb_t_363v_150_10_0.05_15.csv.gz"),
          row.names = FALSE)



saveRDS(xgbmod,"xgb_t_all_250_8_0.05_10_0.684.rds")
# my_model <- readRDS("xgb_t_all_250_8_0.05_10_0.684.rds")




#####


