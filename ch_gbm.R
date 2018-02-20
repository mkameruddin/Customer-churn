
### modelling using XGB of caret

train <- read.csv("train_ch_encoded.csv")
test <- read.csv("test_ch_encoded.csv")





# Max shrinkage for gbm
nl = nrow(training)
max(0.01, 0.1*min(1, nl/10000))
# Max Value for interaction.depth
floor(sqrt(NCOL(training)))
gbmGrid <-  expand.grid(interaction.depth = c(1, 3, 6, 9, 10),
                        n.trees = c(50,100,150), 
                        shrinkage = c(0.001,0.01,0.1),
                        n.minobsinnode = 10) # you can also put something        like c(5, 10, 15, 20)

fitControl <- trainControl(method = "cv",
                           number = 5,
                           ## Estimate class probabilities
                           classProbs = TRUE,
                           ## Evaluate performance using
                           ## the following function
                           summaryFunction = twoClassSummary)

# Method + Date + distribution
set.seed(1)
system.time(GBM0604ada <- train(Outcome ~ ., data = training,
                                method = "gbm", bag.fraction = 0.5),
                                trControl = fitControl,
                                verbose = TRUE,
                                tuneGrid = gbmGrid,
                                ## Specify which metric to optimize
                                metric = "ROC")

##############




#dtrain <-train [,-c(256:280,390:438)]
#dtest <-test [,-c(256:280,391:445)]


# removing zero var columns
mea <- apply(train[-ncol(train)],2,mean)
zero_var<- which(mea==0)
na_mean <- which(is.na(mea))
summary(train[,zero_var])
summary(train[,na_mean ])
dtrain <- train [,-c(zero_var,239,240,241,256:282,390)]

mea1 <- apply(test,2,mean)
zero_var1<- which(mea==0)
na_mean1 <- which(is.na(mea1))
summary(test[,zero_var1])
summary(test[,na_mean1 ])
dtest<- test [,-c(zero_var1,239,240,241,256:282,391)]


dtrain <- dtrain [,-c(217,351,353,365,349,355,357)]

table(names(dtrain)%in%names(dtest))


FE <- read.csv("/home/mkameruddin/mljar-importance-x13mlyxkl4jw-gbm.csv",header=T)
FE_288v <- FE[1:288,1]
FE_288v <- as.character (FE_288v)

dtrain <-data.frame(UCIC_ID=train[,1],train[FE_288v ], Responders=train[,ncol(train)])
dtest <-data.frame(UCIC_ID=test[,1],test[FE_288v ])



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
  number= 2,
  summaryFunction = twoClassSummary,
  classProbs = TRUE,
  verboseIter = TRUE,
  allowParallel = TRUE)


# create the tuning grid. Again keeping this small to avoid exceeding kernel memory limits.
# You can expand as your compute resources allow. 
set.seed(238)
gbmGrid <-  expand.grid(interaction.depth = c(10),
                        n.trees = c(100), 
                        shrinkage = c(0.1),
                        n.minobsinnode = c(10)) # you can also put something        like c(5, 10, 15, 20)
#The final values used for the model were nrounds = 250, max_depth = 8, eta
#= 0.05, gamma = 0, colsample_bytree = 0.5, min_child_weight = 10
#and subsample = 0.5.


library(e1071)
# train the xgboost learner
gbmod <- train(
  x = x_train,
  y = y_train,
  method = 'gbm',
  metric = 'ROC',
  trControl = trControl,
  tuneGrid = gbmGrid)


print(gbmod)
#print(Sys.time() - start)

# Diagnostics
print(gbmod$results)
print(gbmod$resample)

# make predictions
preds <- predict(gbmod, newdata = x_test, type = "prob")
preds_final <- predict(gbmod, newdata = dtest, type = "prob")
preds1 <- predict(gbmod, newdata = x_test, type = "raw")
preds_final1 <- predict(gbmod, newdata = dtest, type = "raw")
table(preds1)
table(y_test,preds1)
table(preds_final1)
confusionMatrix(preds1,y_test)

# plot the ROC curve
#roc.plot(y_test_raw, preds$Yes, plot.thres = c(0.02, 0.03, 0.04, 0.05))


# prep the predictions for submissions
sub <- data.frame(UCIC_ID= as.integer(dtest[,1]), Responders = preds_final$Y)
w<- which(sub[,2]>0.5)
summary(sub[,2])


# write to csv
write.csv(sub,gzfile("/home/mkameruddin/churn/gbm_t_357v_150_10_0.1.csv.gz"),
          row.names = FALSE)



saveRDS(gbmod,"gbm_t_all_250_8_0.05_10_0.684.rds")
# my_model <- readRDS("gbm_t_all_250_8_0.05_10_0.684.rds")

varImp(gbmod)


#####


