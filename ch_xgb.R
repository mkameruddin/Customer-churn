dtrain <- read.csv("train_ch_encoded.csv")
dtest <- read.csv("test_ch_encoded.csv")

library(caret)
library(xgboost)
set.seed(101)
i <- createDataPartition(dtrain$Responders, p = 0.8, list = FALSE)


dtrain$Responders <- as.numeric (dtrain$Responders)
dtrain$Responders[dtrain$Responders==1] <-0
dtrain$Responders[dtrain$Responders==2] <-1
table(dtrain$Responders)




dtrain1 <- xgb.DMatrix(as.matrix(dtrain[i,-c(1,ncol(dtrain))]), 
                       label=(dtrain[i,ncol(dtrain)]),missing=NA)

dtrain2 <- xgb.DMatrix(as.matrix(dtrain[-i,-c(1,ncol(dtrain))]),
                       label=dtrain[-i,ncol(dtrain)],missing=NA)

dtest1 <- xgb.DMatrix(as.matrix(dtest[,-1]),missing=NA)

table(dtrain[-i,ncol(dtrain)])


watchlist <- list(train=dtrain1, test=dtrain2)

set.seed(200)
bst <- xgb.train(data=dtrain1, max_depth=8, eta=0.05,nrounds=350,subsample=0.5,
                 verbose=2, watchlist=watchlist, eval_metric = "auc",
                 early_stopping_rounds = 10,objective = "binary:logistic")


pred_train2 <- predict(bst, dtrain2)
cl <- which(pred_train2 >0.5)
dtrain_test <-dtrain[-i,]
dtrain_test$pred <-0
dtrain_test$pred[cl]<-1
table(dtrain_test$Responders,dtrain_test$pred)



preds <- predict(bst, dtest1)
summary(preds)

# prep the predictions for submissions
sub <- data.frame(UCIC_ID = as.integer(dtest[,1]), Responders = preds)
w<- which(sub[,2]>0.5)
summary(sub[,2])
sub[w,1]
# write to csv
write.csv(sub,gzfile("/home/mkameruddin/churn/xgb_all_auc_8_0.05.csv.gz"), 
          row.names = FALSE)


saveRDS(bst,"xgb_all_250_8_0.05_10_0.684.rds")
# my_model <- readRDS("xgb_t_all_250_8_0.05_10_0.684.rds")



importance <- xgb.importance(colnames(dtrain1), model = bst)
head(importance)
dim(importance)
write.csv(importance, "ch_xgb_all_imp.csv", row.names = FALSE)

trees <- xgb.dump(bst, with_stats = T)

