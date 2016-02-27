library(caret);
set.seed(772349);

finalTest <- read.csv("data/pml-testing.csv");
initTrain <- read.csv("data/pml-training.csv");

cleanData <- function(data) {
  # Remove X, user_name, raw_timestamp_part_1, raw_timestamp_part_2, cvtd_timestamp, new_window, num_window
  df <- data[,-c(1:7)];
  # Turn factor variables into numerics (apart from classe), turn NANs into 0s.
  df <- cbind(apply(df[1:152], c(1,2), function(e){
    n <- as.numeric(as.character(e));
    if (is.na(n)) {
      0
    } else {
      n
    }
  }), df[153]);
}

cleanTrain <- cleanData(initTrain);

inTrain <- createDataPartition(cleanTrain$classe, p = 2/3)[[1]];
training <- cleanTrain[inTrain,];
testing <- cleanTrain[-inTrain,];

nearZeroVarCols <- nearZeroVar(training, saveMetrics = TRUE);

training2 <- training[,!nearZeroVarCols$nzv];

fit1 <- train(classe ~ ., data = training2, method = "rpart", trControl = trainControl(method = "cv", number = 3));
fit2 <- train(classe ~ ., data = training2, method = "rf", trControl = trainControl(method = "cv", number = 3));
fit3 <- train(classe ~ ., data = training2, method = "gbm", trControl = trainControl(method = "cv", number = 3));
fit4 <- train(classe ~ ., data = training2, method = "lda", trControl = trainControl(method = "cv", number = 3));

prconf <- function(fit, data) {
  pred <- predict(fit, newdata = data);
  confusionMatrix(pred, data$classe);
}

prconf(fit1, testing); # rpart: 49% accuracy
prconf(fit2, testing); #    rf: 99% accuracy
prconf(fit3, testing); #   gbm: 96% accuracy
prconf(fit4, testing); #   lda: 70% accuracy

t1 <- predict(fit1, finalTest);
t2 <- predict(fit2, finalTest);
t3 <- predict(fit3, finalTest);
t4 <- predict(fit4, finalTest);

finalPredictions <- data.frame(rpart = t1, lda = t4, gbm = t3, rf = t2);
