json_train<-fromJSON('/Users/Taylor/Desktop/Pred 413 Midterm/train.json')
vars <- setdiff(names(json_train), c("photos", "features"))
json_train <- map_at(json_train, vars, unlist) %>% tibble::as_tibble(.)
train <- select(json_train, c(listing_id, bedrooms, bathrooms, created, price, street_address, interest_level, longitude, latitude))
train$photos_count <- sapply(1:nrow(json_train), function(x) length(unlist(json_train$photos[x])))
train$features_count <- sapply(1:nrow(json_train), function(x) length(unlist(json_train$features[x])))
#do the same for json_test
json_test<-fromJSON('/Users/Taylor/Desktop/Pred 413 Midterm/test.json')
vars2 <- setdiff(names(json_test), c("photos", "features"))
json_test <- map_at(json_test, vars2, unlist) %>% tibble::as_tibble(.)
test <- select(json_test, c(listing_id, bedrooms, bathrooms, created, price, street_address, longitude, latitude))
test$photos_count <- sapply(1:nrow(json_test), function(x) length(unlist(json_test$photos[x])))
test$features_count <- sapply(1:nrow(json_test), function(x) length(unlist(json_test$features[x])))
#new feature measures distance from city
ny_lat <- 40.785091
ny_lon <- -73.968285
traindistance_city <-
   mapply(function(lon, lat) sqrt((lon - ny_lon)^2  + (lat - ny_lat)^2),
          train$longitude,
          train$latitude)
test$distance_city <-
   mapply(function(lon, lat) sqrt((lon - ny_lon)^2  + (lat - ny_lat)^2),
          test$longitude,
         test$latitude)
#create factors
interest_factor <- rep(NA, nrow(train))
train <- cbind(train, interest_factor)
train$interest_factor[train$interest_level == "low"] <- 0
train$interest_factor[train$interest_level == "medium"] <- 1
train$interest_factor[train$interest_level == "high"] <- 2
train$interest_factor <- factor(train$interest_factor)
train$interest_factor_mult2 <- relevel(train$interest_factor, ref = "0")
split <- createDataPartition(train$listing_id, p=0.8, list=FALSE)
train_split <- train[split, ]
test_split <- train[-split, ]
MLR_2 <- multinom(interest_factor_mult2 ~ bathrooms + bedrooms + price + features_count + photos_count, data = train_split)
MLR2_pred = predict(MLR_2, newdata=test_split)
#create confusion matrix
cm <- table(MLR2_pred, test_split$interest_factor_mult2)
cm
#following command calculates accuracy of the model
mean(MLR2_pred == test_split$interest_factor_mult2)
MLR_3 <- multinom(interest_factor_mult2 ~ bathrooms + bedrooms + price + features_count + photos_count + distance_city, data = train_split)
MLR3_pred = predict(MLR_3, newdata=test_split)
#XGBoost
split_trainXG <- train_split
split_trainXG$interest_level<-as.integer(factor(train_split$interest_level))
split_testXG <- test_split
split_testXG$interest_level<-as.integer(factor(test_split$interest_level))
y <- split_trainXG$interest_level
y = y-1
split_trainXG$interest_level = NULL
split_testXG$interest_level = NULL
head(split_testXG)
#Parameters for XGBoost
xgb_params = list(
   colsample_bytree= 0.7,
   subsample = 0.7,
   eta = 0.1,
   objective= 'multi:softprob',
   max_depth= 4,
   min_child_weight= 1,
   eval_metric= "mlogloss",
   num_class = 3,
   seed = 123
 )
split_testd <- xgb.DMatrix(data.matrix(split_testXG))
#create folds
kfolds<- 10
folds<-createFolds(y, k = kfolds, list = TRUE, returnTrain = FALSE)
fold <- as.numeric(unlist(folds[1]))
x_trainXG<-split_trainXG[-fold,] #Train set
x_valXG<-split_trainXG[fold,] #Out of fold validation set
y_trainXG<-y[-fold]
y_valXG<-y[fold]
split_traind = xgb.DMatrix(as.matrix(x_trainXG), label=y_trainXG)
# got an error Error in xgb.DMatrix(as.matrix(x_trainXG), label = y_trainXG) : 
#  [16:47:14] amalgamation/../dmlc-core/src/io/local_filesys.cc:66: #LocalFileSystem.GetPathInfo 7170325 Error:No such file or directory
