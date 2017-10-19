libs <- c("jsonlite", "dplyr", "purrr", "lubridate", "nnet", "tidyr", "ggplot2", "scales", "caret","xgboost","MLmetrics","tidytext","reshape2")
lapply(libs, require, character.only = TRUE)
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
train$distance_city <-
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
#model
MLR_2 <- multinom(interest_factor_mult2 ~ bathrooms + bedrooms + price + features_count + photos_count, data = train)
> pred_test2 <- predict(MLR_2, test)
> head(pred_test2)
[1] 0 0 0 0 0 0
Levels: 0 1 2
> prob_pred2 <- predict(MLR_2, test, type = "prob")
> colnames(prob_pred2) <- c("Low", "Medium", "High")
> rownames(prob_pred2) <- test$listing_id
> head(prob_pred2)
> write.csv(prob_pred2, '/Users/Taylor/Desktop/Pred 413 Midterm/prob_pred2.csv')
#second model
 #add distance from "City Center
 train
ny_lat <- 40.785091
ny_lon <- -73.968285
train$distance_city <-
   mapply(function(lon, lat) sqrt((lon - ny_lon)^2  + (lat - ny_lat)^2),
          train$longitude,
          train$latitude)
test2 <- test
 test2$distance_city <-
   mapply(function(lon, lat) sqrt((lon - ny_lon)^2  + (lat - ny_lat)^2),
          test$longitude,
         test$latitude)
MLR_3 <- multinom(interest_factor_mult2 ~ bathrooms + bedrooms + price + features_count + photos_count + distance_city, data = train2)
pred_test3 <- predict(MLR_3, test)
prob_pred3 <- predict(MLR_3, test, type = "prob")
colnames(prob_pred3) <- c("Low", "Medium", "High")
rownames(prob_pred3) <- test$listing_id
write.csv(prob_pred3, '/Users/Taylor/Desktop/Pred 413 Midterm/prob_pred3.csv')
#model3
json_train<-fromJSON('/Users/Taylor/Desktop/Pred 413 Midterm/train.json')
vars <- setdiff(names(json_train), c("photos", "features"))
json_train <- map_at(json_train, vars, unlist) %>% tibble::as_tibble(.)
json_train$photos_count <- sapply(1:nrow(json_train), function(x) length(unlist(json_train$photos[x])))
json_train$features_count <- sapply(1:nrow(json_train), function(x) 
length(unlist(json_train$features[x])))
json_test<-fromJSON('/Users/Taylor/Desktop/Pred 413 Midterm/test.json')
vars2 <- setdiff(names(json_test), c("photos", "features"))
json_test <- map_at(json_test, vars2, unlist) %>% tibble::as_tibble(.)
json_test$photos_count <- sapply(1:nrow(json_test), function(x) length(unlist(json_test$photos[x])))
json_test$features_count <- sapply(1:nrow(json_test), function(x) length(unlist(json_test$features[x])))
libs2 <- c("caret","xgboost","MLmetrics","tidytext","reshape2")
lapply(libs2, require, character.only=TRUE)
set.seed(123)
train_id <- json_train$listing_id
test_id <- json_test$listing_id
#add filler for listing lacking any features
json_train[unlist(map(json_train$features,is_empty)),]$features = 'Nofeat'
json_test[unlist(map(json_test$features,is_empty)),]$features = 'Nofeat'

json_train$distance_city <-
mapply(function(lon, lat) sqrt((lon - ny_lon)^2  + (lat - ny_lat)^2),json_train$longitude,json_train$latitude)
json_test$distance_city <-
mapply(function(lon, lat) sqrt((lon - ny_lon)^2  + (lat - ny_lat)^2),json_test$longitude,json_test$latitude)
#add dummy interest level for test
json_test$interest_level <- 'none'
#combine train set and test set
train_test <- rbind(json_train, json_test)
#features to use
feat <- c("bathrooms","bedrooms","building_id", "created", "description",
           "listing_id","manager_id", "price", "features",
           "display_address", "street_address","feature_count","photo_count", 			"distance_city", "interest_level")
train_test = train_test[,names(train_test) %in% feat]
#process word features
word_remove = c('allowed', 'building','center', 'space','2','2br','bldg','24',
                '3br','1','ft','3','7','1br','hour','bedrooms','bedroom','true',                 'stop','size','blk','4br','4','sq','0862','1.5','373','16','3rd','block',
                 'st','01','bathrooms')
#create sparse matrix for word features
word_sparse<-train_test[,names(train_test) %in% c("features","listing_id")]
train_test$features = NULL
#create word features
word_sparse <- word_sparse %>%
   filter(map(features, is_empty) != TRUE) %>%
   tidyr::unnest(features) %>%
   unnest_tokens(word, features)
data("stop_words")
 #remove stop words and other words
word_sparse = word_sparse[!(word_sparse$word %in% stop_words$word),]
word_sparse = word_sparse[!(word_sparse$word %in% word_remove),]
#get top 150 features and use
top_word <- as.character(as.data.frame(sort(table(word_sparse$word),decreasing = TRUE)[1:25])$Var1)
word_sparse = word_sparse[word_sparse$word %in% top_word,]
word_sparse$word = as.factor(word_sparse$word)
word_sparse<-dcast(word_sparse, listing_id ~ word,length, value.var = "word")
#merge word features into main dataframe
train_test<-merge(train_test,word_sparse, by = "listing_id", sort = FALSE,all.x=TRUE)
#non word features
#convert building and manager_id to integer
train_test$building_id<-as.integer(factor(train_test$building_id))
train_test$manager_id<-as.integer(factor(train_test$manager_id))
#convert street and display address to integer
train_test$display_address<-as.integer(factor(train_test$display_address))
train_test$street_address<-as.integer(factor(train_test$street_address))
#convert date
train_test$created<-ymd_hms(train_test$created)
train_test$month<- month(train_test$created)
train_test$day<- day(train_test$created)
train_test$hour<- hour(train_test$created)
train_test$created = NULL
#split train test
train3 <- train_test[train_test$listing_id %in%train_id,]
test3 <- train_test[train_test$listing_id %in%test_id,]
#convert labels to integers
train3 <- train_test[train_test$listing_id %in%train_id,]
test3 <- train_test[train_test$listing_id %in%test_id,]
train3$interest_level<-as.integer(factor(train3$interest_level))
y <- train3$interest_level
y = y - 1
train3$interest_level = NULL
test3$interest_level = NULL
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
#convert xbgmatrix
dtest2 <- xgb.DMatrix(data.matrix(test2))
#create folds
kfolds<- 10
folds<-createFolds(y, k = kfolds, list = TRUE, returnTrain = FALSE)
fold <- as.numeric(unlist(folds[1]))
x_train2<-train2[-fold,] #Train set
x_val2<-train2[fold,] #Out of fold validation set
y_train2<-y[-fold]
y_val2<-y[fold]
#convert to xbgmatrix
dtrain2 = xgb.DMatrix(as.matrix(x_train2), label=y_train2)
dval2 = xgb.DMatrix(as.matrix(x_val2), label=y_val2)
#perform training
gbdt = xgb.train(params = xgb_params,
                 data = dtrain2,
                 nrounds =475,
                 watchlist = list(train = dtrain2, val=dval2),
                 print_every_n = 25,
                 early_stopping_rounds=50)
allpredictions =  (as.data.frame(matrix(predict(gbdt,dtest2), nrow=dim(test2), byrow=TRUE)))
#generate submission
allpredictions = cbind (allpredictions, test2$listing_id)
names(allpredictions)<-c("high","low","medium","listing_id")
allpredictions=allpredictions[,c(1,3,2,4)]
write.csv(allpredictions, '/Users/Taylor/Desktop/Pred 413 Midterm/xgboost1.csv')
allpredictions2 =  (as.data.frame(matrix(predict(gbdt3,dtest3), nrow=dim(test2), byrow=TRUE)))