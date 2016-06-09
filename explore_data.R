library(data.table)
library(class)
library(dplyr)
library(xgboost)
library(randomForest)
library(rpart)

set.seed(123)
file_train = "data/train.csv"
file_test="data/test.csv"
nrows = 100000
df_train <- fread(file_train, header = TRUE, nrows = nrows, integer64 = "character")
df_test <- fread(file_test, header = TRUE, nrows = nrows, integer64 = "character")



#str(df_train)

# exporing 
summary(df_train)
hist(df_train$x)
hist(df_train$y)
hist(df_train$time)
hist(df_train$accuracy)

# feature engineering 
df_train_1q <- df_train[df_train$accuracy>27,]



#knn
nrows <- nrow(df_train)
smp_size <- floor(0.75 * nrows)
## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrows), size = smp_size)

train <- df_train[train_ind, c("x", "y"), with=FALSE]
test <- df_train[-train_ind, c("x","y"), with=FALSE]
train_y <- df_train[train_ind,][["place_id"]]
test_y <- df_train[-train_ind,][["place_id"]]

model <- knn(train, test, train_y, k=1, prob = TRUE)

# accuacy 
sum(model == test_y) / length(test_y)


# knn with Leave-one-out cross validation 
col <- c("x", "y")
model <- knn.cv(df_train[, col, with=FALSE],df_train[[c("place_id")]],1,0)
sum(model == df_train[[c("place_id")]]) / length(model)

# xgboost

nrows <- nrow(df_train)
smp_size <- floor(0.75 * nrows)
## set the seed to make your partition reproductible
train_ind <- sample(seq_len(nrows), size = smp_size)
train <- df_train[train_ind]
test <- df_train[-train_ind]

col <- c("x", "y")

dtrain <- xgb.DMatrix(data = as.matrix(train[, col, with=FALSE]), 
                      label = as.numeric(train$place_id))

bst <- xgboost(data = dtrain, max.depth=2, eta =1, objective = "multi:softmax", nrounds = 2)


# random forest   rows數太多無法使用

df_train_rf <- df_train_1q

nrows <- nrow(df_train_rf)
smp_size <- floor(0.75 * nrows)
## set the seed to make your partition reproductible
train_ind <- sample(seq_len(nrows), size = smp_size)
train <- df_train_rf[train_ind]
test <- df_train_rf[-train_ind]

train$place_id <- as.factor(train$place_id) 

model.rf <- randomForest(place_id ~ x + y  , data=train, importance=FALSE,
                        proximity=TRUE)

#print(model.rf)
#str(model.rf)
#round(importance(model.rf), 2)

test_pred <- predict(model.rf, test[,c("x", "y"), with=FALSE])
test_y <- test[["place_id"]]

sum(test_pred == test_y) / length(test_pred)


# decision tree
control=rpart.control(minsplit=30, cp=0.001) 
model.dt <- rpart(place_id ~ x + y, data = train, method = "class", control = control  ) 
