# split the data into small pieces

library(data.table)
library(class)

set.seed(123)
file_train = "data/train.csv"
file_test="data/test.csv"
nrows = -1
df_train <- fread(file_train, header = TRUE, nrows = nrows, integer64 = "character", 
                  colClasses=c("character", "numeric", "numeric", "integer", "NULL",NA))
df_test <- fread(file_test, header = TRUE, nrows = nrows, integer64 = "character", 
                 colClasses=c("character", "numeric", "numeric", "integer", "NULL"))


margin <- 0.5
min_mar <- 0
max_mar <- 10.01
accuracy_threshold <- 27

lower_x <- min_mar
lower_y <- min_mar


df_train_sub <- grid(df_train, lower_x, lower_y, accuracy_threshold)
df_test_sub <- grid(df_test, lower_x, lower_y, accuracy_threshold)
submission_sub <- submission(df_train_sub, df_test_sub)


grid <- function(data, lower_x, lower_y, margin=0.5, accuracy_threshold=-1)
{
  upper_x <- lower_x + margin - 0.00005
  upper_y <- lower_y + margin - 0.00005
  
  if (lower_x >= max_mar-margin) {upper_x <- 11}
  if (lower_y == max_mar-margin) {upper_y <- 11}
  
  print(paste("upper_x: ", upper_x))
  print(paste("upper_y: ", upper_y))
  data_sub <- data[(data$x >= lower_x & data$x < upper_x
                    & data$y >= lower_y & data$y < upper_y
                    & data$accuracy > accuracy_threshold),]
  return(data_sub)
}


submission <- function (train, test, k_list, w_list){
  
  train$x_w <- train$x * w_list[1]
  train$y_w <- train$y * w_list[2]
  
  test$x_w <- test$x * w_list[1]
  test$y_w <- test$y * w_list[2]
  
  feature <- c("x_w", "y_w")
  y <- "place_id"
  train <- train[, feature, with=FALSE]
  test_x <- test[, feature, with=FALSE]
  test_id <- test[, "row_id", with=FALSE]
  
  model_1 <- knn(train, test_x, df_train_sub[[y]]  , k=k_list[1], prob = TRUE)
  model_2 <- knn(train, test_x, df_train_sub[[y]]  , k=k_list[2], prob = TRUE)
  model_3 <- knn(train, test_x, df_train_sub[[y]]  , k=k_list[3], prob = TRUE)
  
  submission <- cbind(test_id, as.character(model_1), as.character(model_2), as.character(model_3))
  submission <- data.table(submission)
  print(str(submission))  
  cols <- colnames(submission)[2:length(submission)]
  #submission$place_id <- paste(c(submission$V1, submission$V2, submission$V3), sep =" ") 
  setDT(submission)[, place_id := Reduce(function(...) paste(..., sep = " "), .SD[, mget(cols)])]
  
  return(submission[,c("row_id","place_id"), with=FALSE])
}
