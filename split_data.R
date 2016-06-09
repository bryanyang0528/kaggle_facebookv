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

summary(df_train)

margin <- 0.5
min_mar <- 0
max_mar <- 10.01
accuracy_threshold <- 27

lower_x <- min_mar
lower_y <- min_mar


df_train_sub <- grid(df_train, lower_x, lower_y, accuracy_threshold)
df_test_sub <- grid(df_test, lower_x, lower_y, accuracy_threshold)
submission_sub <- submission(df_train_sub, df_test_sub)


grid <- function(data, lower_x, lower_y, accuracy_threshold)
{
  upper_x <- lower_x + margin
  upper_y <- lower_y + margin
  data_sub <- data[(data$x >= lower_x & data$x < upper_x-0.0005
                    & data$y >= lower_y & data$y < upper_y-0.0005
                    & data$accuracy > accuracy_threshold),]
  return(data_sub)
}


submission <- function (train, test){
  
  feature <- c("x", "y")
  y <- "place_id"
  train <- train[, feature, with=FALSE]
  test_x <- test[, feature, with=FALSE]
  test_id <- test[, "row_id", with=FALSE]
  
  model_1 <- knn(train, test_x, df_train_sub[[y]]  , k=1, prob = TRUE)
  model_2 <- knn(train, test_x, df_train_sub[[y]]  , k=5, prob = TRUE)
  model_3 <- knn(train, test_x, df_train_sub[[y]]  , k=10, prob = TRUE)
  
  submission <- cbind(test_id, as.character(model_1), as.character(model_2), as.character(model_3))
  submission <- data.table(submission)
  print(str(submission))  
  cols <- colnames(submission)[2:length(submission)]
  #submission$place_id <- paste(c(submission$V1, submission$V2, submission$V3), sep =" ") 
  setDT(submission)[, place_id := Reduce(function(...) paste(..., sep = " "), .SD[, mget(cols)])]
  
  return(submission[,c("row_id","place_id"), with=FALSE])
}

cols=c("row_id", "row_id")
setDT(submission_sub)[, x := Reduce(function(...) paste(..., sep = "-"), .SD[, mget(cols)])]


colnames(df_test)[2:length(df_test)]
