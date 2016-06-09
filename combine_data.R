# combine all subsets 

library(data.table)
library(class)

margin <- 0.5
min_mar <- 0
max_mar <- 10
accuracy_threshold <- 27
set.seed(123)
file_train = "data/train.csv"
file_test="data/test.csv"
nrows = -1

df_train <- fread(file_train, header = TRUE, nrows = nrows, integer64 = "character", 
                  colClasses=c("character", "numeric", "numeric", "integer", "NULL",NA))
df_test <- fread(file_test, header = TRUE, nrows = nrows, integer64 = "character", 
                 colClasses=c("character", "numeric", "numeric", "integer", "NULL"))

# initialization 
lower_x <- min_mar
submission_data <- data.table(row_id=character(),place_id=character())


# set while loop 

while (lower_x < max_mar){
  lower_y <- min_mar
    while (lower_y < max_mar){
    print(paste(lower_x, lower_y, sep="   "))
    
    #main function here 
    df_train_sub <- grid(df_train, lower_x, lower_y, accuracy_threshold)
    df_test_sub <- grid(df_test, lower_x, lower_y, accuracy_threshold)
    submission_sub <- submission(df_train_sub, df_test_sub)
    submission_data <- rbind(submission_data, submission_sub)    
  
    lower_y<- lower_y + margin  
  }
  lower_x <- lower_x + margin
}






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
  cols <- colnames(submission)[2:length(submission)]
  #submission$place_id <- paste(c(submission$V1, submission$V2, submission$V3), sep =" ") 
  setDT(submission)[, place_id := Reduce(function(...) paste(..., sep = " "), .SD[, mget(cols)])]
  #str(submission)
  return(submission[,c("row_id", "place_id"), with=FALSE])
}


