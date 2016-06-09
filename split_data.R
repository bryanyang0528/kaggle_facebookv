# split the data into small pieces

library(data.table)

set.seed(123)
file_train = "data/train.csv"
file_test="data/test.csv"
nrows = -1
df_train <- fread(file_train, header = TRUE, nrows = nrows, integer64 = "character", 
                  colClasses=c("NULL", "numeric", "numeric", "integer", "NULL",NA))
df_test <- fread(file_test, header = TRUE, nrows = nrows, integer64 = "character", 
                 colClasses=c("NULL", "numeric", "numeric", "integer", "NULL"))

summary(df_train)

margin <- 0.5
min_mar <- 0
max_mar <- 10.01
accuracy_threshold <- 27

lower_x <- min_mar
lower_y <- min_mar

grid <- function(data, lower_x, lower_y, accuracy_threshold)
{
  upper_x <- lower_x + margin
  upper_y <- lower_y + margin
  data_sub <- data[(data$x >= lower_x & data$x < upper_x-0.0005
                & data$y >= lower_y & data$y < upper_y-0.0005
                & data$accuracy > accuracy_threshold),]
  return(data_sub)
}


df_train_sub <- grid(df_train, lower_x, lower_y, accuracy_threshold)
summary(df_train_sub)

