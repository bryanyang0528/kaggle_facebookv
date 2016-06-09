# split the data into small pieces

library(data.table)
library(class)

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
df_test_sub <- grid(df_test, lower_x, lower_y, accuracy_threshold)

feature <- c("x", "y")
y <- "place_id"
train <- df_train_sub[, feature, with=FALSE]
test <- df_test_sub[, feature, with=FALSE]

model_1 <- knn(train, test, df_train_sub[[y]]  , k=1, prob = TRUE)
model_2 <- knn(train, test, df_train_sub[[y]]  , k=5, prob = TRUE)
model_3 <- knn(train, test, df_train_sub[[y]]  , k=10, prob = TRUE)


submission_sub <- cbind(as.character(model_1), as.character(model_2), as.character(model_3))


