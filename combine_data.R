# combine all subsets 

library(data.table)
library(class)

margin <- 0.5
min_mar <- 0
max_mar <- 9.99
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

#lower_x <- min_mar
lower_x <- 6
submission_data <- data.table(row_id=character(),place_id=character())


# set while loop 

while (lower_x < max_mar){
  lower_y <- min_mar
    while (lower_y < max_mar){
    print(paste(lower_x, lower_y, sep="   "))
    
    #main function here 
    df_train_sub <- grid(df_train, lower_x, lower_y, accuracy_threshold)
    df_test_sub <- grid(df_test, lower_x, lower_y)
    submission_sub <- submission(df_train_sub, df_test_sub)
    submission_data <- rbind(submission_data, submission_sub)    
  
    lower_y<- lower_y + margin  
  }
  lower_x <- lower_x + margin
}

submission_data <- submission_data[order(as.numeric(row_id))]

write.csv(submission_data,file = 'data/submission_data.csv',quote = FALSE,row.names = FALSE)


# find missing data

temp <- merge(df_test, submission_data, by.x="row_id", by.y="row_id",all = TRUE )
head(temp)
temp[is.na(temp$place_id),][order(x,y)]

write.csv(temp[,c("row_id","place_id"),with=FALSE][order(as.numeric(row_id))], file = 'data/submission_data_temp.csv',quote = FALSE,row.names = FALSE)
