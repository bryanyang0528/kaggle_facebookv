# combine all subsets 

library(data.table)
library(class)

margin <- 0.5
min_mar <- 0
max_mar <- 10
accuracy_threshold <- 50
k_list <- c(50,25,1)
w_list <- c(4550, 5500)

set.seed(123)
file_train = "data/train.csv"
file_test="data/test.csv"
nrows = -1

df_train <- fread(file_train, header = TRUE, nrows = nrows, integer64 = "character", 
                  colClasses=c("character", "numeric", "numeric", "integer", "NULL",NA))
df_test <- fread(file_test, header = TRUE, nrows = nrows, integer64 = "character", 
                 colClasses=c("character", "numeric", "numeric", "integer", "NULL"))

# initialization 


#lower_x <- 6
submission_data <- data.table(row_id=character(),place_id=character())

# set while loop 

lower_x <- min_mar
while (lower_x <= max_mar-margin){
  lower_y <- min_mar
    while (lower_y < max_mar){
    print(paste(lower_x, lower_y, sep="   "))
    
    #main function here 
      
    df_train_sub <- grid(df_train, lower_x, lower_y, margin, accuracy_threshold)
    df_test_sub <- grid(df_test, lower_x, lower_y)
    submission_sub <- submission(df_train_sub, df_test_sub, k_list, w_list)
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

write.csv(submission_data[,c("row_id","place_id"),with=FALSE][order(as.numeric(row_id))], 
          file = 'data/submission_data_0.47.csv',quote = FALSE,row.names = FALSE)

