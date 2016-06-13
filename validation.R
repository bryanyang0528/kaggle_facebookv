library(randomForest)

margin <- 0.5
min_mar <- 0
max_mar <- 9.99
accuracy_threshold <- 55
lower_x <- 5
lower_y <- 5

df_train_sub <- grid(df_train, lower_x, lower_y, margin, accuracy_threshold)
df_train_sub$x_w <- df_train_sub$x * 4550
df_train_sub$y_w <- df_train_sub$y * 5500

# knn
col <- c("x_w", "y_w")
model <- knn.cv(df_train_sub[, col, with=FALSE],df_train_sub[[c("place_id")]],50,0)
sum(model == df_train_sub[[c("place_id")]]) / length(model)


