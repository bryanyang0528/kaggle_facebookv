library(randomForest)

margin <- 0.5
min_mar <- 0
max_mar <- 9.99
accuracy_threshold <- 50
lower_x <- min_mar
lower_y <- min_mar

df_train_sub <- grid(df_train, lower_x, lower_y, accuracy_threshold)
df_train_sub$x_10 <- df_train_sub$x/10
summary(df_train_sub)

df_test_sub <- grid(df_test, lower_x, lower_y)

# knn
col <- c("x", "y")
model <- knn.cv(df_train_sub[, col, with=FALSE],df_train_sub[[c("place_id")]],60,0)
sum(model == df_train_sub[[c("place_id")]]) / length(model)


