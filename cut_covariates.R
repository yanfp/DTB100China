
load("D:/DTB100/samples.RData")

cov_data <- DATA[, !(colnames(DATA) %in% c("ID", "LON", "LAT", "DTB"))]
DTB_data <- DATA[, "DTB"]
cut_cov_name <- vector()
R2 <- 0
RMSE <- 0

#####################################################################
#####################################################################
cov_data <- cov_data[, !(colnames(cov_data) %in% cut_cov_name)]
# n_tree <- 1000
# mtry <- floor(ncol(cov_data) / 3)
# rf <- randomForest(x = cov_data, y = DTB_data, 
#                    ntree = n_tree, 
#                    mtry = mtry)

cov_data_m <- as.matrix(apply(cov_data, 2, as.numeric))
DTB_data_m <- as.matrix(DTB_data)
xg <- xgboost(data = cov_data_m, label = DTB_data_m, 
              eta = 0.1, 
              nrounds = 37, 
              max_depth = 9, 
              min_child_weight = 1, 
              gamma = 0,
              subsample = 0.7, 
              colsample_bytree = 0.7, 
              lambda = 1,
              lambda_bias = 5,
              alpha = 1)
# new_R2 <- 1 - var(rf$y - rf$predicted, na.rm = TRUE) / var(rf$y - mean(rf$y), na.rm = TRUE)
# new_RMSE <- sqrt(mean((rf$y - rf$predicted)^2, na.rm = TRUE))

pred <- predict(xg, newdata = cov_data_m)
new_R2 <- 1 - var(DTB_data_m - pred, na.rm = TRUE) / var(DTB_data - mean(DTB_data_m), na.rm=TRUE)
new_RMSE <- sqrt(mean((DTB_data_m - pred)^2, na.rm=TRUE))

print("previous R2:");print(R2)
print("new R2:");print(new_R2)

print("previous RMSE:");print(RMSE)
print("new RRMSE:");print(new_RMSE)

R2 <- new_R2
RMSE <- new_RMSE

importance_tb <- xgb.importance(feature_names = colnames(cov_data), model = xg)
cut_cov_name <- importance_tb[nrow(importance_tb)]$Feature
cut_cov_name

