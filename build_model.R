# build models
# 2018/11/06


# load("D:/DTB100/RData/matrix_for_modelling_CLI_PAR.RData")
# cli_matrix <- matrix_for_modeling
# str(cli_matrix)
# 
# load("D:/DTB100/RData/matrix_for_modelling_SOI_PAR.RData")
# soi_matrix <- matrix_for_modeling
# str(soi_matrix)
# 
# load("D:/DTB100/RData/matrix_for_modelling_EAR_PAR.RData")
# ear_matrix <- matrix_for_modeling
# str(ear_matrix)
# 
# load("D:/DTB100/RData/matrix_for_modelling_TIM_PAR.RData")
# tim_matrix <- matrix_for_modeling
# str(ear_matrix)
# 
# load("D:/DTB100/RData/matrix_for_modelling_ORG_PAR.RData")
# org_matrix <- matrix_for_modeling
# str(org_matrix)
# 
# load("D:/DTB100/RData/matrix_for_modelling_13_DEM_PAR.RData")
# dem_matrix <- matrix_for_modeling
# str(dem_matrix)
# 
# 
# matrix_list <- list(soi_matrix, org_matrix, ear_matrix, tim_matrix, cli_matrix, dem_matrix)
# save(matrix_list, file = "D:/DTB100/matrix_list.RData")
# 
# load(file = "D:/DTB100/RData/matrix_list.RData")
# 
# 
# # merge all the covariates
# all_matrix <- data.frame(ID = 1:6382)
# str(all_matrix)
# for(i in matrix_list)
# {
#   all_matrix <- merge(all_matrix, as.data.frame(i), by = "ID", all.x = TRUE)
# 
#   all_matrix_no_na <- na.omit(all_matrix)
#   #str(all_matrix_no_na)
# 
#   all_matrix_no_repeat <- all_matrix_no_na[!duplicated(all_matrix_no_na$ID),]
#   #str(all_matrix_no_repeat)
#   all_matrix <- all_matrix_no_repeat
# 
#   print(names(i))
# }
# str(all_matrix)
# 
# load("D:/DTB100/RData/dem_no_twi_matrix.RData")
# dem_no_twi_matrix <- dem_no_twi_matrix[!duplicated(dem_no_twi_matrix[,1]),]
# str(dem_no_twi_matrix)
# all_matrix <- merge(all_matrix, dem_no_twi_matrix, by = "ID", all.x = TRUE)
# all_matrix <- na.omit(all_matrix)
# str(all_matrix)
# anyNA(all_matrix)
# 
# ## add the DTB to data
# load("D:/DTB100/samples.RData")
# data <- merge(all_matrix, samples, by = "ID", all.x = TRUE)

#############################################################################
#############################################################################

## laod the data
load("D:/DTB100/DATA.RData")
## select the covariate columus and DTB columus
cov_data <- DATA[, !(colnames(DATA) %in% c("ID", "LON", "LAT", "DTB", "VBF"))]
str(cov_data)
DTB_data <- DATA[, "DTB"]

# using randomForest
library(randomForest)
rf <- randomForest(x = cov_data, y = DTB_data, 
                   ntree = 1000, 
                   mtry = ncol(cov_data) / 3,
                   importance = TRUE)

1 - var(rf$y - rf$predicted, na.rm = TRUE) / var(rf$y - mean(rf$y), na.rm = TRUE)
sqrt(mean((rf$y - rf$predicted)^2, na.rm = TRUE))
mean(rf$y - rf$predicted)
varImpPlot(rf)
rf_file_name <- "D:/DTB100/rf.RData"
if(!file.exists(rf_file_name))
{
  save(rf, file = rf_file_name)
}

# using xgboost
library(xgboost)
cov_data_m <- as.matrix(apply(cov_data, 2, as.numeric))
DTB_data_m <- as.matrix(DTB_data)

xg <- xgboost(data = cov_data_m, label = DTB_data_m, 
              eta = 0.1, 
              nrounds = 38, 
              max_depth = 9, 
              min_child_weight = 1, 
              gamma = 0,
              subsample = 1, 
              colsample_bytree = 0.6, 
              lambda = 0.8, 
              lambda_bias = 5,
              alpha = 0.5)

pred <- predict(xg, newdata = cov_data_m)
1 - var(DTB_data_m - pred, na.rm = TRUE) / var(DTB_data - mean(DTB_data_m), na.rm=TRUE)
sqrt(mean((DTB_data_m - pred)^2, na.rm=TRUE))
mean(DTB_data_m - pred)
xg_file_name <- "D:/DTB100/xg.RData"
if(!file.exists(xg_file_name))
{
  save(xg, file = xg_file_name)
}

library(quantregForest)
qrf <- quantregForest(x = cov_data, y = DTB_data, keep.inbag = TRUE, ntree = 800, mtry = ncol(cov_data) / 3)
1 - var(qrf$y - qrf$predicted, na.rm = TRUE) / var(qrf$y - mean(qrf$y), na.rm = TRUE)
sqrt(mean((DTB_data - qrf$predicted)^2, na.rm=TRUE))
qrf_file_name <- "D:/DTB100/qrf.RData"
if(!file.exists(qrf_file_name))
{
  save(qrf, file = qrf_file_name)
}


