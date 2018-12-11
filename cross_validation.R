# to get the best parameters to build models with the best performance
# using cross validation
# 2018/11/06

load("D:/DTB100/DATA.RData")
n_fold <- 10
crossValidation <- function(model, data, n_fold,
                            n_tree = 800, ntry = 15,
                            eta = 0.08, gamma = 0, max_depth = 6, min_c_w = 1, subsample = 1, colsample_byt = 1, nrounds = 36)
{
  num_of_samples <- nrow(data)
  sub_num <- floor(num_of_samples/n_fold)
  cv_result <- data.frame(measured = NA, predicted = NA)
  cv_result <- cv_result[-1, ]
  
  all_index <- 1:num_of_samples
  sampled_index<-vector()
  index_list<-list()
  for(i in 1:n_fold)
  {
    if(i == n_fold)
    {
      index_list <- c(index_list,list(all_index))
      break
    }
    sub_index <- sample(all_index, sub_num)
    index_list <- c(index_list, list(sub_index))
    sampled_index <- c(sampled_index, sub_index)
    all_index <- 1:num_of_samples
    all_index <- all_index[-sampled_index]
  }
  pred <- vector()
  for(j in 1:n_fold)
  {
    train_index <- vector()
    test_index <- index_list[[j]]
    for(k in (1:n_fold)[-j])
    {
      train_index <- c(train_index,index_list[[k]])  
    }
    
    train_data <- data[train_index, ]
    test_data <- data[test_index, ]
    
    train_cov_data <- train_data[ , !(colnames(data) %in% c("ID", "LON", "LAT", "DTB"))]
    train_DTB_data <- train_data[ , "DTB"]
    
    test_cov_data <- test_data[ , !(colnames(data) %in% c("ID", "LON", "LAT", "DTB"))]
    test_DTB_data <- test_data[ , "DTB"]

    if(model == 1)
    {
      library(randomForest)
      rf <- randomForest(x = train_cov_data, y = train_DTB_data, ntree = n_tree, ntry = ntry)
      pred <- predict(rf, newdata = test_cov_data)
      
    }
    else if(model == 2)
    {
      train_cov_data_m <- as.matrix(train_cov_data)
      train_DTB_data_m <- as.matrix(train_DTB_data)
      
      test_cov_data_m <- as.matrix(test_cov_data)
      test_DTB_data_m <- as.matrix(test_DTB_data)
      
      library(xgboost)
      xg <- xgboost(data = train_cov_data_m, 
                     label = train_DTB_data_m, 
                     silent = 1,
                     eta = eta,
                     nrounds = nrounds,
                     gamma = gamma,
                     max_depth = max_depth,
                     min_child_weight = min_c_w,
                     subsample = subsample,
                     colsample_bytree = colsample_byt,
                     num_parallel_bytree = 1)
      
      pred<-predict(xg, newdata = test_cov_data_m)
    }
    cv_result<- rbind(cv_result, data.frame(measured = test_DTB_data, predicted = pred))
  }
  return(cv_result)
}
############################################################################
crossValidationForRF <- function(data = DATA, 
                                 n_tree = 1000, 
                                 mtry = 18)
{
  n_fold <- 10
  num_of_samples <- nrow(DATA)
  sub_num <- floor(num_of_samples/n_fold)
  cv_result <- data.frame(ID = NA, measured = NA, predicted = NA)
  cv_result <- cv_result[-1, ]
  
  all_index <- 1:num_of_samples
  sampled_index<-vector()
  index_list<-list()
  for(i in 1:n_fold)
  {
    if(i == n_fold)
    {
      index_list <- c(index_list,list(all_index))
      break
    }
    sub_index <- sample(all_index, sub_num)
    index_list <- c(index_list, list(sub_index))
    sampled_index <- c(sampled_index, sub_index)
    all_index <- 1:num_of_samples
    all_index <- all_index[-sampled_index]
  }
  pred <- vector()
  for(j in 1:n_fold)
  {
    train_index <- vector()
    test_index <- index_list[[j]]
    for(k in (1:n_fold)[-j])
    {
      train_index <- c(train_index,index_list[[k]])  
    }
    
    train_data <- DATA[train_index, ]
    test_data <- DATA[test_index, ]
    
    train_cov_data <- train_data[ , !(colnames(DATA) %in% c("ID", "LON", "LAT", "DTB"))]
    train_DTB_data <- train_data[ , "DTB"]
    
    test_cov_data <- test_data[ , !(colnames(DATA) %in% c("ID", "LON", "LAT", "DTB"))]
    test_DTB_data <- test_data[ , "DTB"]
    test_ID <- test_data[, "ID"]
    
    library(randomForest)
    rf <- randomForest(x = train_cov_data, y = train_DTB_data, ntree = n_tree, mtry = mtry)
    pred <- predict(rf, newdata = test_cov_data)
      
    cv_result<- rbind(cv_result, data.frame(ID = test_ID, measured = test_DTB_data, predicted = pred))
  }
  return(cv_result)
}

crossValidationForQRF <- function(data = DATA, 
                                  n_tree = 1000, 
                                  mtry = 18)
{
  n_fold <- 10
  num_of_samples <- nrow(DATA)
  sub_num <- floor(num_of_samples/n_fold)
  cv_result <- data.frame(ID = NA, measured = NA, predicted = NA)
  cv_result <- cv_result[-1, ]
  
  all_index <- 1:num_of_samples
  sampled_index<-vector()
  index_list<-list()
  for(i in 1:n_fold)
  {
    if(i == n_fold)
    {
      index_list <- c(index_list,list(all_index))
      break
    }
    sub_index <- sample(all_index, sub_num)
    index_list <- c(index_list, list(sub_index))
    sampled_index <- c(sampled_index, sub_index)
    all_index <- 1:num_of_samples
    all_index <- all_index[-sampled_index]
  }
  pred <- vector()
  for(j in 1:n_fold)
  {
    train_index <- vector()
    test_index <- index_list[[j]]
    for(k in (1:n_fold)[-j])
    {
      train_index <- c(train_index,index_list[[k]])  
    }
    
    train_data <- DATA[train_index, ]
    test_data <- DATA[test_index, ]
    
    train_cov_data <- train_data[ , !(colnames(DATA) %in% c("ID", "LON", "LAT", "DTB"))]
    train_DTB_data <- train_data[ , "DTB"]
    
    test_cov_data <- test_data[ , !(colnames(DATA) %in% c("ID", "LON", "LAT", "DTB"))]
    test_DTB_data <- test_data[ , "DTB"]
    test_ID <- test_data[, "ID"]
    
    library(quantregForest)
    qrf <- quantregForest(x = train_cov_data, y = train_DTB_data, 
                         ntree = n_tree, 
                         mtry = mtry,
                         keep.inbag = TRUE)
    pred <- predict(qrf, newdata = test_cov_data, what = 0.5)
      
    cv_result<- rbind(cv_result, data.frame(ID= test_ID, measured = test_DTB_data, predicted = pred))
  }
  return(cv_result)
}

crossValidationForXG <- function(data = DATA,
                               eta = 0.1, 
                               nrounds = 38, 
                               max_depth = 9, 
                               min_child_weight = 0, 
                               gamma = 0, 
                               subsample = 1, 
                               colsample = 0.6,
                               lambda = 0.8,
                               lambda_bias = 5,
                               alpha = 0.5)
{
  num_of_samples <- nrow(DATA)
  sub_num <- floor(num_of_samples/n_fold)
  cv_result <- data.frame(ID = NA, measured = NA, predicted = NA)
  cv_result <- cv_result[-1, ]
  
  all_index <- 1:num_of_samples
  sampled_index<-vector()
  index_list<-list()
  for(i in 1:n_fold)
  {
    if(i == n_fold)
    {
      index_list <- c(index_list,list(all_index))
      break
    }
    sub_index <- sample(all_index, sub_num)
    index_list <- c(index_list, list(sub_index))
    sampled_index <- c(sampled_index, sub_index)
    all_index <- 1:num_of_samples
    all_index <- all_index[-sampled_index]
  }
  pred <- vector()
  for(j in 1:n_fold)
  {
    train_index <- vector()
    test_index <- index_list[[j]]
    for(k in (1:n_fold)[-j])
    {
      train_index <- c(train_index,index_list[[k]])  
    }
    
    train_data <- DATA[train_index, ]
    test_data <- DATA[test_index, ]
    
    train_cov_data <- train_data[ , !(colnames(DATA) %in% c("ID", "LON", "LAT", "DTB"))]
    train_DTB_data <- train_data[ , "DTB"]
    
    test_cov_data <- test_data[ , !(colnames(DATA) %in% c("ID", "LON", "LAT", "DTB"))]
    test_DTB_data <- test_data[ , "DTB"]
    test_ID <- test_data[, "ID"]
    
    train_cov_data_m <- as.matrix(apply(train_cov_data, 2, as.numeric))
    train_DTB_data_m <- as.matrix(train_DTB_data)
    
    test_cov_data_m <- as.matrix(apply(test_cov_data, 2, as.numeric))
    test_DTB_data_m <- as.matrix(test_DTB_data)
    
    library(xgboost)
    xg <- xgboost(data = train_cov_data_m, label = train_DTB_data_m, 
              eta = eta, 
              nrounds = nrounds, 
              max_depth = max_depth, 
              min_child_weight = min_child_weight, 
              gamma = gamma, 
              subsample = subsample, 
              colsample_bytree = colsample,
              lambda = lambda, 
              lambda_bias = lambda_bias,
              alpha = alpha)
    
    pred<-predict(xg, newdata = test_cov_data_m)
    cv_result<- rbind(cv_result, data.frame(ID = test_ID, measured = test_DTB_data, predicted = pred))
  }
  return(cv_result)
}

# ret2 <- crossValidationForXG()
# R2(ret2)

R2 <- function(ret)
{
  return(1 - var(ret$measured - ret$predicted, na.rm = TRUE) / var(ret$measured - mean(ret$measured, na.rm = TRUE)))
}

RMSE <- function(ret)
{
  return(sqrt(mean((ret$measured - ret$predicted)^2, na.rm=TRUE)))
}

ME <- function(ret)
{
  return(mean(ret$measured - ret$predicted, na.rm=TRUE))
}

cvTestForXG <- function()
{
  cv_ret <- data.frame(eta = NA, 
                       nrounds = NA, 
                       max_depth = NA, 
                       min_child_weight = NA, 
                       gamma = NA, 
                       subsample = NA, 
                       colsample = NA, 
                       lambda = NA,
                       lambda_bias = NA,
                       alpha = NA)
  
  cv_ret <- cv_ret[-1,]
  count <- 0
  for(eta in 0.1)
    for(gamma in 0)
      for(max_depth in 9)
        for(min_c_w in 0)
          for(subsample in 1)
            for(colsample in 0.6)
              for(nrounds in 38)
                for(lambda in c(0.5, 0.7, 0.8, 1))
                  for(lambda_bias in c(3, 5, 8, 10))
                    for(alpha in c(0.5, 0.7, 0.8, 1))
                    {
                      ret <- crossValidationForXG(data = DATA,
                                                  eta = eta, 
                                                  nrounds = nrounds, 
                                                  max_depth = max_depth, 
                                                  min_child_weight = min_c_w, 
                                                  gamma = gamma, 
                                                  subsample = subsample, 
                                                  colsample = colsample,
                                                  lambda = lambda,
                                                  lambda_bias = lambda_bias,
                                                  alpha = alpha)
                      r2 <- R2(ret)
                      ret_df <- data.frame(eta, nrounds, max_depth, min_c_w, gamma, subsample, 
                                           colsample, lambda, lambda_bias, alpha, r2)
                      cv_ret <- rbind(cv_ret, ret_df)
                      
                      count <- count + 1
                    }
  print(count)
  best_para <- cv_ret[which.max(cv_ret$r2), ]
  write.table(best_para, file = "D:/DTB100/best_para_xg_4.txt", row.names = FALSE)
}

cvTestForRF <- function()
{
  cv_ret <- data.frame(ntree = NA, ntry = NA)
  cv_ret <- cv_ret[-1, ]
  n <- ncol(DATA) - 4
  for(n_tree in c(900, 1000, 1100)[2])
  {
    for(mtry in seq(floor(n / 3 - 5), n / 3 + 5, 2)[4])
    {
      ret <- crossValidationForRF(DATA, n_tree, mtry)
      r2 <- R2(ret)
      cv_ret <- rbind(cv_ret, data.frame(n_tree, mtry, r2))
    }
    print(n_tree)
  }
  best_para <- cv_ret[which.max(cv_ret$r2), ]
  write.table(best_para, file = "D:/DTB100/best_para_rf.txt", row.names = FALSE)
}


#cvTestForXG()

#cvTestForRF()

x <- crossValidationForXG(data = DATA,
                     eta = 0.1,
                     nrounds = 38,
                     max_depth = 9,
                     min_child_weight = 1,
                     gamma = 0,
                     subsample = 1,
                     colsample = 0.6,
                     lambda = 0.8,
                     lambda_bias = 5,
                     alpha = 0.5)
R2(x)
RMSE(x)

# x <- crossValidationForRF(data = DATA, 
#                           n_tree = 800, 
#                           mtry = 19)
# R2(x)
# RMSE(x)



