######
#cross validation of generalized linear model
######



library(GSIF)
library(sp)

#glmÄ£ÐÍ

load(file = "D:/SCI data/data.RData")
col_names <- colnames(DATA)
col_num <- length(col_names)
cov_names <- col_names[5:col_num]
samples_num <- nrow(DATA)

trend_string <- paste0("DTB~", paste0(cov_names, collapse = "+"))
trend <- as.formula(trend_string)

# glm_model <- glm(formula = trend, family = gaussian(link = "identity"), data = DATA[1:5000, 4:col_num])
# summary(glm_model)
# glm_model2 <- step(glm_model)
# summary(glm_model2)
# 
# pred <- predict(glm_model2, newdata = DATA[5001:6000, 5:col_num], type = "response")
# mean(pred - DATA[5001:6000,]$DTB)
# plot(pred, DATA[5001:6000,]$DTB)


n_fold <- 10
sub_num <- floor(samples_num/n_fold)
all_index <- 1:samples_num
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
  all_index <- 1:samples_num
  all_index <- all_index[-sampled_index]
}

y_pred <- data.frame(measured=NA, predicted=NA) 
y_pred <- y_pred[-1,]
y_pred

for(j in 1:n_fold)
{
  train_index <- vector()
  test_index <- index_list[[j]]
  for(k in (1:n_fold)[-j])
  {
    train_index <- c(train_index,index_list[[k]])  
  }
  
  train_data <- DATA[train_index, 4:col_num]
  test_data <- DATA[test_index, 4:col_num]
  
  
  glm_model <- lm(formula = trend,
                   #family = gaussian(link = "identity"), 
                   data = train_data)
  glm_model2 <- step(glm_model)
  
  pred <- predict(glm_model2, newdata = test_data[, c(-1)], type = "response")
  y_pred <- rbind(y_pred, data.frame(measured=test_data[, 1], predicted=pred))
  
}


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
R2(y_pred)
RMSE(y_pred)


