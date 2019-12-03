
library(gstat)
library(sp)
load(file = "D:/SCI data/data.RData")

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

d <- DATA[2:4]
spdf <- DATA[2:4]
coordinates(spdf) <- ~LON+LAT
pt <- DATA[2:3]
coordinates(pt) <- ~LON+LAT

#######
#n_fold折交叉验证
######
n_fold <- 7
samples_num <- nrow(d)
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




col_names <- colnames(DATA)
col_num <- length(col_names)
cov_names <- col_names[5:col_num]
samples_num <- nrow(DATA)

trend_string <- paste0("DTB~", paste0(cov_names, collapse = "+"))
trend <- as.formula(trend_string)
glm_model <- glm(formula = trend, 
                 family = gaussian(link = "identity"),
                 data = DATA[, 4:col_num])
summary(glm_model)
final_glm_model <- step(glm_model)
glm_res <- final_glm_model$fitted.values - final_glm_model$y
#######################
#生成glm残差的半变异函数
#######################
glm_spdf <- DATA[,2:3]
glm_spdf$RES <- glm_res
coordinates(glm_spdf) <- ~LON+LAT
glm_v <- variogram(RES ~ 1, glm_spdf, width = 1)
plot(glm_v, main = "Variogram of residuals(GLM)")
glm_vgm <- vgm(psill = 4000, range = 5, nugget = 2700, model = "Sph")
glm_fit_var <- fit.variogram(glm_v, glm_vgm)
plot(glm_v, model = glm_fit_var)


##############################
#回归克里金交叉验证
##############################
##用于保存测量值和预测值
y_pred <- data.frame(measured=NA, predicted=NA) 
y_pred <- y_pred[-1,]
for(j in 1:n_fold)
{
    #将数据分为训练集和验证集
    train_index <- vector()
    test_index <- index_list[[j]]
    for(k in (1:n_fold)[-j])
    {
        train_index <- c(train_index,index_list[[k]])  
    }
    
    train_data <- DATA[train_index, 4:col_num]
    test_data <- DATA[test_index, 4:col_num]
    test_points <- DATA[test_index, 2:3]
    coordinates(test_points) <- ~LON+LAT
    
    #模型预测
    glm_model <- lm(formula = trend,
                    #family = gaussian(link = "identity"), 
                    data = train_data)
    final_glm_model <- step(glm_model)
    glm_pred <- predict(final_glm_model, 
                        newdata = test_data[, c(-1)], 
                        type = "response")
    
    res <- final_glm_model$fitted.values - DATA[train_index, 4]
    krige_data <- data.frame(LON = DATA[train_index,2], 
                             LAT = DATA[train_index,3], 
                             RES = res)
    
    kg_model <- krige(RES~1, 
                      loc = ~LON+LAT, 
                      data = krige_data, 
                      newdata = test_points, 
                      model = glm_fit_var)
    glm_res <- kg_model@data$var1.pred
    pred = glm_pred + glm_res
    y_pred <- rbind(y_pred, data.frame(measured=test_data[, 1], predicted=pred))
    print(j)
}
R2(y_pred)
RMSE(y_pred)
R2(y_pred)
