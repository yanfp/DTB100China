

load("D:/DTB100/RData/rf.RData")
tmp_rf <- rf

cov_code_desc <- read.table(file = "D:/DTB100/cov_code_attribute.txt", header = TRUE, sep = "\t")
apply(cov_code_desc, 2, as.character)
imp_tb <- tmp_rf$importance
imp_tb <- as.data.frame(imp_tb)
imp_tb$CODE <- rownames(imp_tb)

imp_tb <- merge(imp_tb, cov_code_desc, by = "CODE", all.x = TRUE)
imp_tb <- imp_tb[order(imp_tb[,2],decreasing = FALSE),]

### scale the importance to 0 ~ 1
imp_vec <- imp_tb[,2]
max_i <- max(imp_vec)
tmp_tmp_rf <- tmp_rf
tmp_rf$importance[, 1] <- tmp_rf$importance[, 1] / max_i


str(imp_tb)

cov_name_vec <- imp_tb$ATTRIBUTE_TITLE
len <- length(cov_name_vec)
cov_name_vec <- cov_name_vec[len:1]
cov_name_show <- cov_name_vec[1:30]
cov_name_show <- cov_name_show[30:1]

library(randomForest)
colnames(tmp_tmp_rf$importance) <- c("Scaled importance(%IncMSE)", "IncNodePurity")
windows()
varImpPlot(tmp_rf,
           labels = cov_name_show,
           n.var = 30,
           main = "Depth to bedrock of China",
           bg = "red",
           pt.cex = 1.5,
           scale = FALSE)
dev.off()




