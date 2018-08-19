### remove or add some covariates from matrix for modelling

load(file = "F:/New/d.RData")


load(file = "F:/New/RData/d.RData")
dOri<-d
which("TX5MOD3a"==colnames(dOri))
d<-d[,c(-50,-8,-21,-41,-34)]
d<-d[,c(-50,-8,-21,-41,-34,-32,-40,-29,-27)]

d<-d[,c(-50,-8,-21,-41,-34,-32,-40,-29,-27,-39,-20,-24)]
