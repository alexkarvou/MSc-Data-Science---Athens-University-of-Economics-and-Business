###big lasso xrisimopoioume genika giati to model matrix 
###einai megethos gama ta kai xtupaei sto Rstudio
###akoma kai na to parakampseis na pas se klassiki R 
### me piragmata gia allowance kai sparse model matrices etc
###ftanei kapou sta 2,2 GB Ram mono tou kai xtupaei kai ekei

##edo loipon kovo tin apokrisis
X<-alldata_together[,-24485]
## tin kratao monh ths
y<-alldata_together[,24485]

library(bigmemory)
#edo to kanei big matrix object pou mpanei input sto biglasso
X.bm <- as.big.matrix(X)
#fitareis kai auto to screen einai kati optimal pou exoun vrei autoi sto paper
fit <- biglasso(X.bm, y, screen = "SSR-BEDPP")
plot(fit)
cvfit <- cv.biglasso(X.bm, y, seed = 1234, nfolds = 10, ncores = q)
summary(cvfit)