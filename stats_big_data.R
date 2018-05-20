library(MASS)
data_gen<-function(N,p){
  mu<-rep(0,p)
  Sigma<-matrix(.2,nrow=p,ncol=p)+diag(p)*0.15
  dataset<-as.data.frame(mvrnorm(n=N,mu = mu,Sigma=Sigma))
  dataset$psi<-0
  coeffs<-rnorm(p,4,2.7)
  for (i in 1:dim(dataset)[1]){
    dataset$psi[i]<-drop(coeffs %*% as.matrix(dataset)[i,1:p])+rnorm(1,0,8)
    print(i)
  }}
#Trying to push memory a bit more, we tried an increasing number of p to
#
##(10k,100)
data_gen(10000,100)
t0<-Sys.time()
results<-lm(psi~.,data = dataset)
t2<-Sys.time()
t2-t0
summary(results)
#recursive regression
library(biglm)
kapa<-100
t0<-Sys.time()
formula <- as.formula(paste('psi',paste(paste('', paste('V', 1:kapa, sep = ''),
                            sep = '', collapse = ' + ')), sep = ' ~ '))
formula
results2<-bigglm(formula,data = dataset,
       chunksize = 5*1e+02, family = gaussian())
t2<-Sys.time()
t2-t0
summary(results2)
#
###(10k/400)
data_gen(10000,400)
t0<-Sys.time()
results<-lm(psi~.,data = dataset)
t2<-Sys.time()
t2-t0
summary(results)
#recursive regression
library(biglm)
kapa<-400
t0<-Sys.time()
formula <- as.formula(paste('psi',paste(paste('', paste('V', 1:kapa, sep = ''),
                                              sep = '', collapse = ' + ')), sep = ' ~ '))
formula
results2<-bigglm(formula,data = dataset,
                 chunksize = 5*1e+02, family = gaussian())
t2<-Sys.time()
t2-t0
summary(results2)


###(50k/400)
data_gen(50000,600)
t0<-Sys.time()
results<-lm(psi~.,data = dataset)
t2<-Sys.time()
t2-t0
summary(results)
#recursive regression
library(biglm)
kapa<-600
t0<-Sys.time()
formula <- as.formula(paste('psi',paste(paste('', paste('V', 1:kapa, sep = ''),
                                              sep = '', collapse = ' + ')), sep = ' ~ '))
formula
results2<-bigglm(formula,data = dataset,
                 chunksize = 5*1e+03, family = gaussian())
t2<-Sys.time()
t2-t0
summary(results2)
