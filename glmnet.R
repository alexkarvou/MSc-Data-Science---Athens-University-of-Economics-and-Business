#######################
# Logistic Regression
#######################

library("AER")
data(HMDA)
head(HMDA)
?HMDA

# Build Unregularized Model
hmda_glm <- glm(deny~., data = HMDA, family = binomial(link = "logit"))
summary(hmda_glm)

# Lasso with glmnet
library(glmnet)
hmda_mat <- model.matrix(deny~., HMDA)[,-1]
lambdas <- 10 ^ seq(8,-4,length=250)
hmda_models_lasso <- glmnet(hmda_mat,HMDA$deny,alpha=1, lambda=lambdas, family="binomial")
hmda_models_ridge <- glmnet(hmda_mat,HMDA$deny,alpha=0, lambda=lambdas, family="binomial")

#to optimization ginetai sunhthws ws pros to lambda. Ws pros to alpha einai too much kai den einai arketa
#megalh h diafora sto prediction accuracy
# Plots
plot(hmda_models_lasso)
print(hmda_models_lasso)
coef(hmda_models_lasso,s=0.1)
coef(hmda_models_lasso,s=0.5)
plot(hmda_models_lasso, xvar = "dev", label = TRUE)
plot(hmda_models_lasso, xvar = "lambda", label = TRUE)

# Lasso Models
print(hmda_models_lasso)

# Extract Coefficients
coef(hmda_models_lasso, s = c(1,0.1,0.01,0.001))

# Cross Validation
lasso.cv <- cv.glmnet(hmda_mat,HMDA$deny, alpha=1, lambda=lambdas, family="binomial")

# Alternative Measures of Performance
lasso.cv2 <- cv.glmnet(hmda_mat,HMDA$deny,alpha=1,lambda=lambdas, family="binomial", type.measure = "class")
lasso.cv3 <- cv.glmnet(hmda_mat,HMDA$deny,alpha=1,lambda=lambdas, family="binomial", type.measure = "auc")
lasso.cv4 <- cv.glmnet(hmda_mat,HMDA$deny,alpha=1,lambda=lambdas, family="binomial", type.measure = "mse")
lasso.cv5 <- cv.glmnet(hmda_mat,HMDA$deny,alpha=1,lambda=lambdas, family="binomial", type.measure = "mae")

# Cross Validation Plots
plot(lasso.cv)
plot(lasso.cv2)
plot(lasso.cv3)
plot(lasso.cv4)
lasso.cv$lambda.min
lasso.cv$lambda.1se

# Many Facets of predict()
predict(hmda_models_lasso, type="coefficients", s = lasso.cv$lambda.min)
preds <- predict(hmda_models_lasso, hmda_mat, type = "response", s = c(lasso.cv$lambda.min, lasso.cv$lambda.1se))
head(preds)
preds <- predict(hmda_models_lasso, hmda_mat, type = "class", s = lasso.cv$lambda.min)

# Performance
table(predicted = preds, actual = HMDA$deny)
mean(preds  == HMDA$deny)


#### compare with ridge and Elastic net
hmda_models_lasso <- glmnet(hmda_mat,HMDA$deny,alpha=1, lambda=lambdas, family="binomial")
hmda_models_ridge <- glmnet(hmda_mat,HMDA$deny,alpha=0, lambda=lambdas, family="binomial")
hmda_models_elastic <- glmnet(hmda_mat,HMDA$deny,alpha=0.5, lambda=lambdas, family="binomial")

plot(hmda_models_lasso)
plot(hmda_models_ridge)
plot(hmda_models_elastic)


lasso.cvL <- cv.glmnet(hmda_mat,HMDA$deny,alpha=1,lambda=lambdas, family="binomial", type.measure = "class")
lasso.cvR <- cv.glmnet(hmda_mat,HMDA$deny,alpha=0,lambda=lambdas, family="binomial", type.measure = "class")
lasso.cvE <- cv.glmnet(hmda_mat,HMDA$deny,alpha=0.5,lambda=lambdas, family="binomial", type.measure = "class")


predsL <- predict(hmda_models_lasso, hmda_mat, type = "class", s = lasso.cvL$lambda.min)
predsR <- predict(hmda_models_ridge, hmda_mat, type = "class", s = lasso.cvR$lambda.min)
predsE <- predict(hmda_models_elastic, hmda_mat, type = "class", s = lasso.cvE$lambda.min)
predsglm <- predict(hmda_glm, HMDA, type = "response")
predsglm<- (predsglm>0.5)
predsglm<- factor(predsglm,labels=c("no","yes"))

table(predsL,predsR)
mean(predsL  == HMDA$deny)
mean(predsR  == HMDA$deny)
mean(predsE  == HMDA$deny)
mean(predsglm  == HMDA$deny) #overfitting
table(HMDA$deny)/length(HMDA$deny)


#######################
# Poisson Regression
#######################

library("AER")
data(PhDPublications)
head(PhDPublications)

# Build Unregularized Model
phd_glm <- glm(articles~.,data = PhDPublications, family = "poisson")
summary(phd_glm)

#Interpret Coefficients
coef(phd_glm)
exp(coef(phd_glm))

# Lasso
phd_mat <- model.matrix(articles~., PhDPublications)[,-1]
lambdas <- 10 ^ seq(8,-4,length=250)
phd_models_lasso= glmnet(phd_mat, PhDPublications$articles, alpha=1, lambda=lambdas, family="poisson")

# Cross Validation
phd_lasso.cv <- cv.glmnet(phd_mat,PhDPublications$articles,alpha=1,lambda=lambdas, family="poisson")

# Performance
predict(phd_models_lasso, phd_mat, type = "link", s = phd_lasso.cv$lambda.min)
preds <- predict(phd_models_lasso, phd_mat, type = "response", s = phd_lasso.cv$lambda.min)