install.packages('mlbench')
library(mlbench)
data("BreastCancer")
dim(BreastCancer) #699rows 11colunms head(BreastCancer,3) #print the first three rows summary(BreastCancer)
?BreastCancer
class(BreastCancer[2,11]) #factor not numeric is.factor(BreastCancer$Class) #TRUE
#Print 24th row of Breast Cancer data and note there is a NA in the Bare.nuclei column: BreastCancer[24,]
#Test whether each element on the 24th row is a NA:
is.na(BreastCancer[24,])
#Count the number of NA in the whole BreastCancer dataset table(is.na(BreastCancer))
#Returns the coordinates of the corresponding row of missing values which(is.na(BreastCancer),arr.ind = TRUE)
#Delete rows containing NA
MyClean=na.omit(BreastCancer)
#We can then use the ‘table’ function to check whether the new data set still contains NA table(is.na(MyClean)) #no number of TRUE
typeof(MyClean$Class) #integer
head(as.integer(MyClean$Class)) #value is 1 or 2
#Delete the first column of Sample code number and the 11th column of Class
#Add a new column for the response variable Class with a value between 0 and 1 MyCancer = data.frame(MyClean[,-c(1,11)] ,Class=as.integer(MyClean$Class)-1)
#Convert the first nine columns of predictor variables to numeric MyCancer$Cl.thickness=as.numeric(MyCancer$Cl.thickness) MyCancer$Cell.size=as.numeric(MyCancer$Cell.size) MyCancer$Cell.shape=as.numeric(MyCancer$Cell.shape) MyCancer$Marg.adhesion=as.numeric(MyCancer$Marg.adhesion) MyCancer$Epith.c.size=as.numeric(MyCancer$Epith.c.size) MyCancer$Bare.nuclei=as.numeric(MyCancer$Bare.nuclei) MyCancer$Bl.cromatin=as.numeric(MyCancer$Bl.cromatin) MyCancer$Normal.nucleoli=as.numeric(MyCancer$Normal.nucleoli) MyCancer$Mitoses=as.numeric(MyCancer$Mitoses)
#Check that the nine cytological characteristics (predictor variables) are already numerical str(MyCancer)
head(MyCancer,3)
dim(MyCancer) #683 rows 10 columns


table(MyCancer$Class) #0-benign:444,1-malignant:239
pairs(MyCancer[,1:9],col=MyCancer[,10]+1) cor(MyCancer) #Find linear relationships
#Logistic model-full model
n=nrow(MyCancer)
p=ncol(MyCancer)-1
logreg_fit = glm(Class ~ ., data=MyCancer, family="binomial") summary(logreg_fit)
summary(logreg_fit)$coef
summary(logreg_fit)$coef[,4]
phat = predict(logreg_fit, MyCancer, type="response")
yhat = as.numeric(ifelse(phat > 0.5, 1, 0)) 1-mean(yhat==MyCancer$Class) #train error:0.03074671
#k-fold: Set the seed (say, at 5) to make the analysis reproducible set.seed(5)
## Sample the fold-assignment index
nfolds = 10
fold_index = sample(nfolds, n, replace=TRUE)
#A random sample of 683 numbers from 0-10 with release ## Print the first few fold-assignments
head(fold_index)
##10-fold test error function logistic_reg_fold_error = function(X, y, test_data) {
Xy = data.frame(X, y=y)
if(ncol(Xy)>1) tmp_fit = glm(y ~ ., data=Xy[!test_data,], family="binomial") else tmp_fit = glm(y ~ 1, data=Xy[!test_data,,drop=FALSE], family="binomial") phat = predict(tmp_fit, Xy[test_data,,drop=FALSE], type="response")
yhat = ifelse(phat > 0.5, 1, 0)
yobs = y[test_data]
test_error = 1 - mean(yobs == yhat)
return(test_error)
}
general_cv = function(X, y, fold_ind, fold_error_function) {
  p = ncol(X)
  Xy = cbind(X, y=y)
  nfolds = max(fold_ind)
  if(!all.equal(sort(unique(fold_ind)), 1:nfolds)) stop("Invalid fold partition.") fold_errors = numeric(nfolds)
  for(fold in 1:nfolds) {
    fold_errors[fold] = fold_error_function(X, y, fold_ind==fold) }
  fold_sizes = numeric(nfolds)
  for(fold in 1:nfolds) fold_sizes[fold] = length(which(fold_ind==fold)) test_error = weighted.mean(fold_errors, w=fold_sizes) return(test_error)
}
test_error = general_cv(MyCancer[,1:p], MyCancer[,p+1], fold_index, logistic_reg_fold_error)
#the full model test error(10-folds):0.03367496
## Calculation of test errors for the full model using the validation set method train_set=sample(c(TRUE,FALSE),nrow(MyCancer),replace=TRUE) lr_train=glm(Class~.,data=MyCancer[train_set,],family='binomial') summary(lr_train) phat_test2=predict(lr_train,MyCancer[!train_set,],family='binomial') yhat_test2=ifelse(phat_test2>0.5,1,0) 1-mean(yhat_test2==MyCancer$Class[!train_set])
##Best subset selection in logistic regression model library(leaps)
library(bestglm)
?bestglm
bss_fit_AIC = bestglm(MyCancer, family=binomial, IC="AIC") bss_fit_BIC = bestglm(MyCancer, family=binomial, IC="BIC") bss_fit_AIC$Subsets
bss_fit_BIC$Subsets
## Identify best-fitting models best_AIC=bss_fit_AIC$ModelReport$Bestk #7 best_BIC=bss_fit_BIC$ModelReport$Bestk #5
##k-fold verification in the best subset logistic_reg_bss_cv = function(X, y, fold_ind) {
p = ncol(X)
Xy = data.frame(X, y=y)
X = as.matrix(X)
nfolds = max(fold_ind)
if(!all.equal(sort(unique(fold_ind)), 1:nfolds)) stop("Invalid fold partition.") fold_errors = matrix(NA, nfolds, p+1)
for(fold in 1:nfolds) {
  tmp_fit = bestglm(Xy[fold_ind!=fold,], family=binomial, IC="AIC") best_models = as.matrix(tmp_fit$Subsets[,2:(1+p)])
  for(k in 1:(p+1)) {
    fold_errors[fold, k] = logistic_reg_fold_error(X[,best_models[k,]], y, fold_ind==fold)
    
  } }
fold_sizes = numeric(nfolds)
for(fold in 1:nfolds) fold_sizes[fold] = length(which(fold_ind==fold)) test_errors = numeric(p+1)
for(k in 1:(p+1)) {
  test_errors[k] = weighted.mean(fold_errors[,k], w=fold_sizes) }
return(test_errors) }
## Apply the cross-validation for best subset selection function
cv_errors = logistic_reg_bss_cv(MyCancer[,1:p], MyCancer[,p+1], fold_index)
#0.34992679 0.07027818 0.04685212 0.04099561 0.04245974 0.03660322 0.03367496 0.03221083 0.03367496 0.03367496
## Identify the number of predictors in the model which minimises test error
best_cv = which.min(cv_errors) – 1 #7
## k-fold validation in the best subset to calculate the test error pstar=7
#check which predictors are in the 7-predictor model bss_fit_AIC$Subsets[pstar+1,]
## Construct a reduced data set containing only the 7 selected predictors bss_fit_AIC$Subsets[pstar+1, 2:(p+1)]
indices = which(bss_fit_AIC$Subsets[pstar+1, 2:(p+1)]==TRUE) MyCancer_red=MyCancer[,c(indices,p+1)]
# Obtain regression coefficients for this best subset model
logreg1_fit = glm(Class ~ ., data=MyCancer_red, family="binomial")
summary(logreg1_fit)
test_error_red = general_cv(MyCancer_red[,1:pstar], MyCancer_red[,pstar+1], fold_index, logistic_reg_fold_error)
test_error_red #0.03221083
# Calculating training error in the best subset logreg_fit_train=glm(Class ~ .,data=MyCancer_red,family='binomial') phat_log_train=predict(logreg_fit_train,MyCancer_red,type='response') yhat_log_train=ifelse(phat_log_train>0.5,1,0) 1-mean(yhat_log_train==MyCancer_red$Class) # 0.03074671
## Test errors for the best subset calculated using the validation set method summary(glm(Class~Cl.thickness+Cell.shape+Marg.adhesion+Bare.nuclei+Bl.cromatin+Normal.nu cleoli+Mitoses,family='binomial',data=MyCancer)) lr_red_train=glm(Class~Cl.thickness+Cell.shape+Marg.adhesion+Bare.nuclei+Bl.cromatin+Normal .nucleoli+Mitoses,data=MyCancer[train_set,],family='binomial')
head(lr_red_train)

phat_red_test=predict(lr_red_train,MyCancer[!train_set,],type='response') yhat_red_test=ifelse(phat_red_test>0.5,1,0) 1-mean(yhat_red_test==MyCancer$Class[!train_set])
#test error of 7-variables subset model: 0.4836795
#LDA model with 7-vriables
install.packages('dplyr')
library(MASS)
## MyCancer_red=MyCancer[,c(indices,p+1)]
lda_fit = lda(Class ~ ., data=MyCancer_red)
lda_predict = predict(lda_fit, MyCancer_red)
lda_fit$prior
lda_fit$means
lda_fit$scaling
yhat_lda = lda_predict$class
confusion_lda = table(Observed=MyCancer_red$Class, Predicted=yhat_lda) 1-sum(diag(confusion_lda))/sum(confusion_lda)
1 - mean(MyCancer_red$Class == yhat_lda) #training error: 0.03953148
## Use 10-fold to verify test error in the LDA model (7-variable) lda_fold_error = function(X, y, test_data) {
Xy = data.frame(X, y=y)
if(ncol(Xy)>1) tmp_fit = lda(y ~ ., data=Xy[!test_data,]) tmp_predict = predict(tmp_fit, Xy[test_data,])
yhat = tmp_predict$class
yobs = y[test_data]
test_error = 1 - mean(yobs == yhat)
return(test_error)
}
test_error_lda = general_cv(MyCancer_red[,1:pstar], MyCancer_red[,pstar+1], fold_index, lda_fold_error)
#test error:0.04099561(10-fold cross-validation)
## 7-variable lda model validation test error with validation set lda_train=lda(Class~Cl.thickness+Cell.shape+Marg.adhesion+Bare.nuclei+Bl.cromatin+Normal.nu cleoli+Mitoses,data=MyCancer_red[train_set,]) lda_test=predict(lda_train,MyCancer_red[!train_set,])
yhat_test_lda=lda_test$class 1-mean(yhat_test_lda==MyCancer_red$Class[!train_set])
#QDA model with 7-variables
qda_fit = qda(Class ~ ., data=MyCancer_red) summary(qda_fit)
qda_predict = predict(qda_fit, MyCancer_red)

yhat_qda = qda_predict$class
confusion_qda = table(Observed=MyCancer_red$Class, Predicted=yhat_qda) 1-sum(diag(confusion_qda))/sum(confusion_qda) 1-mean(yhat_qda==MyCancer_red$Class) #training error: 0.04538799
#10-fold test error for QDA model qda_fold_error = function(X, y, test_data) {
Xy = data.frame(X, y=y)
if(ncol(Xy)>1) tmp_fit = qda(y ~ ., data=Xy[!test_data,]) tmp_predict = predict(tmp_fit, Xy[test_data,])
yhat = tmp_predict$class
yobs = y[test_data]
test_error = 1 - mean(yobs == yhat)
return(test_error)
}
test_error_qda = general_cv(MyCancer_red[,1:pstar], MyCancer_red[,pstar+1], fold_index, qda_fold_error)
#validation set method test error for QDA model qda_train=qda(Class~Cl.thickness+Cell.shape+Marg.adhesion+Bare.nuclei+Bl.cromatin+Normal.n ucleoli+Mitoses, data=MyCancer_red[train_set,]) qda_test=predict(qda_train,MyCancer_red[!train_set,])
yhat_test_qda=qda_test$class
1-mean(yhat_test_qda==MyCancer_red$Class[!train_set])