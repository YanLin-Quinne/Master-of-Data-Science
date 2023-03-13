library(skimr)
library(dplyr)
library(ggcorrplot)
library(ggtext)

setwd("~/Desktop")
wine <- read.csv("winequality-red.csv")
d1<- read.table( "winequality-red.csv" , sep = ";" , header = TRUE )
d2<- read.table("winequality-white.csv", sep = ";" , header = TRUE)

#the red and white variants
#the correlations between the predictor variables vary greatly
cor(d1)
cor(d2)

#d1$colour="red"
#d2$colour="white"
#d4=rbind(d1,d2)
#d4$colour=c(rep("red",dim(d1)[1]),rep("white",dim(d2)[1]))
#d4$colour <- as.factor(d4$colour)
skim(d2)
colSums(is.na(d2)) #no NA value
sapply(d2, length)
str(d2)

table(d2$quality)
(1457+2198+880)/(20+163+1457+2198+880+175+5)
barplot(table(d2$quality), main = "Wine Quality Distribution", xlab = "Quality", ylab = "Frequency")


d2$quality <- as.numeric(ifelse(d2$quality >= 7, 1, 0))
table(d2$quality)

cor_matrix <- cor(mutate(d2))
cor_matrix

#d2_0 <- d2[d2$quality == 0]
#d2_1 <- d2[d2$quality == 1]
#var_matrix_0 <- var(d2_0)
#cov_matrix_0 <- cov(d2_0)
#var_matrix_1 <- var(d2_1)
#cov_matrix_1 <- cov(d2_1)

library(car)
scatterplotMatrix(d2[,11:1], 
                  col=d2[,12]+2, 
                  diagonal=list(method ="histogram"))

library(corrplot)
corrplot(cor(d2),method= "number",diag = FALSE)

ggcorrplot(cor_matrix, hc.order = TRUE, type = "upper",
           outline.color = "white", colors = c("#6D9EC1", "white", "#E46726"),
           lab = TRUE, lab_size = 3, lab_col = "black") +
  ggtitle("Correlation Matrix") +
  theme(plot.title = element_text(size = 16, hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

library(GGally)
ggpairs(d2[,11:1])

library(ggplot2)
ggplot(data = d2, aes(x = alcohol, y = density, color = as.factor(quality))) +
  geom_point() +
  labs(x = "Alcohol", y = "Density", color = "Quality") +
  scale_color_manual(values = c("blue", "red")) +
  theme_classic()

#Classification trees
par(mfrow=c(1,1))
library(tree)
set.seed(180)
d2=na.omit(d2)
d2$quality <- as.factor(ifelse(d2$quality >= 7, 'good', 'bad'))
table(d2$quality)
str(d2$quality)
#good not good 
#1060     3838 

train=sample(1:nrow(d2),floor(nrow(d2)/2))
data_train=d2[train,]
data_test=d2[-train,]

tree.wine=tree(quality~.,data_train)
summary(tree.wine)
plot(tree.wine);text(tree.wine,pretty=0)

tree.pred=predict(tree.wine,data_test,type='class')
class_table=table(tree.pred,data_test$quality)
success_rate=(class_table[1,1]+class_table[2,2])/sum(class_table)
success_rate #0.7966517

cv.wine=cv.tree(tree.wine,FUN=prune.misclass)
cv.wine
plot(cv.wine)

prune.wine=prune.misclass(tree.wine, best=3) 
prune.wine
summary(prune.wine)
plot(prune.wine);text(prune.wine,pretty=0)

tree.pred=predict(prune.wine,data_test,type='class')
class_table=table(tree.pred,data_test$quality)
success_rate=(class_table[1,1]+class_table[2,2])/sum(class_table)
success_rate  #0.7937934


library(randomForest)
set.seed(472)
bag.wine=randomForest(quality~.,data=data_train,mtry=11,ntree=10,importance=TRUE)
bag.wine
summary(bag.wine)
pred.bag=predict(bag.wine,newdata=data_test,type='class')
class_table=table(pred.bag,data_test$quality)
success_rate_bagging=(class_table[1,1]+class_table[2,2])/sum(class_table)
success_rate_bagging #0.8370764

bag.wine.100=randomForest(quality~.,data=data_train,mtry=11,ntree=100,importance=TRUE)
bag.wine.100
pred.bag.100=predict(bag.wine.100,newdata=data_test,type='class')
class_table=table(pred.bag.100,data_test$quality)
success_rate_bagging=(class_table[1,1]+class_table[2,2])/sum(class_table)
success_rate_bagging #0.8562679

bag.wine.1000=randomForest(quality~.,data=data_train,mtry=11,ntree=1000,importance=TRUE)
summary(bag.wine.1000)
pred.bag.1000=predict(bag.wine.1000,newdata=data_test,type='class')
class_table=table(pred.bag.1000,data_test$quality)
success_rate_bagging=(class_table[1,1]+class_table[2,2])/sum(class_table)
success_rate_bagging #0.8562679

# check the importance feature
importance(bag.wine.1000)
varImpPlot(bag.wine.1000)

# check the Gini coefficient
var.gini <- function(x) {
  1 - sum((x/sum(x))^2)
}
bag.gini <- apply(bag.wine.1000$confusion, 1, var.gini)
names(bag.gini) <- rownames(bag.wine.1000$confusion)
bag.gini

randomForest::getTree(bag.wine.1000, k = 1, labelVar = TRUE)
plot(bag.wine.1000, main = "Bagging Model")


rf.wine=randomForest(quality~.,data=data_train,mtry=4,ntree=1000,importance=TRUE)
rf.wine #OOB estimate of  error rate: 13.56%
pred.rf=predict(rf.wine,newdata=data_test,type='class')
class_table=table(pred.rf,data_test$quality)
success_rate_rf=(class_table[1,1]+class_table[2,2])/sum(class_table)
success_rate_rf #0.8534096

par(mfrow=c(1,1))
importance(rf.wine)
varImpPlot(rf.wine)

randomForest::getTree(rf.wine, k = 1, labelVar = TRUE)
plot(rf.wine, main = "Random Forest Model")


library(gbm)
boost.wine=gbm(unclass(quality)-1~.,data=data_train,distribution = 'bernoulli',n.trees=1000,interaction.depth =2)
boost.wine
summary(boost.wine)

pred.boost=predict(boost.wine,newdata=data_test,n.trees=1000,type='response')
quality_pred=ifelse(pred.boost<=0.5,'bad','good')

class_table=table(quality_pred,data_test$quality)
success_rate_boost=(class_table[1,1]+class_table[2,2])/sum(class_table)
success_rate_boost #0.8223765

gbm::plot.gbm(boost.wine, i.trees = 1, main = "Boosting Model")
