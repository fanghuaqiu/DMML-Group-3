library(ggplot2)
library(skimr)
library(tidyr)
library(dplyr)
library(MASS)
library(e1071)
library(stringr)
imdb <- read.csv("D:/File/Courses/Data Mining and Machine Learning/Group Project/group_3.csv") %>%
  mutate(ROI = (gross-budget)/budget) %>% #define ROI
  mutate(scs = ifelse(ROI>=1,1,0)) #define binary labelled output

set.seed(84)

index_test <- sample(1:nrow(imdb), round(0.25*nrow(imdb)))
index_val <- sample((1:nrow(imdb))[-index_test], round(0.25*nrow(imdb)))
index_train <- setdiff(1:nrow(imdb),c(index_test,index_val))


test_imdb <- imdb[index_test,]
valid_imdb <- imdb[index_val,]
train_imdb <- imdb[index_train,]



datacleaning <- function(data){
  data = data[,c("duration","num_critic_for_reviews","num_user_for_reviews",
                 "num_voted_users","cast_total_facebook_likes","movie_facebook_likes",
                 "facenumber_in_poster","color","title_year",
                 "content_rating","aspect_ratio","scs")]
}#"language""country"

train_imdb = datacleaning(train_imdb)
valid_imdb = datacleaning(valid_imdb)


dd <- function(data){
  data[,"color"] = as.factor(data[,"color"])
  data[,"content_rating"] = as.factor(data[,"content_rating"])
  data[,"scs"] = as.factor(data[,"scs"])
  return (data)
}#data[,"country"] = as.factor(data[,"country"])
#data[,"language"] = as.factor(data[,"language"])

train_imdb = dd(train_imdb)
valid_imdb = dd(valid_imdb) 



Model <- svm(scs~., train_imdb, type="C-classification", kernel="linear")
summary(Model)
aa2=predict(Model,valid_imdb)
table(valid_imdb$scs,aa2)

cost_range <- c(0.01,0.1,1,10,100,150,200,250,300)
degree_range <- 2:20
gamma_range <- c(0.001,0.01,0.1,1,10,100)

SVM_poly <- tune.svm(scs~., data=train_imdb, type="C-classification", kernel="polynomial", cost=cost_range, degree=degree_range)
summary(SVM_poly)

SVM_RBF <- tune.svm(scs~., data=train_imdb, type="C-classification", kernel="radial", cost=cost_range, gamma=gamma_range)
summary(SVM_RBF)

SVM_linear <- tune.svm(scs~.,data = train_imdb,type="C-classification",kernel="linear",cost = c(0.01,0.1,1,10,100))
summary(SVM_linear)


gamma.opt <- SVM_poly$best.parameters[1]
cost.opt <- SVM_poly$best.parameters[2]
SVM_final <- svm(scs~., train_imdb, type="C-classification", kernel="polynomial", gamma=gamma.opt, cost=cost.opt)
test.pred <- predict(SVM_final,valid_imdb)
table(valid_imdb$scs,test.pred)
sum(diag(table(valid_imdb$scs,test.pred)))/length(valid_imdb$scs)


gamma.opt <- SVM_RBF$best.parameters[1]
cost.opt <- SVM_RBF$best.parameters[2]
SVM_final <- svm(scs~., train_imdb, type="C-classification", kernel="radial", gamma=gamma.opt, cost=cost.opt)
test.pred <- predict(SVM_final,valid_imdb)
table(valid_imdb$scs,test.pred)
sum(diag(table(valid_imdb$scs,test.pred)))/length(valid_imdb$scs)


cost.opt <- SVM_linear$best.parameters[1]
SVM_final <- svm(scs~., train_imdb, type="C-classification", kernel="linear",cost=cost.opt)
test.pred <- predict(SVM_final,valid_imdb)
table(valid_imdb$scs,test.pred)
sum(diag(table(valid_imdb$scs,test.pred)))/length(valid_imdb$scs)
