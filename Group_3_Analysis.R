library(dplyr)
library(tidyr)

imdb <- read.csv("D:/Fanghua/Documents/坚果云/我的坚果云/文档/Working On/Semester2/5099 DMML/CA/new_group_3.csv") %>%
  mutate(ROI = (gross-budget)/budget) %>% #define ROI
  mutate(scs = ifelse(ROI>=1,1,0)) #define binary labelled output

str(imdb)
sum(imdb$scs) #output should be 291

set.seed(84)

index_test <- sample(1:nrow(imdb), round(0.25*nrow(imdb)))
index_val <- sample((1:nrow(imdb))[-index_test], round(0.25*nrow(imdb)))
index_train <- setdiff(1:nrow(imdb),c(index_test,index_val))


test_imdb <- imdb[index_test,]
index_val <- imdb[index_val,]
train_imdb <- imdb[index_train,]

imdb_g <- imdb[,c("movie_title","genres")] %>%

  separate_wider_delim(genres,"|",names=c("1st_g","2nd_g","3rd_g"),too_few = "align_start",too_many = "merge")
colnames(imdb_g)
head(imdb_g)

write.csv(imdb,"D:/Fanghua/Documents/坚果云/我的坚果云/文档/Working On/Semester2/5099 DMML/CA/new_group_3_11.csv")
?separate_wider_delim
df <- tibble(x = c(NA, "x.y", "x.z", "y.z")) %>%
  separate_wider_delim(x, ".", names = c("A", "B"))
df  
