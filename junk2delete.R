# servr::daemon_stop(servr::daemon_list())

library(keras)

fashion_mnist <- dataset_fashion_mnist()

c(train_images, train_labels) %<-% fashion_mnist$train
c(test_images, test_labels) %<-% fashion_mnist$test

library(tidyr)
library(ggplot2)

image_1 <- as.data.frame(train_images[1, , ])
colnames(image_1) <- seq_len(ncol(image_1))
image_1$y <- seq_len(nrow(image_1))
image_1 <- gather(image_1, "x", "value", -y)
image_1$x <- as.integer(image_1$x)

test = train_images[2
                    ,,]
test1 <- test %>% 
  as_tibble() %>% 
  rowid_to_column("Y") %>% 
  pivot_longer(-Y,names_to = "X", values_to = "PIX") %>% 
  mutate(X = as.numeric(gsub("V","",X))) %>%
  mutate(Y = abs(Y - max(Y)+1))

test1 %>%
  ggplot(aes(X,Y, fill = PIX)) + 
  geom_tile() + 
  scale_fill_gradient(low="black", high="white")

library(gridExtra)

for(i in 1:dim(test_images)[1]){
  test = train_images[i,,]
  test1 <- test %>% 
    as_tibble() %>% 
    rowid_to_column("Y") %>% 
    pivot_longer(-Y,names_to = "X", values_to = "PIX") %>% 
    mutate(X = as.numeric(gsub("V","",X))) %>%
    mutate(Y = abs(Y - max(Y)+1))
  nam <- paste("P",i, sep="")
  assign(nam,
         test1 %>%
    ggplot(aes(X,Y, fill = PIX)) + 
    geom_tile() + 
    scale_fill_gradient(low="black", high="white")
    )
}
cloth <- function(x){
  test = train_images[x,,]
}
rotate <- function(x){
  t(apply(x,2,rev))
}

# library(imager) 
# plot(cifar10$train$x[89,,,] %>% as.cimg %>% imrotate(,angle = 90), main = cifar10$test$y[89])
# blogdown::hugo_cmd("--templateMetrics")