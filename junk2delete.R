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

###########################################################

library(tidyverse)
library(tidymodels)
getOption("contrasts")
options("contrasts" = c(unordered = "contr.treatment", ordered = "contr.poly"))
dat <- iris %>%
  recipe(Sepal.Length ~.) %>%
  step_dummy(all_nominal()) %>% 
  prep() %>%
  juice()

my_fit <- linear_reg() %>%
  set_mode("regression") %>%
  set_engine("lm") %>%
  fit(Sepal.Length ~., dat)

aov(Sepal.Length ~., iris) %>% tidy

anova(my_fit$fit) %>% tidy
####
x <- ames_train_baked %>% select(!Sale_Price) %>% as.matrix()
y <- ames_train_baked %>% select(Sale_Price) %>% as.matrix()
sparse_fit <- sparsenet::cv.sparsenet(x = x, y = y, ngamma = 40, lambda = seq(100,0.001,length.out = 100))
pred_sparse <- predict(sparse_fit,ames_test_baked %>% select(!Sale_Price) %>% as.matrix(),"parms.min")
metr = cbind(pred_sparse, ames_test_baked %>% select(Sale_Price))
colnames(metr) <- c(".pred","Sale_Price")
res_sparse <- metr %>%   metrics(truth = Sale_Price,
                                 estimate = .pred)
Metrics::rmsle(actual = metr$Sale_Price, predicted = sapply(metr$.pred,function(x) max(x,0)))

###
x <- ames_train_baked %>% select(!Sale_Price) %>% as.matrix()
y <- ames_train_baked %>% select(Sale_Price) %>% as.matrix()
lasso_fit <- glmnet::cv.glmnet(x = x %>% as.matrix, y = y %>% as.matrix)
pred_lasso <- predict.glmnet(lasso_fit$glmnet.fit,ames_test_baked %>% select(!Sale_Price) %>% as.matrix(), lasso_fit$lambda.min)
metr = cbind(pred_lasso, ames_test_baked %>% select(Sale_Price))
colnames(metr) <- c(".pred","Sale_Price")
res_lasso <- metr %>%   metrics(truth = Sale_Price,
                                estimate = .pred)
Metrics::rmsle(actual = metr$Sale_Price, predicted = sapply(metr$.pred,function(x) max(x,0)))

##
http://euclid.psych.yorku.ca/www/psy6135/tutorials/gapminder.html
##
library(tidyverse)
library(plotly)
library(scales)
library(gapminder)
gapminder <- gapminder::gapminder
g<- gapminder %>% 
  ggplot(aes(x = gdpPercap, y = lifeExp, color = continent, frame = year)) + 
  geom_point(aes(size = pop, id = country), alpha = 0.4) + 
  geom_smooth(method = "lm", formula = y~x, se = FALSE) +
  scale_x_log10(labels = label_comma()) + 
  scale_y_continuous(labels = label_comma()) + 
  ggtitle("Life Expectancy of a Country by GDP per Capita")
g %>% ggplotly() %>% config(displayModeBar = FALSE)
##
g <- gapminder %>%
  ggplot(aes(
    x = gdpPercap,
    y = lifeExp,
    color = continent,
    frame = year
  )) +
  geom_point(aes(size = pop, id = country), alpha = 0.4) +
  geom_smooth(method = "lm",
              formula = y ~ x,
              se = FALSE) +
  scale_x_log10(labels = label_comma()) +
  scale_size(name = "Population Scale", labels = label_comma()) +
  ggtitle("Life Expectancy of a Country by GDP per Capita") +
  labs(color = "Continent") +
  xlab("GDP per Capita") +
  ylab("Life Expectancy")
g %>% ggplotly()
animate(g + transition_time(year))
##
g <-gapminder %>%
  ggplot(mapping = aes(x = gdpPercap,
                       y = lifeExp,
                       color = continent)) +
  geom_point(mapping = aes(size = pop)) + 
  geom_smooth(method = "lm",
              se = FALSE,
              formula = y~x) +
  scale_x_log10(labels = scales::label_comma()) + 
  labs(color = "Continent",
       size = "Population",
       y = "Life Expectancy",
       x = "GDP per Capita") + 
  scale_size(labels = scales::label_comma()) +
  labs(title = "A",
       subtitle = "B",
       caption = "C",
       tag = "D")
animate(g + transition_time(year))
####
g <-gapminder %>%
  ggplot(mapping = aes(x = gdpPercap,
                       y = lifeExp,
                       color = continent)) +
  geom_point(mapping = aes(size = pop),
             alpha = 0.4) + 
  geom_smooth(method = "lm",
              se = FALSE,
              formula = y~x) +
  scale_x_log10(labels = scales::label_comma()) + 
  labs(color = "Continent",
       size = "Population",
       y = "Life Expectancy",
       x = "GDP per Capita") + 
  scale_size(labels = scales::label_comma()) +
  labs(title = "Life Expectancy of a Country by GDP per Capita",
       subtitle = "Year: {frame_time}",
       caption = "Least Squares fit on Log10 transformed X",
       tag = waiver())

animate(g + transition_time(year), height = 800, width = 800)
anim_save("test.gif")
