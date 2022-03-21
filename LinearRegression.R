library(tidyverse)  
library(modelr)     
library(broom)

reg <- read.csv("Downloads/smoteData.csv")
reg

set.seed(123)
sample <- sample(c(TRUE, FALSE), nrow(reg), replace = T, prob = c(0.6,0.4))
train1 <- reg[sample, ]
test1 <- reg[!sample, ]

model1 <- lm(IL17a ~ IL4 + IL5 + IL10 , data = train1)
summary(model1)
tidy(model1)
confint(model1)

sigma(model1)
sigma(model1)/mean(train1$IL17a)

rsquare(model1, data = train1)
cor(train1$IL17a, train1$IL4 + IL5 + IL10)^2

ggplot(train1, aes(IL4 + IL5 + IL10, IL17a)) + geom_point() + geom_smooth(method = "lm") + geom_smooth(se = FALSE, color = "red")

