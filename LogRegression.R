library(ggplot2)
library(cowplot)
attach(data)

#View the structure of the dataset.
str(data)

#Set Dx to factor: 0 as "Healthy" and 1 as "Unhealthy"
data$Dx <- ifelse(test=data$Dx == 0, yes="Healthy", no="Unhealthy")
data$Dx <- as.factor(data$Dx)

#Set other characters to factors.
data$Gender <- as.factor(data$Gender)
#data$IFNy <- as.factor(data$IFNy)
#data$IL10 <- as.factor(data$IL10)
#rmdata$IL12P70 <- as.factor(data$IL12P70)
#data$IL13 <- as.factor(data$IL13)
#data$IL15 <- as.factor(data$IL15)
#data$IL17a <- as.factor(data$IL17a)
#data$IL1a <- as.factor(data$IL1a)
#data$IL1b <- as.factor(data$IL1b)
#data$IL2 <- as.factor(data$IL2)
#data$IL4 <- as.factor(data$IL4)
#data$IL23 <- as.factor(data$IL23)
#data$Il5 <- as.factor(data$IL5)
#data$Il6 <- as.factor(data$IL6)
#data$IL8 <- as.factor(data$IL8)
#data$TNFa <- as.factor(data$TNFa)
#data$TNFb <- as.factor(data$TNFb)

#Ensure that we changed all the columns to type factor.
str(data)


