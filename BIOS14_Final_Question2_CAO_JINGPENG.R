rm(list=ls())

#read and clean data
  #male
  #older than 3 years old
  #low population density at birth

data <- read.table("exam2022_part2.txt", header=T)
subd <- data[,c("sex","age","hornR","mass","density")]
subd <- subd[subd$sex == "M",]
subd <- subd[subd$age >= 3,]
subd <- subd[subd$density == "low",]

#linear model

m <- lm(subd$hornR ~ subd$mass)
summary(m)

#quick check the normal distribution for residual

hist(residuals(m))

#predict data for plotting

pred <- data.frame(pred_horn = predict(m, newdata = data.frame(subd$mass)), 
                   mass = subd$mass)

#plot

library(ggplot2)

ggplot(data = subd, aes(x = mass, y = hornR)) +
  geom_point(color = "grey") +
  geom_line(data = pred, aes(x = mass, y = pred_horn), color = "red") +
  labs(x = "Body mass (kg)", y = "Length of the right horn (mm)") +
  theme_classic() +
  theme(text = element_text(size = 12, family = "serif"))
