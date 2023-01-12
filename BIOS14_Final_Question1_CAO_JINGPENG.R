rm(list=ls())

#read and clean data

data <- read.csv("exam2022_part1.csv")
subd <- data[,c("sp","treat","ASD")]

subd <- na.omit(subd)

#ANOVA modal

m <- lm(subd$ASD ~ subd$treat * subd$sp)
anova(m)
summary(m)

#summary data for plotting

subd$group <- paste(subd$treat, subd$sp, sep="_")

mean <- aggregate(subd$ASD,list(subd$group), FUN = mean)
n <- table(subd$group)
sd <- aggregate(subd$ASD,list(subd$group), FUN = sd)
se <- as.data.frame(sd$x / sqrt(n))

sum <- cbind(mean,se)
sum <- sum[,-3]
sum[,4] <- c("Dry","Dry","Wet","Wet")
sum[,5] <- c("L","S","L","S")
colnames(sum)[3] <- "se"
colnames(sum)[2] <- "means"
colnames(sum)[4] <- "treat"
colnames(sum)[5] <- "sp"

#summary data for quantification

sp_mean <- aggregate(subd$ASD,list(subd$sp), FUN = mean)
treat_mean <- aggregate(subd$ASD,list(subd$treat), FUN = mean)

#plot

library(ggplot2)

ggplot(data = sum,
       mapping = aes(x = treat, y = means, group = sp)) + 
  geom_point(aes(shape = sp), size = 2) +
  scale_shape_manual(values = c(15,17)) +
  geom_errorbar(aes(ymin = means-se, ymax = means+se), width = 0.04) +
  geom_line(aes(group = sp)) +
  labs(x = "Treatment", y = "Anther-stigma distance (mm)", shape = "species ID") +
  theme_classic() +
  theme(text = element_text(size = 12, family = "serif"))

