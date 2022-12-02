rm(list=ls())

#data organization

data <- read.csv("Eulaema.csv")

#subdata: 
  #method = net
  #no effort column, do not know what that is

library(tidyverse)

subdata <- data %>%
  filter(method == "Net") %>%
  select(-method,-effort)

#check all variables

for (i in 1:ncol(subdata)) {
  p <- plot(subdata[,i],subdata$Eulaema_nigrita, 
            xlab = colnames(subdata[i]))
  print(p)
}

#one data is way out. Deleted

subdata <- subdata[ - which(subdata$Eulaema_nigrita > 500),]

#glm model

library(MuMIn)
library(MASS)

m <- glm.nb(Eulaema_nigrita ~ forest. + Pseason, data = subdata)
summary(m)

r.squaredGLMM(m)
1-(m$deviance/m$null.deviance)

#plot

library(RColorBrewer)

newf <- seq(min(subdata$forest.),max(subdata$forest.), by = 0.005)
meanp <- rep(mean(subdata$Pseason),length(newf))
meanp_up <- rep(mean(subdata$Pseason)+sd(subdata$Pseason),length(newf))
meanp_down <- rep(mean(subdata$Pseason)-sd(subdata$Pseason),length(newf))

p_mean <- predict(m,
                  newdata = list(Pseason = meanp,
                                 forest. = newf),
                  type = "response")

p_meanup <- predict(m,
                    newdata = list(Pseason = meanp_up,
                                   forest. = newf),
                    type = "response")

p_meandown <- predict(m,
                      newdata = list(Pseason = meanp_down,
                                     forest. = newf),
                      type = "response")


par(family = "serif",cex = 1.3)
plot(x = subdata$forest., y = subdata$Eulaema_nigrita,
     las = 1,
     col = "grey",
     pch = 20,
     xlab = "Proportional forest cover",
     ylab = "Seasonal precipitation (mm)"
     )

lines(newf, p_meanup, col = "#66C2A5",lwd = 2)
lines(newf, p_mean, col = "#FC8D62",lwd = 2)
lines(newf, p_meandown, col = "#8DA0CB",lwd = 2)

legend("topright",
       lty = 1,
       lwd = 2,
       col = brewer.pal(n = 3, name = "Set2"),
       bty="n",
       legend = c("Pseason = mean + SD",
                  "Pseason = mean",
                  "Pseason = mean - SD"))

