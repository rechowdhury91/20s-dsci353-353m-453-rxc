# Library Packages

library(ggplot2)
library(MASS)
library(qtlmt)
library(car)
library(splines)


# Final Models _ Photolysis

test <- indoor.testdf.clear

pred <- as.data.frame(predict(model.indoor.clear, test))
test$pred.dE <- pred$dE
test$pred.dGloss60 <- pred$dGloss60
test$pred.dHaze <- pred$dHaze

# Region 1

data <- subset(test, Material == "626" & Exposure.Type == "Dry - FS" & UVA.340 < 75 )

ggplot(data, aes(x = UVA.340, y = pred.dGloss60, col = as.factor(Chamber.Temp), group = as.factor(Exposure))) + geom_point() + 
  #facet_grid(Material ~ Exposure.Type, scales = "free") +
  xlab("UVA 340 Dose [MJ]") +
  ylab("Change in Gloss at 60 Degrees") +
  scale_color_discrete(name = "Chamber Temperature") +
  theme(legend.text = element_text(size=15, face="bold"), legend.title = element_text(size=15, face="bold"), 
        plot.title=element_text(size=15,face="bold"), strip.text.x = element_text(size=15,face="bold"))

ggplot(data, aes(x = UVA.340, y = pred.dGloss60, col = as.factor(Chamber.Temp))) + geom_point() + 
#  facet_grid(mat ~ Exposure.Type, scales = "free_x") +
  xlab(bquote(~UVA["<"][360] ~ (MJ/m^2))) +
  ylab(expression(Delta*Gloss[60])) +
  scale_color_discrete(name = "Chamber Temperature") +
  theme(legend.text = element_text(size=15, face="bold"), legend.title = element_text(size=15, face="bold"), 
        axis.text=element_text(size=12,face="bold"), axis.title=element_text(size=15,face="bold"), 
        plot.title=element_text(size=15,face="bold"), strip.text.x = element_text(size=15,face="bold"))

rate.model <- lm(pred.dGloss60 ~ UVA.340:as.factor(Chamber.Temp), data = data)
summary(rate.model)

x <- c(47,70,80)

y <- c(-0.0550774,-0.1719661,-0.2227873 )

plot(x,y)

plot(1/x,log(abs(y)), xlab = "1/T [1/K]", ylab = expression(paste("log(k)",sep = "")))

photo.model <- lm(log(abs(y)) ~ I(1/x))
summary(photo.model)
abline(photo.model)

Ea <- -1 *  -160.25259  * 1.987 * 10^-3
Ea

data$pred <- predict(rate.model,data)

# Region 2

data2 <- subset(test, Material == "626" & Exposure.Type == "Dry - FS" & UVA.340 >= 75 & UVA.340 < 135 )

plot(data2$UVA.340,data2$pred.dGloss60)

rate.model2 <- lm(pred.dGloss60 ~ UVA.340:as.factor(Chamber.Temp) + as.factor(Chamber.Temp), data = data2)
summary(rate.model2)

abline(rate.model2)

x <- c(47,70,80)

y <- c( -0.089338,-0.292930 ,-0.381449)



plot(1/x,log(abs(y)), xlab = "1/T [1/K]", ylab = expression(paste("log(k)",sep = "")))
photo.model2 <- lm(log(abs(y)) ~ I(1/x))
summary(photo.model2)
abline(photo.model2)

Ea2 <- -1 * -166.65057* 1.987 * 10^-3
Ea2

data2$pred <- predict(rate.model2,data2)

# Calculate percent increae in Ea

perc.inc <- (Ea2 - Ea)/Ea * 100
perc.inc


# Full Plot

data0 <- subset(test, Material == "626" & Exposure.Type == "Dry - FS" & UVA.340 < 135)

ggplot(data0, aes(x = UVA.340, y = pred.dGloss60, col = as.factor(Chamber.Temp), shape = as.factor(Chamber.Temp), group = as.factor(Exposure))) + geom_point(size = 2.5) + 
  geom_line(aes(x=UVA.340,y=pred,group = Exposure),size = 1.25, data = data, col = "black") + 
  geom_line(aes(x=UVA.340,y=pred), size = 1.25, data = data2, col = "black", linetype = "dashed") + 
  xlab(bquote(~UVA["<"][360] ~ (MJ/m^2))) +
  ylab(expression(Delta*Gloss[60])) +
  scale_color_discrete(name = expression("Chamber Temperature" ~ "["~degree~C ~"]")) +
  scale_shape_discrete(name=expression("Chamber Temperature" ~ "["~degree~C ~"]")) +
  theme(legend.text = element_text(size=15, face="bold"), legend.title = element_text(size=15, face="bold"),legend.position="bottom",
        axis.text=element_text(size=15,face="bold"), axis.title=element_text(size=15,face="bold"), 
        plot.title=element_text(size=15,face="bold"), strip.text = element_text(size=15,face="bold"),axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

# Photolysis


test <- testdf.white

pred <- as.data.frame(predict(model.outdoor.white, test))
test$pred.dE <- pred$dE
test$pred.dGloss60 <- pred$dGloss60
test$pred.dHaze <- pred$dHaze

data <- subset(test, Material == "U2")
data <- subset(data, Exposure == "F26O")
data <- subset(data, Mount == "Open")

test <- indoor.testdf.clear

pred <- as.data.frame(predict(model.indoor.clear, test))
test$pred.dE <- pred$dE
test$pred.dGloss60 <- pred$dGloss60
test$pred.dHaze <- pred$dHaze

data <- subset(test, Material == "SMD")
data <- subset(data, Exposure == "UV20")


x <- data$UVA.340[-c(1)] *1000

y <- abs(data$pred.dE[-c(1)])

plot(x,y)
plot(1/x,log(y), xlab = "1/UVA 340 Dose [1/kJ]", ylab = expression(paste("log(",Delta,"E)",sep = "")))
arr.model <- lm(log(y) ~ I(1/x))
summary(arr.model)

abline(arr.model)

Ea <- -1 *-8.330e+03 * 1.987 * 10^-3
Ea 

# Hydrolysis

test <- indoor.testdf.white

pred <- as.data.frame(predict(model.indoor.white, test))
test$pred.dE <- pred$dE
test$pred.dGloss60 <- pred$dGloss60
test$pred.dHaze <- pred$dHaze

data <- subset(test, Material == "238")
data <- subset(data, Exposure == "FS42")


x <- data$UVA.340[-1] *10^3

y <- data$pred.dE[-1]

plot(x,y)
plot(1/x,log(y), xlab = "1/UVA 340 Dose [1/kJ]", ylab = expression(paste("log(",Delta,"E)",sep = "")))

arr.model <- lm(log(y) ~ I(1/x))
summary(arr.model)

abline(arr.model)

Ea <- -1 * -1.382e+04 * 8.314
Ea 

