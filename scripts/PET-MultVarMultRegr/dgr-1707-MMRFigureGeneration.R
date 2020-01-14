
# Load Data and packages

load("../data/1706wksp.RData")
library(ggplot2)
library(splines)
library(stats)
library(gridExtra)

# Set working directory

setwd("H:/Git/17-pet-degrmodels/figs")

# Model Statistics and Plots

## Overlaid plots for outdoor clear model

test <- testdf.clear

pred <- as.data.frame(predict(model.outdoor.clear, test))
test$pred.dE <- pred$dE
test$pred.dGloss60 <- pred$dGloss60
test$pred.dHaze <- pred$dHaze

oc.dE <- ggplot(outdoor.clear, aes(x = UVA.340, y = dE, col = Mount)) + geom_point() + 
  geom_line(aes(x=UVA.340,y=pred.dE), data = test[test$Mount == "Open",]) + 
  geom_line(aes(x=UVA.340,y=pred.dE), data = test[test$Mount == "UnderGlass",], linetype = "dashed") + 
  facet_grid(Material ~ Location, scales = "free_x") +
  xlab("UVA 340 Dose [MJ]") +
  ylab("Change in Color") +
  theme(legend.text = element_text(size=15, face="bold"), legend.title = element_text(size=15, face="bold"), 
        axis.text=element_text(size=15,face="bold"), axis.title=element_text(size=15,face="bold"), 
        plot.title=element_text(size=15,face="bold"), strip.text.x = element_text(size=15,face="bold"), legend.position = "bottom")


oc.dG <- ggplot(outdoor.clear, aes(x = UVA.340, y = dGloss60, col = Mount)) + geom_point() + 
  geom_line(aes(x=UVA.340,y=pred.dGloss60), data = test[test$Mount == "Open",]) + 
  geom_line(aes(x=UVA.340,y=pred.dGloss60), data = test[test$Mount == "UnderGlass",], linetype = "dashed") + 
  facet_grid(Material ~ Location, scales = "free_x") +
  xlab("UVA 340 Dose [MJ]") +
  ylab("Change in Gloss at 60 Degrees") +
  theme(legend.text = element_text(size=15, face="bold"), legend.title = element_text(size=15, face="bold"), 
        axis.text=element_text(size=15,face="bold"), axis.title=element_text(size=15,face="bold"), 
        plot.title=element_text(size=15,face="bold"), strip.text.x = element_text(size=15,face="bold"), legend.position = "bottom")

oc.dH <- ggplot(outdoor.clear, aes(x = UVA.340, y = dHaze, col = Mount)) + geom_point() + 
  geom_line(aes(x=UVA.340,y=pred.dHaze), data = test[test$Mount == "Open",]) + 
  geom_line(aes(x=UVA.340,y=pred.dHaze), data = test[test$Mount == "UnderGlass",], linetype = "dashed") + 
  facet_grid(Material ~ Location, scales = "free_x") +
  xlab("UVA 340 Dose [MJ]") +
  ylab("Change in Haze") +
  theme(legend.text = element_text(size=15, face="bold"), legend.title = element_text(size=15, face="bold"), 
        axis.text=element_text(size=15,face="bold"), axis.title=element_text(size=15,face="bold"), 
        plot.title=element_text(size=15,face="bold"), strip.text.x = element_text(size=15,face="bold") , legend.position = "bottom")

png(filename = "./oc.png", width = 1400, height = 600)
grid.arrange(oc.dE, oc.dG, oc.dH, ncol = 3)
dev.off()

# Diagonistic Plots for Outdoor Clear Model

f <- fitted(model.outdoor.clear)

r <- resid(model.outdoor.clear)

png(filename = "./oc_diagnostics.png", width = 772, height = 600)

par(mfrow=c(3,1))

plot(f[,1],r[,1], main = expression(paste("Model 1: ","Residuals - ",Delta,"E")), xlab = "Fitted Values", ylab = "Residuals", cex = 1.5, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5)  + abline(0, 0)

plot(f[,2],r[,2], main = expression(paste("Model 1: ","Residuals - ",Delta*Gloss[60])), xlab = "Fitted Values", ylab = "Residuals", cex = 1.5, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5)  + abline(0, 0)

plot(f[,3],r[,3], main = expression(paste("Model 1: ","Residuals - ",Delta,"Haze")), xlab = "Fitted Values", ylab = "Residuals", cex =1.5, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5)  + abline(0, 0)

qqnorm(r[,1], main = expression(paste("Model 1: ","Normal Q-Q - ",Delta,"E")), cex = 1.5, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5) + qqline(r[,1])

qqnorm(r[,2], main = expression(paste("Model 1: ","Normal Q-Q - ",Delta*Gloss[60])), cex = 1.5, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5) + qqline(r[,2])

qqnorm(r[,3], main = expression(paste("Model 1: ","Normal Q-Q - ",Delta,"Haze")), cex = 1.5, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5) + qqline(r[,3])

dev.off()
# White PET, Outdoor Exposure Overlaid plots

test <- testdf.white


pred <- as.data.frame(predict(model.outdoor.white,test))
test$pred.dE <- pred$dE
test$pred.dGloss60 <- pred$dGloss60

ow.dE <- ggplot(outdoor.white, aes(x = UVA.340, y = dE, col = Mount)) + geom_point() + 
  geom_line(aes(x=UVA.340,y=pred.dE), data = test[test$Mount == "Open",]) + 
  geom_line(aes(x=UVA.340,y=pred.dE), data = test[test$Mount == "UnderGlass",], linetype = "dashed") +  
  facet_grid(Material ~ Location, scales = "free_x") +
  xlab("UVA 340 Dose [MJ]") +
  ylab("Change in Color") +
  theme(legend.text = element_text(size=15, face="bold"), legend.title = element_text(size=15, face="bold"), 
        axis.text=element_text(size=15,face="bold"), axis.title=element_text(size=15,face="bold"), 
        plot.title=element_text(size=15,face="bold"), strip.text.x = element_text(size=15,face="bold"), legend.position = "bottom")

ow.dG <- ggplot(outdoor.white, aes(x = UVA.340, y = dGloss60, col = Mount)) + geom_point() + 
  geom_line(aes(x=UVA.340,y=pred.dGloss60), data = test[test$Mount == "Open",]) + 
  geom_line(aes(x=UVA.340,y=pred.dGloss60), data = test[test$Mount == "UnderGlass",], linetype = "dashed") + 
  facet_grid(Material ~ Location, scales = "free_x") +
  xlab("UVA 340 Dose [MJ]") +
  ylab("Change in Gloss at 60 Degrees") +
  theme(legend.text = element_text(size=15, face="bold"), legend.title = element_text(size=15, face="bold"), 
        axis.text=element_text(size=15,face="bold"), axis.title=element_text(size=15,face="bold"), 
        plot.title=element_text(size=15,face="bold"), strip.text.x = element_text(size=15,face="bold"), legend.position = "bottom")

png(filename = "./ow.png", width = 950, height = 600)
grid.arrange(ow.dE, ow.dG, ncol = 2)
dev.off()

# Diagonistic Plots for Outdoor Clear Model

f <- fitted(model.outdoor.white)

r <- resid(model.outdoor.white)
           
png(filename = "./ow_diagnostics.png", width = 772, height = 400)

par(mfrow=c(2,1))

plot(f[,1],r[,1], main = expression(paste("Model 2: ","Residuals - ",Delta,"E")), xlab = "Fitted Values", ylab = "Residuals", cex = 1, cex.axis = 1, cex.lab = 1, cex.main = 1)  + abline(0, 0)

plot(f[,2],r[,2], main = expression(paste("Model 2: ","Residuals - ",Delta*Gloss[60])), xlab = "Fitted Values", ylab = "Residuals", cex = 1, cex.axis = 1, cex.lab = 1, cex.main = 1)  + abline(0, 0)


qqnorm(r[,1], main = expression(paste("Model 2: ","Normal Q-Q - ",Delta,"E"))) + qqline(r[,1])

qqnorm(r[,2], main = expression(paste("Model 2: ","Normal Q-Q - ",Delta*Gloss[60]))) + qqline(r[,2])

dev.off()

# Clear PET, Indoor Exposure Overlaid plots

test <- indoor.testdf.clear

pred <- as.data.frame(predict(model.indoor.clear, test))
test$pred.dE <- pred$dE
test$pred.dGloss60 <- pred$dGloss60
test$pred.dHaze <- pred$dHaze

ic.dE <- ggplot(indoor.clear, aes(x = UVA.340, y = dE, col = as.factor(Chamber.Temp))) + geom_point() + 
  geom_line(aes(x=UVA.340,y=pred.dE, group = Exposure), data = test[test$Chamber.Temp == 70,]) + 
  geom_line(aes(x=UVA.340,y=pred.dE), data = test[test$Chamber.Temp == 80,], linetype = "dashed") +
  geom_line(aes(x=UVA.340,y=pred.dE), data = test[test$Chamber.Temp == 47,], linetype = "dotdash") +
  facet_grid(Material ~ Exposure.Type, scales = "free_x") +
  xlab("UVA 340 Dose [MJ]") +
  ylab("Change in Color") +
  scale_color_discrete(name = "Chamber Temperature") +
  theme(legend.text = element_text(size=15, face="bold"), legend.title = element_text(size=15, face="bold"), 
        axis.text=element_text(size=15,face="bold"), axis.title=element_text(size=15,face="bold"), 
        plot.title=element_text(size=15,face="bold"), strip.text.x = element_text(size=15,face="bold"), legend.position = "bottom")

ic.dG <- ggplot(indoor.clear, aes(x = UVA.340, y = dGloss60, col = as.factor(Chamber.Temp))) + geom_point() + 
  geom_line(aes(x=UVA.340,y=pred.dGloss60, group = Exposure), data = test[test$Chamber.Temp == 70,]) + 
  geom_line(aes(x=UVA.340,y=pred.dGloss60), data = test[test$Chamber.Temp == 80,], linetype = "dashed") +
  geom_line(aes(x=UVA.340,y=pred.dGloss60), data = test[test$Chamber.Temp == 47,], linetype = "dotdash") +
  facet_grid(Material ~ Exposure.Type, scales = "free_x") +
  xlab("UVA 340 Dose [MJ]") +
  ylab("Change in Gloss at 60 Degrees") +
  scale_color_discrete(name = "Chamber Temperature") +
  theme(legend.text = element_text(size=15, face="bold"), legend.title = element_text(size=15, face="bold"), 
        axis.text=element_text(size=15,face="bold"), axis.title=element_text(size=15,face="bold"), 
        plot.title=element_text(size=15,face="bold"), strip.text.x = element_text(size=15,face="bold"), legend.position = "bottom")

ic.dH <- ggplot(indoor.clear, aes(x = UVA.340, y = dHaze, col = as.factor(Chamber.Temp))) + geom_point() + 
  geom_line(aes(x=UVA.340,y=pred.dHaze, group = Exposure), data = test[test$Chamber.Temp == 70,]) + 
  geom_line(aes(x=UVA.340,y=pred.dHaze), data = test[test$Chamber.Temp == 80,], linetype = "dashed") +
  geom_line(aes(x=UVA.340,y=pred.dHaze), data = test[test$Chamber.Temp == 47,], linetype = "dotdash") +
  facet_grid(Material ~ Exposure.Type, scales = "free_x") +
  xlab("UVA 340 Dose [MJ]") +
  ylab("Change in Haze") +
  scale_color_discrete(name = "Chamber Temperature") +
  theme(legend.text = element_text(size=15, face="bold"), legend.title = element_text(size=15, face="bold"), 
        axis.text=element_text(size=15,face="bold"), axis.title=element_text(size=15,face="bold"), 
        plot.title=element_text(size=15,face="bold"), strip.text.x = element_text(size=15,face="bold"), legend.position = "bottom")

png(filename = "./ic.png", width = 1400, height = 600)
grid.arrange(ic.dE, ic.dG, ic.dH, ncol = 3)
dev.off()

# Diagonistic Plots for Indoor Clear Model

f <- fitted(model.indoor.clear)

r <- resid(model.indoor.clear)

png(filename = "./ic_diagnostics.png", width = 772, height = 600)

par(mfrow=c(3,1))

plot(f[,1],r[,1], main = expression(paste("Model 3: ","Residuals - ",Delta,"E")), xlab = "Fitted Values", ylab = "Residuals", cex = 1.5, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5)  + abline(0, 0)

plot(f[,2],r[,2], main = expression(paste("Model 3: ","Residuals - ",Delta*Gloss[60])), xlab = "Fitted Values", ylab = "Residuals", cex = 1.5, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5)  + abline(0, 0)

plot(f[,3],r[,3], main = expression(paste("Model 3: ","Residuals - ",Delta,"Haze")), xlab = "Fitted Values", ylab = "Residuals", cex =1.5, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5)  + abline(0, 0)

qqnorm(r[,1], main = expression(paste("Model 3: ","Normal Q-Q - ",Delta,"E")), cex = 1.5, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5) + qqline(r[,1])

qqnorm(r[,2], main = expression(paste("Model 3: ","Normal Q-Q - ",Delta*Gloss[60])), cex = 1.5, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5) + qqline(r[,2])

qqnorm(r[,3], main = expression(paste("Model 3: ","Normal Q-Q - ",Delta,"Haze")), cex = 1.5, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5) + qqline(r[,3])

dev.off()

# White PET, Indoor Models

test <- indoor.testdf.white

pred <- as.data.frame(predict(model.indoor.white, test))
test$pred.dE <- pred$dE
test$pred.dGloss60 <- pred$dGloss60

iw.dE <- ggplot(indoor.white, aes(x = UVA.340, y = dE, col = as.factor(Chamber.Temp))) + geom_point() + 
  geom_line(aes(x=UVA.340,y=pred.dE, group = Exposure), data = test[test$Chamber.Temp == 70,]) + 
  geom_line(aes(x=UVA.340,y=pred.dE), data = test[test$Chamber.Temp == 80,], linetype = "dashed") +
  geom_line(aes(x=UVA.340,y=pred.dE), data = test[test$Chamber.Temp == 47,], linetype = "dotdash") +
  facet_grid(Material ~ Exposure.Type) +
  xlab("UVA 340 Dose [MJ]") +
  ylab("Change in Color") +
  scale_color_discrete(name = "Chamber Temperature") +
  theme(legend.text = element_text(size=15, face="bold"), legend.title = element_text(size=15, face="bold"), 
        axis.text=element_text(size=15,face="bold"), axis.title=element_text(size=15,face="bold"), 
        plot.title=element_text(size=15,face="bold"), strip.text.x = element_text(size=15,face="bold"), legend.position = "bottom")

iw.dG <- ggplot(indoor.white, aes(x = UVA.340, y = dGloss60, col = as.factor(Chamber.Temp))) + geom_point() + 
  geom_line(aes(x=UVA.340,y=pred.dGloss60, group = Exposure), data = test[test$Chamber.Temp == 70,]) + 
  geom_line(aes(x=UVA.340,y=pred.dGloss60), data = test[test$Chamber.Temp == 80,], linetype = "dashed") +
  geom_line(aes(x=UVA.340,y=pred.dGloss60), data = test[test$Chamber.Temp == 47,], linetype = "dotdash") +
  facet_grid(Material ~ Exposure.Type) +
  xlab("UVA 340 Dose [MJ]") +
  ylab("Change in Gloss at 60 Degrees") +
  scale_color_discrete(name = "Chamber Temperature") +
  theme(legend.text = element_text(size=15, face="bold"), legend.title = element_text(size=15, face="bold"), 
        axis.text=element_text(size=15,face="bold"), axis.title=element_text(size=15,face="bold"), 
        plot.title=element_text(size=15,face="bold"), strip.text.x = element_text(size=15,face="bold"), legend.position = "bottom")

png(filename = "./iw.png", width = 950, height = 600)
grid.arrange(iw.dE, iw.dG, ncol = 2)
dev.off()

# Diagonistic Plots for Outdoor Clear Model

f <- fitted(model.indoor.white)

r <- resid(model.indoor.white)

png(filename = "./iw_diagnostics.png", width = 772, height = 400)

par(mfrow=c(2,1))

plot(f[,1],r[,1], main = expression(paste("Model 4: ","Residuals - ",Delta,"E")), xlab = "Fitted Values", ylab = "Residuals", cex = 1, cex.axis = 1, cex.lab = 1, cex.main = 1)  + abline(0, 0)

plot(f[,2],r[,2], main = expression(paste("Model 4: ","Residuals - ",Delta*Gloss[60])), xlab = "Fitted Values", ylab = "Residuals", cex = 1, cex.axis = 1, cex.lab = 1, cex.main = 1)  + abline(0, 0)


qqnorm(r[,1], main = expression(paste("Model 4: ","Normal Q-Q - ",Delta,"E"))) + qqline(r[,1])

qqnorm(r[,2], main = expression(paste("Model 4: ","Normal Q-Q - ",Delta*Gloss[60]))) + qqline(r[,2])

dev.off()

