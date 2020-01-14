
# Set working directory
setwd("C:\\Users\\abdulkerimgok\\Desktop\\Papers\\15-yi-haze-statmodeling\\analysis")

# Load necessary packages
library(lme4)
library(nlme)
library(car)
library(psych)
library(ggplot2)
library(grid)
library(arm)
library(RLRsim)
library(MuMIn)
library(ggfortify)

# Read in data
dt <- read.csv("DataFrame-v14-singles.csv")
dt=dt[,c(1:3,5,7,13,14)]
names(dt)[4]="Exp"
head(dt)
names(dt)

# Data overview
str(dt)
dttable=table(dt$Material,dt$Exp)
margin.table(dttable,1)
margin.table(dttable,2)
attach(dt)
dttable1=ftable(xtabs(~Material+Exp+Step))
dttable1
detach(dt)

##################################### Yellowing ######################################################

# YI without outliers...USE THIS ONE FOR EDA OF ALL GRADES 
YIdtEDA=subset(dt,Exp!="Baseline" & Sample!="sa19603.14" & Rowkey!="sa19601.11-step1" & Rowkey!="sa19601.00-step0 " & Rowkey!="sa19601.29-step0" & Sample!="sa19603.00" & Sample!="sa19604.00")  
# Exposures ordered for ggplot
YIdtEDA$Exp_f <- factor(YIdtEDA$Exp, levels=c("CyclicQUV", "HotQUV", "DampHeat", "FreezeThaw"))

# EDA of All
ggplot(YIdtEDA, aes(x = Step, y = YI, group = Sample, col = Material)) + geom_point() +
  geom_line() + facet_wrap(~Exp_f, ncol = 5, scales="free") + 
  theme_bw() + 
  scale_color_manual(values=c("red", "black", "blue")) +
  xlab("Exposure Step") +
  ylab("Yellowness Index (YI)") +
  theme(legend.text = element_text(size=15, face="bold"), legend.title = element_text(size=15, face="bold"), 
        axis.text=element_text(size=15,face="bold"), axis.title=element_text(size=15,face="bold"), 
        plot.title=element_text(size=15,face="bold"), strip.text.x = element_text(size=15,face="bold"))
ggsave(paste("../figs//Yellowing-ALL-FreeScale", ".png", sep=""))  


### Model 1) CyclicQUV and HotQUV

#YI without outliers... Only HotQUV and CyclicQUV... USE THIS ONE FOR MODEL 1 ANALYSIS
YIdtQUV=subset(dt,Exp!="Baseline" & Exp!="DampHeat" & Exp!="FreezeThaw" & Sample!="sa19603.14" & Rowkey!="sa19601.11-step1" & Rowkey!="sa19601.00-step0 " & Rowkey!="sa19601.29-step0" & Sample!="sa19603.00" & Sample!="sa19604.00")  

# EDA of HotQUV and CyclicQUV
ggplot(YIdtQUV, aes(x = Step, y = YI, group = Sample, col = Material)) + geom_point() +
  geom_line() + facet_wrap(~Exp, ncol = 5, scales="free") +
  theme_bw() + 
  scale_color_manual(values=c("red", "black", "blue")) +
  xlab("Exposure Step") +
  ylab("Yellowness Index (YI)") +
  theme(legend.text = element_text(size=15, face="bold"), legend.title = element_text(size=15, face="bold"), 
        axis.text=element_text(size=15,face="bold"), axis.title=element_text(size=15,face="bold"), 
        plot.title=element_text(size=15,face="bold"), strip.text.x = element_text(size=15,face="bold"))
ggsave(paste("../figs/Yellowing-HQ-CQ-FreeScale", ".png", sep=""))  

# FIXED EFFECT MODEL of Yellowing of HotQUV and CyclicQUV (Model 1)

png(file="../figs/boxCox-HQ-CQ-Fixed.png", width=500, height=500)
par(mar=c(4.1, 4.1, 2, 1), xpd=FALSE)
boxCox(lm(YI~Step*Material*Exp+I(Step^2)*Material+I(Step^3)*Material,data=YIdtQUV),  grid=TRUE, cex.axis=1.25, cex.lab=1.25) ## WHY THIS?
dev.off()
# LAMBDA=0.5 SELECTED SO THE MODEL BELOW IS TRANSFORMED TO QUADRATIC


lmodelQUV=lm(YI^(0.5)~Step*Material*Exp+I(Step^2)*Material+I(Step^3)*Material,data=YIdtQUV) ## WHY THIS MODEL? USE THIS FINAL MODEL IN BELOW...
# THIS IS THE FINAL-MODEL
# THIS MODEL DOESN'T HAVE INTERACTIONS BETWEEN MATERIAL AND EXPOSURE IN THE QUADRATIC AND CUBIC TERMS...
# HYDSTAB AND HOTQUV ARE TAKEN AS REFERENCE MATERIAL AND EXPOSURE AND THEREFORE THEY DON'T SHOW UP IN THE MODEL...

# These plots are used to assess regression assumptions of independence (Residuals vs fitted), homogeneity (Standardized residuals vs fitted) and normality (Normal Q-Q)... 
plot(lmodelQUV, sub="") 

# ggplot version of the diagnostics plots (which=1 for residual vs fitted and which=2 for normal q-q)
autoplot(lmodelQUV, which=1, label.size=1, smooth.colour="red") + geom_point(size=2) + theme_bw() + 
   theme(plot.title=element_blank(), axis.text=element_text(size=20), axis.title=element_text(size=20))

# Model summary statistics 
summary(lmodelQUV) 

# Predicted values by the model added to dataframe 
YIdtQUV$predYI01=predict(lmodelQUV) 

# EDA of HotQUV and CyclicQUV (Model 1) with the model superimposed on the data
ggplot(YIdtQUV, aes(x = Step, y = YI, group = Sample, col = Material)) + geom_point() + 
  geom_line(aes(x=Step, y=YI), linetype="dotted") +
  geom_line(aes(x=Step, y=(predYI01)^(2))) + facet_wrap(~Exp, ncol = 4, scales="fixed") + 
  theme_bw() + 
  scale_color_manual(values=c("red", "black", "blue")) +
  xlab("Exposure Step") +
  ylab("Yellowness Index (YI)") +
  theme(legend.text = element_text(size=15, face="bold"), legend.title = element_text(size=15, face="bold"), 
        axis.text=element_text(size=15,face="bold"), axis.title=element_text(size=15,face="bold"), 
        plot.title=element_text(size=15,face="bold"), strip.text.x = element_text(size=15,face="bold"))
ggsave(paste("../figs/Yellowing-HQ-CQ-FixedScale-FittedModel-Fixed", ".png", sep=""))  

#################################################################################################################

### Model 2) DampHeat and FreezeThaw

#YI without outliers... Only DampHeat and FreezeThaw... USE THIS ONE FOR MODEL 2 ANALYSIS
YIdtDF=subset(dt, Exp!="Baseline" & Exp!="HotQUV" & Exp!="CyclicQUV" & Sample!="sa19603.14" & Rowkey!="sa19601.11-step1" & Rowkey!="sa19601.29-step0" & Sample!="sa19603.00" & Sample!="sa19604.00") 


# EDA of DampHeat and FreezeThaw
ggplot(YIdtDF, aes(x = Step, y = YI, group = Sample, col = Material)) + geom_point() +
  geom_line() + facet_wrap(~Exp, ncol = 5, scales="fixed") + 
  theme_bw() + 
  scale_color_manual(values=c("red", "black", "blue")) +
  xlab("Exposure Step") +
  ylab("Yellowness Index (YI)") +
  theme(legend.text = element_text(size=15, face="bold"), legend.title = element_text(size=15, face="bold"), 
        axis.text=element_text(size=15,face="bold"), axis.title=element_text(size=15,face="bold"), 
        plot.title=element_text(size=15,face="bold"), strip.text.x = element_text(size=15,face="bold"))
ggsave(paste("../figs/Yellowing-DH-FT-FixedScale", ".png", sep=""))  


# FIXED EFFECT MODEL of Yellowing of DampHeat and FreezeThaw (Model 2)

png(file="../figs/figs/boxCox-DH-FT-Fixed-v2.png", width=500, height=500)
par(mar=c(4.1, 4.1, 2, 1), xpd=FALSE)
boxCox(lm(YI~Step*Material+Step*Exp+I(Step^2)*Material+I(Step^2)*Exp+I(Step^3)*Material+I(Step^3)*Exp+Exp*Material,data=YIdtDF), grid=TRUE, cex.axis=1.25, cex.lab=1.25) ## WHY THIS? 
dev.off()
# LAMBDA=1 SELECTED SO THE MODEL BELOW IS NOT TRANSFORMED 

lmYI_DF=lm(YI~Step*Material+Step*Exp+I(Step^2)*Material+I(Step^2)*Exp+I(Step^3)*Material+I(Step^3)*Exp+Exp*Material,data=YIdtDF)     
# THIS IS FINAL-MODEL 

# These plots are used to assess regression assumptions of independence (Residuals vs fitted), homogeneity (Standardized residuals vs fitted) and normality (Normal Q-Q)... 
plot(lmYI_DF, sub="")

# ggplot version of the diagnostics plots (which=1 for residual vs fitted and which=2 for normal q-q)
autoplot(lmYI_DF, which=1, label.size=1, smooth.colour="red") + geom_point(size=2) + theme_bw() +
  theme(plot.title=element_blank(), axis.text=element_text(size=20), axis.title=element_text(size=20))

# Model summary statistics 
summary(lmYI_DF) 

# Predicted values by the model added to dataframe 
YIdtDF$predYI01=predict(lmYI_DF)

# EDA of DampHeat and FreezeThaw (Model 2) with the model superimposed on the data
ggplot(YIdtDF, aes(x = Step, y = YI, group = Sample, col = Material)) + geom_point() + 
  geom_line(aes(x=Step, y=YI), linetype="dotted") +
  geom_line(aes(x=Step,y=predYI01)) + facet_wrap(~Exp, ncol = 4, scales="free") + 
  theme_bw() + 
  scale_color_manual(values=c("red", "black", "blue")) +
  xlab("Exposure Step") +
  ylab("Yellowness Index (YI)") +
  theme(legend.text = element_text(size=15, face="bold"), legend.title = element_text(size=15, face="bold"), 
        axis.text=element_text(size=15,face="bold"), axis.title=element_text(size=15,face="bold"), 
        plot.title=element_text(size=15,face="bold"), strip.text.x = element_text(size=15,face="bold"))
ggsave(paste("../figs/Yellowing-DH-FT-FreeScale-FittedModel-Fixed", ".png", sep=""))  

#################################################################################################################

# Model 1 and Model 2 combined to be plotted 
# geom_line(aes(x=Step,y=(predYI01)^(2))) for CQ-HQ
# geom_line(aes(x=Step,y=predYI01)) for DH-FT

YIdtQUV$predYI <- as.numeric((YIdtQUV$predYI01)^2)
YIdtDF$predYI <- as.numeric(YIdtDF$predYI01)

YIdtQUVDF <- rbind(YIdtQUV, YIdtDF)
YIdtQUVDF$Exp_f <- factor(YIdtQUVDF$Exp, levels=c("CyclicQUV", "HotQUV", "DampHeat", "FreezeThaw"))


ggplot(YIdtQUVDF, aes(x = Step, y = YI, group = Sample, col = Material)) + geom_point() + 
  geom_line(aes(x=Step,y=YI), linetype="dotted") +
  geom_line(aes(x=Step,y=(predYI))) + 
  facet_wrap(~Exp_f, ncol = 4, scales="free") + 
  theme_bw() + 
  scale_color_manual(values=c("red", "black", "blue")) +
  xlab("Exposure Step") +
  ylab("Yellowness Index (YI)") +
  theme(legend.text = element_text(size=15, face="bold"), legend.title = element_text(size=15, face="bold"), 
        axis.text=element_text(size=15,face="bold"), axis.title=element_text(size=15,face="bold"), 
        plot.title=element_text(size=15,face="bold"), strip.text.x = element_text(size=15,face="bold"))
ggsave(paste("../figs/Yellowing-HQ-CQ-DH-FT-FreeScale-FittedModel-Fixed", ".png", sep=""), units="in", width=9.72, height=4.86)  


##################################### Hazing ######################################################

# Haze with outliers
Hazedt=subset(dt, Exp!="Baseline" )  

# Haze without outliers... USE THIS ONE FOR EDA OF ALL
Hazedt=subset(dt, Exp!="Baseline" & Rowkey!="sa19601.00-step0" & Rowkey!="sa19601.09-step1" & Rowkey!="sa19601.14-step1") 
# Exposures ordered for ggplot
Hazedt$Exp_f <- factor(Hazedt$Exp, levels=c("CyclicQUV", "HotQUV", "DampHeat", "FreezeThaw"))

# EDA of ALL
ggplot(Hazedt, aes(x = Step, y = Haze, group = Sample, col = Material)) + geom_point() +
  geom_line() + facet_wrap(~Exp_f, ncol = 5, scales="free") + 
  theme_bw() + 
  scale_color_manual(values=c("red", "black", "blue")) +
  xlab("Exposure Step") +
  ylab("Haze (%)") +
  theme(legend.text = element_text(size=15, face="bold"), legend.title = element_text(size=15, face="bold"), 
        axis.text=element_text(size=15,face="bold"), axis.title=element_text(size=15,face="bold"), 
        plot.title=element_text(size=15,face="bold"), strip.text.x = element_text(size=15,face="bold"))
ggsave(paste("../figs/Hazing-ALL-FreeScale", ".png", sep=""))  


### Model 3) CyclicQUV

# Haze without outliers... Only CyclicQUV... USE THIS ONE FOR MODEL 3 ANALYSIS
cyclicQUV=subset(Hazedt, Exp =="CyclicQUV")

# EDA of CyclicQUV
ggplot(cyclicQUV, aes(x = Step, y = Haze, group = Sample, col = Material)) + geom_point() +
  geom_line() + facet_wrap(~Exp, ncol = 5,scales="free") + 
  theme_bw() + 
  scale_color_manual(values=c("red", "black", "blue")) +
  xlab("Exposure Step") +
  ylab("Haze (%)") +
  theme(legend.text = element_text(size=15, face="bold"), legend.title = element_text(size=15, face="bold"), 
        axis.text=element_text(size=15,face="bold"), axis.title=element_text(size=15,face="bold"), 
        plot.title=element_text(size=15,face="bold"), strip.text.x = element_text(size=15,face="bold"))
ggsave(paste("../figs/Haze-CQ-FreeScale", ".png", sep="")) 


#  MIXED EFFECT MODEL of Hazing of CyclicQUV (Model 3)

fitcyclic1=lme(Haze~Step*Material+I(Step^2)*Material+I(Step^3),data=cyclicQUV, random = list(Sample = ~(-1+Step+I(Step^2))))
# THIS IS FINAL-MODEL

# Marginal and Conditional R^2
r.squaredGLMM(fitcyclic1) # Nakagawa (2013)
# Fitted R^2
1-var(residuals(fitcyclic1))/(var(model.response(model.frame(fitcyclic1)))) #Xu 2003

# Model summary statistics
summary(fitcyclic1) 
# Random effects
ranef(fitcyclic1) 

# These plots are used to assess regression assumptions of independence (Residuals vs fitted), homogeneity (Standardized residuals vs fitted) and normality (Normal Q-Q)... 
x=predict(fitcyclic1) # Fitted values
y=resid(fitcyclic1)   # Residuals 
y2=sqrt(abs(resid(fitcyclic1))) # Square root of Standardized residuals
t=qqnorm(y,plot.it=F)$x         # Theoretical Quantiles

png(file="../figs/ResidualsFitted-CQ-Mixed.png", width=500, height=500)
par(mar=c(4, 4, 1, 1), xpd=FALSE)
plot(x,y, col=c('red', "black", 'blue'),ylab="Residuals",xlab="Fitted Values", pch=19, cex.axis=1.25, cex.lab=1.25)
legend("topright", inset=c(0.01,0.01), legend=c("HydStab","UnStab","UVStab"), fill=c(2,1,4), title="Material")
abline(h=0 ,lwd=1, col="gray", lty="dashed")
abline(h=c(-4, 4, 8) ,lwd=1, col="lightgray")
abline(v=c(0, 20, 40), lwd=1, col="lightgray")
dev.off()
identify(x,y)

png(file="../figs/ScaleLocation-CQ-Mixed.png", width=500, height=500)
par(mar=c(4, 4.3, 1, 1), xpd=FALSE)
plot(x,y2, col=c('red', "black", 'blue'), ylab=expression(sqrt(Standardised~Residuals)), xlab="Fitted Values", pch=19, cex.axis=1.25, cex.lab=1.25)
legend("topright", inset=c(0.01,0.01), legend=c("HydStab","UnStab","UVStab"), fill=c(2,1,4), title="Material")
abline(h=c(0.5, 1.5, 2.5) ,lwd=1, col="lightgray")
abline(v=c(0, 20, 40), lwd=1, col="lightgray")
dev.off()
identify(x,y2)

png(file="../figs/NormalQQ-CQ-Mixed.png", width=500, height=500)
par(mar=c(4, 4, 1, 1), xpd=FALSE)
plot(t,y, col=c('red', "black", 'blue'), ylab="Standardised Residuals", xlab="Theoretical Quantiles", pch=19, cex.axis=1.25, cex.lab=1.25)
legend("topleft", inset=c(0.01,0.01), legend=c("HydStab","UnStab","UVStab"), fill=c(2,1,4), title="Material")
qqline(y, lty=3)
abline(h=c(-4, 0, 4, 8) ,lwd=1, col="lightgray")
abline(v=c(-2, 0, 2), lwd=1, col="lightgray")
dev.off()
identify(t,y)

# Predicted values by the model added to dataframe 
cyclicQUV$predH02=predict(fitcyclic1)

# EDA of CyclicQUV (Model 3) with the model superimposed on the data
ggplot(cyclicQUV, aes(x = Step, y = Haze, group = Sample, col = Material)) + geom_point() + 
  geom_line(aes(x=Step,y=Haze),linetype="dotted")+
  geom_line(aes(x=Step,y=predH02)) + facet_wrap(~Exp, ncol = 4, scales="free") + 
  theme_bw() + 
  scale_color_manual(values=c("red", "black", "blue")) +  
  xlab("Exposure Step") +
  ylab("Haze (%)") +
  theme(legend.text = element_text(size=15, face="bold"), legend.title = element_text(size=15, face="bold"), 
        axis.text=element_text(size=15,face="bold"), axis.title=element_text(size=15,face="bold"), 
        plot.title=element_text(size=15,face="bold"), strip.text.x = element_text(size=15,face="bold"))
ggsave(paste("../figs/Haze-CQ-FreeScale-FittedModel-Mixed", ".png", sep=""))  

#Points of interest
cyclicQUV$Resid=resid(fitcyclic1)
View(cyclicQUV[c(50),])    

#################################################################################################################

### Model 4) DampHeat, FreezeThaw, HotQUV

# Haze without outliers... Only HotQUV, DampHeat, and FreezeThaw... USE THIS ONE FOR MODEL 4 ANALYSIS
nocyclicQUV=subset(Hazedt, Exp!="CyclicQUV")

# EDA of DampHeat, FreezeThaw, HotQUV
ggplot(nocyclicQUV, aes(x = Step, y = Haze, group = Sample, col = Material)) + geom_point() +
  geom_line() + facet_wrap(~Exp, ncol = 5,scales="free") + 
  theme_bw() + 
  scale_color_manual(values=c("red", "black", "blue")) +  
  xlab("Exposure Step") +
  ylab("Haze (%)") +
  theme(legend.text = element_text(size=15, face="bold"), legend.title = element_text(size=15, face="bold"), 
        axis.text=element_text(size=15,face="bold"), axis.title=element_text(size=15,face="bold"), 
        plot.title=element_text(size=15,face="bold"), strip.text.x = element_text(size=15,face="bold"))
ggsave(paste("../figs/Haze-DH-FT-HQ-FreeScale", ".png", sep=""))  


# MIXED EFFECT MODEL of Hazing of DampHeat, FreezeThaw, HotQUV (Model 4)

fit2=lme(fixed=Haze~Step + I(Step^2)+I(Step^3)+Material*Exp,data=nocyclicQUV, random = list(Sample = ~(-1+Step+I(Step^2))))
# THIS IS FINAL-MODEL

# Marginal and Conditional R^2
r.squaredGLMM(fit2) # Nakagawa (2013)
# Fitted R^2
1-var(residuals(fit2))/(var(model.response(model.frame(fit2)))) #Xu 2003		 

# Model summary statistics
summary(fit2)
# Random effects of Model 4
ranef(fit2)

# These plots are used to assess regression assumptions of independence (Residuals vs fitted), homogeneity (Standardized residuals vs fitted) and normality (Normal Q-Q)... 
x=predict(fit2)            # Fitted values
y=resid(fit2)              # Residuals
y2=sqrt(abs(resid(fit2)))  # Square root of standardized residuals
t=qqnorm(y,plot.it=F)$x    # Theoretical quantiles  

png(file="../figs/ResidualsFitted-DH-FT-HQ-Mixed.png", width=500, height=500)
par(mar=c(4, 4, 1, 1), xpd=FALSE)
plot(x,y, col=c('red', "black", 'blue'), ylab="Residuals",xlab="Fitted Values", pch=19, cex.axis=1.25, cex.lab=1.25)
legend("topright", inset=c(0.01,0.01), legend=c("HydStab","UnStab","UVStab"), fill=c(2,1,4), title="Material")
abline(h=0 ,lwd=1, col="gray", lty="dashed")
abline(h=c(-2, 2) ,lwd=1, col="lightgray")
abline(v=c(2, 6, 10), lwd=1, col="lightgray")
dev.off()
identify(x,y)

png(file="../figs/ScaleLocation-DH-FT-HQ-Mixed.png", width=500, height=500)
par(mar=c(4.1, 4.3, 1, 1), xpd=FALSE)
plot(x,y2, col=c('red', "black", 'blue'), ylab=expression(sqrt(Standardised~Residuals)), xlab="Fitted Values", pch=19, cex.axis=1.25, cex.lab=1.25)
legend("topright", inset=c(0.01,0.01), legend=c("HydStab","UnStab","UVStab"), fill=c(2,1,4), title="Material")
abline(h=c(0.2, 0.6, 0.8, 1.2), lwd=1, col="lightgray")
abline(v=c(2, 6, 10), lwd=1, col="lightgray")
dev.off()
identify(x,y2)

png(file="../figs/NormalQQ-DH-FT-HQ-Mixed.png", width=500, height=500)
par(mar=c(4, 4, 1, 1), xpd=FALSE)
plot(t,y, col=c('red', "black", 'blue'), ylab="Standardised Residuals", xlab="Theoretical Quantiles", pch=19, cex.axis=1.25, cex.lab=1.25)
legend("topleft", inset=c(0.01,0.01), legend=c("HydStab","UnStab","UVStab"), fill=c(2,1,4), title="Material")
qqline(y, lty=3)
abline(h=c(-2, 0, 2) ,lwd=1, col="lightgray")
abline(v=c(-3, -1, 1, 3), lwd=1, col="lightgray")
dev.off()
identify(t,y)

# Predicted values by the model added to dataframe 
nocyclicQUV$predH00=predict(fit2)


# EDA of DampHeat, FreezeThaw, HotQUV (Model 4) with the model superimposed on the data
ggplot(nocyclicQUV, aes(x = Step, y = Haze, group = Sample, col = Material)) + geom_point() + 
  geom_line(aes(x=Step,y=Haze),linetype="dotted") +
  geom_line(aes(x=Step,y=predH00)) + facet_wrap(~Exp, ncol = 4,scales="free") + 
  theme_bw() + 
  scale_color_manual(values=c("red", "black", "blue")) +  
  xlab("Exposure Step") +
  ylab("Haze (%)") +
  theme(legend.text = element_text(size=15, face="bold"), legend.title = element_text(size=15, face="bold"), 
        axis.text=element_text(size=15,face="bold"), axis.title=element_text(size=15,face="bold"), 
        plot.title=element_text(size=15,face="bold"), strip.text.x = element_text(size=15,face="bold"))
ggsave(paste("../figs/Haze-DH-FT-HQ-FreeScale-FittedModel-Mixed", ".png", sep=""))  


#################################################################################################################

# Model 3 and Model 4 combined to be plotted 

cyclicQUV$predHaze <- as.numeric(cyclicQUV$predH02)
nocyclicQUV$predHaze <- as.numeric(nocyclicQUV$predH00)

colnames(nocyclicQUV)[8] <- "predH02"

HazeAll <- rbind(cyclicQUV, nocyclicQUV)
HazeAll$Exp_f <- factor(HazeAll$Exp, levels=c("CyclicQUV", "HotQUV", "DampHeat", "FreezeThaw"))


ggplot(HazeAll, aes(x = Step, y = Haze, group = Sample, col = Material)) + geom_point() + 
  geom_line(aes(x=Step,y=Haze), linetype="dotted") +
  geom_line(aes(x=Step,y=(predHaze))) + 
  facet_wrap(~Exp_f, ncol = 4, scales="free") + 
  theme_bw() + 
  scale_color_manual(values=c("red", "black", "blue")) +
  xlab("Exposure Step") +
  ylab("Haze (%)") +
  theme(legend.text = element_text(size=15, face="bold"), legend.title = element_text(size=15, face="bold"), 
        axis.text=element_text(size=15,face="bold"), axis.title=element_text(size=15,face="bold"), 
        plot.title=element_text(size=15,face="bold"), strip.text.x = element_text(size=15,face="bold"))
ggsave(paste("../figs/Hazing-HQ-CQ-DH-FT-FreeScale-FittedModel-Mixed", ".png", sep=""), units="in", width=9.72, height=4.86)  

