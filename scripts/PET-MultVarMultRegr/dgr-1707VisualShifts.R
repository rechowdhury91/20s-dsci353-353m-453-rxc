#Define data frames

test.oc <- testdf.clear
pred <- as.data.frame(predict(model.outdoor.clear, test.oc))
test.oc$pred.dE <- pred$dE
test.oc$pred.dGloss60 <- pred$dGloss60
test.oc$pred.dHaze <- pred$dHaze

test.ow <- testdf.white
pred <- as.data.frame(predict(model.outdoor.white,test.ow))
test.ow$pred.dE <- pred$dE
test.ow$pred.dGloss60 <- pred$dGloss60

test.ic <- indoor.testdf.clear
pred <- as.data.frame(predict(model.indoor.clear, test.ic))
test.ic$pred.dE <- pred$dE
test.ic$pred.dGloss60 <- pred$dGloss60
test.ic$pred.dHaze <- pred$dHaze

test.iw <- indoor.testdf.white
pred <- as.data.frame(predict(model.indoor.white, test.iw))
test.iw$pred.dE <- pred$dE
test.iw$pred.dGloss60 <- pred$dGloss60

# Subset data frames

test.oc.a <- subset(test.oc, Location == "Arizona")
test.oc.ao <- subset(test.oc.a, Mount == "Open")
test.oc.au <- subset(test.oc.a, Mount == "UnderGlass")
test.oc.f <- subset(test.oc, Location == "Florida")
test.oc.fo <- subset(test.oc.f, Mount == "Open")
test.oc.fu <- subset(test.oc.f, Mount == "UnderGlass")

test.ow.a <- subset(test.ow, Location == "Arizona")
test.ow.ao <- subset(test.ow.a, Mount == "Open")
test.ow.au <- subset(test.ow.a, Mount == "UnderGlass")
test.ow.f <- subset(test.ow, Location == "Florida")
test.ow.fo <- subset(test.ow.f, Mount == "Open")
test.ow.fu <- subset(test.ow.f, Mount == "UnderGlass")

test.ic.uv <- subset(test.ic, Exposure.Type == "Wet - UV")
test.ic.fs <- subset(test.ic, Exposure.Type == "Wet - FS")
test.iw.uv <- subset(test.iw, Exposure.Type == "Wet - UV")
test.iw.fs <- subset(test.iw, Exposure.Type == "Wet - FS")

# Define colors for legends

cols <- c("Wet,UV Accelerated" = "black","Wet,FS Accelerated" = "red","AZ,Open" = "blue", "AZ,UnderGlass" = "green","FL,Open"="orange","FL,UnderGlass"="purple")

# NOT SHIFTED PLOTS

ggplot(test.ic.uv, aes(x = UVA.340, y = pred.dE, col = "Wet,UV Accelerated")) + geom_line(size = 1.25) + 
  geom_line(aes(x=UVA.340,y=pred.dE, col = "Wet,FS Accelerated"), data = test.ic.fs, linetype = "dashed",size = 1.5) + 
  geom_line(aes(x=UVA.340,y=pred.dE, col = "AZ,Open"), data = test.oc.ao, linetype = "dotted",size = 1.5) +
  geom_line(aes(x=UVA.340,y=pred.dE, col = "AZ,UnderGlass"), data = test.oc.au, linetype = "dotdash",size = 1.5) +
  geom_line(aes(x=UVA.340,y=pred.dE, col = "FL,Open"), data = test.oc.fo, linetype = "longdash",size = 1.5) +
  geom_line(aes(x=UVA.340,y=pred.dE, col = "FL,UnderGlass"), data = test.oc.fu, linetype = "twodash",size = 1.5) +
  facet_grid(~mat, scales = "free_x") +
  xlab(bquote(~UVA["<"][360] ~ (MJ/m^2))) +
  ylab(expression(Delta*E)) +
  scale_color_manual(name="Exposure", values = cols) +
  theme(legend.text = element_text(size=15, face="bold"), legend.title = element_text(size=15, face="bold"),legend.position="bottom",
        axis.text=element_text(size=15,face="bold"), axis.title=element_text(size=15,face="bold"), 
        plot.title=element_text(size=15,face="bold"), strip.text = element_text(size=15,face="bold"),axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggplot(test.ic.uv, aes(x = UVA.340, y = pred.dE, col = "Wet,UV Accelerated")) + geom_line(size = 1.5) + 
  geom_line(aes(x=UVA.340,y=pred.dE, col = "Wet,FS Accelerated"), data = test.ic.fs, linetype = "dashed", size = 1.5) + 
  geom_line(aes(x=UVA.340,y=pred.dE, col = "AZ,Open"), data = test.oc.ao, linetype = "dotted", size = 1.5) +
  geom_line(aes(x=UVA.340,y=pred.dE, col = "AZ,UnderGlass"), data = test.oc.au, linetype = "dotdash", size = 1.5) +
  geom_line(aes(x=UVA.340,y=pred.dE, col = "FL,Open"), data = test.oc.fo, linetype = "longdash", size = 1.5) +
  geom_line(aes(x=UVA.340,y=pred.dE, col = "FL,UnderGlass"), data = test.oc.fu, linetype = "twodash", size = 1.5) +
  facet_grid(~mat, scales = "free_x") +
  xlab(bquote(~UVA["<"][360] ~ (MJ/m^2))) +
  ylab(expression(Delta*E)) +
  scale_color_manual(name="Exposure", values = cols) +
  theme(legend.text = element_text(size=15, face="bold"), legend.title = element_text(size=15, face="bold"), 
        axis.text=element_text(size=12,face="bold"), axis.title=element_text(size=15,face="bold"), 
        plot.title=element_text(size=15,face="bold"), strip.text.x = element_text(size=15,face="bold"), strip.text.y = element_text(size=15,face="bold"), axis.title.x =element_text(size=18,face="bold"), axis.title.y =element_text(size=18,face="bold"), legend.position="bottom")

ggplot(test.ic.uv, aes(x = UVA.340, y = pred.dGloss60, col = "Wet,UV Accelerated")) + geom_line() + 
  geom_line(aes(x=UVA.340,y=pred.dGloss60, col = "Wet,FS Accelerated"), data = test.ic.fs, linetype = "dashed") + 
  geom_line(aes(x=UVA.340,y=pred.dGloss60, col = "AZ,Open"), data = test.oc.ao, linetype = "dotted") +
  geom_line(aes(x=UVA.340,y=pred.dGloss60, col = "AZ,UnderGlass"), data = test.oc.au, linetype = "dotdash") +
  geom_line(aes(x=UVA.340,y=pred.dGloss60, col = "FL,Open"), data = test.oc.fo, linetype = "longdash") +
  geom_line(aes(x=UVA.340,y=pred.dGloss60, col = "FL,UnderGlass"), data = test.oc.fu, linetype = "twodash") +
  facet_grid(~mat, scales = "free_x") +
  xlab(bquote(~UVA["<"][360] ~ (MJ/m^2))) +
  ylab(expression(Delta*E)) +
  scale_color_manual(name="Exposure", values = cols) +
  theme(legend.text = element_text(size=15, face="bold"), legend.title = element_text(size=15, face="bold"), 
        axis.text=element_text(size=15,face="bold"), axis.title=element_text(size=15,face="bold"), 
        plot.title=element_text(size=15,face="bold"), strip.text.x = element_text(size=15,face="bold"))

ggplot(test.ic.uv, aes(x = UVA.340, y = pred.dGloss60, col = "Wet,UV Accelerated")) + geom_line() + 
  geom_line(aes(x=UVA.340,y=pred.dGloss60, col = "Wet,FS Accelerated"), data = test.ic.fs, linetype = "dashed") + 
  geom_line(aes(x=UVA.340,y=pred.dGloss60, col = "AZ,Open"), data = test.oc.ao, linetype = "dotted") +
  geom_line(aes(x=UVA.340,y=pred.dGloss60, col = "AZ,UnderGlass"), data = test.oc.au, linetype = "dotdash") +
  geom_line(aes(x=UVA.340,y=pred.dGloss60, col = "FL,Open"), data = test.oc.fo, linetype = "longdash") +
  geom_line(aes(x=UVA.340,y=pred.dGloss60, col = "FL,UnderGlass"), data = test.oc.fu, linetype = "twodash") +
  facet_grid(~mat, scales = "free_x") +
  xlab(bquote(~UVA["<"][360] ~ (MJ/m^2))) +
  ylab(expression(Delta*Gloss[60])) +
  scale_color_manual(name="Exposure", values = cols) +
  theme(legend.text = element_text(size=15, face="bold"), legend.title = element_text(size=15, face="bold"), 
        axis.text=element_text(size=15,face="bold"), axis.title=element_text(size=15,face="bold"), 
        plot.title=element_text(size=15,face="bold"), strip.text.x = element_text(size=15,face="bold"))

ggplot(test.ic.uv, aes(x = UVA.340, y = pred.dHaze, col = "Wet,UV Accelerated")) + geom_line(size = 1.25) + 
  geom_line(aes(x=UVA.340,y=pred.dHaze, col = "Wet,FS Accelerated"), data = test.ic.fs, linetype = "dashed",size = 1.5) + 
  geom_line(aes(x=UVA.340,y=pred.dHaze, col = "AZ,Open"), data = test.oc.ao, linetype = "dotted",size = 1.5) +
  geom_line(aes(x=UVA.340,y=pred.dHaze, col = "AZ,UnderGlass"), data = test.oc.au, linetype = "dotdash",size = 1.5) +
  geom_line(aes(x=UVA.340,y=pred.dHaze, col = "FL,Open"), data = test.oc.fo, linetype = "longdash",size = 1.5) +
  geom_line(aes(x=UVA.340,y=pred.dHaze, col = "FL,UnderGlass"), data = test.oc.fu, linetype = "twodash",size = 1.5) +
  facet_grid(~mat, scales = "free_x") +
  xlab(bquote(~UVA["<"][360] ~ (MJ/m^2))) +
  ylab(expression(Delta*Haze)) +
  scale_color_manual(name="Exposure", values = cols) +
  theme(legend.text = element_text(size=15, face="bold"), legend.title = element_text(size=15, face="bold"),legend.position="bottom",
        axis.text=element_text(size=15,face="bold"), axis.title=element_text(size=15,face="bold"), 
        plot.title=element_text(size=15,face="bold"), strip.text = element_text(size=15,face="bold"),axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

# Outdoor White PET

ggplot(test.iw.uv, aes(x = UVA.340, y = pred.dE, col = "Wet,UV Accelerated")) + geom_line() + 
  geom_line(aes(x=UVA.340,y=pred.dE, col = "Wet,FS Accelerated"), data = test.iw.fs, linetype = "dashed") + 
  geom_line(aes(x=UVA.340,y=pred.dE, col = "AZ,Open"), data = test.ow.ao, linetype = "dotted") +
  geom_line(aes(x=UVA.340,y=pred.dE, col = "AZ,UnderGlass"), data = test.ow.au, linetype = "dotdash") +
  geom_line(aes(x=UVA.340,y=pred.dE, col = "FL,Open"), data = test.ow.fo, linetype = "longdash") +
  geom_line(aes(x=UVA.340,y=pred.dE, col = "FL,UnderGlass"), data = test.ow.fu, linetype = "twodash") +
  facet_grid(~mat, scales = "free_x") +
  xlab(bquote(~UVA["<"][360] ~ (MJ/m^2))) +
  ylab(expression(Delta*E)) +
  scale_color_manual(name="Exposure", values = cols) +
  theme(legend.text = element_text(size=15, face="bold"), legend.title = element_text(size=15, face="bold"), 
        axis.text=element_text(size=12,face="bold"), axis.title=element_text(size=15,face="bold"), 
        plot.title=element_text(size=15,face="bold"), strip.text.x = element_text(size=15,face="bold"))

ggplot(test.iw.uv, aes(x = UVA.340, y = pred.dGloss60, col = "Wet,UV Accelerated")) + geom_line() + 
  geom_line(aes(x=UVA.340,y=pred.dGloss60, col = "Wet,FS Accelerated"), data = test.iw.fs, linetype = "dashed") + 
  geom_line(aes(x=UVA.340,y=pred.dGloss60, col = "AZ,Open"), data = test.ow.ao, linetype = "dotted") +
  geom_line(aes(x=UVA.340,y=pred.dGloss60, col = "AZ,UnderGlass"), data = test.ow.au, linetype = "dotdash") +
  geom_line(aes(x=UVA.340,y=pred.dGloss60, col = "FL,Open"), data = test.ow.fo, linetype = "longdash") +
  geom_line(aes(x=UVA.340,y=pred.dGloss60, col = "FL,UnderGlass"), data = test.ow.fu, linetype = "twodash") +
  facet_grid(~mat, scales = "free_x") +
  xlab(bquote(~UVA["<"][360] ~ (MJ/m^2))) +
  ylab(expression(Delta*Gloss[60])) +
  scale_color_manual(name="Exposure", values = cols) +
  theme(legend.text = element_text(size=15, face="bold"), legend.title = element_text(size=15, face="bold"), 
        axis.text=element_text(size=12,face="bold"), axis.title=element_text(size=15,face="bold"), 
        plot.title=element_text(size=15,face="bold"), strip.text.x = element_text(size=15,face="bold"))

# Shift Factor Analysis

# Clear PET

# Get minimum Dose range

range <- min(c(max(test.ic.uv$UVA.340),max(test.ic.fs$UVA.340),max(test.oc.ao$UVA.340),max(test.oc.au$UVA.340),max(test.oc.fo$UVA.340),max(test.oc.fu$UVA.340)))

range <- 160
# Subset Dose Range

test.ic.uv <- subset(test.ic.uv, UVA.340 <= range & UVA.340 >= 60)
test.ic.fs <- subset(test.ic.fs, UVA.340 <= range & UVA.340 >= 60)
test.ic.fs <- rbind(test.ic.fs,test.ic.fs)
test.oc.ao <- subset(test.oc.ao, UVA.340 <= range & UVA.340 >= 60)
test.oc.au <- subset(test.oc.au, UVA.340 <= range & UVA.340 >= 60)
test.oc.fo <- subset(test.oc.fo, UVA.340 <= range & UVA.340 >= 60)
test.oc.fu <- subset(test.oc.fu, UVA.340 <= range & UVA.340 >= 60)

# Version 1 - Not so great

#id1 <- order(test.ic.uv$UVA.340)
#id2 <- order(test.ic.fs$UVA.340)
#id3 <- order(test.ic.fs$UVA.340)

#AUC <- function(dat,xc,id){sum(diff(xc*dat$UVA.340[id])*rollmean(dat$pred.dE[id],2))}

#SSE <- function(xc){(AUC(test.ic.uv,1) - AUC(dat,xc))^2}

# Version 2 - Color

DIF <- function(xc){
  
  # Find max value of response of curves of interest
  
  row.mat <- which.min(abs(dat$pred.dE-max(dat$pred.dE)))
  
  # Find closest equivalent value in base curves
  
  row.base <- which.min(abs(test.ic.uv$pred.dE[test.ic.uv$Material == dat$Material[row.mat]]-max(dat$pred.dE)))
  
  # Minimize difference
  
  abs(test.ic.uv$UVA.340[row.base] - xc*dat$UVA.340[row.mat][1])
}

dat <- test.ic.fs 
shift.ic.fs.color <- nlm(DIF,1.1, iterlim = 100)
shift.ic.fs.color

dat <- test.oc.ao 
shift.oc.ao.color <- nlm(DIF,2.2, iterlim = 100)
shift.oc.ao.color

dat <- test.oc.au 
shift.oc.au.color <- nlm(DIF,1, iterlim = 100)
shift.oc.au.color

dat <- test.oc.fo 
shift.oc.fo.color <- nlm(DIF,1.7, iterlim = 100)
shift.oc.fo.color

dat <- test.oc.fu 
shift.oc.fu.color <- nlm(DIF,1.1, iterlim = 100)
shift.oc.fu.color

# Gloss

# Get minimum Dose range

range <- 125

# Subset Dose Range

test.ic.uv <- subset(test.ic.uv, UVA.340 <= range & UVA.340 >= 60)
test.ic.fs <- subset(test.ic.fs, UVA.340 <= range & UVA.340 >= 60)
test.ic.fs <- rbind(test.ic.fs,test.ic.fs)
test.oc.ao <- subset(test.oc.ao, UVA.340 <= range & UVA.340 >= 60)
test.oc.au <- subset(test.oc.au, UVA.340 <= range & UVA.340 >= 60)
test.oc.fo <- subset(test.oc.fo, UVA.340 <= range & UVA.340 >= 60)
test.oc.fu <- subset(test.oc.fu, UVA.340 <= range & UVA.340 >= 60)

DIF <- function(xc){
  
  row.mat <- which.min(abs(dat$pred.dGloss-min(dat$pred.dGloss)))
  
  row.base <- which.min(abs(test.oc.fo$pred.dGloss[test.oc.fo$Material == dat$Material[row.mat]]-min(dat$pred.dGloss)))
  
  abs(test.oc.fo$UVA.340[row.base] - xc*dat$UVA.340[row.mat][1])
}

dat <- test.ic.fs 
shift.ic.fs.gloss <- nlm(DIF,0.3, iterlim = 100)
shift.ic.fs.gloss

dat <- test.oc.ao 
shift.oc.ao.gloss <- nlm(DIF,0.3, iterlim = 100)
shift.oc.ao.gloss

dat <- test.oc.au 
shift.oc.au.gloss <- nlm(DIF,0.5, iterlim = 100)
shift.oc.au.gloss

dat <- test.ic.uv 
shift.ic.uv.gloss <- nlm(DIF,1.0, iterlim = 100)
shift.ic.uv.gloss

dat <- test.oc.fu 
shift.oc.fu.gloss <- nlm(DIF,0.5, iterlim = 100)
shift.oc.fu.gloss

# Rebase shift factors to IC-UV

shift.oc.fo.gloss <- shift.ic.uv.gloss

shift.ic.fs.gloss$estimate <- shift.ic.fs.gloss$estimate/shift.ic.uv.gloss$estimate
shift.oc.ao.gloss$estimate <- shift.oc.ao.gloss$estimate/shift.ic.uv.gloss$estimate
shift.oc.au.gloss$estimate <- shift.oc.au.gloss$estimate/shift.ic.uv.gloss$estimate
shift.oc.fo.gloss$estimate <- 1/shift.ic.uv.gloss$estimate
shift.oc.fu.gloss$estimate <- shift.oc.fu.gloss$estimate/shift.ic.uv.gloss$estimate

# Haze

DIF <- function(xc){
  
  row.mat <- which.min(abs(dat$pred.dHaze-max(dat$pred.dHaze)))
  
  row.base <- which.min(abs(test.oc.fo$pred.dHaze[test.oc.fo$Material == dat$Material[row.mat]]-max(dat$pred.dHaze)))
  
  abs(test.oc.fo$UVA.340[row.base] - xc*dat$UVA.340[row.mat][1])
}

dat <- test.ic.fs 
shift.ic.fs.haze <- nlm(DIF,0.5, iterlim = 100)
shift.ic.fs.haze

dat <- test.oc.ao 
shift.oc.ao.haze <- nlm(DIF,2.2, iterlim = 100)
shift.oc.ao.haze

dat <- test.oc.au 
shift.oc.au.haze <- nlm(DIF,1, iterlim = 100)
shift.oc.au.haze

dat <- test.ic.uv
shift.ic.uv.haze <- nlm(DIF,1.0, iterlim = 100)
shift.ic.uv.haze

dat <- test.oc.fu 
shift.oc.fu.haze <- nlm(DIF,1.6, iterlim = 100)
shift.oc.fu.haze

# Rebase shift factors to IC-UV

shift.oc.fo.haze <- shift.ic.uv.haze

shift.ic.fs.haze$estimate <- shift.ic.fs.haze$estimate/shift.ic.uv.haze$estimate
shift.oc.ao.haze$estimate <- shift.oc.ao.haze$estimate/shift.ic.uv.haze$estimate
shift.oc.au.haze$estimate <- shift.oc.au.haze$estimate/shift.ic.uv.haze$estimate
shift.oc.fo.haze$estimate <- 1/shift.ic.uv.haze$estimate
shift.oc.fu.haze$estimate <- shift.oc.fu.haze$estimate/shift.ic.uv.haze$estimate

# White PET

# Get minimum Dose range

range <- min(c(max(test.iw.uv$UVA.340),max(test.iw.fs$UVA.340),max(test.ow.ao$UVA.340),max(test.ow.au$UVA.340),max(test.ow.fo$UVA.340),max(test.ow.fu$UVA.340)))

range <- 45

# Subset Dose Range

test.iw.uv <- subset(test.iw.uv, UVA.340 <= range & UVA.340 >= 0 & Material!= "U2" & Material!= "UL")
test.iw.fs <- subset(test.iw.fs, UVA.340 <= range & UVA.340 >= 0& Material!= "U2" & Material!= "UL")
test.iw.fs <- rbind(test.iw.fs,test.iw.fs)
test.ow.ao <- subset(test.ow.ao, UVA.340 <= range & UVA.340 >= 0 & Material!= "U2" & Material!= "UL")
test.ow.au <- subset(test.ow.au, UVA.340 <= range & UVA.340 >= 0 & Material!= "U2" & Material!= "UL")
test.ow.fo <- subset(test.ow.fo, UVA.340 <= range & UVA.340 >= 0 & Material!= "U2" & Material!= "UL")
test.ow.fu <- subset(test.ow.fu, UVA.340 <= range & UVA.340 >= 0 & Material!= "U2" & Material!= "UL")

# Color

DIF <- function(xc){
  
  row.mat <- which.min(abs(dat$pred.dE-max(dat$pred.dE)))
  
  row.base <- which.min(abs(test.iw.uv$pred.dE[test.iw.uv$Material == dat$Material[row.mat]]-max(dat$pred.dE)))
  
  abs(test.iw.uv$UVA.340[row.base] - xc*dat$UVA.340[row.mat][1])
}

dat <- test.iw.fs 
shift.iw.fs.color <- nlm(DIF,1.0, iterlim = 100)
shift.iw.fs.color

dat <- test.ow.ao 
shift.ow.ao.color <- nlm(DIF,0.3, iterlim = 100)
shift.ow.ao.color

dat <- test.ow.au 
shift.ow.au.color <- nlm(DIF,0.7, iterlim = 100)
shift.ow.au.color

dat <- test.ow.fo 
shift.ow.fo.color <- nlm(DIF,0.69, iterlim = 100)
shift.ow.fo.color

dat <- test.ow.fu 
shift.ow.fu.color <- nlm(DIF,0.65, iterlim = 100)
shift.ow.fu.color

# Gloss

range <- 125

# Subset Dose Range

test.iw.uv <- subset(test.iw.uv, UVA.340 <= range & UVA.340 >= 0 & Material!= "U2" & Material!= "UL")
test.iw.fs <- subset(test.iw.fs, UVA.340 <= range & UVA.340 >= 0& Material!= "U2" & Material!= "UL")
test.iw.fs <- rbind(test.iw.fs,test.iw.fs)
test.ow.ao <- subset(test.ow.ao, UVA.340 <= range & UVA.340 >= 0 & Material!= "U2" & Material!= "UL")
test.ow.au <- subset(test.ow.au, UVA.340 <= range & UVA.340 >= 0 & Material!= "U2" & Material!= "UL")
test.ow.fo <- subset(test.ow.fo, UVA.340 <= range & UVA.340 >= 0 & Material!= "U2" & Material!= "UL")
test.ow.fu <- subset(test.ow.fu, UVA.340 <= range & UVA.340 >= 0 & Material!= "U2" & Material!= "UL")

DIF <- function(xc){
  
  row.mat <- which.min(abs(dat$pred.dGloss60-min(dat$pred.dGloss60)))
  
  row.base <- which.min(abs(test.ow.fo$pred.dGloss60[test.ow.fo$Material == dat$Material[row.mat]]-min(dat$pred.dGloss60)))
  
  abs(test.iw.uv$UVA.340[row.base] - xc*dat$UVA.340[row.mat][1])
}

dat <- test.iw.fs 
shift.iw.fs.gloss <- nlm(DIF,0.5, iterlim = 100)
shift.iw.fs.gloss

dat <- test.ow.ao 
shift.ow.ao.gloss <- nlm(DIF,0.5, iterlim = 100)
shift.ow.ao.gloss

dat <- test.ow.au 
shift.ow.au.gloss <- nlm(DIF,1, iterlim = 100)
shift.ow.au.gloss

dat <- test.iw.uv 
shift.iw.uv.gloss <- nlm(DIF,1.7, iterlim = 100)
shift.iw.uv.gloss

dat <- test.ow.fu 
shift.ow.fu.gloss <- nlm(DIF,1.6, iterlim = 100)
shift.ow.fu.gloss

# Rebase shift factors to IC-UV

shift.ow.fo.gloss <- shift.iw.uv.gloss

shift.iw.fs.gloss$estimate <- shift.iw.fs.gloss$estimate/shift.iw.uv.gloss$estimate
shift.ow.ao.gloss$estimate <- shift.ow.ao.gloss$estimate/shift.iw.uv.gloss$estimate
shift.ow.au.gloss$estimate <- shift.ow.au.gloss$estimate/shift.iw.uv.gloss$estimate
shift.ow.fo.gloss$estimate <- 1/shift.iw.uv.gloss$estimate
shift.ow.fu.gloss$estimate <- shift.ow.fu.gloss$estimate/shift.iw.uv.gloss$estimate

# White PET - Teteron Only

# Get minimum Dose range

range <- min(c(max(test.iw.uv$UVA.340),max(test.iw.fs$UVA.340),max(test.ow.ao$UVA.340),max(test.ow.au$UVA.340),max(test.ow.fo$UVA.340),max(test.ow.fu$UVA.340)))

range <- 45

# Subset Dose Range

test.iw.uv <- subset(test.iw.uv, UVA.340 <= range & UVA.340 >= 0 | Material== "U2" | Material== "UL")
test.iw.fs <- subset(test.iw.fs, UVA.340 <= range & UVA.340 >= 0 | Material== "U2" | Material== "UL")
test.iw.fs <- rbind(test.iw.fs,test.iw.fs)
test.ow.ao <- subset(test.ow.ao, UVA.340 <= range & UVA.340 >= 0 | Material== "U2" | Material== "UL")
test.ow.au <- subset(test.ow.au, UVA.340 <= range & UVA.340 >= 0 | Material== "U2" | Material== "UL")
test.ow.fo <- subset(test.ow.fo, UVA.340 <= range & UVA.340 >= 0 | Material== "U2" | Material== "UL")
test.ow.fu <- subset(test.ow.fu, UVA.340 <= range & UVA.340 >= 0 | Material== "U2" | Material== "UL")

# Color

DIF <- function(xc){
  
  row.mat <- which.min(abs(dat$pred.dE-max(dat$pred.dE)))
  
  row.base <- which.min(abs(test.iw.uv$pred.dE[test.iw.uv$Material == dat$Material[row.mat]]-max(dat$pred.dE)))
  
  abs(test.iw.uv$UVA.340[row.base] - xc*dat$UVA.340[row.mat][1])
}

dat <- test.iw.fs 
shift.iw.fs.color <- nlm(DIF,0.5, iterlim = 100)
shift.iw.fs.color

dat <- test.ow.ao 
shift.ow.ao.color <- nlm(DIF,0.3, iterlim = 100)
shift.ow.ao.color

dat <- test.ow.au 
shift.ow.au.color <- nlm(DIF,1, iterlim = 100)
shift.ow.au.color

dat <- test.ow.fo 
shift.ow.fo.color <- nlm(DIF,1.0, iterlim = 100)
shift.ow.fo.color

dat <- test.ow.fu 
shift.ow.fu.color <- nlm(DIF,1.6, iterlim = 100)
shift.ow.fu.color

# Gloss

range <- 125

# Subset Dose Range

test.iw.uv <- subset(test.iw.uv, UVA.340 <= range & UVA.340 >= 0 | Material== "U2" | Material== "UL")
test.iw.fs <- subset(test.iw.fs, UVA.340 <= range & UVA.340 >= 0 | Material== "U2" | Material== "UL")
test.iw.fs <- rbind(test.iw.fs,test.iw.fs)
test.ow.ao <- subset(test.ow.ao, UVA.340 <= range & UVA.340 >= 0 | Material== "U2" | Material== "UL")
test.ow.au <- subset(test.ow.au, UVA.340 <= range & UVA.340 >= 0 | Material== "U2" | Material== "UL")
test.ow.fo <- subset(test.ow.fo, UVA.340 <= range & UVA.340 >= 0 | Material== "U2" | Material== "UL")
test.ow.fu <- subset(test.ow.fu, UVA.340 <= range & UVA.340 >= 0 | Material== "U2" | Material== "UL")

DIF <- function(xc){
  
  row.mat <- which.min(abs(dat$pred.dGloss60-min(dat$pred.dGloss60)))
  
  row.base <- which.min(abs(test.ow.au$pred.dGloss60[test.ow.au$Material == dat$Material[row.mat]]-min(dat$pred.dGloss60)))
  
  abs(test.ow.au$UVA.340[row.base] - xc*dat$UVA.340[row.mat][1])
}

dat <- test.iw.fs 
shift.iw.fs.gloss <- nlm(DIF,0.5, iterlim = 100)
shift.iw.fs.gloss

dat <- test.ow.ao 
shift.ow.ao.gloss <- nlm(DIF,0.5, iterlim = 100)
shift.ow.ao.gloss

dat <- test.iw.uv 
shift.iw.uv.gloss <- nlm(DIF,0.5, iterlim = 100)
shift.iw.uv.gloss

dat <- test.ow.fo 
shift.ow.fo.gloss <- nlm(DIF,1.0, iterlim = 100)
shift.ow.fo.gloss

dat <- test.ow.fu 
shift.ow.fu.gloss <- nlm(DIF,1.6, iterlim = 100)
shift.ow.fu.gloss

# Rebase shift factors to IC-UV

shift.ow.au.gloss <- shift.iw.uv.gloss

shift.iw.fs.gloss$estimate <- shift.iw.fs.gloss$estimate/shift.iw.uv.gloss$estimate
shift.ow.ao.gloss$estimate <- shift.ow.ao.gloss$estimate/shift.iw.uv.gloss$estimate
shift.ow.au.gloss$estimate <- 1/shift.iw.uv.gloss$estimate
shift.ow.fo.gloss$estimate <- shift.ow.fo.gloss$estimate/shift.iw.uv.gloss$estimate
shift.ow.fu.gloss$estimate <- shift.ow.fu.gloss$estimate/shift.iw.uv.gloss$estimate


# SHIFTED PLOTS

# Clear PET

ggplot(test.ic.uv, aes(x = UVA.340, y = pred.dE, col = "Wet,UV Accelerated")) + geom_line(size = 1.25) + 
  geom_line(aes(x=UVA.340*shift.ic.fs.color$estimate,y=pred.dE, col = "Wet,FS Accelerated"), data = test.ic.fs, linetype = "dashed",size = 1.5) + 
  geom_line(aes(x=UVA.340*shift.oc.ao.color$estimate,y=pred.dE, col = "AZ,Open"), data = test.oc.ao, linetype = "dotted",size = 1.5) +
  geom_line(aes(x=UVA.340*shift.oc.au.color$estimate,y=pred.dE, col = "AZ,UnderGlass"), data = test.oc.au, linetype = "dotdash",size = 1.5) +
  geom_line(aes(x=UVA.340*shift.oc.fo.color$estimate,y=pred.dE, col = "FL,Open"), data = test.oc.fo, linetype = "longdash",size = 1.5) +
  geom_line(aes(x=UVA.340*shift.oc.fu.color$estimate,y=pred.dE, col = "FL,UnderGlass"), data = test.oc.fu, linetype = "twodash",size = 1.5) +
  facet_grid(~mat, scales = "free_x") +
  xlab(bquote(~UVA["<"][360] ~ (MJ/m^2))) +
  ylab(expression(Delta*E)) +
  scale_color_manual(name="Exposure", values = cols) +
  theme(legend.text = element_text(size=15, face="bold"), legend.title = element_text(size=15, face="bold"),legend.position="bottom",
        axis.text=element_text(size=15,face="bold"), axis.title=element_text(size=15,face="bold"), 
        plot.title=element_text(size=15,face="bold"), strip.text = element_text(size=15,face="bold"),axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggplot(test.ic.uv, aes(x = UVA.340, y = pred.dGloss60, col = "Wet,UV Accelerated")) + geom_line(size = 1.25) + 
  geom_line(aes(x=UVA.340*shift.ic.fs.gloss$estimate,y=pred.dGloss60, col = "Wet,FS Accelerated"), data = test.ic.fs, linetype = "dashed",size = 1.25) + 
  geom_line(aes(x=UVA.340*shift.oc.ao.gloss$estimate,y=pred.dGloss60, col = "AZ,Open"), data = test.oc.ao, linetype = "dotted",size = 1.25) +
  geom_line(aes(x=UVA.340*shift.oc.au.gloss$estimate,y=pred.dGloss60, col = "AZ,UnderGlass"), data = test.oc.au, linetype = "dotdash",size = 1.25) +
  geom_line(aes(x=UVA.340*shift.oc.fo.gloss$estimate,y=pred.dGloss60, col = "FL,Open"), data = test.oc.fo, linetype = "longdash",size = 1.25) +
  geom_line(aes(x=UVA.340*shift.oc.fu.gloss$estimate,y=pred.dGloss60, col = "FL,UnderGlass"), data = test.oc.fu, linetype = "twodash",size = 1.25) +
  facet_grid(~mat, scales = "free_x") +
  xlab(bquote(~UVA["<"][360] ~ (MJ/m^2))) +
  ylab(expression(Delta*Gloss[60])) +
  scale_color_manual(name="Exposure", values = cols) +
  theme(legend.text = element_text(size=15, face="bold"), legend.title = element_text(size=15, face="bold"),legend.position="bottom",
        axis.text=element_text(size=15,face="bold"), axis.title=element_text(size=15,face="bold"), 
        plot.title=element_text(size=15,face="bold"), strip.text = element_text(size=15,face="bold"),axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggplot(test.ic.uv, aes(x = UVA.340, y = pred.dHaze, col = "Wet,UV Accelerated")) + geom_line(size = 1.25) + 
  geom_line(aes(x=UVA.340*shift.ic.fs.haze$estimate,y=pred.dHaze, col = "Wet,FS Accelerated"), data = test.ic.fs, linetype = "dashed",size = 1.25) + 
  geom_line(aes(x=UVA.340*shift.oc.ao.haze$estimate,y=pred.dHaze, col = "AZ,Open"), data = test.oc.ao, linetype = "dotted",size = 1.25) +
  geom_line(aes(x=UVA.340*shift.oc.au.haze$estimate,y=pred.dHaze, col = "AZ,UnderGlass"), data = test.oc.au, linetype = "dotdash",size = 1.25) +
  geom_line(aes(x=UVA.340*shift.oc.fo.haze$estimate,y=pred.dHaze, col = "FL,Open"), data = test.oc.fo, linetype = "longdash",size = 1.25) +
  geom_line(aes(x=UVA.340*shift.oc.fu.haze$estimate,y=pred.dHaze, col = "FL,UnderGlass"), data = test.oc.fu, linetype = "twodash",size = 1.25) +
  facet_grid(~mat, scales = "free_x") +
  xlab(bquote(~UVA["<"][360] ~ (MJ/m^2))) +
  ylab(expression(Delta*Haze)) +
  scale_color_manual(name="Exposure", values = cols) +
  theme(legend.text = element_text(size=15, face="bold"), legend.title = element_text(size=15, face="bold"),legend.position="bottom",axis.text=element_text(size=15,face="bold"), axis.title=element_text(size=15,face="bold"), plot.title=element_text(size=15,face="bold"), strip.text = element_text(size=15,face="bold"),axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

# White PET - No Teteron

ggplot(subset(test.iw.uv,Material!= "U2" & Material!= "UL" ), aes(x = UVA.340, y = pred.dE, col = "Wet,UV Accelerated")) + geom_line(size = 1.25) + 
  geom_line(aes(x=UVA.340*shift.iw.fs.color$estimate,y=pred.dE, col = "Wet,FS Accelerated"), data = subset(test.iw.fs,Material!= "U2" & Material!= "UL"), linetype = "dashed",size = 1.25) + 
  geom_line(aes(x=UVA.340*shift.ow.ao.color$estimate,y=pred.dE, col = "AZ,Open"), data = subset(test.ow.ao,Material!= "U2" & Material!= "UL"), linetype = "dotted",size = 1.25) +
  geom_line(aes(x=UVA.340*shift.ow.au.color$estimate,y=pred.dE, col = "AZ,UnderGlass"), data = subset(test.ow.au,Material!= "U2" & Material!= "UL"), linetype = "dotdash",size = 1.25) +
  geom_line(aes(x=UVA.340*shift.ow.fo.color$estimate,y=pred.dE, col = "FL,Open"), data = subset(test.ow.fo,Material!= "U2" & Material!= "UL"), linetype = "longdash",size = 1.25) +
  geom_line(aes(x=UVA.340*shift.ow.fu.color$estimate,y=pred.dE, col = "FL,UnderGlass"), data = subset(test.ow.fu,Material!= "U2" & Material!= "UL"), linetype = "twodash",size = 1.25) +
  facet_grid(~mat, scales = "free_x") +
  xlab(bquote(~UVA["<"][360] ~ (MJ/m^2))) +
  ylab(expression(Delta*E)) +
  scale_color_manual(name="Exposure", values = cols) +
  theme(legend.text = element_text(size=15, face="bold"), legend.title = element_text(size=15, face="bold"),legend.position="bottom",axis.text=element_text(size=15,face="bold"), axis.title=element_text(size=15,face="bold"), plot.title=element_text(size=15,face="bold"), strip.text = element_text(size=15,face="bold"),axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggplot(subset(test.iw.uv,Material!= "U2" & Material!= "UL" ), aes(x = UVA.340, y = pred.dGloss60, col = "Wet,UV Accelerated")) + geom_line(size = 1.25) + 
  geom_line(aes(x=UVA.340*shift.iw.fs.gloss$estimate,y=pred.dGloss60, col = "Wet,FS Accelerated"), data = subset(test.iw.fs,Material!= "U2" & Material!= "UL"), linetype = "dashed",size = 1.25) + 
  geom_line(aes(x=UVA.340*shift.ow.ao.gloss$estimate,y=pred.dGloss60, col = "AZ,Open"), data = subset(test.ow.ao,Material!= "U2" & Material!= "UL"), linetype = "dotted",size = 1.25) +
  geom_line(aes(x=UVA.340*shift.ow.au.gloss$estimate,y=pred.dGloss60, col = "AZ,UnderGlass"), data = subset(test.ow.au,Material!= "U2" & Material!= "UL"), linetype = "dotdash",size = 1.25) +
  geom_line(aes(x=UVA.340*shift.ow.fo.gloss$estimate,y=pred.dGloss60, col = "FL,Open"), data = subset(test.ow.fo,Material!= "U2" & Material!= "UL"), linetype = "longdash",size = 1.25) +
  geom_line(aes(x=UVA.340*shift.ow.fu.gloss$estimate,y=pred.dGloss60, col = "FL,UnderGlass"), data = subset(test.ow.fu,Material!= "U2" & Material!= "UL"), linetype = "twodash",size = 1.25) +
  facet_grid(~mat, scales = "free_x") +
  xlab(bquote(~UVA["<"][360] ~ (MJ/m^2))) +
  ylab(expression(Delta*Gloss[60])) +
  scale_color_manual(name="Exposure", values = cols) +
  theme(legend.text = element_text(size=15, face="bold"), legend.title = element_text(size=15, face="bold"),legend.position="bottom",axis.text=element_text(size=15,face="bold"), axis.title=element_text(size=15,face="bold"), plot.title=element_text(size=15,face="bold"), strip.text = element_text(size=15,face="bold"),axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

# White PET - Only Teteron

ggplot(subset(test.iw.uv,Material== "U2" | Material== "UL" ), aes(x = UVA.340, y = pred.dE, col = "Wet,UV Accelerated")) + geom_line(size=1.25) + 
  geom_line(aes(x=UVA.340*shift.iw.fs.color$estimate,y=pred.dE, col = "Wet,FS Accelerated"), data = subset(test.iw.fs,Material== "U2" | Material== "UL"), linetype = "dashed",size=1.25) + 
  geom_line(aes(x=UVA.340*shift.ow.ao.color$estimate,y=pred.dE, col = "AZ,Open"), data = subset(test.ow.ao,Material== "U2" | Material== "UL"), linetype = "dotted",size=1.25) +
  geom_line(aes(x=UVA.340*shift.ow.au.color$estimate,y=pred.dE, col = "AZ,UnderGlass"), data = subset(test.ow.au,Material== "U2" | Material== "UL"), linetype = "dotdash",size=1.25) +
  geom_line(aes(x=UVA.340*shift.ow.fo.color$estimate,y=pred.dE, col = "FL,Open"), data = subset(test.ow.fo,Material== "U2" | Material== "UL"), linetype = "longdash",size=1.25) +
  geom_line(aes(x=UVA.340*shift.ow.fu.color$estimate,y=pred.dE, col = "FL,UnderGlass"), data = subset(test.ow.fu,Material== "U2" | Material== "UL"), linetype = "twodash",size=1.25) +
  facet_grid(~mat, scales = "free_x") +
  xlab(bquote(~UVA["<"][360] ~ (MJ/m^2))) +
  ylab(expression(Delta*E)) +
  scale_color_manual(name="Exposure", values = cols) +
  theme(legend.text = element_text(size=15, face="bold"), legend.title = element_text(size=15, face="bold"),legend.position="bottom",axis.text=element_text(size=15,face="bold"), axis.title=element_text(size=15,face="bold"), plot.title=element_text(size=15,face="bold"), strip.text = element_text(size=15,face="bold"),axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggplot(subset(test.iw.uv,Material== "U2" | Material== "UL" ), aes(x = UVA.340, y = pred.dGloss60, col = "Wet,UV Accelerated")) + geom_line(size=1.25) + 
  geom_line(aes(x=UVA.340*shift.iw.fs.gloss$estimate,y=pred.dGloss60, col = "Wet,FS Accelerated"), data = subset(test.iw.fs,Material== "U2" | Material== "UL"), linetype = "dashed",size=1.25) + 
  geom_line(aes(x=UVA.340*shift.ow.ao.gloss$estimate,y=pred.dGloss60, col = "AZ,Open"), data = subset(test.ow.ao,Material== "U2" | Material== "UL"), linetype = "dotted",size=1.25) +
  geom_line(aes(x=UVA.340*shift.ow.au.gloss$estimate,y=pred.dGloss60, col = "AZ,UnderGlass"), data = subset(test.ow.au,Material== "U2" | Material== "UL"), linetype = "dotdash",size=1.25) +
  geom_line(aes(x=UVA.340*shift.ow.fo.gloss$estimate,y=pred.dGloss60, col = "FL,Open"), data = subset(test.ow.fo,Material== "U2" | Material== "UL"), linetype = "longdash",size=1.25) +
  geom_line(aes(x=UVA.340*shift.ow.fu.gloss$estimate,y=pred.dGloss60, col = "FL,UnderGlass"), data = subset(test.ow.fu,Material== "U2" | Material== "UL"), linetype = "twodash",size=1.25) +
  facet_grid(~mat, scales = "free_x") +
  xlab(bquote(~UVA["<"][360] ~ (MJ/m^2))) +
  ylab(expression(Delta*Gloss[60])) +
  scale_color_manual(name="Exposure", values = cols) +
  theme(legend.text = element_text(size=15, face="bold"), legend.title = element_text(size=15, face="bold"),legend.position="bottom",axis.text=element_text(size=15,face="bold"), axis.title=element_text(size=15,face="bold"), plot.title=element_text(size=15,face="bold"), strip.text = element_text(size=15,face="bold"),axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

