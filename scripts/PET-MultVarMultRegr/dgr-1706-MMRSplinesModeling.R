# Load Sample Mounting metadata

mount <- read.csv("v:/vuv-data/proj/3M/WRC/mmmOutdoorMount.csv")

# Library Packages

library(ggplot2)
library(MASS)
library(qtlmt)
library(car)
library(splines)


# Create data frames for SAMPLE MOUNTING Modeling

outdoor <- pit[
  pit$Exposure == "File" 
  | pit$Exposure == "A2axis" 
  | pit$Exposure == "A34O" 
  | pit$Exposure == "F2axis" 
  | pit$Exposure == "F26O"
  ,]

# Add UVA.340 Dose for outdoor exposures

outdoor$UVA.340 <- 0

for (i in 1:nrow(outdoor)){
  
  col <- grep(outdoor$Exposure[i], colnames(outkey))
  
  row <- grep(as.character(outdoor$Time[i]), as.character(outkey$Time))
  
  if (length(col) > 0){
    outdoor$UVA.340[i] <- outkey[row,col]
  }
  
}

#Set File Dose to 0

for (i in 1:nrow(outdoor)){
  
  if (outdoor$Exposure[i] == "File"){
    outdoor$UVA.340[i] <- 0
  }
  
}


outdoor.clear <- outdoor[outdoor$Material == "626" | outdoor$Material == "618" | outdoor$Material == "SMD",]

outdoor.white <- outdoor[outdoor$Material == "243" | outdoor$Material == "238" | outdoor$Material == "U2" | outdoor$Material == "UL" | outdoor$Material == "3M3" | outdoor$Material == "3M6",]

# Start Analysis for Clear PET

# Correct baseline data to have one set of baseline data for each exposure

add <- outdoor.clear[outdoor.clear$Exposure == "File",]

for (i in 1:nrow(outdoor.clear)){
  
  if (outdoor.clear$Exposure[i] == "File"){
    outdoor.clear$Exposure[i] <- "A2axis"
  }
  
}

add$Exposure <- "A34O"

outdoor.clear <- rbind(outdoor.clear,add)

add$Exposure <- "F2axis"

outdoor.clear <- rbind(outdoor.clear,add)

add$Exposure <- "F26O"

outdoor.clear <- rbind(outdoor.clear,add)

# Add Mount Parameters to Data Frame

outdoor.clear$Mount <- NA

for (i in 1:nrow(outdoor.clear)){
  
  row <- mount[mount$Sample == outdoor.clear$Sample[i],]
  
  rownum <- as.numeric(rownames(row))
  
  if (nrow(row) > 0){
    
    outdoor.clear$Mount[i] <- as.character(mount$Mount[rownum])
    
  }
  
}


# Add Psuedo-mount baseline data


add <- outdoor.clear[outdoor.clear$Time == 0,]

for (i in 1:nrow(outdoor.clear)){
  
  if (outdoor.clear$Time[i] == 0){
    outdoor.clear$Mount[i] <- "Open"
  }
  
}

add$Mount <- "Glass"

outdoor.clear <- rbind(outdoor.clear,add)

add$Mount <- "Al"

outdoor.clear <- rbind(outdoor.clear,add)

add$Mount <- "UnderGlass"

outdoor.clear <- rbind(outdoor.clear,add)

# Define Logistic Predictors for Mount Parameters

outdoor.clear$OnGlass <- 0
outdoor.clear$UnderGlass <- 0
outdoor.clear$OnAl <- 0

for (i in 1:nrow(outdoor.clear)){
  
  if (outdoor.clear$Mount[i] == "Glass"){
    outdoor.clear$OnGlass[i] <- 1} else
      if (outdoor.clear$Mount[i] == "UnderGlass"){
        outdoor.clear$UnderGlass[i] <- 1} else
          if (outdoor.clear$Mount[i] == "Al"){
            outdoor.clear$OnAl[i] <- 1}
}

# Reduce mount parameters to only UnderGlass and Open

for (i in 1:nrow(outdoor.clear)){
  
  if (outdoor.clear$Mount[i] == "Glass"){
    outdoor.clear$Mount[i] <- "Open"
  }
  
  if (outdoor.clear$Mount[i] == "Al"){
    outdoor.clear$Mount[i] <- "Open"
  }
  
}

# Add meta-data logistic variables

outdoor.clear$Tracking <- NA
outdoor.clearHumidity <- NA
outdoor.clear$M626 <- NA
outdoor.clear$M618 <- NA

for (i in 1:nrow(outdoor.clear)){
  
  if(outdoor.clear$Exposure[i] == "A2axis"){
    outdoor.clear$Tracking[i] <- 1} else {
      outdoor.clear$Tracking[i] <- 0
    }
  
  if(outdoor.clear$Exposure[i] == "F2axis"){
    outdoor.clear$Tracking[i] <- 1}
  
  if(outdoor.clear$Exposure[i] == "F2axis"){
    outdoor.clear$Humidity[i] <- 1} else {
      outdoor.clear$Humidity[i] <- 0
    }
  
  if(outdoor.clear$Exposure[i] == "F26O"){
    outdoor.clear$Humidity[i] <- 1}
  
  if(outdoor.clear$Material[i] == "626"){
    outdoor.clear$M626[i] <- 1} else {
      outdoor.clear$M626[i] <- 0
    }
  
  if(outdoor.clear$Material[i] == "618"){
    outdoor.clear$M618[i] <- 1} else {
      outdoor.clear$M618[i] <- 0
    }
}

# Add Location to data frame

outdoor.clear$Location <- NA

for (i in 1:nrow(outdoor.clear)){
  
  if (substr(outdoor.clear$Exposure[i],1,1) == "F"){
    outdoor.clear$Location[i] <- "Florida"
  }
  
  if (substr(outdoor.clear$Exposure[i],1,1) == "A"){
    outdoor.clear$Location[i] <- "Arizona"
  }
  
}

# Add Material deidentifier

outdoor.clear$mat <- NA

for (i in 1:nrow(outdoor.clear)){
  
  if (outdoor.clear$Material[i] == "SMD"){
    outdoor.clear$mat[i] <- "C1"
  } else if (outdoor.clear$Material[i] == "618"){
    outdoor.clear$mat[i] <- "C2"
  } else if (outdoor.clear$Material[i] == "626"){
    outdoor.clear$mat[i] <- "C3"
  }
}

# White PET

add <- outdoor.white[outdoor.white$Exposure == "File",]

for (i in 1:nrow(outdoor.white)){
  
  if (outdoor.white$Exposure[i] == "File"){
    outdoor.white$Exposure[i] <- "A2axis"
  }
  
}

add$Exposure <- "A34O"

outdoor.white <- rbind(outdoor.white,add)

add$Exposure <- "F2axis"

outdoor.white <- rbind(outdoor.white,add)

add$Exposure <- "F26O"

outdoor.white <- rbind(outdoor.white,add)

# Add Mount Parameters to Data Frame

outdoor.white$Mount <- NA

for (i in 1:nrow(outdoor.white)){
  
  row <- mount[mount$Sample == outdoor.white$Sample[i],]
  
  rownum <- as.numeric(rownames(row))
  
  if (nrow(row) > 0){
    
    outdoor.white$Mount[i] <- as.character(mount$Mount[rownum])
    
  }
  
}

# Add Psuedo-mount baseline data


add <- outdoor.white[outdoor.white$Time == 0,]

for (i in 1:nrow(outdoor.white)){
  
  if (outdoor.white$Time[i] == 0){
    outdoor.white$Mount[i] <- "Open"
  }
  
}

add$Mount <- "Glass"

outdoor.white <- rbind(outdoor.white,add)

add$Mount <- "Al"

outdoor.white <- rbind(outdoor.white,add)

add$Mount <- "UnderGlass"

outdoor.white <- rbind(outdoor.white,add)

# Define Logistic Predictors for Mount Parameters

outdoor.white$OnGlass <- 0
outdoor.white$UnderGlass <- 0
outdoor.white$OnAl <- 0

for (i in 1:nrow(outdoor.white)){
  
  if (outdoor.white$Mount[i] == "Glass"){
    outdoor.white$OnGlass[i] <- 1} else
      if (outdoor.white$Mount[i] == "UnderGlass"){
        outdoor.white$UnderGlass[i] <- 1} else
          if (outdoor.white$Mount[i] == "Al"){
            outdoor.white$OnAl[i] <- 1}
}

# Reduce mount parameters to only UnderGlass and Open

for (i in 1:nrow(outdoor.white)){
  
  if (outdoor.white$Mount[i] == "Glass"){
    outdoor.white$Mount[i] <- "Open"
  }
  
  if (outdoor.white$Mount[i] == "Al"){
    outdoor.white$Mount[i] <- "Open"
  }
  
}

# Add meta-data logistic variables

outdoor.white$Tracking <- NA
outdoor.white$Humidity <- NA
outdoor.white$M243 <- NA
outdoor.white$M238 <- NA
outdoor.white$U2 <- NA
outdoor.white$UL <- NA
outdoor.white$MMM6 <- NA

for (i in 1:nrow(outdoor.white)){
  
  if(outdoor.white$Exposure[i] == "A2axis"){
    outdoor.white$Tracking[i] <- 1} else {
      outdoor.white$Tracking[i] <- 0
    }
  
  if(outdoor.white$Exposure[i] == "F2axis"){
    outdoor.white$Tracking[i] <- 1}
  
  if(outdoor.white$Exposure[i] == "F2axis"){
    outdoor.white$Humidity[i] <- 1} else {
      outdoor.white$Humidity[i] <- 0
    }
  
  if(outdoor.white$Material[i] == "243"){
    outdoor.white$M243[i] <- 1} else {
      outdoor.white$M243[i] <- 0
    }
  
  if(outdoor.white$Material[i] == "238"){
    outdoor.white$M238[i] <- 1} else {
      outdoor.white$M238[i] <- 0
    }
  
  if(outdoor.white$Material[i] == "U2"){
    outdoor.white$U2[i] <- 1} else {
      outdoor.white$U2[i] <- 0
    }
  
  if(outdoor.white$Material[i] == "UL"){
    outdoor.white$UL[i] <- 1} else {
      outdoor.white$UL[i] <- 0
    }
  
  if(outdoor.white$Material[i] == "3M6"){
    outdoor.white$MMM6[i] <- 1} else {
      outdoor.white$MMM6[i] <- 0
    }
}

# Add Location to data frame

outdoor.white$Location <- NA

for (i in 1:nrow(outdoor.white)){
  
  if (substr(outdoor.white$Exposure[i],1,1) == "F"){
    outdoor.white$Location[i] <- "Florida"
  }
  
  if (substr(outdoor.white$Exposure[i],1,1) == "A"){
    outdoor.white$Location[i] <- "Arizona"
  }
  
}

# Add Material deidentifier

outdoor.white$mat <- NA

for (i in 1:nrow(outdoor.white)){
  
  if (outdoor.white$Material[i] == "238"){
    outdoor.white$mat[i] <- "W1"
  } else if (outdoor.white$Material[i] == "3M6"){
    outdoor.white$mat[i] <- "W2"
  } else if (outdoor.white$Material[i] == "UL"){
    outdoor.white$mat[i] <- "W3"
  } else if (outdoor.white$Material[i] == "U2"){
    outdoor.white$mat[i] <- "W4"
  } else if (outdoor.white$Material[i] == "3M3"){
    outdoor.white$mat[i] <- "W5"
  } else if (outdoor.white$Material[i] == "243"){
    outdoor.white$mat[i] <- "W6"
  }
}

# Renumber data frames

rownames(outdoor.clear) <- 1:nrow(outdoor.clear)

rownames(outdoor.white) <- 1:nrow(outdoor.white)

# Remove outliers

outdoor.clear$dE[54] <- NA
outdoor.white$dE[106] <- NA

# Forward Selection - BOTH CLEAR

null <- lm(cbind(dE,dGloss60,dHaze)~1,data = outdoor.clear)
full <- lm(cbind(dE,dGloss60,dHaze)~bs(UVA.340)+Tracking+Humidity+OnGlass+UnderGlass+M618+M626, data = outdoor.clear)
mStep(null, trace = TRUE,scope=list(upper=full), k=2, direction="forward", data=outdoor.clear)

# B-Spline Version

model.outdoor.clear <- lm(cbind(dE, dGloss60, dHaze) ~ bs(UVA.340) + bs(UVA.340):M618 + bs(UVA.340):M626 + bs(UVA.340):Humidity + bs(UVA.340):UnderGlass + bs(UVA.340):UnderGlass:Humidity - Humidity - UnderGlass - M626 - M618, data=outdoor.clear)
summary(model.outdoor.clear)

# Natural Spline Verison

vper <- c(91,92)
Q <- quantile(outdoor.clear$UVA.340,vper/100)

model.outdoor.clear <- lm(cbind(dE, dGloss60, dHaze) ~ ns(UVA.340,knot=Q) + 
                           ns(UVA.340,knot=Q):M618 +ns(UVA.340,knot=Q):M626 + 
                           ns(UVA.340,knot=Q):Humidity +
                           ns(UVA.340,knot=Q):UnderGlass + ns(UVA.340,knot=Q):UnderGlass:Humidity , data=outdoor.clear)

bk <- c(-50,300)
bkh <- c(45,150)

model.outdoor.clear <- lm(cbind(dE, dGloss60, dHaze) ~ M618 + M626 + Humidity + UnderGlass + ns(UVA.340,knot=Q, Boundary.knots = bk) + 
                            ns(UVA.340,knot=Q, Boundary.knots = bk):M618 +ns(UVA.340,knot=Q, Boundary.knots = bk):M626 + 
                            ns(UVA.340,knot=Q, Boundary.knots = bkh):Humidity +
                            ns(UVA.340,knot=Q, Boundary.knots = bk):UnderGlass + ns(UVA.340,knot=Q, Boundary.knots = bkh):UnderGlass:Humidity , data=outdoor.clear)

summary(model.outdoor.clear)


# Prediction

test <- testdf.clear

pred <- as.data.frame(predict(model.outdoor.clear, test))
test$pred.dE <- pred$dE
test$pred.dGloss60 <- pred$dGloss60
test$pred.dHaze <- pred$dHaze

# Plots

ggplot(outdoor.clear, aes(x = UVA.340, y = dE, col = Mount, shape = Mount)) + geom_point(size = 2.5) + 
  geom_line(aes(x=UVA.340,y=pred.dE), data = test[test$Mount == "Open",], size = 1.25) + 
  geom_line(aes(x=UVA.340,y=pred.dE), data = test[test$Mount == "UnderGlass",], linetype = "dashed", size = 1.25) + 
  facet_grid(mat ~ Location, scales = "free_x") +
  xlab(bquote(~UVA["<"][360] ~ (MJ/m^2))) +
  ylab(expression(Delta*E)) +
  scale_color_discrete(labels = c("Open","Cover")) +
  scale_shape_discrete(labels = c("Open","Cover")) +
  theme(legend.text = element_text(size=15, face="bold"), legend.title = element_text(size=15, face="bold"),legend.position="bottom",
        axis.text=element_text(size=15,face="bold"), axis.title=element_text(size=15,face="bold"), 
        plot.title=element_text(size=15,face="bold"), strip.text = element_text(size=15,face="bold"),axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


ggplot(outdoor.clear, aes(x = UVA.340, y = dGloss60, col = Mount, shape = Mount)) + geom_point(size = 2.5) + 
  geom_line(aes(x=UVA.340,y=pred.dGloss60), data = test[test$Mount == "Open",], size = 1.25) + 
  geom_line(aes(x=UVA.340,y=pred.dGloss60), data = test[test$Mount == "UnderGlass",], linetype = "dashed", size = 1.25) + 
  facet_grid(mat ~ Location, scales = "free_x") +
  scale_y_continuous(breaks=seq(-150,50,50))+
  xlab(bquote(~UVA["<"][360] ~ (MJ/m^2))) +
  ylab(expression(Delta*Gloss[60])) +
  scale_color_discrete(labels = c("Open","Cover")) +
  scale_shape_discrete(labels = c("Open","Cover")) +
  theme(legend.text = element_text(size=15, face="bold"), legend.title = element_text(size=15, face="bold"),legend.position="bottom",
        axis.text=element_text(size=15,face="bold"), axis.title=element_text(size=15,face="bold"), 
        plot.title=element_text(size=15,face="bold"), strip.text = element_text(size=15,face="bold"),axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggplot(outdoor.clear, aes(x = UVA.340, y = dHaze, col = Mount, shape = Mount)) + geom_point(size = 2.5) + 
  geom_line(aes(x=UVA.340,y=pred.dHaze), data = test[test$Mount == "Open",], size = 1.25) + 
  geom_line(aes(x=UVA.340,y=pred.dHaze), data = test[test$Mount == "UnderGlass",], linetype = "dashed", size = 1.25) + 
  facet_grid(mat ~ Location, scales = "free_x") +
  xlab(bquote(~UVA["<"][360] ~ (MJ/m^2))) +
  ylab(expression(Delta*Haze)) +
  scale_color_discrete(labels = c("Open","Cover")) +
  scale_shape_discrete(labels = c("Open","Cover")) +
  theme(legend.text = element_text(size=15, face="bold"), legend.title = element_text(size=15, face="bold"),legend.position="bottom",
        axis.text=element_text(size=15,face="bold"), axis.title=element_text(size=15,face="bold"), 
        plot.title=element_text(size=15,face="bold"), strip.text = element_text(size=15,face="bold"),axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

# Define materials, empty list for models, and create models for each material type

m.clear <- c("618", "626", "SMD")

models.outdoor.clear <- list()

for (i in 1:length(m.clear)){
  
  df <- subset(outdoor.clear, Material = m.clear[i])
  
  
  models.outdoor.clear[[i]] <- lm(cbind(dE, dGloss60, dHaze) ~ bs(UVA.340) + bs(UVA.340)*Humidity + bs(UVA.340)*UnderGlass - Humidity - UnderGlass, data=df)
  

}

# Forward Selection - White PET
null <- lm(cbind(dE,dGloss60)~1,data = outdoor.white)
full <- lm(cbind(dE,dGloss60)~bs(UVA.340)+Tracking+Humidity+OnGlass+UnderGlass+M238+M243+MMM6+UL+U2, data = outdoor.white)
mStep(null, trace = TRUE,scope=list(upper=full), k=2, direction="forward", data=outdoor.white)

# B-Spline Version

model.outdoor.white <- lm(cbind(dE, dGloss60) ~ bs(UVA.340) + bs(UVA.340, knots = c(90)):UL + bs(UVA.340, knots = c(90)):U2 + bs(UVA.340):M243 + bs(UVA.340):M238 + bs(UVA.340):MMM6 + bs(UVA.340):Humidity, data=outdoor.white)
summary(model.outdoor.white)


# Natural Spline Version

vper <- c(91,94)
knot_Q <- quantile(outdoor.white$UVA.340,vper/100)

model.outdoor.white <- lm(cbind(dE, dGloss60) ~ M238 + MMM6 + UL + U2 + M243 + Humidity + 
                            ns(UVA.340,knot=knot_Q) + ns(UVA.340,knot=knot_Q):M243 +
                           ns(UVA.340,knot=knot_Q):M238 + ns(UVA.340,knot=knot_Q):U2 + ns(UVA.340,knot=knot_Q):UL +
                           ns(UVA.340,knot=knot_Q):MMM6 + ns(UVA.340,knot=knot_Q):Humidity , data=outdoor.white)
summary(model.outdoor.white)


# Prediction

test <- testdf.white


pred <- as.data.frame(predict(model.outdoor.white,test))
test$pred.dE <- pred$dE
test$pred.dGloss60 <- pred$dGloss60

# Plots

ggplot(outdoor.white, aes(x = UVA.340, y = dE, col = Mount, shape = Mount)) + geom_point(size = 2.5) + 
  geom_line(aes(x=UVA.340,y=pred.dE), data = test[test$Mount == "Open",], size = 1.25) + 
  geom_line(aes(x=UVA.340,y=pred.dE), data = test[test$Mount == "UnderGlass",], linetype = "dashed", size = 1.25) +  
  facet_grid(mat ~ Location, scales = "free_x") +
  scale_y_continuous(breaks=seq(0,10,5))+
  xlab(bquote(~UVA["<"][360] ~ (MJ/m^2))) +
  ylab(expression(Delta*E)) +
  scale_color_discrete(labels = c("Open","Cover")) +
  scale_shape_discrete(labels = c("Open","Cover")) +
  theme(legend.text = element_text(size=15, face="bold"), legend.title = element_text(size=15, face="bold"),legend.position="bottom",
        axis.text=element_text(size=15,face="bold"), axis.title=element_text(size=15,face="bold"), 
        plot.title=element_text(size=15,face="bold"), strip.text = element_text(size=15,face="bold"),axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


ggplot(outdoor.white, aes(x = UVA.340, y = dGloss60, col = Mount, shape = Mount)) + geom_point(size = 2.5) + 
  geom_line(aes(x=UVA.340,y=pred.dGloss60), data = test[test$Mount == "Open",], size = 1.25) + 
  geom_line(aes(x=UVA.340,y=pred.dGloss60), data = test[test$Mount == "UnderGlass",], linetype = "dashed", size = 1.25) + 
  facet_grid(mat ~ Location, scales = "free_x") +
  scale_y_continuous(breaks=seq(-150,50,50))+
  xlab(bquote(~UVA["<"][360] ~ (MJ/m^2))) +
  ylab(expression(Delta*Gloss[60])) +
  scale_color_discrete(labels = c("Open","Cover")) +
  scale_shape_discrete(labels = c("Open","Cover")) +
  theme(legend.text = element_text(size=15, face="bold"), legend.title = element_text(size=15, face="bold"),legend.position="bottom",
        axis.text=element_text(size=15,face="bold"), axis.title=element_text(size=15,face="bold"), 
        plot.title=element_text(size=15,face="bold"), strip.text = element_text(size=15,face="bold"),axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

# Create data frames for UVA.340 Dose Modeling

indoor <- pit[
  pit$Exposure == "File" 
  | pit$Exposure == "UV20" 
  | pit$Exposure == "G154c4"
  | pit$Exposure == "UV22"
  | pit$Exposure == "FS40"
  | pit$Exposure == "FS42"
  | pit$Exposure == "FS10"
  | pit$Exposure == "FS80_90BPT"
  | pit$Exposure == "FS40_70ChT" 
  | pit$Exposure == "FS30_80ChT"
  ,]

rownames(indoor) <- 1:nrow(indoor)


# Add Exposure parameteres for indoor exposures

indoor$UVA.340 <- NA
indoor$Chamber.Temp <- NA
indoor$Relative.Humidity <- NA
indoor$Wet.Time <- NA


for (i in 1:nrow(indoor)){
  
  row <- grep(indoor$Exposure[i], inkey$Exposure)
  
  if (length(row) > 0){
    indoor$UVA.340[i] <- inkey$UVA.340.perhr[row] * indoor$Time[i]
    indoor$Chamber.Temp[i] <- inkey$Chamber.Temp[row]
    indoor$Relative.Humidity[i] <- inkey$Relative.Humidity[row]
    indoor$Wet.Time[i] <- inkey$Wet.Time.perh[row] * indoor$Time[i]
  }
  
}

# Subset dataframes for clear vs. white PET

indoor.clear <- indoor[indoor$Material == "626" | indoor$Material == "618" | indoor$Material == "SMD",]

indoor.white <- indoor[indoor$Material == "243" | indoor$Material == "238" | indoor$Material == "U2" | indoor$Material == "UL" | indoor$Material == "3M3" | indoor$Material == "3M6",]

# Start Analysis for Clear PET

# Correct baseline data to have one set of baseline data for each exposure

add <- indoor.clear[indoor.clear$Exposure == "File",]

for (i in 1:nrow(indoor.clear)){
  
  if (indoor.clear$Exposure[i] == "File"){
    indoor.clear$Exposure[i] <- "UV20"
  }
  
}

add$Exposure <- "G154c4"

indoor.clear <- rbind(indoor.clear,add)

add$Exposure <- "UV22"

indoor.clear <- rbind(indoor.clear,add)

add$Exposure <- "FS40"

indoor.clear <- rbind(indoor.clear,add)

add$Exposure <- "FS42"

indoor.clear <- rbind(indoor.clear,add)

add$Exposure <- "FS10"

indoor.clear <- rbind(indoor.clear,add)

add$Exposure <- "FS80_90BPT"

indoor.clear <- rbind(indoor.clear,add)

add$Exposure <- "FS40_70ChT"

indoor.clear <- rbind(indoor.clear,add)

add$Exposure <- "FS30_80ChT"

indoor.clear <- rbind(indoor.clear,add)

# Correct chamber T and relative humidity for converted file exposures

for (i in 1:nrow(indoor.clear)){
  
  row <- grep(indoor.clear$Exposure[i], inkey$Exposure)
  
  if (length(row) > 0){
    indoor.clear$Chamber.Temp[i] <- inkey$Chamber.Temp[row]
    indoor.clear$Relative.Humidity[i] <- inkey$Relative.Humidity[row]
  }
  
}

# Add meta-data logistic variables

indoor.clear$M626 <- NA
indoor.clear$M618 <- NA

for (i in 1:nrow(indoor.clear)){
  
  if(indoor.clear$Material[i] == "626"){
    indoor.clear$M626[i] <- 1} else {
      indoor.clear$M626[i] <- 0
    }
  
  if(indoor.clear$Material[i] == "618"){
    indoor.clear$M618[i] <- 1} else {
      indoor.clear$M618[i] <- 0
    }
}

# Add Exposure Type to data frame

indoor.clear$Exposure.Type <- NA

for (i in 1:nrow(indoor.clear)){
  
  if (indoor.clear$Exposure[i] == "UV22"){
    txt1 <- "Wet -"
  } else if (indoor.clear$Exposure[i] == "G154c4"){
    txt1 <- "Wet -"
  } else if (indoor.clear$Exposure[i] == "FS42") {
    txt1 <- "Wet -"
  } else 
    txt1 <- "Dry -"
  
  if (substr(indoor.clear$Exposure[i],1,1) == "F"){
    txt2 <- "FS"
  }
  
  if (substr(indoor.clear$Exposure[i],1,1) == "U"){
    txt2 <- "UV"
  }
  
  if (substr(indoor.clear$Exposure[i],1,1) == "G"){
    txt2 <- "UV"
  }
  
  indoor.clear$Exposure.Type[i] <- paste(txt1,txt2)
  
}

# Add Moisture logistic variable to indoor data

indoor.clear$Moisture <- NA

for (i in 1:nrow(indoor.clear)){
  
  if (substr(indoor.clear$Exposure.Type[i],1,3) == "Dry"){
    indoor.clear$Moisture[i] <- 0
  } else if (substr(indoor.clear$Exposure.Type[i],1,3) == "Wet"){
    indoor.clear$Moisture[i] <- 1
  }
}

# Add Material deidentifier

indoor.clear$mat <- NA

for (i in 1:nrow(indoor.clear)){
  
  if (indoor.clear$Material[i] == "SMD"){
    indoor.clear$mat[i] <- "C1"
  } else if (indoor.clear$Material[i] == "618"){
    indoor.clear$mat[i] <- "C2"
  } else if (indoor.clear$Material[i] == "626"){
    indoor.clear$mat[i] <- "C3"
  }
}


# Start Analysis for White PET

# Correct baseline data to have one set of baseline data for each exposure

add <- indoor.white[indoor.white$Exposure == "File",]

for (i in 1:nrow(indoor.white)){
  
  if (indoor.white$Exposure[i] == "File"){
    indoor.white$Exposure[i] <- "UV20"
  }
  
}

add$Exposure <- "G154c4"

indoor.white <- rbind(indoor.white,add)

add$Exposure <- "UV22"

indoor.white <- rbind(indoor.white,add)

add$Exposure <- "FS40"

indoor.white <- rbind(indoor.white,add)

add$Exposure <- "FS42"

indoor.white <- rbind(indoor.white,add)

add$Exposure <- "FS10"

indoor.white <- rbind(indoor.white,add)

add$Exposure <- "FS80_90BPT"

indoor.white <- rbind(indoor.white,add)

add$Exposure <- "FS40_70ChT"

indoor.white <- rbind(indoor.white,add)

add$Exposure <- "FS30_80ChT"

indoor.white <- rbind(indoor.white,add)

# Correct chamber T and relative humidity for converted file exposures

for (i in 1:nrow(indoor.white)){
  
  row <- grep(indoor.white$Exposure[i], inkey$Exposure)
  
  if (length(row) > 0){
    indoor.white$Chamber.Temp[i] <- inkey$Chamber.Temp[row]
    indoor.white$Relative.Humidity[i] <- inkey$Relative.Humidity[row]
  }
  
}

# Add meta-data logistic variables

indoor.white$M243 <- NA
indoor.white$M238 <- NA
indoor.white$U2 <- NA
indoor.white$UL <- NA
indoor.white$MMM6 <- NA

for (i in 1:nrow(indoor.white)){
  
  if(indoor.white$Material[i] == "243"){
    indoor.white$M243[i] <- 1} else {
      indoor.white$M243[i] <- 0
    }
  
  if(indoor.white$Material[i] == "238"){
    indoor.white$M238[i] <- 1} else {
      indoor.white$M238[i] <- 0
    }
  
  if(indoor.white$Material[i] == "U2"){
    indoor.white$U2[i] <- 1} else {
      indoor.white$U2[i] <- 0
    }
  
  if(indoor.white$Material[i] == "UL"){
    indoor.white$UL[i] <- 1} else {
      indoor.white$UL[i] <- 0
    }
  
  if(indoor.white$Material[i] == "3M6"){
    indoor.white$MMM6[i] <- 1} else {
      indoor.white$MMM6[i] <- 0
    }
}

# Add Exposure Type to data frame

indoor.white$Exposure.Type <- NA

for (i in 1:nrow(indoor.white)){
  
  if (indoor.white$Exposure[i] == "UV22"){
    txt1 <- "Wet -"
  } else if (indoor.white$Exposure[i] == "G154c4"){
    txt1 <- "Wet -"
  } else if (indoor.white$Exposure[i] == "FS42") {
    txt1 <- "Wet -"
  } else 
    txt1 <- "Dry -"
  
  if (substr(indoor.white$Exposure[i],1,1) == "F"){
    txt2 <- "FS"
  }
  
  if (substr(indoor.white$Exposure[i],1,1) == "U"){
    txt2 <- "UV"
  }
  
  if (substr(indoor.white$Exposure[i],1,1) == "G"){
    txt2 <- "UV"
  }
  
  indoor.white$Exposure.Type[i] <- paste(txt1,txt2)
  
}

# Add Moisture logistic variable to indoor data

indoor.white$Moisture <- NA

for (i in 1:nrow(indoor.white)){
  
  if (substr(indoor.white$Exposure.Type[i],1,3) == "Dry"){
    indoor.white$Moisture[i] <- 0
  } else if (substr(indoor.white$Exposure.Type[i],1,3) == "Wet"){
    indoor.white$Moisture[i] <- 1
  }
}

# Add material deidentifier to data

indoor.white$mat <- NA

for (i in 1:nrow(indoor.white)){
  
  if (indoor.white$Material[i] == "238"){
    indoor.white$mat[i] <- "W1"
  } else if (indoor.white$Material[i] == "3M6"){
    indoor.white$mat[i] <- "W2"
  } else if (indoor.white$Material[i] == "UL"){
    indoor.white$mat[i] <- "W3"
  } else if (indoor.white$Material[i] == "U2"){
    indoor.white$mat[i] <- "W4"
  } else if (indoor.white$Material[i] == "3M3"){
    indoor.white$mat[i] <- "W5"
  } else if (indoor.white$Material[i] == "243"){
    indoor.white$mat[i] <- "W6"
  }
}

# Renumber data frames

rownames(indoor.clear) <- 1:nrow(indoor.clear)

rownames(indoor.white) <- 1:nrow(indoor.white)

# Remove outliers

indoor.clear$dHaze[158] <- NA
indoor.clear$dHaze[148] <- NA

indoor.white$dGloss60[186] <- NA
indoor.white$dGloss60[246] <- NA

# Forward Selection - CLEAR

null <- lm(cbind(dE,dGloss60,dHaze)~1,data = indoor.clear)
full <- lm(cbind(dE,dGloss60,dHaze)~bs(UVA.340)+Wet.Time+Chamber.Temp+M618+M626, data = indoor.clear)
mStep(null, trace = TRUE,scope=list(upper=full), k=2, direction="forward", data=indoor.clear)

# MMR with Wet Time

model.indoor.clear <- lm(cbind(dE, dGloss60, dHaze) ~ bs(UVA.340) + bs(UVA.340):M618 + bs(UVA.340):M626 + bs(UVA.340):Wet.Time + bs(UVA.340):Chamber.Temp - M626 - M618, data=indoor.clear)
summary(model.indoor.clear)

#MMR with Moisture (0,1 logistic variable)

model.indoor.clear <- lm(cbind(dE, dGloss60, dHaze) ~ bs(UVA.340) + bs(UVA.340):M618 + bs(UVA.340):M626 + bs(UVA.340):Moisture + bs(UVA.340):Chamber.Temp - M626 - M618, data=indoor.clear)
summary(model.indoor.clear)

# Natural Spline Verison

vper <- c(88,91)
Q <- quantile(indoor.clear$UVA.340,vper/100)

model.indoor.clear <- lm(cbind(dE, dGloss60, dHaze) ~ M618 + M626 + Moisture + Chamber.Temp + ns(UVA.340,knot=Q) + 
                           ns(UVA.340,knot=Q):M618 +ns(UVA.340,knot=Q):M626 + 
                           ns(UVA.340,knot=Q):Moisture +
                           ns(UVA.340,knot=Q):Chamber.Temp , data=indoor.clear)
summary(model.indoor.clear)

bk <- c(50,175)

model.indoor.clear <- lm(cbind(dE, dGloss60, dHaze) ~ M618 + M626 + Moisture + Chamber.Temp + ns(UVA.340,knot=Q, Boundary.knots = bk) + 
                           ns(UVA.340,knot=Q,Boundary.knots = bk):M618 +ns(UVA.340,knot=Q,Boundary.knots = bk):M626 + 
                           ns(UVA.340,knot=Q,Boundary.knots = bk):Moisture +
                           ns(UVA.340,knot=Q,Boundary.knots = bk):Chamber.Temp , data=indoor.clear)
summary(model.indoor.clear)




# Prediction
test <- indoor.testdf.clear

pred <- as.data.frame(predict(model.indoor.clear, test))
test$pred.dE <- pred$dE
test$pred.dGloss60 <- pred$dGloss60
test$pred.dHaze <- pred$dHaze

# Plots

ggplot(indoor.clear, aes(x = UVA.340, y = dE, col = as.factor(Chamber.Temp), shape = as.factor(Chamber.Temp))) + geom_point(size = 2.5) + 
  geom_line(aes(x=UVA.340,y=pred.dE, group = Exposure), data = test[test$Chamber.Temp == 70,], size = 1.25) + 
  geom_line(aes(x=UVA.340,y=pred.dE), data = test[test$Chamber.Temp == 80,], linetype = "dashed",  size = 1.25) +
  geom_line(aes(x=UVA.340,y=pred.dE), data = test[test$Chamber.Temp == 47,], linetype = "dotdash",  size = 1.25) +
  facet_grid(mat ~ Exposure.Type) +
  xlab(bquote(~UVA["<"][360] ~ (MJ/m^2))) +
  ylab(expression(Delta*E)) +
  scale_y_continuous(breaks=seq(0,20,10))+
  scale_color_discrete(name = expression("Chamber Temperature" ~ "["~degree~C ~"]")) +
  scale_shape_discrete(name=expression("Chamber Temperature" ~ "["~degree~C ~"]")) +
  theme(legend.text = element_text(size=15, face="bold"), legend.title = element_text(size=15, face="bold"),legend.position="bottom",
        axis.text=element_text(size=15,face="bold"), axis.title=element_text(size=15,face="bold"), 
        plot.title=element_text(size=15,face="bold"), strip.text = element_text(size=15,face="bold"),axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggplot(indoor.clear, aes(x = UVA.340, y = dGloss60, col = as.factor(Chamber.Temp), shape = as.factor(Chamber.Temp))) + geom_point(size = 2.5) + 
  geom_line(aes(x=UVA.340,y=pred.dGloss60, group = Exposure), data = test[test$Chamber.Temp == 70,], size = 1.25) + 
  geom_line(aes(x=UVA.340,y=pred.dGloss60), data = test[test$Chamber.Temp == 80,], linetype = "dashed",  size = 1.25) +
  geom_line(aes(x=UVA.340,y=pred.dGloss60), data = test[test$Chamber.Temp == 47,], linetype = "dotdash",  size = 1.25) +
  facet_grid(mat ~ Exposure.Type) +
  scale_y_continuous(breaks=seq(-150,50,50))+
  xlab(bquote(~UVA["<"][360] ~ (MJ/m^2))) +
  ylab(expression(Delta*Gloss[60])) +
  scale_color_discrete(name = expression("Chamber Temperature" ~ "["~degree~C ~"]")) +
  scale_shape_discrete(name=expression("Chamber Temperature" ~ "["~degree~C ~"]")) +
  theme(legend.text = element_text(size=15, face="bold"), legend.title = element_text(size=15, face="bold"),legend.position="bottom",
        axis.text=element_text(size=15,face="bold"), axis.title=element_text(size=15,face="bold"), 
        plot.title=element_text(size=15,face="bold"), strip.text = element_text(size=15,face="bold"),axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggplot(indoor.clear, aes(x = UVA.340, y = dHaze, col = as.factor(Chamber.Temp), shape = as.factor(Chamber.Temp))) + geom_point(size = 2.5) + 
  geom_line(aes(x=UVA.340,y=pred.dHaze, group = Exposure), data = test[test$Chamber.Temp == 70,], size = 1.25) + 
  geom_line(aes(x=UVA.340,y=pred.dHaze), data = test[test$Chamber.Temp == 80,], linetype = "dashed",  size = 1.25) +
  geom_line(aes(x=UVA.340,y=pred.dHaze), data = test[test$Chamber.Temp == 47,], linetype = "dotdash",  size = 1.25) +
  facet_grid(mat ~ Exposure.Type) +
  xlab(bquote(~UVA["<"][360] ~ (MJ/m^2))) +
  ylab(expression(Delta*Haze)) +
  scale_color_discrete(name = expression("Chamber Temperature" ~ "["~degree~C ~"]")) +
  scale_shape_discrete(name=expression("Chamber Temperature" ~ "["~degree~C ~"]")) +
  theme(legend.text = element_text(size=15, face="bold"), legend.title = element_text(size=15, face="bold"),legend.position="bottom",
        axis.text=element_text(size=15,face="bold"), axis.title=element_text(size=15,face="bold"), 
        plot.title=element_text(size=15,face="bold"), strip.text = element_text(size=15,face="bold"),axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


# Forward Selection - White PET
null <- lm(cbind(dE,dGloss60)~1,data = indoor.white)
full <- lm(cbind(dE,dGloss60)~bs(UVA.340)+Wet.Time+Chamber.Temp+M238+M243+MMM6+UL+U2, data = indoor.white)
mStep(null, trace = TRUE,scope=list(upper=full), k=2, direction="forward", data=indoor.white)

# model with Wet.Time

model.indoor.white <- lm(cbind(dE, dGloss60) ~ bs(UVA.340) + bs(UVA.340, knots = c(90)):UL + bs(UVA.340, knots = c(90)):U2 + bs(UVA.340):M243 + bs(UVA.340):M238 + bs(UVA.340):MMM6 + bs(UVA.340):Wet.Time + bs(UVA.340):Chamber.Temp, data=indoor.white)
summary(model.indoor.white)

# Model with logistic (0,1) moisture 

model.indoor.white <- lm(cbind(dE, dGloss60) ~ bs(UVA.340) + bs(UVA.340, knots = c(90)):UL + bs(UVA.340, knots = c(90)):U2 + bs(UVA.340):M243 + bs(UVA.340):M238 + bs(UVA.340):MMM6 + bs(UVA.340):Moisture + bs(UVA.340):Chamber.Temp, data=indoor.white)
summary(model.indoor.white)

# Natural Spline Version

vper <- c(65,90)
knot_Q <- quantile(indoor.white$UVA.340,vper/100)

model.indoor.white <- lm(cbind(dE, dGloss60) ~ M238 + MMM6 + UL + U2 + M243 + Moisture+ Chamber.Temp + ns(UVA.340,knot=knot_Q) +       ns(UVA.340,knot=knot_Q):M243 +
                           ns(UVA.340,knot=knot_Q):M238 + ns(UVA.340,knot=knot_Q):U2 + ns(UVA.340,knot=knot_Q):UL +
                           ns(UVA.340,knot=knot_Q):MMM6 + ns(UVA.340,knot=knot_Q):Moisture +
                           ns(UVA.340,knot=knot_Q):Chamber.Temp , data=indoor.white)
summary(model.indoor.white)



# Prediction
test <- indoor.testdf.white

pred <- as.data.frame(predict(model.indoor.white, test))
test$pred.dE <- pred$dE
test$pred.dGloss60 <- pred$dGloss60

ggplot(indoor.white, aes(x = UVA.340, y = dE, col = as.factor(Chamber.Temp), shape = as.factor(Chamber.Temp))) + geom_point(size = 2.5) + 
  geom_line(aes(x=UVA.340,y=pred.dE, group = Exposure), data = test[test$Chamber.Temp == 70,], size = 1.25) + 
  geom_line(aes(x=UVA.340,y=pred.dE), data = test[test$Chamber.Temp == 80,], linetype = "dashed",  size = 1.25) +
  geom_line(aes(x=UVA.340,y=pred.dE), data = test[test$Chamber.Temp == 47,], linetype = "dotdash",  size = 1.25) +
  facet_grid(mat ~ Exposure.Type) +
  scale_y_continuous(breaks=seq(0,40,20))+
  xlab(bquote(~UVA["<"][360] ~ (MJ/m^2))) +
  ylab(expression(Delta*E)) +
  scale_color_discrete(name = expression("Chamber Temperature" ~ "["~degree~C ~"]")) +
  scale_shape_discrete(name=expression("Chamber Temperature" ~ "["~degree~C ~"]")) +
  theme(legend.text = element_text(size=15, face="bold"), legend.title = element_text(size=15, face="bold"),legend.position="bottom",
        axis.text=element_text(size=15,face="bold"), axis.title=element_text(size=15,face="bold"), 
        plot.title=element_text(size=15,face="bold"), strip.text = element_text(size=15,face="bold"),axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggplot(indoor.white, aes(x = UVA.340, y = dGloss60, col = as.factor(Chamber.Temp), shape = as.factor(Chamber.Temp))) + geom_point(size = 2.5) + 
  geom_line(aes(x=UVA.340,y=pred.dGloss60, group = Exposure), data = test[test$Chamber.Temp == 70,], size = 1.25) + 
  geom_line(aes(x=UVA.340,y=pred.dGloss60), data = test[test$Chamber.Temp == 80,], linetype = "dashed",  size = 1.25) +
  geom_line(aes(x=UVA.340,y=pred.dGloss60), data = test[test$Chamber.Temp == 47,], linetype = "dotdash",  size = 1.25) +
  facet_grid(mat ~ Exposure.Type) +
  scale_y_continuous(breaks=seq(-150,50,50))+
  xlab(bquote(~UVA["<"][360] ~ (MJ/m^2))) +
  ylab(expression(Delta*Gloss[60])) +
  scale_color_discrete(name = expression("Chamber Temperature" ~ "["~degree~C ~"]")) +
  scale_shape_discrete(name=exfpression("Chamber Temperature" ~ "["~degree~C ~"]")) +
  theme(legend.text = element_text(size=15, face="bold"), legend.title = element_text(size=15, face="bold"),legend.position="bottom",
        axis.text=element_text(size=15,face="bold"), axis.title=element_text(size=15,face="bold"), 
        plot.title=element_text(size=15,face="bold"), strip.text = element_text(size=15,face="bold"),axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

 


###############################################################################################################

#DIAGONISTIC PLOTS

# Make Residuals Plots

plot(model.outdoor.clear$fitted.values[,1],model.outdoor.clear$residuals[,1], main = "dE Residuals Plot - Outdoor Model for Clear PET", xlab = "Fitted Values", ylab = "Residuals - dE")
abline(0, 0)
plot(model.outdoor.clear$fitted.values[,2],model.outdoor.clear$residuals[,2], main = "dGloss60 Residuals Plot - Outdoor Model for Clear PET", xlab = "Fitted Values", ylab = "Residuals - dGloss60")
abline(0, 0)
plot(model.outdoor.clear$fitted.values[,3],model.outdoor.clear$residuals[,3], main = "dHaze Residuals Plot - Outdoor Model for Clear PET", xlab = "Fitted Values", ylab = "Residuals - dHaze")
abline(0, 0)

plot(model.outdoor.white$fitted.values[,1],model.outdoor.white$residuals[,1], main = "dE Residuals Plot - Outdoor Model for White PET", xlab = "Fitted Values", ylab = "Residuals - dE")
abline(0, 0)
plot(model.outdoor.white$fitted.values[,2],model.outdoor.white$residuals[,2], main = "dGloss60 Residuals Plot - Outdoor Model for White PET", xlab = "Fitted Values", ylab = "Residuals - dGloss60")
abline(0, 0)

plot(model.indoor.clear$fitted.values[,1],model.indoor.clear$residuals[,1], main = "dE Residuals Plot - Indoor Model for Clear PET", xlab = "Fitted Values", ylab = "Residuals - dE")
abline(0, 0)
plot(model.indoor.clear$fitted.values[,2],model.indoor.clear$residuals[,2], main = "dGloss60 Residuals Plot - Indoor Model for Clear PET", xlab = "Fitted Values", ylab = "Residuals - dGloss60")
abline(0, 0)
plot(model.indoor.clear$fitted.values[,3],model.indoor.clear$residuals[,3], main = "dHaze Residuals Plot - Indoor Model for Clear PET", xlab = "Fitted Values", ylab = "Residuals - dHaze")
abline(0, 0)

plot(model.indoor.white$fitted.values[,1],model.indoor.white$residuals[,1], main = "dE Residuals Plot - Indoor Model for White PET", xlab = "Fitted Values", ylab = "Residuals - dE")
abline(0, 0)
plot(model.indoor.white$fitted.values[,2],model.indoor.white$residuals[,2], main = "dGloss60 Residuals Plot - Indoor Model for White PET", xlab = "Fitted Values", ylab = "Residuals - dGloss60")
abline(0, 0)

# qq Plots

qqnorm(model.outdoor.clear$residuals[,1], main = "Normal Q-Q Plot: dE - Clear PET, Outdoor Exposure")
qqline(model.outdoor.clear$residuals[,1])
qqnorm(model.outdoor.clear$residuals[,2], main = "Normal Q-Q Plot: dGloss60 - Clear PET, Outdoor Exposure")
qqline(model.outdoor.clear$residuals[,2])
qqnorm(model.outdoor.clear$residuals[,3], main = "Normal Q-Q Plot: dHaze - Clear PET, Outdoor Exposure")
qqline(model.outdoor.clear$residuals[,3])

qqnorm(model.outdoor.white$residuals[,1], main = "Normal Q-Q Plot: dE - White PET, Outdoor Exposure")
qqline(model.outdoor.white$residuals[,1])
qqnorm(model.outdoor.white$residuals[,2], main = "Normal Q-Q Plot: dGloss60 - White PET, Outdoor Exposure")
qqline(model.outdoor.white$residuals[,2])

qqnorm(model.indoor.clear$residuals[,1], main = "Normal Q-Q Plot: dE - Clear PET, Indoor Exposure")
qqline(model.indoor.clear$residuals[,1])
qqnorm(model.indoor.clear$residuals[,2], main = "Normal Q-Q Plot: dGloss60 - Clear PET, Indoor Exposure")
qqline(model.indoor.clear$residuals[,2])
qqnorm(model.indoor.clear$residuals[,3], main = "Normal Q-Q Plot: dHaze - Clear PET, Indoor Exposure")
qqline(model.indoor.clear$residuals[,3])

qqnorm(model.indoor.white$residuals[,1], main = "Normal Q-Q Plot: dE - White PET, Indoor Exposure")
qqline(model.indoor.white$residuals[,1])
qqnorm(model.indoor.white$residuals[,2], main = "Normal Q-Q Plot: dGloss60 - White PET, Indoor Exposure")
qqline(model.indoor.white$residuals[,2])






