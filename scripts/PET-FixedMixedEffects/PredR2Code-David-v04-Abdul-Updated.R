############################################
## Code by David K.Ngendahimana,Sept 2015 ##
############################################

### Libraries###
library(lme4)
library(nlme)
library(car)
library(psych)
library(ggplot2)
library(grid)
library(arm)
library(RLRsim)
library(bootstrap)
library(plyr)
### Functions###

#Function to combine (concatenate) files in a directory
load_data <- function(path) { 
  files <- dir(path, pattern = '\\.csv', full.names = TRUE)
  tables <- lapply(files, read.csv)
  do.call(rbind, tables)
}

### Data Setup##
setwd("V:\\vuv-data\\proj\\PETPilot\\Papers\\YI-Haze-StatModeling\\15-yi-haze-statmodeling\\analysis")

dt <- read.csv("DataFrame-v14-singles-DKN.csv")
dt=dt[,c(1:14)]
head(dt)
names(dt)
names(dt)[5]="Exp"

###############################################################################################
###################################### YI #####################################################
###############################################################################################

################################### MODEL 2 ###################################################

# Yellowing under CyclicQUV AND HotQUV

YIdtQUV=subset(dt,Sample!="sa19603.14" & Exp!="Baseline" & Exp!="DampHeat" & Exp!="FreezeThaw"& Rowkey !="sa19601.04-step5" ) 

for( i in 1:222){
  fitYI01=lm(YI^(0.5)~Step*Material*Exp+I(Step^2)*Material+I(Step^3)*Material,data=YIdtQUV[-i,])
  YIdtQUV$pred[i]=predict(fitYI01,YIdtQUV[i,])**2 
}

YIdtQUV$resid = YIdtQUV$YI - YIdtQUV$pred
YIdtQUVk = subset(YIdtQUV)

# Predicted Squared Error
PSE=mean((YIdtQUVk$YI-YIdtQUVk$pred)**2)   
# or 
# PSE=mean((YIdtQUVk$YI-YIdtQUV$pred)**2)

# Set working directory to store individual sample csv files
setwd("V:\\vuv-data\\proj\\PETPilot\\Papers\\YI-Haze-StatModeling\\15-yi-haze-statmodeling\\analysis\\demo")

# Subset for CyclicQUV
YcyQUV=subset(YIdtQUVk,Exp=="CyclicQUV")

for(i in c(1,3,4)){ 
  k=subset(YcyQUV,substring(Rowkey,7,7)==i)
  for(j in 2:8){
    write.csv(subset(k, substring(Sample,10,10)==j ),paste0("sa1960",i,".2",j,".C.csv"))
  }
}

# Subset for HotQUV
YIhotQUV=subset(YIdtQUVk,Exp=="HotQUV")

for(i in c(1,3,4)){ 
  k=subset(YIhotQUV,substring(Rowkey,7,7)==i)
  for(j in 15:21){
    write.csv(subset(k, substring(Sample,9)==j ),paste0("sa1960",i,".",j,".H.csv"))
  }
}

# Store all samples file names into fileNames
fileNames <- Sys.glob("*.csv")

# I don't know what this does?
# MovAvg=NULL

# Compute deviance from Mean for all files 
for (fileName in fileNames) {
  
  sample <- read.csv(fileName,header = TRUE,sep = ",")
  
  # MovAvg[fileName]=mean(sample$YI)
  sample$dev <- (sample$YI - mean(sample$YI))**2
  
  #overwriting results on original file
  write.table(sample, fileName,append = FALSE, quote = FALSE, sep = ",", row.names = FALSE, col.names = TRUE)
  
}

# read all extracted individual csv files 
YImod2<- load_data("V:\\vuv-data\\proj\\PETPilot\\Papers\\YI-Haze-StatModeling\\15-yi-haze-statmodeling\\analysis\\demo") 

# Calculate predicted R-squared
Rsquared=1-(PSE/mean(YImod2$dev))  ### got 0.9498
print(Rsquared)



################################### MODEL 4 ###################################################

# Yellowing under DampHeat and FreezeThaw

YIdtDF=subset(dt,Sample!="sa19603.14" & Exp!="Baseline" & Exp!="HotQUV" & Exp!="CyclicQUV"& Rowkey !="sa19601.04-step5")


### the fitYI04 below is not the same with the original model -I(Step^3) added at the end...
for( i in 1:214){  ## There are 214 observations but only taken 202 here. Changed to 214...
# fitYI04=lm(YI~Step*Material+Step*Exp+I(Step^2)*Material+I(Step^2)*Exp+I(Step^3)*Material+I(Step^3)*Exp+Exp*Material-I(Step^3),data=YIdtDF[-i,])    
  fitYI04=lm(YI~Step*Material+Step*Exp+I(Step^2)*Material+I(Step^2)*Exp+I(Step^3)*Material+I(Step^3)*Exp+Exp*Material,data=YIdtDF[-i,])    
  YIdtDF$pred[i]=predict(fitYI04,YIdtDF[i,]) 
}

YIdtDF$resid = YIdtDF$YI - YIdtDF$pred
YIdtDFk=subset(YIdtDF,resid < 0.45) #Remove two data points with very poor predictive value

# Predicted Squared Error
PSE=mean((YIdtDFk$YI-YIdtDFk$pred)**2)

# Set working directory to store individual sample csv files
setwd("V:\\vuv-data\\proj\\PETPilot\\Papers\\YI-Haze-StatModeling\\15-yi-haze-statmodeling\\analysis\\demo")

# Subset for DampHeat
YIdtDF01=subset(YIdtDFk,Exp=="DampHeat")

for(i in c(1,3,4)){ 
  k=subset(YIdtDF01,substring(Rowkey,7,7)==i)
  for(j in 1:7){
    write.csv(subset(k, as.numeric(substring(Sample,9,10))==j), paste0("sa1960",i,".",j,".D.csv"))
  }
}

# Subset for FreezeThaw
YIdtDF02=subset(YIdtDFk,Exp=="FreezeThaw")

for(i in c(1,3,4)){ 
  k=subset(YIdtDF02,substring(Rowkey,7,7)==i)
  for(j in 8:14){
    write.csv(subset(k, as.numeric(substring(Sample,9,10))==j), paste0("sa1960",i,".",j,".F.csv"))
  }
}

# Store all samples file names into fileNames
fileNames <- Sys.glob("*.csv")

# I don't know what this does?
# MovAvg=NULL

# Compute deviance from Mean for all files 
for (fileName in fileNames) {
  
  sample <- read.csv(fileName,header = TRUE,sep = ",")
  
#   MovAvg[fileName]=mean(sample$YI)
  sample$dev <- (sample$YI - mean(sample$YI))**2
  
  #overwriting results on original file
  write.table(sample, fileName,append = FALSE,quote = FALSE,sep = ",",row.names = FALSE,col.names = TRUE)
  
}

# read all extracted individual csv files 
YImod2<- load_data("V:\\vuv-data\\proj\\PETPilot\\Papers\\YI-Haze-StatModeling\\15-yi-haze-statmodeling\\analysis\\demo")

# Calculate predicted R-squared
Rsquared=1-(PSE/mean(YImod2$dev))  ## got 0.4174
print(Rsquared)


###############################################################################################
#################################### HAZE #####################################################
###############################################################################################

# Removing outliers
dt01<-dt[!(dt$Rowkey=="sa19601.09-step1"), ]
dt02<-dt01[!(dt01$Rowkey=="sa19603.23-step2"), ]
dt03<-dt02[!(dt02$Rowkey=="sa19603.02-step2"), ]
dt04<-dt03[!(dt03$Rowkey=="sa19604.24-step3"), ]
dt05<-dt04[!(dt04$Rowkey=="sa19604.15-step1"), ]
Hazedt=dt05 # Haze without outliers


################################ MODEL 1 ######################################################

# Hazing under CyclicQUV
cyclicQUV=subset(Hazedt, Exp =="CyclicQUV")

for( i in c(1:70,72:109)){ # Why not include 71th row   
    fit=lme(Haze~Step*Material+I(Step^2)*Material+I(Step^3),data= cyclicQUV[-i,],random = list(Sample = ~(-1+Step+I(Step^2))))
    cyclicQUV$pred[i]=predict(fit,cyclicQUV[i,]) 
}


# Predicted Squared Error
PSE=mean((cyclicQUV$Haze-cyclicQUV$pred)**2,na.rm=TRUE)

# Set working directory to store individual files 
setwd("V:\\vuv-data\\proj\\PETPilot\\Papers\\YI-Haze-StatModeling\\15-yi-haze-statmodeling\\analysis\\demo")

# Extract individual csv files for each samples 
for(i in c(1,3,4)){ 
  k=subset(cyclicQUV,substring(Rowkey,7,7)==i)
  for(j in 2:8){
    write.csv(subset(k, substring(Sample,10,10)==j ),paste0("sa1960",i,".2",j,".csv"))
  }
}

# Store all sample file names into fileNames
fileNames <- Sys.glob("*.csv")

# I don't know what this does..
# MovAvg=NULL

# Compute deviance from Mean for all files 
for (fileName in fileNames) {
  
  sample <- read.csv(fileName,header = TRUE,sep = ",")
  
  # MovAvg[fileName]=mean(sample$Haze)
  sample$dev <- (sample$Haze - mean(sample$Haze))**2
  
  #overwriting results on original file
  write.table(sample, fileName,append = FALSE,quote = FALSE,sep = ",",row.names = FALSE,col.names = TRUE)
  
}

# read all extracted individual csv files 
cyc<- load_data("V:\\vuv-data\\proj\\PETPilot\\Papers\\YI-Haze-StatModeling\\15-yi-haze-statmodeling\\analysis\\demo")

# I don't know how to interpret this plot
# plot(MovAvg,type="l")

# Calculate Predictive R squared
Rsquared=1-(PSE/mean(cyc$dev))  ## got 0.82
print(Rsquared)



################################ MODEL 3 ######################################################

#Hazing under HotQUV, DampHeat and FreezeThaw
nocyclicQUV=subset(Hazedt,Exp !="Baseline" & Exp!="CyclicQUV" & Rowkey !="sa19601.04-step5")

for( i in 1:330){ ## There are 330 observation but only taken 312..Changed to 330
  fitH02=lme(fixed=Haze~Step + I(Step^2)+I(Step^3)+Material*Exp,data=nocyclicQUV[-i,], 
           random = list(Sample = ~(-1+Step+I(Step^2))))
  nocyclicQUV$pred[i]=predict(fitH02,nocyclicQUV[i,]) 
}

nocyclicQUV$resid = nocyclicQUV$Haze-nocyclicQUV$pred
nocyclicQUVk=subset(nocyclicQUV,resid <=5)

# Predicted Squared Error
PSE=mean((nocyclicQUVk$Haze-nocyclicQUVk$pred)**2,na.rm=T)

# Set working directory to store individual files 
setwd("V:\\vuv-data\\proj\\PETPilot\\Papers\\YI-Haze-StatModeling\\15-yi-haze-statmodeling\\analysis\\demo")

# Subset for DampHeat
nocyclicQUV01=subset(nocyclicQUVk,Exp=="DampHeat")

for(i in c(1,3,4)){ 
  k=subset(nocyclicQUV01,substring(Rowkey,7,7)==i)
  for(j in 1:7){
    write.csv(subset(k, as.numeric(substring(Sample,9))==j ),paste0("sa1960",i,".",j,".D.csv"))
  }
}

# Subset for FreezeThaw
nocyclicQUV02=subset(nocyclicQUVk,Exp=="FreezeThaw")

for(i in c(1,3,4)){ 
  k=subset(nocyclicQUV02,substring(Rowkey,7,7)==i)
  for(j in 8:14){
    write.csv(subset(k, as.numeric(substring(Sample,9))==j ),paste0("sa1960",i,".",j,".F.csv"))
  }
}

# Subset for HotQUV
nocyclicQUV03=subset(nocyclicQUVk,Exp=="HotQUV")

for(i in c(1,3,4)){ 
  k=subset(nocyclicQUV03,substring(Rowkey,7,7)==i)
  for(j in 15:21){
    write.csv(subset(k, as.numeric(substring(Sample,9))==j ),paste0("sa1960",i,".",j,".H.csv"))
  }
}

# Store all sample file names into fileNames
fileNames <- Sys.glob("*.csv")

# I don't know what this does..
# MovAvg=NULL

# Compute deviance from Mean for all files 
for (fileName in fileNames) {
  
  sample <- read.csv(fileName,header = TRUE,sep = ",")
  
  # MovAvg[fileName]=mean(sample$Haze)
  sample$dev <- (sample$Haze - mean(sample$Haze))**2
  
  #overwriting results on original file
  write.table(sample, fileName,append = FALSE,quote = FALSE,sep = ",",row.names = FALSE,col.names = TRUE)
  
}

# read all extracted individual csv files 
nocyclicQUV04<- load_data("V:\\vuv-data\\proj\\PETPilot\\Papers\\YI-Haze-StatModeling\\15-yi-haze-statmodeling\\analysis\\demo")

# Calculate Predictive R squared
Rsquared=1-(PSE/mean(nocyclicQUV04$dev))
print(Rsquared)













