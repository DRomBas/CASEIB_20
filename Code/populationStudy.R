rm(list=ls())
library(lme4)
library(boot)

nSim <- 1e3 # Number of bootstrap samples
bootMethod = "perc" # Bootstrapping method

# Rim Height (Altura máxima)
Data = read.csv('../Data/rimHeight.csv')
Data$rimHeight = Data$rimHeight*1e3
morf_mixed = lmer(rimHeight ~ Gender + Age + (1|PatientId), data=Data)
summary(morf_mixed)
b_par<-bootMer(x=morf_mixed,FUN=fixef,nsim=nSim)
boot.ci(b_par,type=bootMethod,index=1)
boot.ci(b_par,type=bootMethod,index=2)
boot.ci(b_par,type=bootMethod,index=3)

# Rim Radious (Radio foveal)
Data = read.csv('../Data/rimRad.csv')
Data$rimRad = Data$rimRad*1e3
morf_mixed = lmer(rimRad ~ Gender + Age + (1|PatientId), data=Data)
summary(morf_mixed)
b_par<-bootMer(x=morf_mixed,FUN=fixef,nsim=nSim)
boot.ci(b_par,type=bootMethod,index=1)
boot.ci(b_par,type=bootMethod,index=2)
boot.ci(b_par,type=bootMethod,index=3)

# Max Slope (Pendiente máxima)
Data = read.csv('../Data/maxSlope.csv')
morf_mixed = lmer(maxSlope ~ Gender + Age + (1|PatientId), data=Data)
summary(morf_mixed)
b_par<-bootMer(x=morf_mixed,FUN=fixef,nsim=nSim)
boot.ci(b_par,type=bootMethod,index=1)
boot.ci(b_par,type=bootMethod,index=2)
boot.ci(b_par,type=bootMethod,index=3)

# Bowl Area (Área foveal)
Data = read.csv('../Data/BowlArea.csv')
Data$BowlArea = Data$BowlArea*1e6
morf_mixed = lmer(BowlArea ~ Gender + Age + (1|PatientId), data=Data)
summary(morf_mixed)
b_par<-bootMer(x=morf_mixed,FUN=fixef,nsim=nSim)
boot.ci(b_par,type=bootMethod,index=1)
boot.ci(b_par,type=bootMethod,index=2)
boot.ci(b_par,type=bootMethod,index=3)



