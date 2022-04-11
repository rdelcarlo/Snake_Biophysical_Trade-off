# Fit Sigmoidal and get parameters for dose response.
# 12/14/17 - Create report with snake info.
# 9/20/18 - Fix snake muscle mass (was using snake's total muscle mass instead of for specific muscle sample)

#install.packages("rlang")
#install.packages("ggplot2")
library(tidyverse)
library(minpack.lm)
setwd("C:/Users/rdelcarlo/Desktop/data_r_script/p4c4pwithTTX/p4c4pwithTTX/Output/")

# Get Snake Info - Species, Genotype etc.
sinf = read.csv("C:/Users/rdelcarlo/Desktop/data_r_script/SnakeInfo-09.30.2020.csv") # C:\Users\rdelcarlo\Desktop\data_r_script.csv

# Get Snake Muscle Masses
smm = read.csv("C:/Users/rdelcarlo/Desktop/data_r_script/SnakeSkeletalMuscleMasses-9.28.2020.csv")
# reset all missing muscle mass values to -1.0smm
smm[is.na(smm)] <- 0.999

smm_gathered <- smm %>% gather(Muscle, MusMassg, M1:M13) %>% select(-Date) %>% rename(Snake = SnakeID) #the %>% chains fx together to nest, -M1OS was an old unnecessary column

df <- read.csv("C:/Users/rdelcarlo/Desktop/data_r_script/p4c4pwithTTX/p4c4pwithTTX/Output/p4C4PwithTTX-ContrAmpl.N.g..csv", row.names = 2)
# df <- read.csv("C:/Users/rdelcarlo/Downloads/Converted/Converted/Output/p4C4PwithTTX-ContrAmpl.N.g..csv", row.names = 2)
#df <- read.csv("C:/Users/rdelcarlo/Desktop/Myography Data/Protocol 6 - Lidocaine/Output/Output/p4C4PwithLido-ContrAmpl.N.g..csv", row.names = 2)
df <- df[1:nrow(df),2:ncol(df)]

### Experimental Code to fix NA for 0 Dose
colMax <- function(df) sapply(df, max, na.rm = TRUE)
# df[1,] <- colMax(df)  ## Replaces all with max - not good
df[1,is.na(df[1,])] <- colMax(df[,is.na(df[1,])])
###

df <- sweep(df,2,as.numeric(df[1,]),"/")
df <- df[,which(colSums(!is.na(df)) >= 4)]
head(df)
sigmoidal <- function(x,A1,A2,x0,dx){
  (A1-A2)/(1 + exp((log(x)-log(x0))/dx)) + A2
}
sigmoidal <- Vectorize(sigmoidal)

varsigmoidal <- function(x,x0,dx){
  (1)/(1 + exp((log(x)-log(x0))/dx))
}
varsigmoidal <- Vectorize(varsigmoidal)

residFun <- function(parS,observed,indices){
  sigmoidal(as.numeric(rownames(df))[indices],parS$A1,parS$A2,parS$x0,parS$dx) - observed
}

varresidFun <- function(parS,observed,indices){
  varsigmoidal(as.numeric(rownames(df))[indices],parS$x0,parS$dx) - observed
}

parStart <- list(A2 = 0, A1 = 1, x0 = 450, dx = 2)
fitParams <- function(x){
  nls.out <- nls.lm(par = parStart, fn = varresidFun, control = nls.lm.control(maxiter = 100), observed = df[!is.na(df[,x]),x], indices = !is.na(df[,x]))
  unlist(nls.out$par[3:4])
}

result <- Vectorize(fitParams)(1:ncol(df))
colnames(result) <- colnames(df)

sigmoidalDeriv <- function(x,A1,A2,x0,dx){
  -((A1 - A2) * exp((log(x) + log(x0))/dx))/(dx * x * (exp(log(x)/dx) + exp(log(x0)/dx))^2)
}
sigmoidalDeriv <- Vectorize(sigmoidalDeriv)

max.abs <- function(x,...){
  sign(x[which.max(abs(x))])*max(abs(x),...)
}
maxslope <- apply(result,2,function(p){max.abs(sigmoidalDeriv(as.numeric(rownames(df)),1,0,p[1],p[2]), na.rm = T)})

solveSigmoidal <- function(y,A1,A2,x0,dx){
  exp(log(-(A2 - A1)/(y - A2) - 1) * dx + log(x0))
}
solveSigmoidal <- Vectorize(solveSigmoidal)
range.10.90 <- abs(solveSigmoidal(0.9,1,0,result[1,],result[2,]) - solveSigmoidal(0.1,1,0,result[1,],result[2,]))

result <- rbind(result,maxslope,range.10.90)
write.csv(t(result), "C:/Users/rdelcarlo/Desktop/data_r_script/p4c4pwithTTX/p4c4pwithTTX/Output/Sigmoidal_Output/p4C4PwithTTX-amplitude-sigmoidal.csv")

rdf <- read.csv("C:/Users/rdelcarlo/Desktop/data_r_script/p4c4pwithTTX/p4c4pwithTTX/Output/Sigmoidal_Output/p4C4PwithTTX-amplitude-sigmoidal.csv")
x <- strsplit(as.character(rdf$X),"_")
rdf$Snake <- unlist(lapply(x,'[[',1))
rdf$Muscle <- unlist(lapply(x,'[[',2))
rdf$Species <- sinf$Species[match(rdf$Snake, sinf$Snake)]
rdf$Genotype <- sinf$Genotype[match(rdf$Snake, sinf$Snake)]
rdf$MAMU <- sinf$MAMU[match(rdf$Snake, sinf$Snake)]
rdf$County <- sinf$COUNTY[match(rdf$Snake, sinf$Snake)]
rdf$Long <- sinf$Longitude[match(rdf$Snake, sinf$Snake)]
rdf$Lat <- sinf$Latitude[match(rdf$Snake, sinf$Snake)]
rdf$SVLmm <- as.character(sinf$SVLmm[match(rdf$Snake, sinf$Snake)])
# muscMass <- smm[which(smm$SnakeID %in% rdf$Snake), rdf$Muscle] / 1000
# #rdf$MusMassg <-
# #  as.character(sinf$MusMassg[match(rdf$Snake, sinf$Snake)])
# rdf$MusMassg <- as.character(muscMass)
rdf <- rdf %>% left_join(smm_gathered)
rdf$BodMassg <- as.character(sinf$BodMassg[match(rdf$Snake, sinf$Snake)])
rdf$Sex <- as.character(sinf$Sex[match(rdf$Snake, sinf$Snake)])
rdf$DtExp <- as.character(sinf$Date_Experimented[match(rdf$Snake, sinf$Snake)])
rdf$DtColl <- as.character(sinf$Date_Collected[match(rdf$Snake, sinf$Snake)])
rdf$DInBet <- as.character(sinf$Days_in_Between[match(rdf$Snake, sinf$Snake)])
write.csv(rdf, "C:/Users/rdelcarlo/Desktop/data_r_script/p4c4pwithTTX/p4c4pwithTTX/Output/Sigmoidal_Output/p4C4PwithTTX-amplitude-sigmoidal-rpt.csv")