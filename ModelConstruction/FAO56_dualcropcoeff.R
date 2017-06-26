#script to implement the FAO56 dual crop coefficient routine
modelscaffoldDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/results/model_scaffold'
#temporary args
setwd(modelscaffoldDir)
almond_parameters <- read.csv('almond_parameters.csv', stringsAsFactors = F)
precip <- read.csv('PRISM_data.csv') #this is a daily summary of precip from 10/1/2003-11/30/2016 from 'free' daily PRISM 4km resolution for cells of interest in California, created in download_PRISM.R script (last day available from June 2017 download)
U2 <- read.csv('SpatialCIMIS_U2_rounded.csv') #this is a daily summary of wind data from download of spatial CIMIS data, created in spatialCIMIS.R script
dates <- colnames(U2)[2:ncol(U2)]
dates <- strptime(dates, '%b_%d_%Y')
years <- format.Date(dates, '%Y')
months <- format.Date(dates, '%m')
days <- format.Date(dates, '%d')
dates <- rbind(years, months, days)
dates <- cbind(c(NA,NA,NA), dates)
dates <- as.data.frame(dates)
colnames(dates) <- colnames(U2)
U2 <- rbind(dates, U2)

RHmin <- read.csv('SpatialCIMIS_minRH_rounded.csv') #this is a daily summary of minimum relative humidity, estimated from download of spatial CIMIS Tdew and Tmax data, created in spatialCIMIS.R script
ETo <- read.csv('SpatialCIMIS_ETo_rounded.csv') #this is a daily summary of reference ET from download of spatial CIMIS data, created in spatialCIMIS.R script
setwd(file.path(modelscaffoldDir, 'paw_check'))
list.files()
model_scaffold <- read.csv('') #this needs to be re-run for new version of SSURGO aggregation

#calculate U2 by year for mid and end of season periods for each cell of interest
U2_mid <- function(U2)
#turn this into another function, needs to be done by year in case of leap years
bloom_date <- strptime(paste0(as.character(almond_parameters$bloom_mo), '/', as.character(almond_parameters$bloom_day)), '%m/%d')
Jdev <- as.integer(format.Date(bloom_date, '%j'))
Jmid <- Jdev+almond_parameters$Ldev
Jlate <- Jmid+almond_parameters$Lmid
Jharv <- Jlate+almond_parameters$Llate

ETc_act <- function(Kc_act, ETo) {#equation 3 in Allen et al. 2005
  Kc_act*ETo
} 
Kc_act <- function(Ks, Kcb, Ke) {#equation 4 in Allen et al. 2005
  Ks*Kcb+Ke
} 
Kcb_mid <- function(Kcb_mid_std, U2_mid, RHmin_mid, h_mid) {#equation 5 from Allen et al. 2005; perform this by year, including separate calculation of RHmin_mid and U2_mid for each year
  Kcb_mid_std + (0.04*(U2_mid-2)-0.004*(RHmin_mid-45))*(h_mid/3)^0.3
}
#test the function
Kcb_mid(0.95, 2.16, 31.4, 4)
Kcb_end <- function(Kcb_end_std, U2_end, RHmin_end, h_end) {#equation 5 from Allen et al. 2005
  Kcb_end_std + (0.04*(U2_end-2)-0.004*(RHmin_end-45))*(h_end/3)^0.3
}
Kcb <- function(doy, Kcb_ini, Kcb_mid, Kcb_end) { #find value of Kcb by doy relative to three reference Kcb points
  ifelse(doy < Jdev, Kcb_ini, ifelse(doy < Jmid, Kcb_ini + (doy-Jdev)/Ldev*(Kcb_mid-Kcb_ini), ifelse(doy < Jlate, Kcb_mid, ifelse(doy < Jharv, Kcb_mid+(doy-Jlate)/Llate*(Kcb_end-Kcb_mid), Kcb_ini))))
}
#test the function
Kcb(1:365, 0.15, 0.95, 0.65)

Ke <- function(Kr, Kc_max, Kcb, Few) 



