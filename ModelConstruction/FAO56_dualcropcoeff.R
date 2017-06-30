#script to implement the FAO56 dual crop coefficient ET routine
modelscaffoldDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/results/model_scaffold' #location of input data
setwd(modelscaffoldDir)
irrigation.parameters <- read.csv('irrigation_parameters.csv', stringsAsFactors = F)
crop.parameters <- read.csv('crop_parameters.csv', stringsAsFactors = F) #necessary parameters by crop to run the FAO56 dual crop coefficient ET model
P.df <- read.csv('PRISM_precip_data.csv', stringsAsFactors = F) #this is a daily summary of precip from 10/1/2003-6/25/17 from 'free' daily PRISM 4km resolution for cells of interest in California, created in download_PRISM.R script (from 6/26/17 download); blanks checked for in 'data_QA_QC.R'
U2.df <- read.csv('SpatialCIMIS_U2_rounded.csv', stringsAsFactors = F) #this is a daily summary of wind data from download of spatial CIMIS data, created in spatialCIMIS.R script.  No missing data except for cell 148533
RHmin.df <- read.csv('SpatialCIMIS_minRH_rounded_QCpass.csv', stringsAsFactors = F) #this is a daily summary of minimum relative humidity, estimated from download of spatial CIMIS Tdew and Tmax data, created in spatialCIMIS.R script.  Blanks filled on "12_08_2011" in data_QA_QC.R.  Now, no missing data except for cell 148533
ETo.df <- read.csv('SpatialCIMIS_ETo_rounded_QCpass.csv', stringsAsFactors = F) #this is a daily summary of reference ET from download of spatial CIMIS data, created in spatialCIMIS.R script.  Blanks filled on multiple days in data_QA_QC.R.  Now, no missing data except for cell 148533
#define functions implement FA56 dual crop coefficients
#includes subroutine that separates evaporable water as P vs. Irr sourced
CropParametersDefine <- function(crop.parameters) {
  bloom.date <- strptime(paste0(as.character(crop.parameters$bloom.mo), '/', as.character(crop.parameters$bloom.day)), '%m/%d')
  crop.parameters$Jdev <- as.integer(format.Date(bloom.date, '%j'))
  crop.parameters$Jmid <- crop.parameters$Jdev + crop.parameters$Ldev
  crop.parameters$Jlate <- crop.parameters$Jmid + crop.parameters$Lmid
  crop.parameters$Jharv <- crop.parameters$Jlate + crop.parameters$Llate
  return(crop.parameters)
}
KcbDefine <- function(doy, parameters, crop) { #find standard value of Kcb by day of year relative to three reference Kcb points defined by crop parameters
  parameters <- crop.parameters[which(crop.parameters$crop==crop), ]
  ifelse(doy < parameters[['Jdev']], parameters[['Kcb.ini']], ifelse(doy < parameters[['Jmid']], parameters[['Kcb.ini']] + (doy - parameters[['Jdev']]) / parameters[['Ldev']] * (parameters[['Kcb.mid']] - parameters[['Kcb.ini']]), ifelse(doy < parameters[['Jlate']], parameters[['Kcb.mid']], ifelse(doy < parameters[['Jharv']], parameters[['Kcb.mid']] + (doy - parameters[['Jlate']]) / parameters[['Llate']] * (parameters[['Kcb.end']] - parameters[['Kcb.mid']]), parameters[['Kcb.ini']]))))
}
#adjust standard Kcb values by daily climate according to equation #5 in Allen et al. 2005 and calculate Kc max at the same time
KcbAdj <- function(Kcb.daily, crop.parameters, crop, U2.df, RHmin.df, SpCIMIScell){
  column.name <- paste0('cell_', as.character(SpCIMIScell))
  height <- crop.parameters$height[which(crop.parameters$crop==crop)]
  U2 <- U2.df[ ,which(colnames(U2.df)==column.name)]
  RHmin <- RHmin.df[ ,which(colnames(RHmin.df)==column.name)]
  Kcb.climate.adj <- Kcb.daily + (0.04 * (U2 - 2) - 0.004 * (RHmin - 45)) * (height / 3) ^ 0.3
  Kcb.climate.adj[which(Kcb.climate.adj < 0)] <- 0
  Kc.max <- pmax(1.2 + (0.04 * (U2 - 2) - 0.004 * (RHmin - 45)) * (height / 3)^0.3, Kcb.climate.adj + 0.05)
  return(as.data.frame(cbind(Kcb.climate.adj, Kc.max)))
}
#calculate fraction of cover ('fc') for specific day of year relative to three points defined in crop parameters
fcCalc <- function(doy, parameters, crop) {
  parameters <- crop.parameters[which(crop.parameters$crop==crop), ]
  ifelse(doy < parameters[['Jdev']], parameters[['fc.ini']], ifelse(doy < parameters[['Jmid']], parameters[['fc.ini']] + (doy-parameters[['Jdev']]) / parameters[['Ldev']] * (parameters[['fc.mid']] - parameters[['fc.ini']]), ifelse(doy < parameters[['Jlate']], parameters[['fc.mid']], ifelse(doy < parameters[['Jharv']], parameters[['fc.mid']] + (doy-parameters[['Jlate']]) / parameters[['Llate']] * (parameters[['fc.end']] - parameters[['fc.mid']]), parameters[['fc.ini']]))))
}
#calculate fraction of soil wetted by both irrigation and precipitation and exposed to rapid drying (fewi) and fraction of soil exposed to rapid drying and is wetted by precipitation only (fewp).  These are dependent upon fraction of cover calculation above
fwSelect <- function(irr.parameters, irr.type) {
  irr.parameters$fw[which(irr.parameters$irrigation.type==irr.type)]
}
fewiCalc <- function(fc, fw) { #see p.147 of FAO-56 Chp.7 re: drip
  fewi.temp <- pmin(1 - fc, fw)
  fewi.temp[which(fewi.temp < 0.001)] <- 0.001 #lower limit on fewi for numeric stability
  fewi.temp[which(fewi.temp > 1)] <- 1 #upper limit on fewi for numeric stability
  return(fewi.temp)
}
fewpCalc <- function(fc, fewi) {
  fewp.temp <- 1 - fc - fewi
  fewp.temp[which(fewp.temp < 0.001)] <- 0.001 #lower limit on fewp for numeric stability
  fewp.temp[which(fewp.temp > 1)] <- 1 #upper limit on fewp for numeric stability
  return(fewp.temp)
}
TEWCalc <- function(ThetaFC, ThetaWP, REW, Ze=0.10) { #Ze is depth of upper soil 
  #layer where evaporation occurs in meters
  result <- 1000 * (ThetaFC - 0.5 * ThetaWP) * Ze
  if (result < REW) {
    stop(print('Hay una problema con TEW')) #could change to next and print or 
    #save model code
  } 
  return(result)
}
DepInitialCalc <- function(Dep.end, P) {
  max(Dep[i-1] - P[i], 0) # DON'T RUN THIS IF i=1
}
DeiInitialCalc <- function(Dei.end, P, Ir, fw) { #DON'T RUN THIS WHEN i=1
  max(Dei.end[i - 1] - P[i] - Ir[i - 1] / fw, 0) #have to use irrigation from 
  #previous day because current day irrigation decision is dependent on this calc
}
KrCalc <- function(TEW, REW, De.initial) { #can be used to calculate Kri or Krp
  max(0, if(De.initial[i] < REW) { #could initialize this in vector above
    1
  } else {
    (TEW - De.initial[i]) / (TEW - REW)
  })
}
WCalc <- function(TEW, Dei.initial, Dep.initial, fewp, fewi) {
  1 / (1 + (fewp[i] / fewi[i]) * (TEW - Dep.initial[i]) / (TEW - Dei.initial[i]))
}
KeiCalc <- function(Kri, W, Kcmax, Kcb, fewi) {
  min(Kri[i] * W[i] * (Kcmax[i] - Kcb[i]), fewi[i] * Kcmax[i])
}
KepCalc <- function(Krp, W, Kcmax, Kcb, fewp) {
  min(Krp[i] * (1 - W[i]) * (Kcmax[i] - Kcb[i]), fewp[i] * Kcmax[i])
}
EpCalc <- function(ETo, Kep) {
  ETo[i] * Kep[i]
}
EiCalc <- function(ETo, Kei) {
  ETo[i] * Kei[i]
}
DPepCalc <- function(P, Dep.initial) {
  max(P[i] - Dep.initial[i], 0)
}
DepEndCalc <- function(Dep.initial, P, Ep, fewp, DPep) {
  Dep.initial[i] - P[i] + Ep[i] / fewp[i] + DPep[i]
}
DPeiCalc <- function(P, Ir, fw, Dei.initial) { #DON'T RUN THIS FOR i=1
  max(0, P[i] + Ir[i - 1] / fw - Dei.initial[i])
}
KcnsCalc <- function(Kcb, Kei, Kep) {
  Kcb[i] + Kei[i] + Kep[i]
}
ETcnsCalc <- function(Kc.ns, ETo) {
  Kc.ns[i] * ETo[i]
}
DrInitialCalc <- function(Dr.end, ETc.ns, P) { #don't run this for i=1
  Dr.end[i - 1] - P[i] + ETc.ns[i]
}
IrCalc <- function(AD, Dr.initial, doys.model, Jdev, Jharv, days.no.irr=21) {
  if (doys.model[i] < Jdev | doys.model[i] > Jharv - days.no.irr) {
    return(0)
  } else if (Dr.initial[i] > AD) {
    return(AD)
  } else {
    return(0)
  }
}
DPrCalc <- function(P, Ir, ETc.ns, Dr.end) { #NOT TO BE USED for i=1
  max(P[i] + Ir[i] - ETc.ns[i] - Dr.end[i - 1], 0)
}
KsCalc <- function(Dr.initial, PAW, AD) {
  if (Dr.initial[i] > AD) {
    (PAW - Dr.initial[i])/(PAW - AD)
  } else { 
    return(1)
  }
}
KcactCalc <- function(Ks, Kcb, Kei, Kep) {#equation 4 in Allen et al. 2005
  Ks[i] * Kcb[i] + Kei[i] + Kep[i]
}
ETcactCalc <- function(Kc.act, ETo) {#equation 3 in Allen et al. 2005
  Kc.act[i] * ETo[i]
}
DrEndCalc <- function(Dr.end, P, Ir, Kc.act, ETo, DPr) { #NOT TO BE USED for i=1
  Dr[i - 1] - P[i] - Ir[i] + Kc.act[i] * ETo[i] + DPr[i]
}

#only done once per run
#get doys [days of years] for the model and ensure SpatialCIMIS coverages match
if (length(U2.df$DOY)==length(RHmin.df$DOY) & length(U2.df$DOY)==length(ETo.df$DOY)) {
  doys.model <- U2.df$DOY
  print('Temporal coverages match in Spatial CIMIS.')
} else { 
  print('There are differing temporal coverages in the Spatial CIMIS data.')
}
#get precip and spatial CIMIS data to same temporal end point
last.date <- ETo.df$dates[nrow(ETo.df)]
P.df <- P.df[1:which(P.df$dates==last.date), ]
crop.parameters <- CropParametersDefine(crop.parameters)

#this could be done once for each crop
Kcb.std <- KcbDefine(doys.model, crop.parameters, 'almond.mature') #this will be substituted with a crop code
plot(Kcb.std, type='l')
Kcb.df <- KcbAdj(Kcb.std, crop.parameters, 'almond.mature', U2.df, RHmin.df, 86833) #last number is the Spatial CIMIS cell number, which will be retrieved from the model scaffold
plot(Kcb.df$Kcb.climate.adj, type='l')
plot(Kcb.df$Kc.max, type='p')


#test the fcCalc function
fc <- fcCalc(doys.model, crop.parameters, 'almond.mature')
plot(fc, type='l')

#TO-DO: implement alternative fc calculation in accordance with Eq. 11 from Allen et al. 2005: ((Kcb-Kcmin)/(Kcmax-Kcmin))^(1+0.5*h).  However, this produced a strange result in spreadsheet model for almonds, where increasing h decreases fc.


print(irrigation.parameters$irrigation.type)
fw <- fwSelect(irrigation.parameters, "Microspray, orchards")

fewi <- fewiCalc(fc, fw)
plot(fewi, type='l')


fewp <- fewpCalc(fc, fewi)
plot(fewp, type='l')
plot(fewp + fewi, type='l')
summary(fewp + fewi)

#calculate Kri and Krp; now dependent on daily soil water balance
#Kri=(TEW-De,j-1)/(TEW-REW) (eqn. 22)
#Krp=(TEW-Dep,j-1)/(TEW-REW) (eqn. 23)
#this is example for model code 233269: PRISM cell=365380; 
#SpatialCIMIS cell=86833; SSURGO cokey=12753827; crop=almonds
crop <- 'almond.mature'
i <- 1
REW.parameter <- 8 #temp definition for readily evaporable water
TEW.fraction <- 0.5
AD <- 80.35 #temp definition for allowable depletion in rootzone
AD.percentange <- 50
PAW <- 80.35/(AD.percentange/100)
Jdev <- crop.parameters$Jdev[which(crop.parameters$crop==crop)]
Jharv <- crop.parameters$Jharv[which(crop.parameters$crop==crop)]

TEW.parameter <- TEWCalc(0.336, 0.211, REW.parameter) #these will need to be 
#taken from model scaffold
model.length <- nrow(ETo.df)
Dei.initial <- numeric(length = model.length)
Dei.initial[1] <- TEW.parameter * TEW.fraction #this is an initial estimate of 
#the water balance for the exposed surface soil where irrigation is not applied 
#that assumes all daily precip occurs early in the morning so as to estimate Ke 
#and Kr.  Same done for Dep.initial
Kri <- numeric(length = model.length)
Kei <- numeric(length = model.length)
Ei <- numeric(length = model.length)
DPei <- numeric(length = model.length)
DPei[1] <- max(P[1] - Dei.initial[1], 0)
Dei.end <- numeric(length = model.length)
Dep.initial <- numeric(length = model.length)
Dep.initial[1] <- TEW.parameter * TEW.fraction
Krp <- numeric(length = model.length)
Kep <- numeric(length = model.length)
Ep <- numeric(length = model.length)
DPep <- numeric(length = model.length)
Dep.end <- numeric(length = model.length)
W <- numeric(length = model.length)
Kc.ns <- numeric(length = model.length)
ETc.ns <- numeric(length = model.length)
Dr.initial <- numeric(length = model.length)
Dr.initial[1] <- max(TEW.parameter * TEW.fraction - P[1] + ETc.ns[1], 0) 
#assumes depletion is zero for root zone except for half of TEW in upper layer
Ir <- numeric(length = model.length)
Ir[1] <- 0 #assume no irrigation on the first day
DPr <- numeric(length = model.length)
DPr[1] <- max(max(P[1] + Ir[i] - TEW.parameter * TEW.fraction - ETc.ns[1], 0)) 
#WARNING, THE ABOVE DEPENDS ON ETcnsCalc
Ks <- numeric(length = model.length)
Kc.act <- numeric(length = model.length)
Dr.end <- numeric(length = model.length)
ETc.act <- numeric(length = model.length)
P <- P.df[ , which(colnames(P.df)=='cell_365380')] #this can be automated based 
#on the model_scaffold.csv that defines all the spatial parameters
ETo <- ETo.df[ , which(colnames(ETo.df)=='cell_86833')]

Dep.initial[i] <- DepInitialCalc(Dep.end, P)
Dei.initial[i] <- DeiInitialCalc(Dei.end, P, Ir, fw)
Kri[i] <- KrCalc(TEW.parameter, REW.parameter, Dei.initial)
Krp[i] <- KrCalc(TEW.parameter, REW.parameter, Dep.initial)
W[i] <- WCalc(TEW.parameter, Dei.initial, Dep.initial, fewp, fewi)
Kei[i] <- KeiCalc(Kri, W, Kcb.df$Kc.max, Kcb.df$Kcb.climate.adj, fewi)
Kep[i] <- KepCalc(Krp, W, Kcb.df$Kc.max, Kcb.df$Kcb.climate.adj, fewp)
Ep[i] <- EpCalc(ETo, Kep)
Ei[i] <- EiCalc(ETo, Kei)
DPep[i] <- DPepCalc(P, Dep.initial)
Dep.end[i] <- DepEndCalc(Dep.initial, P, Ep, fewp, DPep)
# now calculate irrigation needs to be able to finish water balance for fewi 

DPei[i] <- DPeiCalc(P, Ir, fw, Dei.initial)
Kc.ns[i] <- KcnsCalc(Kcb.df$Kcb.climate.adj, Kei, Kep)
ETc.ns[i] <- ETcnsCalc(Kc.ns, ETo)
Dr.initial[i] <- DrInitialCalc(Dr.end, ETc.ns, P)
Ir[i] <- IrCalc(AD, Dr.initial, doys.model, Jdev, Jharv)
DPr[i] <- DPrCalc(P, Ir, ETc.ns, Dr.end)
Ks[i] <- KsCalc(Dr.initial, PAW, AD)
Kc.act[i] <- KcactCalc(Ks, Kcb.df$Kcb.climate.adj, Kei, Kep)
ETc.act[i] <- ETcactCalc(Kc.act, ETo)
#De,j=De,j-1 - P,j - Ij/fw + Ej/fewi + DPei,j (again, ignoring tranpiration from upper 10 cm and runoff, eqn. 21) 
#pseudo-code outline of 'separate prediction of evaporation from soil wetted by precipitation only' following Allen et al. 2005, except ignoring runoff, essentially assuming that runoff will really only occur when soils are near field capacity, so partitioning this as 'deep percolation' is acceptable and is consciously preferred over introduced errors from the curve number approach
#Ke = Kei + Kep (eqn. 14)
#where Kei=evaporation coefficient for the exposed fraction of the soil wetted by both irrigation and by precipitation and Kep=evaporation coefficient for the exposed fraction of the soil wetted by precipitation only
#Kei = Kri*W*(Kcmax-Kcb) <= fewi*Kcmax (eqn. 15)
#Kep = Krp*(1-W)*(Kcmax-Kcb) <= fewp*Kcmax (eqn. 16)
#where fewi=fraction of soil wetted by both irrigation and precipitation and is exposed to rapid drying due to exposure to solar radiation and/or ventilation and is calculated as: min(1-fc, fw) (eqn. 18);
#where fewp=fraction of soil exposed to rapid drying and is wetted by precipitation only and is calculated as: 1-fc-fewi (eqn. 17)
#where Kri and Krp=evaporation reduction coefficients for the fewi and fewp fractions, respectively
#where W=weighting coefficient for partitioning the energy available for evaporation in the fewi and fewp soil fractions, depending on water availability, and is calculated as: 1/(1+(fewp/fewi)*(TEW-Dep)/(TEW-De)) (eqn. 19);
#where De=cumulative depletion depth (mm) from the evaporating layer for the fewi fraction of soil
#where Dep=cumulative depletion depth (mm) from the evaporating layer for the fewp fraction of soil
#finally, the water balance formation for fraction of soil only wetted by precipitation:
#Dep,j=Dep,j-1 - Pj + Ep,j/fewp + DPep,j (ignoring transpiration from upper 10 cm and runoff, eqn. 20)
#where Dep,j-1 and Dep,j are the cumulative depletion depths at the ends of days j-1 and j in the fewp fraction of the surface (mm)
#where Ep,j=evaporation from kewp fraction on day j and is calculated as: Kep*ETo in mm
#where DPep,j=deep percolation from the fewp fraction of the evaporation layer on day j if soil water content exceeds field capacity
#where 0 <= Dep, j <= TEW
#TEW=total evaporable water in upper layer and is calculated as: 1000*(field_capacity-0.5*wilting_point)*Ze
#where Ze=effective depth of wetting and field capacity and wilting point is assumed to be moisture content at 1/3 and 15 bars, respectively, determined from the SSURGO database
#and the water balance formulation for the fraction of soil wetted by both precipitation and irrigation
#De,j=De,j-1 - P,j - Ij/fw + Ej/fewi + DPei,j (again, ignoring tranpiration from upper 10 cm and runoff, eqn. 21)
#where 0 <= De,j <= TEW
#where Kri=(TEW-De,j-1)/(TEW-REW) (eqn. 22)
#where Krp=(TEW-Dep,j-1)/(TEW-REW) (eqn. 23)
#where Dpei,j=Pj + Ij/fw - Dei,j-1 >= 0
#where DPep,j=Pj-Dep,j-1 >= 0

Ke <- function(Kr, Kc_max, Kcb, Few) #Kr depends on daily water balance of surface layer (upper 10-15 cm); Few will also depend on daily water balance if separate calculations are done for soil wetted by precipitation vs. by both precip and irrigation (see "Separate Prediction of Evaporation from Soil Wetted by Precipitation Only" in Allen et al. 2005)

#run the model as a loop through all of the model_scaffold files; could rbind these ahead of time to eliminate loop
setwd(file.path(modelscaffoldDir, 'paw_check'))
model_scaffold_fnames <- list.files(pattern = glob2rx('*.csv'))
#set up data.frame for results output of all unique model codes
for (i in 1:length(model_scaffold_fnames)) {
  model_scaffold <- read.csv(model_scaffold_fnames[i], stringsAsFactors = F) #this needs to be re-run for new version of SSURGO aggregation
  #insert additional functions here to run FAO-56 dual crop coefficient model for each unique crop, soil, and climate combination; this could be run as an apply-like function?
  #save results file after each model_scaffold_fname is complete; these could be combined later using rbind
}

#alternative approach using means for different periods of the Kc curve.  This was used in the FAO56 spreadsheet program.
#calculate U2 and minRH by year for mid to late period for each cell of interest; this is used by Kcb function.  Should just use average of years for 2017
U2_mid <- function(U2.df, col_index, Jmid, Jlate) {
  U2_temp <- U2.df[which(U2.df$DOY >= Jmid & U2.df$DOY <= Jlate), ]
  result <- as.data.frame(tapply(U2_temp[,col_index], U2_temp$year, mean)) #or could do all cells via this arg to replace col_index <- 6:ncol(U2_temp)
  colnames(result) <- colnames(U2.df)[col_index]
  return(result) #rownames are years, search by which(rownames(result)=='year of interest')
}
#check function
U2_mid_allyrs <- U2_mid(U2.df, 6, almond_parameters$Jmid, almond_parameters$Jlate)

RHmin_mid <- function(RHmin, col_index, Jmid, Jlate) {
  RHmin_temp <- RHmin[which(RHmin$DOY >= Jmid & RHmin$DOY <= Jlate), ]
  result <- as.data.frame(tapply(RHmin_temp[,col_index], RHmin_temp$year, mean))
  colnames(result) <- colnames(RHmin)[col_index]
  return(result)
}
RHmin_mid_allyrs <- RHmin_mid(RHmin.df, 6, almond_parameters$Jmid, almond_parameters$Jlate)

U2_end <- function(U2.df, col_index, Jlate, Jharv) {
  U2_temp <- U2.df[which(U2.df$DOY >= Jlate & U2.df$DOY <= Jharv), ]
  result <- as.data.frame(tapply(U2_temp[,col_index], U2_temp$year, mean)) #or could do all cells via this arg to replace col_index <- 6:ncol(U2_temp)
  colnames(result) <- colnames(U2.df)[col_index]
  return(result) #rownames are years, search by which(rownames(result)=='year of interest')
}
#check function
U2_end_allyrs <- U2_end(U2.df, 6, almond_parameters$Jlate, almond_parameters$Jharv)

RHmin_end <- function(RHmin.df, col_index, Jlate, Jharv) {
  RHmin_temp <- RHmin.df[which(RHmin.df$DOY >= Jlate & RHmin.df$DOY <= Jharv), ]
  result <- as.data.frame(tapply(RHmin_temp[,col_index], RHmin_temp$year, mean))
  colnames(result) <- colnames(RHmin.df)[col_index]
  return(result)
}
RHmin_end_allyrs <- RHmin_mid(RHmin.df, 6, almond_parameters$Jlate, almond_parameters$Jharv)

Kcb_mid <- function(Kcb_mid_std, U2_summary, RHmin_summary, h_mid, yr) {#equation 5 from Allen et al. 2005; 
  U2_mid_mean <- U2_summary[which(rownames(U2_summary)==yr),]
  RHmin_mid_mean <- RHmin_summary[which(rownames(RHmin_summary)==yr),]
  Kcb_mid_std + (0.04*(U2_mid_mean-2)-0.004*(RHmin_mid_mean-45))*(h_mid/3)^0.3
}
#test the function
Kcb_mid(almond_parameters$Kcb_mid, U2_mid_allyrs, RHmin_mid_allyrs, almond_parameters$height, 2004)
Kcb_mid(almond_parameters$Kcb_mid, U2_mid_allyrs, RHmin_mid_allyrs, almond_parameters$height, 2016)

Kcb_end <- function(Kcb_end_std, U2_summary, RHmin_summary, h_end, yr) {#equation 5 from Allen et al. 2005
  U2_end_mean <- U2_summary[which(rownames(U2_summary)==yr),]
  RHmin_end_mean <- RHmin_summary[which(rownames(RHmin_summary)==yr),]
  Kcb_end_std + (0.04*(U2_end_mean-2)-0.004*(RHmin_end_mean-45))*(h_end/3)^0.3
}
Kcb_end(almond_parameters$Kcb_end, U2_end_allyrs, RHmin_end_allyrs, almond_parameters$height, 2004)
Kcb_end(almond_parameters$Kcb_end, U2_end_allyrs, RHmin_end_allyrs, almond_parameters$height, 2016)