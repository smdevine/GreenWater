#TODO: #1 Track irrigation and precip T and E separately
       #2 Read "Implementing the Dual Crop Coefficient Approach in Interactive
        # Software"
       #3 Revise "SSURGO_processing_awc_r.R"
       #4 
#script to implement the FAO56 dual crop coefficient ET routine
modelscaffoldDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/results/model_scaffold' #location of input data
resultsDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/results/trial6_30_2017/almonds_majcomps'
rounding_digits <- 3
setwd(modelscaffoldDir)
irrigation.parameters <- read.csv('irrigation_parameters.csv', stringsAsFactors = F)
crop.parameters <- read.csv('crop_parameters.csv', stringsAsFactors = F) #necessary parameters by crop to run the FAO56 dual crop coefficient ET model
P.df <- read.csv('PRISM_precip_data.csv', stringsAsFactors = F) #this is a daily summary of precip from 10/1/2003-6/25/17 from 'free' daily PRISM 4km resolution for cells of interest in California, created in download_PRISM.R script (from 6/26/17 download); blanks checked for in 'data_QA_QC.R'
U2.df <- read.csv('SpatialCIMIS_U2_rounded.csv', stringsAsFactors = F) #this is a daily summary of wind data from download of spatial CIMIS data, created in spatialCIMIS.R script.  No missing data except for cell 148533
RHmin.df <- read.csv('SpatialCIMIS_minRH_rounded_QCpass.csv', stringsAsFactors = F) #this is a daily summary of minimum relative humidity, estimated from download of spatial CIMIS Tdew and Tmax data, created in spatialCIMIS.R script.  Blanks filled on "12_08_2011" in data_QA_QC.R.  Now, no missing data except for cell 148533
ETo.df <- read.csv('SpatialCIMIS_ETo_rounded_QCpass.csv', stringsAsFactors = F) #this is a daily summary of reference ET from download of spatial CIMIS data, created in spatialCIMIS.R script.  Blanks filled on multiple days in data_QA_QC.R.  Now, no missing data except for cell 148533
model_scaffold <- read.csv('model_scaffold_majcomps.csv', stringsAsFactors = F)
cropscape_legend <- read.csv('cropscape_legend.txt', stringsAsFactors = FALSE)

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
KcbAdj <- function(Kcb.daily, crop.parameters, crop, U2, RHmin){
  height <- crop.parameters$height[which(crop.parameters$crop==crop)]
  Kcb.climate.adj <- Kcb.daily + (0.04 * (U2 - 2) - 0.004 * (RHmin - 45)) * (height / 3) ^ 0.3
  Kcb.climate.adj[which(Kcb.climate.adj < 0)] <- 0
  Kc.max <- pmax(1.2 + (0.04 * (U2 - 2) - 0.004 * (RHmin - 45)) * (height / 3) ^ 0.3, Kcb.climate.adj + 0.05)
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
  max(Dep.end[i-1] - P[i], 0) # DON'T RUN THIS IF i=1
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
  1 / (1 + (fewp[i] / fewi[i]) * max(TEW - Dep.initial[i], 0.001) / max((TEW - Dei.initial[i]), 0.001))
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
DPepCalc <- function(P, Dep.end) { #DON'T RUN THIS FOR i=1
  max(P[i] - Dep.end[i-1], 0)
}
DepEndCalc <- function(Dep.end, P, Ep, fewp, DPep) { #DON'T RUN THIS FOR i=1
  Dep.end[i - 1] - P[i] + Ep[i] / fewp[i] + DPep[i]
} #ignores transpiration and runoff from upper layer
DPeiCalc <- function(P, Ir, fw, Dei.end) { #DON'T RUN THIS FOR i=1
  max(0, P[i] + Ir[i - 1] / fw - Dei.end[i-1])
}
DeiEndCalc <- function(Dei.end, P, Ir, fw, fewi, Ei, DPei) { #DON'T RUN THIS FOR i=1
  Dei.end[i - 1] - P[i] - Ir[i - 1] / fw + Ei[i] / fewi[i] + DPei[i]
} #again, ignoring tranpiration from upper 10 cm and runoff, eqn. 21)
KcnsCalc <- function(Kcb, Kei, Kep) {
  Kcb[i] + Kei[i] + Kep[i]
}
ETcnsCalc <- function(Kc.ns, ETo) {
  Kc.ns[i] * ETo[i]
}
DrInitialCalc <- function(Dr.end, ETc.ns, P, Ir) { #DON'T RUN THIS FOR i=1
  Dr.end[i - 1] - P[i] - Ir[i-1] + ETc.ns[i]
}
IrCalc <- function(AD, Dr.initial, doys.model, Jdev, Jharv, days.no.irr=21) {
  if (doys.model[i] < Jdev | doys.model[i] > Jharv - days.no.irr) {
    return(0)
  } else if (Dr.initial[i] > AD) {
    return(Dr.initial[i])
  } else {
    return(0)
  }
}
DPrCalc <- function(P, Ir, ETc.ns, Dr.end) { #DON'T RUN THIS FOR i=1
  max(P[i] + Ir[i-1] - ETc.ns[i] - Dr.end[i - 1], 0)
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
  Dr.end[i - 1] - P[i] - Ir[i-1] + Kc.act[i] * ETo[i] + DPr[i]
}

#results functions
model_length_yrs <- max(ETo.df$year) - min(ETo.df$year)
model_scaffold_results <- for (i in 1:nrow(model_scaffold)) { #this is slow
  if (i==1) {
    temp <- rep(model_scaffold[i,], model_length_yrs)
    next
  } else {
    chunk <- rep(model_scaffold[i,], model_length_yrs)
    temp <- rbind(temp, chunk)
    print(i)
  }
}


results_all <- data.frame(model_scaffold_results, Model.Year=rep(seq(from=(min(ETo.df$year)+1), to=max(ETo.df$year), by=1), times=nrow(model_scaffold)), Irr.1=NA, Irr.2=NA, Irr.3=NA, Irr.4=NA, Irr.5=NA, Irr.Last=NA, GreenWaterT=NA, GreenWaterE=NA, GreeWaterET=NA, DeepPercWinter=NA, DeepPercFall=NA, DeepPercSpring=NA, DeltaWinterDr=NA, EndSeasonDr=NA))
#assumes ETo data starts at the beginning of a water year in the fall
#DeepPercFall is from doy=Irr.Last to doy=355 (approx 12/21) 
#DeepPercWinter is Irr.Last to flowering (Jini)
#DeepPercSpring is flowering (Jini) to doy=152 (approx. 6/1)
#DeltaWinterDr is change in soil root zone depletion from Jharv (leaf drop) to Jini (flowering) and is a measure of green water capture relative to winter precip and end of season soil water storage
#EndSeasonDr is soil root zone depletion

#all these tasks only done once per run where run is initialzing model and then looping through all rows in model_scaffold
#ID crop codes
alfalfa_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Alfalfa']
grape_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Grapes']
almond_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Almonds']
walnut_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Walnuts']
#limit model_scaffold to almonds only for now
j <- which(model_scaffold$crop_code==almond_code) #80,401 unique crop, soil, and climate combinations for almond (spatially, this is the equivalent of only 7,236 ha)
model_scaffold <- model_scaffold[j, ]
cropname <- 'almond.mature'
#get doys [days of years] for the model and ensure SpatialCIMIS coverages match
if (length(U2.df$DOY)==length(RHmin.df$DOY) & length(U2.df$DOY)==length(ETo.df$DOY)) {
  doys.model <- U2.df$DOY
  dates <- U2.df$dates
  days <- U2.df$day
  months <- U2.df$month
  year <- U2.df$year
  print('Temporal coverages match in Spatial CIMIS.')
} else { 
  print('There are differing temporal coverages in the Spatial CIMIS data.')
}
model.length <- nrow(ETo.df)
#get precip and spatial CIMIS data to same temporal end point
last.date <- ETo.df$dates[nrow(ETo.df)]
P.df <- P.df[1:which(P.df$dates==last.date), ]
crop.parameters <- CropParametersDefine(crop.parameters)

#temporary organization of these irrigation and crop specific paramters outside the loop, since only almonds are modeled now
first_irrigation <- 0
AD.percentange <- 50 #crop and scenario dependent
Kcb.std <- KcbDefine(doys.model, crop.parameters, cropname) #this will be substituted with a crop code
fc <- fcCalc(doys.model, crop.parameters, cropname) #TO-DO: implement alternative fc calculation in accordance with Eq. 11 from Allen et al. 2005: ((Kcb-Kcmin)/(Kcmax-Kcmin))^(1+0.5*h).  However, this produced a strange result in spreadsheet model for almonds, where increasing h decreases fc.
Jdev <- crop.parameters$Jdev[which(crop.parameters$crop==cropname)]
Jharv <- crop.parameters$Jharv[which(crop.parameters$crop==cropname)]
fw <- fwSelect(irrigation.parameters, "Microspray, orchards")
fewi <- fewiCalc(fc, fw)
fewp <- fewpCalc(fc, fewi)

#these soil parameters to be improved by new SSURGO aggregation
REW.parameter <- 8 #temp definition for readily evaporable water
TEW.fraction <- 0.5
TEW.parameter <- TEWCalc(0.336, 0.211, REW.parameter) #temp definition for total evaporable water
#also need to define z to set up water balance tracking and labels of different soil layers (n=4)

#loop through all rows of model scaffold but only do these operations once for each row
setwd(resultsDir)
for (n in 1:nrow(model_scaffold)) {
  n <- 1000
  model_code <- model_scaffold$unique_model_code[n]
  AD <- 10*model_scaffold$allowable_depletion[n] #converts AD in cm to mm; can redefine this script based on PAW
  if (is.na(AD)) {
    next #TO-DO: write this result to separate file of NAs
  }
  if (AD==0) {
    next ##TO-DO: write this result to separate file of NAs
  }
  #identify climatic and soil parameters
  spCIMIScell <- model_scaffold$CIMIScellnumber[n]
  PRISMcell <- model_scaffold$PRISMcellnumber[n]
  cokey <- model_scaffold$cokey_model[n]
  comppctr <- model_scaffold$comppct_r[n]
  mukey <- model_scaffold$mukey[n]
  P <- P.df[ , which(colnames(P.df)==paste0('cell_', as.character(PRISMcell)))]
  ETo <- ETo.df[ , which(colnames(ETo.df)==paste0('cell_', as.character(spCIMIScell)))]
  U2 <- U2.df[ ,which(colnames(U2.df)==paste0('cell_', as.character(spCIMIScell)))]
  RHmin <- RHmin.df[ ,which(colnames(RHmin.df)==paste0('cell_', as.character(spCIMIScell)))]
  Kcb.df <- KcbAdj(Kcb.std, crop.parameters, cropname, U2, RHmin)
  Kcb.adjusted <- Kcb.df$Kcb.climate.adj
  Kcmax <- Kcb.df$Kc.max
  PAW <- AD/(AD.percentange/100)
  Dei.initial <- numeric(length = model.length)
  DPei <- numeric(length = model.length)
  Dep.initial <- numeric(length = model.length)
  Kri <- numeric(length = model.length)
  Kei <- numeric(length = model.length)
  Ei <- numeric(length = model.length)
  Dei.end <- numeric(length = model.length)
  Krp <- numeric(length = model.length)
  Kep <- numeric(length = model.length)
  Ep <- numeric(length = model.length)
  DPep <- numeric(length = model.length)
  Dep.end <- numeric(length = model.length)
  W <- numeric(length = model.length)
  Kc.ns <- numeric(length = model.length)
  ETc.ns <- numeric(length = model.length)
  Dr.initial <- numeric(length = model.length)
#assumes depletion is zero for root zone except for half of TEW in upper layer
  Ir <- numeric(length = model.length)
  DPr <- numeric(length = model.length)
  Ks <- numeric(length = model.length)
  Kc.act <- numeric(length = model.length)
  Dr.end <- numeric(length = model.length)
  ETc.act <- numeric(length = model.length)
  #for i=1
  i <- 1
  Dei.initial[i] <- max(TEW.parameter * TEW.fraction - P[i], 0) #this is an initial estimate of 
  #the water balance for the exposed surface soil where irrigation is not applied 
  #that assumes all daily precip occurs early in the morning so as to estimate Ke 
  #and Kr.  Same done for Dep.initial
  Dep.initial[i] <- max(TEW.parameter * TEW.fraction - P[i], 0)
  Kri[i] <- KrCalc(TEW.parameter, REW.parameter, Dei.initial)
  Krp[i] <- KrCalc(TEW.parameter, REW.parameter, Dep.initial)
  W[i] <- WCalc(TEW.parameter, Dei.initial, Dep.initial, fewp, fewi)
  Kei[i] <- KeiCalc(Kri, W, Kcmax, Kcb.adjusted, fewi)
  Kep[i] <- KepCalc(Krp, W, Kcmax, Kcb.adjusted, fewp)
  Ep[i] <- EpCalc(ETo, Kep)
  Ei[i] <- EiCalc(ETo, Kei)
  DPep[i] <- DPepCalc(P, Dep.initial)
  Dep.end[i] <- TEW.parameter * TEW.fraction - P[i] + Ep[i] / fewp[i] + DPep[i] #replaces Dep.end[i-1] with TEW.parameter * TEW.fraction
  DPei[i] <- max(P[i] - TEW.parameter * TEW.fraction, 0) #initial estimate assumes irrigation is zero on previous day
  Dei.end[i] <- TEW.parameter * TEW.fraction - P[i] + Ei[i] / fewi[i] + DPei[i] #replaces Dei.end[i-1] with Dei.intial[i]
  Kc.ns[i] <- KcnsCalc(Kcb.adjusted, Kei, Kep)
  ETc.ns[i] <- ETcnsCalc(Kc.ns, ETo)
  Dr.initial[i] <- max(TEW.parameter * TEW.fraction - P[i] + ETc.ns[i], 0) #initial calc
  Ir[i] <- IrCalc(AD, Dr.initial, doys.model, Jdev, Jharv)
  DPr[i] <- max(max(P[i] + Ir[i] - TEW.parameter * TEW.fraction - ETc.ns[i], 0)) #initial calc
  Ks[i] <- KsCalc(Dr.initial, PAW, AD)
  Kc.act[i] <- KcactCalc(Ks, Kcb.adjusted, Kei, Kep)
  ETc.act[i] <- ETcactCalc(Kc.act, ETo)
  Dr.end[i] <- TEW.parameter * TEW.fraction - P[i] + Kc.act[i] * ETo[i] + DPr[i] #initial calc
  for (i in 2:model.length) { #now for days 2...model.length after initialization
    Dei.initial[i] <- DeiInitialCalc(Dei.end, P, Ir, fw)
    Dep.initial[i] <- DepInitialCalc(Dep.end, P)
    Kri[i] <- KrCalc(TEW.parameter, REW.parameter, Dei.initial)
    Krp[i] <- KrCalc(TEW.parameter, REW.parameter, Dep.initial)
    W[i] <- WCalc(TEW.parameter, Dei.initial, Dep.initial, fewp, fewi)
    Kei[i] <- KeiCalc(Kri, W, Kcmax, Kcb.adjusted, fewi)
    Kep[i] <- KepCalc(Krp, W, Kcmax, Kcb.adjusted, fewp)
    Ep[i] <- EpCalc(ETo, Kep)
    Ei[i] <- EiCalc(ETo, Kei)
    DPep[i] <- DPepCalc(P, Dep.initial)
    Dep.end[i] <- DepEndCalc(Dep.end, P, Ep, fewp, DPep)
    DPei[i] <- DPeiCalc(P, Ir, fw, Dei.initial)
    Dei.end[i] <- DeiEndCalc(Dei.end, P, Ir, fw, fewi, Ei, DPei)
    Kc.ns[i] <- KcnsCalc(Kcb.adjusted, Kei, Kep)
    ETc.ns[i] <- ETcnsCalc(Kc.ns, ETo)
    Dr.initial[i] <- DrInitialCalc(Dr.end, ETc.ns, P, Ir)
    Ir[i] <- IrCalc(AD, Dr.initial, doys.model, Jdev, Jharv)
    DPr[i] <- DPrCalc(P, Ir, ETc.ns, Dr.end)
    Ks[i] <- KsCalc(Dr.initial, PAW, AD)
    Kc.act[i] <- KcactCalc(Ks, Kcb.adjusted, Kei, Kep)
    Dr.end[i] <- DrEndCalc(Dr.end, P, Ir, Kc.act, ETo, DPr)
    ETc.act[i] <- ETcactCalc(Kc.act, ETo)
  }
  result <- data.frame(dates, months, days, year, P, ETo, RHmin, U2, lapply(X=list(Kcb.std=Kcb.std, Kcb.adjusted=Kcb.adjusted, Kcmax=Kcmax, fceff=fc, fw=fw, fewi=fewi, fewp=fewp, Dei.initial=Dei.initial, Dep.initial=Dep.initial, Kri=Kri, Krp=Krp, W=W, Kei=Kei, Kep=Kep, Ei=Ei, Ep=Ep, Dpei=DPei, DPep=DPep, Dei.end=Dei.end, Dep.end=Dep.end, Kc.ns=Kc.ns, ETc.ns=ETc.ns, Dr.initial=Dr.initial, Ir=Ir, DPr=DPr, Ks=Ks, Kc.act=Kc.act, ETc.act=ETc.act, Dr.end=Dr.end), round, digits=rounding_digits))
  write.csv(result, paste0('almond_root0.91m_', as.character(model_code), '_', as.character(cokey), '_', Sys.Date(), '.csv'), row.names=F)
  print(n)
  
  
}
#for comparing result in Excel spreadsheet model
writeClipboard(as.character(PRISMcell))
writeClipboard(as.character(spCIMIScell))
writeClipboard(as.character(cokey))
writeClipboard(as.character(model_code))
writeClipboard(as.character(ETo))
writeClipboard(as.character(Kcb.adjusted))
writeClipboard(as.character(Kcmax))
writeClipboard(as.character(P))
writeClipboard(as.character(RHmin))
writeClipboard(as.character(U2))
#print coords

coords_data <- read.csv()
model_scaffold$[n]
n
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
#Kr depends on daily water balance of surface layer (upper 10-15 cm); Few will also depend on daily water balance if separate calculations are done for soil wetted by precipitation vs. by both precip and irrigation (see "Separate Prediction of Evaporation from Soil Wetted by Precipitation Only" in Allen et al. 2005)

#plot checks
plot(Kcb.std, type='l')
plot(Kcb.adjusted, type='l')
plot(Kcmax, type='p')
plot(fewi, type='l')
plot(fc, type='l')
plot(fewp, type='l')
plot(fewp + fewi, type='l')

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