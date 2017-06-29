#script to implement the FAO56 dual crop coefficient ET routine
modelscaffoldDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/results/model_scaffold' #location of input data
setwd(modelscaffoldDir)
irrigation_parameters <- read.csv('irrigation_parameters.csv', stringsAsFactors = F)
crop_parameters <- read.csv('crop_parameters.csv', stringsAsFactors = F) #necessary parameters by crop to run the FAO56 dual crop coefficient ET model
PRISMprecip <- read.csv('PRISM_precip_data.csv', stringsAsFactors = F) #this is a daily summary of precip from 10/1/2003-6/25/17 from 'free' daily PRISM 4km resolution for cells of interest in California, created in download_PRISM.R script (from 6/26/17 download); blanks checked for in 'data_QA_QC.R'
U2 <- read.csv('SpatialCIMIS_U2_rounded.csv', stringsAsFactors = F) #this is a daily summary of wind data from download of spatial CIMIS data, created in spatialCIMIS.R script.  No missing data except for cell 148533
RHmin <- read.csv('SpatialCIMIS_minRH_rounded_QCpass.csv', stringsAsFactors = F) #this is a daily summary of minimum relative humidity, estimated from download of spatial CIMIS Tdew and Tmax data, created in spatialCIMIS.R script.  Blanks filled on "12_08_2011" in data_QA_QC.R.  Now, no missing data except for cell 148533
ETo <- read.csv('SpatialCIMIS_ETo_rounded_QCpass.csv', stringsAsFactors = F) #this is a daily summary of reference ET from download of spatial CIMIS data, created in spatialCIMIS.R script.  Blanks filled on multiple days in data_QA_QC.R.  Now, no missing data except for cell 148533

#get doys [days of years] for the model and ensure SpatialCIMIS coverages match
if (length(U2$DOY)==length(RHmin$DOY) & length(U2$DOY)==length(ETo$DOY)){
  doys_model <- U2$DOY
  print('Temporal coverages match in Spatial CIMIS.')
} else { print('There are differing temporal coverages in the Spatial CIMIS data.')
}
#get precip and spatial CIMIS data to same temporal end point
last_date <- ETo$dates[nrow(ETo)]
PRISMprecip <- PRISMprecip[1:which(PRISMprecip$dates==last_date), ]

#trim the PRISM data to the Spatial CIMIS temporal coverage

crop_parameters_define <- function(crop_parameters) {
  bloom_date <- strptime(paste0(as.character(crop_parameters$bloom_mo), '/', as.character(crop_parameters$bloom_day)), '%m/%d')
  crop_parameters$Jdev <- as.integer(format.Date(bloom_date, '%j'))
  crop_parameters$Jmid <- crop_parameters$Jdev+crop_parameters$Ldev
  crop_parameters$Jlate <- crop_parameters$Jmid+crop_parameters$Lmid
  crop_parameters$Jharv <- crop_parameters$Jlate+crop_parameters$Llate
  return(crop_parameters)
}
crop_parameters <- crop_parameters_define(crop_parameters)

Kcb <- function(doy, parameters, crop) { #find standard value of Kcb by day of year relative to three reference Kcb points defined by crop parameters
  parameters <- crop_parameters[which(crop_parameters$crop==crop), ]
  ifelse(doy < parameters[['Jdev']], parameters[['Kcb_ini']], ifelse(doy < parameters[['Jmid']], parameters[['Kcb_ini']] + (doy-parameters[['Jdev']])/parameters[['Ldev']]*(parameters[['Kcb_mid']]-parameters[['Kcb_ini']]), ifelse(doy < parameters[['Jlate']], parameters[['Kcb_mid']], ifelse(doy < parameters[['Jharv']], parameters[['Kcb_mid']]+(doy-parameters[['Jlate']])/parameters[['Llate']]*(parameters[['Kcb_end']]-parameters[['Kcb_mid']]), parameters[['Kcb_ini']]))))
}

#adjust standard Kcb values by daily climate according to equation #5 in Allen et al. 2005 and calculate Kc max at the same time
Kcb_adj <- function(Kcb_daily, crop_parameters, crop, U2, RHmin, SpCIMIScell){
  column_name <- paste0('cell_', as.character(SpCIMIScell))
  height <- crop_parameters$height[which(crop_parameters$crop==crop)]
  U2_daily <- U2[ ,which(colnames(U2)==column_name)]
  RHmin_daily <- RHmin[ ,which(colnames(RHmin)==column_name)]
  Kcb_climate_adj <- Kcb_daily + (0.04*(U2_daily-2)-0.004*(RHmin_daily-45))*(height/3)^0.3
  Kcb_climate_adj[which(Kcb_climate_adj<0)] <- 0
  Kc_max <- pmax(1.2+(0.04*(U2_daily-2)-0.004*(RHmin_daily-45))*(height/3)^0.3, Kcb_climate_adj+0.05)
  return(as.data.frame(cbind(Kcb_climate_adj, Kc_max)))
}
#test the Kcb functions
Kcb_std <- Kcb(doys_model, crop_parameters, 'almond_mature') #this will be substituted with a crop code
plot(Kcb_std, type='l')
Kcb_df <- Kcb_adj(Kcb_std, crop_parameters, 'almond_mature', U2, RHmin, 86833) #last number is the Spatial CIMIS cell number, which will be retrieved from the model scaffold
plot(Kcb_df$Kcb_climate_adj, type='l')
plot(Kcb_df$Kc_max, type='p')

#calculate fraction of cover ('fc') for specific day of year relative to three points defined in crop parameters
fc_calc <- function(doy, parameters, crop) {
  parameters <- crop_parameters[which(crop_parameters$crop==crop), ]
  ifelse(doy < parameters[['Jdev']], parameters[['fc_ini']], ifelse(doy < parameters[['Jmid']], parameters[['fc_ini']] + (doy-parameters[['Jdev']])/parameters[['Ldev']]*(parameters[['fc_mid']]-parameters[['fc_ini']]), ifelse(doy < parameters[['Jlate']], parameters[['fc_mid']], ifelse(doy < parameters[['Jharv']], parameters[['fc_mid']]+(doy-parameters[['Jlate']])/parameters[['Llate']]*(parameters[['fc_end']]-parameters[['fc_mid']]), parameters[['fc_ini']]))))
}
#test the fc_calc function
fc_alldays <- fc_calc(doys_model, crop_parameters, 'almond_mature')
plot(fc_alldays, type='l')

#TO-DO: implement alternative fc calculation in accordance with Eq. 11 from Allen et al. 2005: ((Kcb-Kcmin)/(Kcmax-Kcmin))^(1+0.5*h).  However, this produced a strange result in spreadsheet model for almonds, where increasing h decreases fc.

#calculate fraction of soil wetted by both irrigation and precipitation and exposed to rapid drying (fewi) and fraction of soil exposed to rapid drying and is wetted by precipitation only (fewp).  These are dependent upon fraction of cover calculation above
fw_select <- function(irr_parameters, irr_type) {
  irr_parameters$fw[which(irr_parameters$irrigation_type==irr_type)]
}
fw <- fw_select(irrigation_parameters, "Microspray, orchards")
fewi_calc <- function(fc, fw) { #see p.147 of FAO-56 Chp.7 re: drip
  fewi_temp <- pmin(1-fc,fw)
  fewi_temp[which(fewi_temp<0.01)] <- 0.01 #lower limit on fewi for numeric stability
  fewi_temp[which(fewi_temp>1)] <- 1 #upper limit on fewi for numeric stability
  return(fewi_temp)
}
print(irrigation_parameters$irrigation_type)
fewi <- fewi_calc(fc_alldays, fw)
plot(fewi, type='l')

fewp_calc <- function(fc, fewi) {
  fewp_temp <- 1-fc-fewi
  fewp_temp[which(fewp_temp < 0.01)] <- 0.01 #lower limit on fewp for numeric stability
  fewp_temp[which(fewp_temp>1)] <- 1 #upper limit on fewp for numeric stability
  return(fewp_temp)
}
fewp <- fewp_calc(fc_alldays, fewi_alldays)
plot(fewp, type='l')
plot(fewp + fewi, type='l')
summary(fewp + fewi)

#calculate Kri and Krp; now dependent on daily soil water balance
#Kri=(TEW-De,j-1)/(TEW-REW) (eqn. 22)
#Krp=(TEW-Dep,j-1)/(TEW-REW) (eqn. 23)
#this is example for model code 233269: PRISM cell=365380; SpatialCIMIS cell=86833; SSURGO cokey=12753827; crop=almonds
i <- 1
model.length <- nrow(ETo)
Dei.initial <- numeric(length = model.length)
Kri <- numeric(length = model.length)
Kei <- numeric(length = model.length)
Ei <- numeric(length = model.length)
DPei <- numeric(length = model.length)
Dei.end <- numeric(length = model.length)
Dep.initial <- numeric(length = model.length)
Krp <- numeric(length = model.length)
Kep <- numeric(length = model.length)
Ep <- numeric(length = model.length)
DPep <- numeric(length = model.length)
Dep.end <- numeric(length = model.length)
W <- numeric(length = model.length)
Kc.ns <- numeric(length = model.length)
ETc.ns <- numeric(length = model.length)
Dr.initial <- numeric(length = model.length)
Ir <- numeric(length = model.length)
DPr <- numeric(length = model.length)
Ks <- numeric(length = model.length)
Kc.adj <- numeric(length = model.length)
Dr.end <- numeric(length = model.length)
ETc.adj <- numeric(length = model.length)
P <- PRISMprecip[ ,which(colnames(PRISMprecip)=='cell_365380')] #this can be automated based on the model_scaffold.csv that defines all the spatial parameters
ETo.model <- ETo[ ,which(colnames(ETo)=='cell_86833')]
REW.parameter <- 8
TEW_calc <- function(ThetaFC, ThetaWP, Ze, REW) { #Ze is depth of upper soil layer where evaporation occurs in meters
  result <- 1000 * (ThetaFC - 0.5 * ThetaWP) * Ze
  if (result < REW) {
    stop(print('Hay una problema con TEW')) #could change to next and print or save model code
  } 
  return(result)
}
TEW.parameter <- TEW(0.336, 0.211, 0.1, REW.parameter)
# irrig.prevday <- function(irrig, i) {
#   if (i==1) {
#     0
#   } else {
#     irrig[i-1]
#   }
# }
Dep.initial.calc <- function(TEW, TEW_fraction, Dep, P, i) {
  if (i==1) {
    TEW*TEW_fraction #this is an initial estimate of the water balance for the exposed surface soil where irrigation is not applied that assumes all daily precip occurs early in the morning so as to estimate Ke and Kr
  } else {
    max(Dep[i-1] - P[i], 0)
  }
}
Dep.initial[i] <- Dep.initial.calc(TEW.parameter, 0.5, Dep, P, i)
Dei.initial.calc <- function(TEW, TEW_fraction, Dei, P, Ir, fw, i) {
  if (i==1) {
    TEW*TEW_fraction
  } else {
    max(Dei[i-1] - P[i] - Ir[i-1] / fw, 0) #have to use irrigation from previous day because current day irrigation decision is dependent on this calc
  }
}
Dei.initial[i] <- Dei.initial.calc(TEW.parameter, 0.5, Dei, P, Ir, fw, i)
Kr_calc <- function(TEW, REW, De.initial) { #can be used to calculate Kri or Krp
  max(if(De.initial < REW) { #could initialize this in vector above
    1
  } else {
    (TEW - De.initial) / (TEW - REW)
  } , 0)
}
#test Krp calc
Kri[i] <- Kr_calc(TEW_parameter, REW_parameter, Dep.initial.calc(TEW.parameter, 0.5, Dep, P, i))
#test Kri calc
Krp[i] <- Kr_calc(TEW_parameter, REW_parameter, Dei.initial.calc(TEW.parameter, 0.5, Dei, P, Ir, fw, i))
W_calc <- function(TEW, Dei, Dep, fewp, fewi, i) {
  1 / (1 + (fewp[i] / fewi[i]) * (TEW - Dep[i]) / (TEW - Dei[i]))
}
W[i] <- W_calc(TEW_parameter, Dei, Dep, fewp, fewi, i)
Kei_calc <- function(Kri, W, Kcmax, Kcb, fewi, i) {
  min(Kri[i] * W[i] * (Kcmax[i] - Kcb[i]), fewi[i] * Kcmax[i])
}
Kei[i] <- Kei_calc(Kri, W, Kcb_df$Kc_max, Kcb_df$Kcb_climate_adj, fewi, i)
Kep_calc <- function(Krp, W, Kcmax, Kcb, fewp, i) {
  min(Krp[i] * (1 - W[i]) * (Kcmax[i]- Kcb[i]), fewp[i] * Kcmax[i])
}
Kep[i] <- Kep_calc(Krp, W, Kcb_df$Kc_max, Kcb_df$Kcb_climate_adj, fewp, i)
Ep_calc <- function(ETo, Kep, i) {
  ETo[i] * Kep[i]
}
Ei_calc <- function(ETo, Kei, i) {
  ETo[i] * Kei[i]
}
Ep[i] <- Ep_calc(ETo.model, Kep, i)
Ei[i] <- Ei_calc(ETo.model, Kei, i)
DPep_calc <- function(P, Dep.initial, i) {
  max(P[i] - Dep.initial[i], 0)
}
DPep[i] <- DPep_calc(P, Dep.initial, i)
Dep.end.calc <- function(Dep.initial, P, Ep, fewp, DPep, i) {
  Dep.initial[i] - P[i] + Ep[i] / fewp[i] + DPep[i]
}
Dep.end[i] <- Dep.end.calc(Dep.initial, P, Ep, fewp, DPep, i)
#now calculate irrigation needs to be able to finish water balance for fewi portion of uper layer
DPei.calc <- function(P, Ir, fw, Dei.initial, i) {
  max(if (i==1) {
    P[i] - Dei.initial[i]
  } else {
    P[i] + Ir[i-1] / fw - Dei.initial[i]
  }, 0)
}
DPei[i] <- DPei.calc(P, Ir, fw, Dei.initial, i)
Kc.ns.calc <- function(Kcb, Kei, Kep, i) {
  Kcb[i] + Kei[i] + Kep[i]
}
Kc.ns[i] <- Kc.ns.calc(Kcb_df$Kcb_climate_adj, Kei, Kep, i)
ETc.ns.calc <- function(Kc.ns, ETo, i) {
  Kc.ns[i] * ETo[i]
}
ETc.ns[i] <- ETc.ns.calc(Kc.ns, ETo.model, i)
#Dr.initial is next

Kc.act.calc <- function(Ks, Kcb, Ke) {#equation 4 in Allen et al. 2005
  Ks[i] * Kcb+Ke
}
ETc.act.calc <- function(Kc_act, ETo) {#equation 3 in Allen et al. 2005
  Kc_act*ETo
}


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
U2_mid <- function(U2, col_index, Jmid, Jlate) {
  U2_temp <- U2[which(U2$DOY >= Jmid & U2$DOY <= Jlate), ]
  result <- as.data.frame(tapply(U2_temp[,col_index], U2_temp$year, mean)) #or could do all cells via this arg to replace col_index <- 6:ncol(U2_temp)
  colnames(result) <- colnames(U2)[col_index]
  return(result) #rownames are years, search by which(rownames(result)=='year of interest')
}
#check function
U2_mid_allyrs <- U2_mid(U2, 6, almond_parameters$Jmid, almond_parameters$Jlate)

RHmin_mid <- function(RHmin, col_index, Jmid, Jlate) {
  RHmin_temp <- RHmin[which(RHmin$DOY >= Jmid & RHmin$DOY <= Jlate), ]
  result <- as.data.frame(tapply(RHmin_temp[,col_index], RHmin_temp$year, mean))
  colnames(result) <- colnames(RHmin)[col_index]
  return(result)
}
RHmin_mid_allyrs <- RHmin_mid(RHmin, 6, almond_parameters$Jmid, almond_parameters$Jlate)

U2_end <- function(U2, col_index, Jlate, Jharv) {
  U2_temp <- U2[which(U2$DOY >= Jlate & U2$DOY <= Jharv), ]
  result <- as.data.frame(tapply(U2_temp[,col_index], U2_temp$year, mean)) #or could do all cells via this arg to replace col_index <- 6:ncol(U2_temp)
  colnames(result) <- colnames(U2)[col_index]
  return(result) #rownames are years, search by which(rownames(result)=='year of interest')
}
#check function
U2_end_allyrs <- U2_end(U2, 6, almond_parameters$Jlate, almond_parameters$Jharv)

RHmin_end <- function(RHmin, col_index, Jlate, Jharv) {
  RHmin_temp <- RHmin[which(RHmin$DOY >= Jlate & RHmin$DOY <= Jharv), ]
  result <- as.data.frame(tapply(RHmin_temp[,col_index], RHmin_temp$year, mean))
  colnames(result) <- colnames(RHmin)[col_index]
  return(result)
}
RHmin_end_allyrs <- RHmin_mid(RHmin, 6, almond_parameters$Jlate, almond_parameters$Jharv)

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