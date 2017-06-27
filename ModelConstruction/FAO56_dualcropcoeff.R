#script to implement the FAO56 dual crop coefficient ET routine
modelscaffoldDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/results/model_scaffold' #location of input data
setwd(modelscaffoldDir)
crop_parameters <- read.csv('crop_parameters.csv', stringsAsFactors = F) #necessary parameters by crop to run the FAO56 dual crop coefficient ET model
PRISMprecip <- read.csv('PRISM_precip_data.csv') #this is a daily summary of precip from 10/1/2003-6/25/17 from 'free' daily PRISM 4km resolution for cells of interest in California, created in download_PRISM.R script (from 6/26/17 download); blanks checked for in 'data_QA_QC.R'
U2 <- read.csv('SpatialCIMIS_U2_rounded.csv', stringsAsFactors = F) #this is a daily summary of wind data from download of spatial CIMIS data, created in spatialCIMIS.R script.  No missing data except for cell 148533
RHmin <- read.csv('SpatialCIMIS_minRH_rounded_QCpass.csv', stringsAsFactors = F) #this is a daily summary of minimum relative humidity, estimated from download of spatial CIMIS Tdew and Tmax data, created in spatialCIMIS.R script.  Blanks filled on "12_08_2011" in data_QA_QC.R.  Now, no missing data except for cell 148533
ETo <- read.csv('SpatialCIMIS_ETo_rounded_QCpass.csv', stringsAsFactors = F) #this is a daily summary of reference ET from download of spatial CIMIS data, created in spatialCIMIS.R script.  Blanks filled on multiple days in data_QA_QC.R.  Now, no missing data except for cell 148533

#get doys for the model
ifelse(length(U2$DOY)==length(RHmin$DOY) & length(U2$DOY)==length(ETo$DOY), doys_model <- U2$DOY, print('There are differing temporal coverages in the Spatial CIMIS data.'))
#trim the PRISM data

crop_parameters_define <- function(crop_parameters) {
  #crop <- 'almond'
  # crop_parameters_selection <- crop_parameters[which(crop_parameters$crop==crop),]
  bloom_date <- strptime(paste0(as.character(crop_parameters$bloom_mo), '/', as.character(crop_parameters$bloom_day)), '%m/%d')
  # height <-crop_parameters$height[crop_row]
  # fc_ini <- crop_parameters$fc.ini[crop_row]
  # fc_mid <- crop_parameters$fc.mid[crop_row]
  # fc_end <- crop_parameters$fc.end[crop_row]
  # Kcb_ini <- crop_parameters$Kcb.ini[crop_row]
  # Kcb_mid <- crop_parameters$Kcb.mid[crop_row]
  # Kcb_end <- crop_parameters$Kcb.end[crop_row]
  crop_parameters$Jdev <- as.integer(format.Date(bloom_date, '%j'))
  crop_parameters$Jmid <- crop_parameters$Jdev+crop_parameters$Ldev
  crop_parameters$Jlate <- crop_parameters$Jmid+crop_parameters$Lmid
  crop_parameters$Jharv <- crop_parameters$Jlate+crop_parameters$Llate
  # return_list <- list(fc_ini, fc_mid, fc_end, Kcb_ini, Kcb_mid, Kcb_end, Jdev, Jmid, Jlate, Jharv, height)
  # names(return_list) <- c('fc_ini', 'fc_mid', 'fc_end', 'Kcb_ini', 'Kcb_mid', 'Kcb_end', 'Jdev', 'Jmid', 'Jlate', 'Jharv', 'height')
  return(crop_parameters)
}
crop_parameters <- crop_parameters_define(crop_parameters)

ETc_act <- function(Kc_act, ETo) {#equation 3 in Allen et al. 2005
  Kc_act*ETo
} 
Kc_act <- function(Ks, Kcb, Ke) {#equation 4 in Allen et al. 2005
  Ks*Kcb+Ke
} 

Kcb <- function(doy, parameters, crop) { #find standard value of Kcb by doy relative to three reference Kcb points defined by crop
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
  Kc_max <- pmax((0.04*(U2_daily-2)-0.004*(RHmin_daily-45))*(height/3)^0.3, Kcb_climate_adj+0.05)
  return(cbind(Kcb_climate_adj, Kc_max))
}
#test the Kcb functions
Kcb_std <- Kcb(doys_model, crop_parameters, 'almond_mature') #this will be substituted with a code
Kcb_df <- Kcb_adj(Kcb_std, crop_parameters, 'almond_mature', U2, RHmin, 86833) #last number is the Spatial CIMIS cell number, which will be retrieved from the model scaffold

fc_calc <- function(doy, parameters, crop) { #find standard value of Kcb by doy relative to three reference Kcb points defined by crop
  parameters <- crop_parameters[which(crop_parameters$crop==crop), ]
  ifelse(doy < parameters[['Jdev']], parameters[['fc_ini']], ifelse(doy < parameters[['Jmid']], parameters[['fc_ini']] + (doy-parameters[['Jdev']])/parameters[['Ldev']]*(parameters[['fc_mid']]-parameters[['fc_ini']]), ifelse(doy < parameters[['Jlate']], parameters[['fc_mid']], ifelse(doy < parameters[['Jharv']], parameters[['fc_mid']]+(doy-parameters[['Jlate']])/parameters[['Llate']]*(parameters[['fc_end']]-parameters[['fc_mid']]), parameters[['fc_ini']]))))
}
#test the fc_calc function
fc_allyrs <- fc_calc(doys_model, crop_parameters, 'almond_mature')

#upper soil water balance


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

#alternative approach using means for different periods of the Kc curve
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