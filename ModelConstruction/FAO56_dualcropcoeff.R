#TODO: #1 Track irrigation and precip T and E separately
       #2 Read "Implementing the Dual Crop Coefficient Approach in Interactive
        # Software"
       #3 Revise "SSURGO_processing_awc_r.R"
       #4 
#script to implement the FAO56 dual crop coefficient ET routine
modelscaffoldDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/model_scaffold/run_model/July2017' #location of input data
resultsDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/results/trialJul_2017'
rounding_digits <- 3
setwd(modelscaffoldDir)
irrigation.parameters <- read.csv('irrigation_parameters.csv', stringsAsFactors = F)
crop.parameters <- read.csv('crop_parameters.csv', stringsAsFactors = F) #necessary parameters by crop to run the FAO56 dual crop coefficient ET model
P.df <- read.csv('PRISM_precip_data.csv', stringsAsFactors = F) #this is a daily summary of precip from 10/1/2003-6/25/17 from 'free' daily PRISM 4km resolution for cells of interest in California, created in download_PRISM.R script (from 6/26/17 download); blanks checked for in 'data_QA_QC.R'
U2.df <- read.csv('SpatialCIMIS_U2_rounded.csv', stringsAsFactors = F) #this is a daily summary of wind data from download of spatial CIMIS data, created in spatialCIMIS.R script.  No missing data except for cell 148533
RHmin.df <- read.csv('SpatialCIMIS_minRH_rounded_QCpass.csv', stringsAsFactors = F) #this is a daily summary of minimum relative humidity, estimated from download of spatial CIMIS Tdew and Tmax data, created in spatialCIMIS.R script.  Blanks filled on "12_08_2011" in data_QA_QC.R.  Now, no missing data except for cell 148533
ETo.df <- read.csv('SpatialCIMIS_ETo_rounded_QCpass.csv', stringsAsFactors = F) #this is a daily summary of reference ET from download of spatial CIMIS data, created in spatialCIMIS.R script.  Blanks filled on multiple days in data_QA_QC.R.  Now, no missing data except for cell 148533
model.scaffold <- read.csv('model_scaffold_majcomps.csv', stringsAsFactors = F)
model.scaffold <- model.scaffold[order(model.scaffold$unique_model_code), ]
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
#need to refine days.no.irr based on ETo data for a given location
DaysNoIrr <- function(P, ETo, Kcb.adjusted, AD, doys.model, years, Jlate, Jharv) {
  df <- data.frame(cbind(Kcb.adjusted, ETo, P, doys.model, years))
  df <- df[which(df$doys.model >= Jlate & df$doys.model <= Jharv), ]
  df$ETcb <- df$Kcb.adjusted * df$ETo
  daily.ETcb <- tapply(df$ETcb, df$doys.model, mean)
  daily.P <- tapply(df$P, df$doys.model, mean)
  daily.WB <- daily.ETcb - daily.P
  for (i in 1:length(daily.WB)) {
    if (sum(daily.WB) < AD) {
      return(Jharv - as.integer(names(daily.WB[1])))
    } else {
      daily.WB <- daily.WB[-1]
    }
  }
} #create a fixed date based on climate and crop
IrCalc <- function(AD, Dr.initial, doys.model, Jdev, Jharv, days.no.irr) {
  if (doys.model[i] < Jdev | doys.model[i] > Jharv - days.no.irr) {
    return(0)
  } else if (Dr.initial[i] > AD) {
    return(Dr.initial[i])
  } else if (doys.model[i] == Jharv - days.no.irr) {
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

#results function to summarize each model's results
#find time to first and last irrigation
IrDateCalc <- function(df) { #as.Date('1900-01-01) is a proxy for NA
  first.irr.index <- head(which(df$Ir > 0 & df$dates < as.Date(paste0(as.character(df$years[1]), '-07-01'))), 1)
  last.irr.index <- tail(which(df$Ir > 0 & df$dates > as.Date(paste0(as.character(df$years[1]), '-07-01'))), 1)
  if (length(first.irr.index) == 0 & length(last.irr.index) == 0) {
    data.frame(Irr.1=as.Date('1900-01-01'), Irr.Last=as.Date('1900-01-01'))
  }
  else if (length(first.irr.index) == 0) {
    data.frame(Irr.1=as.Date('1900-01-01'), Irr.Last=df$dates[last.irr.index])
  }
  else if (length(last.irr.index) == 0) {
    data.frame(Irr.1=df$dates[first.irr.index], Irr.Last=as.Date('1900-01-01'))
  } else {
      data.frame(Irr.1=df$dates[first.irr.index], Irr.Last=df$dates[last.irr.index])
  }
}
do.call(rbind, lapply(split(result, result$years), IrDateCalc))

#calculate Green Water utilized
#this asssumes that residual irrigation water storage from previous fall will not contribute to the following year's growing season ET for the purpose of calculating green water utilization.  However a correction is applied to the growing season ET for residual irrigation water storage to correctly estimate green water utilization within the same year.
WaterBalanceCalc <- function(df) { #concept is to run by year on a data.frame trimmed to Jdev-Jharv each year
  first_irr_index <- which(df$Ir>0)[1]
  last_irr_index <- which(df$Ir > 0)[length(which(df$Ir>0))]
  jharv_index <- which(df$doys.model==Jharv)
  jdev_index <- which(df$doys.model==Jdev)
  if (df$doys.model[1] > Jdev | df$doys.model[nrow(df)] < Jharv) { #if there is not a complete growing season, don't report any data
    if (length(jharv_index)==0) { #data begins after leaf-drop or ends before leaf-drop; either way, can't get entire season or end season data
      data.frame(RAW.end.season = NA, PAW.end.season = NA, Dr.end.season = NA, P.end.season=NA, Irr.end.storage = NA, GW.ET.growing = NA, Irr.app.total = NA, Irr.app.last = NA, ET.growing = NA, E.growing = NA, T.growing = NA)
    }
    else if (length(last_irr_index)==0) { #implies no data when last irrigation occurred but there is data at Jharv; thus can get end of season storage data
      data.frame(RAW.end.season = max(AD - df$Dr.end[jharv_index], 0), PAW.end.season = max(PAW - df$Dr.end[jharv_index], 0), Dr.end.season = df$Dr.end[jharv_index], P.end.season = NA, Irr.end.storage = NA, GW.ET.growing = NA, Irr.app.total = NA, Irr.app.last = NA, ET.growing = NA, E.growing = NA, T.growing = NA)
    } else { #implies data exists from at least when last irrigation occurred to Jharv, so can get everything but entire growing season data
      data.frame(RAW.end.season = max(AD - df$Dr.end[jharv_index], 0), PAW.end.season = max(PAW - df$Dr.end[jharv_index], 0), Dr.end.season = df$Dr.end[jharv_index], P.end.season = sum(df$P[last_irr_index:jharv_index]), Irr.end.storage = max(AD - df$Dr.end[jharv_index] - sum(df$P[last_irr_index:jharv_index]), 0), GW.ET.growing = NA, Irr.app.total = NA, Irr.app.last = df$Ir[last_irr_index], ET.growing = NA, E.growing = NA, T.growing = NA)
    }
  } else {
    irr_storage <- max(AD - df$Dr.end[jharv_index] - sum(df$P[last_irr_index:jharv_index]), 0)
    data.frame(RAW.end.season = max(AD - df$Dr.end[jharv_index], 0), PAW.end.season = max(PAW - df$Dr.end[jharv_index], 0), Dr.end.season = df$Dr.end[jharv_index], P.end.season = sum(df$P[last_irr_index:jharv_index]), Irr.end.storage = irr_storage, GW.ET.growing = sum(df$ETc.act[jdev_index:jharv_index] - df$Ir[jdev_index:jharv_index]) + irr_storage, Irr.app.total = sum(df$Ir), Irr.app.last = df$Ir[last_irr_index], ET.growing = sum(df$ETc.act[jdev_index:jharv_index]), E.growing = sum(df$Ei[jdev_index:jharv_index], df$Ep[jdev_index:jharv_index]), T.growing = sum(df$ETc.act[jdev_index:jharv_index] - df$Ei[jdev_index:jharv_index] - df$Ep[jdev_index:jharv_index]))
  }
}
do.call(rbind, lapply(split(result, result$years), WaterBalanceCalc))

##this splits the overall results data.frame into subsets by year and then runs the GreenWaterCalc on each subset via lapply.  The result from each year is then bound together via rbind called by do.call; AD minus Dr.end at leaf-drop is the readily available water remaining in storage at leaf-drop; the source of this readily available water is minus precip since the last irrigation is Irr.End.Storage

GreenWaterIrr1Calc <- function(df) { #works on a data.frame split by year
  first_irr_index <- which(df$Ir > 0)[1]
  jdev_index <- which(df$doys.model==Jdev)
  if (df$doys.model[1] > Jdev | is.na(first_irr_index)) {
    data.frame(GW.ET.to.Irr1=NA, GW.E.to.Irr1=NA, GW.T.to.Irr1=NA)
  } else {
      data.frame(GW.ET.to.Irr1 = sum(df$ETc.act[jdev_index:first_irr_index]), GW.E.to.Irr1 = sum(df$Ei[jdev_index:first_irr_index] + df$Ep[jdev_index:first_irr_index]), GW.T.to.Irr1 = sum(df$Kcb.adjusted[jdev_index:first_irr_index] * df$Ks[jdev_index:first_irr_index] * df$ETo[jdev_index:first_irr_index]))
  }
}
do.call(rbind, lapply(split(result, result$years), GreenWaterIrr1Calc))

#determine deep percolation and annual water balance using subsetting by year
DeepPercCalc <- function(df) { #assumes Jharv index is after 10/1
  jan.1.index <- which(df$dates==as.Date(paste0(as.character(df$years[1]), '-01-01')))
  first.irr.index <- head(which(df$Ir > 0 & df$dates < as.Date(paste0(as.character(df$years[1]), '-07-01'))), 1)
  last.irr.index <- tail(which(df$Ir > 0 & df$dates > as.Date(paste0(as.character(df$years[1]), '-07-01'))), 1)
  jul.1.index <- which(df$dates==as.Date(paste0(as.character(df$years[1]), '-07-01')))
  dec.31.index <- which(df$dates==as.Date(paste0(as.character(df$years[1]), '-12-31')))
  if (df$dates[1] > as.Date(paste0(as.character(df$years[1]), '-01-01')) | df$dates[nrow(df)] < as.Date(paste0(as.character(df$years[nrow(df)]), '-12-31'))) { #this means they ain't a whole year's data
    if (length(last.irr.index) == 0 & length(first.irr.index) == 0) { #not sufficient coverage to calc anything
      data.frame(ET.annual = NA, E.annual = NA, T.annual = NA, deep.perc.annual = NA, winter.deep.perc = NA, post.Irr1.deep.perc=NA, fall.deep.perc = NA)
    }
    else if (length(jan.1.index) != 0 & length(first.irr.index) != 0 & length(jul.1.index) == 0) { #winter deep perc only
      data.frame(ET.annual = NA, E.annual = NA, T.annual = NA, deep.perc.annual = NA, winter.deep.perc = sum(df$DPr[jan.1.index:first.irr.index]), post.Irr1.deep.perc=NA, fall.deep.perc = NA)
    }
    else if (length(jan.1.index) != 0 & length(jul.1.index) != 0 & length(last.irr.index) == 0) { #winter and post Irr deep perc
      data.frame(ET.annual = NA, E.annual = NA, T.annual = NA, deep.perc.annual = NA, winter.deep.perc = sum(df$DPr[jan.1.index:first.irr.index]), post.Irr1.deep.perc=sum(df$DPr[(first.irr.index+1):jul.1.index]), fall.deep.perc = NA)
    }
    else if (length(jan.1.index) == 0 & length(first.irr.index) != 0 & length(jul.1.index) != 0 & length(last.irr.index) == 0) { #only Spring deep perc available
      data.frame(ET.annual = NA, E.annual = NA, T.annual = NA, deep.perc.annual = NA, winter.deep.perc = NA, post.Irr1.deep.perc=sum(df$DPr[(first.irr.index+1):jul.1.index]), fall.deep.perc = NA)
    }
    else if (length(jan.1.index) == 0 & length(first.irr.index) != 0 & length(dec.31.index) != 0) { #spring and fall deep perc
      data.frame(ET.annual = NA, E.annual = NA, T.annual = NA, deep.perc.annual = NA, winter.deep.perc = NA, post.Irr1.deep.perc=sum(df$DPr[(first.irr.index+1):jul.1.index]), fall.deep.perc =  sum(df$DPr[last.irr.index:dec.31.index]))
    } else {#fall perc only
        data.frame(ET.annual = NA, E.annual = NA, T.annual = NA, deep.perc.annual = NA, winter.deep.perc = NA, post.Irr1.deep.perc=NA,  fall.deep.perc =  sum(df$DPr[last.irr.index:(dec.31.index-1)]))
    }
  } else { #entire annual coverage available
      data.frame(ET.annual = sum(df$ETc.act), E.annual = sum(df$Ei, df$Ep), T.annual = sum(df$ETc.act, -df$Ei, -df$Ep), deep.perc.annual = sum(df$DPr), winter.deep.perc = sum(df$DPr[jan.1.index:first.irr.index]), post.Irr1.deep.perc = sum(df$DPr[(first.irr.index+1):jul.1.index]), fall.deep.perc = sum(df$DPr[last.irr.index:(dec.31.index)]))
  }
}
do.call(rbind, lapply(split(result, result$years), DeepPercCalc))

#determine green water capture from leaf-drop to flowering
GreenWaterCaptureCalc <- function(df) {
  if (df$doys.model[1] > Jharv | df$doys.model[nrow(df)] < Jmid) {
    return(data.frame(GW.capture.net = NA))
  } else {
      return(data.frame(GW.capture.net = df$Dr.end[which(df$doys.model == Jharv)] - df$Dr.end[which(df$doys.model == Jdev)]))
  }
}
do.call(rbind, lapply(split(result, result$water.year), GreenWaterCaptureCalc))


#all these tasks only done once per run where run is initialzing model and then looping through all rows in model.scaffold and pasting results from all
#ID crop codes
alfalfa_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Alfalfa']
grape_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Grapes']
almond_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Almonds']
walnut_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Walnuts']

#limit model.scaffold to almonds only for now
model.scaffold <- model.scaffold[which(model.scaffold$crop_code==almond_code), ] #80,401 unique crop, soil, and climate combinations for almond (spatially, this is the equivalent of only 7,236 ha)

#E=evaporation
#T=transpiration
#ET=evapotranspriation
#Irr.1=first irrigation of year
#Irr.Last=last irrigation of year
#GW.ET.to.Irr1=ET sourced from green water from flowering to first irrigation (should subtract previous year's carryover irrigation storage)
#ET.growing=total ET from flowering (Jdev) to leaf-drop (Jharv)
#ET.WY=total annual ET on water-year basis
#deep.perc is annual deep percolation ()
#GW.capture.net is net change in soil root zone depletion from Jharv (leaf drop) to Jdev (flowering and development)
#end.season.Dr is soil root zone depletion at Jharv (leaf drop)

#get doys [days of years] for the model and ensure SpatialCIMIS coverages match
if (length(U2.df$DOY)==length(RHmin.df$DOY) & length(U2.df$DOY)==length(ETo.df$DOY)) {
  doys.model <- U2.df$DOY
  dates <- as.Date(U2.df$dates, format='%m_%d_%Y')
  days <- U2.df$day
  months <- U2.df$month
  years <- U2.df$year
  water.year <- years
  water.year[which(months >= 10)] <- years[which(months >= 10)] + 1
  print('Temporal coverages match in Spatial CIMIS.')
} else {
  print('There are differing temporal coverages in the Spatial CIMIS data.')
}
model.length <- nrow(ETo.df)
#get precip and spatial CIMIS data to same temporal end point
last.date <- ETo.df$dates[nrow(ETo.df)]
P.df <- P.df[1:which(P.df$dates==last.date), ]
crop.parameters <- CropParametersDefine(crop.parameters)

#irrigation and crop specific paramters outside the loop, since only almonds are modeled now
cropname <- 'almond.mature'
scenario.name <- 'almonds_majcomps/scenario_1m_50AD'
if (!dir.exists(file.path(resultsDir, scenario.name))) {
  dir.create(file.path(resultsDir, scenario.name))
}
AD.percentage <- 50 #crop and scenario dependent
root_depth <- '1.0m'
paw.var <- 'z1.0m_cmH2O_modified_comp'

#initialization assumptions
first_irrigation <- 0
TEW.fraction <- 0.5

#make a results data.frame
model.length.yrs <- max(ETo.df$year) - min(ETo.df$year) + 1 #data starts 10/2003
paw.vector <- model.scaffold[[paw.var]]
model.scaffold2 <- model.scaffold[ ,-12:-21]
model.scaffold2$paw <- paw.vector
colnames(model.scaffold2)[15] <- paw.var
model.scaffold.results <- model.scaffold2[rep(seq.int(1, nrow(model.scaffold2)), model.length.yrs), 1:ncol(model.scaffold2)] #makes a new data.frame with each row repeated model.length.yrs number of times
model.scaffold.results <- model.scaffold.results[order(model.scaffold.results$unique_model_code, model.scaffold.results$cokey), ] #for some reason it is produced out of order
model.scaffold.results <- data.frame(model.scaffold.results, Model.Year=rep(seq(from=min(ETo.df$year), to=max(ETo.df$year), by=1), times=nrow(model.scaffold)), Irr.1=as.Date('1900-01-01'), Irr.Last=as.Date('1900-01-01'), RAW.end.season=NA, PAW.end.season=NA, Dr.end.season=NA, P.end.season=NA, Irr.end.storage=NA, GW.ET.growing=NA, Irr.app.total=NA, Irr.app.last=NA, ET.growing=NA, E.growing=NA, T.growing=NA, GW.ET.to.Irr1=NA, GW.E.to.Irr1=NA, GW.T.to.Irr1=NA, ET.annual=NA, E.annual=NA, T.annual=NA, deep.perc.annual=NA, winter.deep.perc=NA, post.Irr1.deep.perc=NA, fall.deep.perc=NA, GW.capture.net=NA)
model.scaffold.results$unique_model_code_final <- paste0(as.character(model.scaffold.results$unique_model_code), as.character(model.scaffold.results$cokey)) #need to use as.character to preserve integrity of long integers
model.scaffold.results[which(model.scaffold.results$unique_model_code==100058 & model.scaffold.results$cokey==13094564), ]
rm(model.scaffold2)

Kcb.std <- KcbDefine(doys.model, crop.parameters, cropname) #this will be substituted with a crop code
fc <- fcCalc(doys.model, crop.parameters, cropname) #TO-DO: implement alternative fc calculation in accordance with Eq. 11 from Allen et al. 2005: ((Kcb-Kcmin)/(Kcmax-Kcmin))^(1+0.5*h).  However, this produced a strange result in spreadsheet model for almonds, where increasing h decreases fc.
Jdev <- crop.parameters$Jdev[which(crop.parameters$crop==cropname)]
Jmid <- crop.parameters$Jmid[which(crop.parameters$crop==cropname)]
Jlate <- crop.parameters$Jlate[which(crop.parameters$crop==cropname)]
Jharv <- crop.parameters$Jharv[which(crop.parameters$crop==cropname)]
fw <- fwSelect(irrigation.parameters, "Microspray, orchards")
fewi <- fewiCalc(fc, fw)
fewp <- fewpCalc(fc, fewi)

#loop through all rows of model scaffold but only do these operations once for each row
rows.to.sample <- sample(1:nrow(model.scaffold), 0.01*nrow(model.scaffold))
save.times <- seq(from=10000, to=nrow(model.scaffold), by=10000)
for (n in 1:nrow(model.scaffold)) {
  model.code <- model.scaffold$unique_model_code[n]
  PAW <- model.scaffold[[paw.var]][n]*10
  AD <- (AD.percentage/100)*PAW #converts AD in cm to mm; can redefine this script based on PAW
  cokey <- model.scaffold$cokey[n]
  REW.parameter <- model.scaffold$REW[n]
  TEW.parameter <- model.scaffold$TEW[n]
  if (is.na(AD) | is.na(REW.parameter) | is.na(TEW.parameter)) {
    next(print(paste('Soils data is missing for cokey ', as.character(cokey)))) #TO-DO: write this result to separate file of NAs
  }
  if (AD==0 | TEW.parameter==0 | REW.parameter==0) {
    next(print(paste('AD is 0 for cokey ', as.character(cokey)))) ##TO-DO: write this result to separate file of NAs
  }
  #identify climatic and soil parameters
  spCIMIScell <- model.scaffold$CIMIScellnumber[n]
  PRISMcell <- model.scaffold$PRISMcellnumber[n]
  comppctr <- model.scaffold$comppct_r[n]
  mukey <- model.scaffold$mukey[n]
  P <- P.df[ , which(colnames(P.df)==paste0('cell_', as.character(PRISMcell)))]
  ETo <- ETo.df[ , which(colnames(ETo.df)==paste0('cell_', as.character(spCIMIScell)))]
  U2 <- U2.df[ ,which(colnames(U2.df)==paste0('cell_', as.character(spCIMIScell)))]
  RHmin <- RHmin.df[ ,which(colnames(RHmin.df)==paste0('cell_', as.character(spCIMIScell)))]
  Kcb.df <- KcbAdj(Kcb.std, crop.parameters, cropname, U2, RHmin)
  Kcb.adjusted <- Kcb.df$Kcb.climate.adj
  days.no.irr <- DaysNoIrr(P, ETo, Kcb.adjusted, AD, doys.model, years, Jlate, Jharv)
  Kcmax <- Kcb.df$Kc.max
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
  Ir[i] <- IrCalc(AD, Dr.initial, doys.model, Jdev, Jharv, days.no.irr)
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
    Ir[i] <- IrCalc(AD, Dr.initial, doys.model, Jdev, Jharv, days.no.irr)
    DPr[i] <- DPrCalc(P, Ir, ETc.ns, Dr.end)
    Ks[i] <- KsCalc(Dr.initial, PAW, AD)
    Kc.act[i] <- KcactCalc(Ks, Kcb.adjusted, Kei, Kep)
    Dr.end[i] <- DrEndCalc(Dr.end, P, Ir, Kc.act, ETo, DPr)
    ETc.act[i] <- ETcactCalc(Kc.act, ETo) #could take this out of loop
  }
  result <- data.frame(dates, months, days, years, water.year, doys.model, P, ETo, RHmin, U2, lapply(X=list(Kcb.std=Kcb.std, Kcb.adjusted=Kcb.adjusted, Kcmax=Kcmax, fceff=fc, fw=fw, fewi=fewi, fewp=fewp, Dei.initial=Dei.initial, Dep.initial=Dep.initial, Kri=Kri, Krp=Krp, W=W, Kei=Kei, Kep=Kep, Ei=Ei, Ep=Ep, Dpei=DPei, DPep=DPep, Dei.end=Dei.end, Dep.end=Dep.end, Kc.ns=Kc.ns, ETc.ns=ETc.ns, Dr.initial=Dr.initial, Ir=Ir, DPr=DPr, Ks=Ks, Kc.act=Kc.act, ETc.act=ETc.act, Dr.end=Dr.end), round, digits=rounding_digits))
  model.scaffold.results[which(model.scaffold.results$unique_model_code==model.code & model.scaffold.results$cokey == cokey), 17:40] <- merge(cbind(do.call(rbind, lapply(split(result, result$years), IrDateCalc)), do.call(rbind, lapply(split(result, result$years), WaterBalanceCalc)), do.call(rbind, lapply(split(result, result$years), GreenWaterIrr1Calc)), do.call(rbind, lapply(split(result, result$years), DeepPercCalc))), do.call(rbind, lapply(split(result, result$water.year), GreenWaterCaptureCalc)), by="row.names", all=TRUE)[ ,2:25]
  print(n)
  # if (n %in% rows.to.sample) {
  #   setwd(file.path(resultsDir, scenario.name))
  #   write.csv(result, paste0(cropname, root_depth, 'AD', as.character(AD.percentage), '_', as.character(model.code), '_', as.character(cokey), '_', Sys.Date(), '.csv'), row.names=FALSE)
  # }
  if (n %in% save.times) {
    setwd(file.path(resultsDir, scenario.name))
    write.csv(model.scaffold.results, paste0(cropname, root_depth, 'AD', as.character(AD.percentage), '_FAO56results.csv'), row.names=FALSE)
  } else {next}
}
#started at 4:15 PM 7/26
#re-ran at 
#test of 100 models took 143.87 seconds
#so, for all almond major components x climate (80401 unique models), 
#one scenario will take 32 hours
mean(model.scaffold.results$GW.ET.growing, na.rm=TRUE)
hist(model.scaffold.results$GW.ET.growing)
hist(tapply(model.scaffold.results$GW.ET.growing, model.scaffold.results$Model.Year, mean, na.rm=TRUE))
hist(tapply(model.scaffold.results$Irr.app.total, model.scaffold.results$Model.Year, mean, na.rm=TRUE))
plot(tapply(model.scaffold.results$Irr.app.total, model.scaffold.results$Model.Year, mean, na.rm=TRUE), tapply(model.scaffold.results$GW.ET.growing, model.scaffold.results$Model.Year, mean, na.rm=TRUE))
plot(tapply(model.scaffold.results$ET.growing, model.scaffold.results$Model.Year, mean, na.rm=TRUE), tapply(model.scaffold.results$Irr.app.total, model.scaffold.results$Model.Year, mean, na.rm=TRUE))
plot(2003:2017, tapply(model.scaffold.results$ET.growing, model.scaffold.results$Model.Year, mean, na.rm=TRUE))
(240/25.4)/12*1000000
plot(tapply(model.scaffold.results$z1.0m_cmH2O_modified_comp, model.scaffold.results$unique_model_code_final, mean, na.rm=TRUE), tapply(model.scaffold.results$GW.ET.growing, model.scaffold.results$unique_model_code_final, mean, na.rm=TRUE))

#for writing overall results to disk
setwd(file.path(resultsDir, scenario.name))
output <- model.scaffold.results #need additional model_code to get these in order
output <- output[order(output$unique_model_code_final, output$Model.Year), ]
write.csv(output, paste0(cropname, root_depth, 'AD', as.character(AD.percentage), '_FAO56results.csv'), row.names = FALSE)
#order(soil_comp_data$mukey, soil_comp_data$comppct_r, soil_comp_data$cokey, decreasing=c(F, T, F))
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

#few stats
sum(is.na(model.scaffold.results$z1.0m_cmH2O_modified_comp)) #32520
sum(is.na(model.scaffold.results$GW.ET.growing)) - 2*nrow(model.scaffold) #each model scaffold row will produce 2 NAs for GW ET growing, so the extra 34,697 NAs are mostly due to 
sum(is.na(model.scaffold$z1.0m_cmH2O_modified_comp)) #2168
sum(is.na(model.scaffold$TEW)) #2626
sum(is.na(model.scaffold$REW)) #2625

plot(x=200tapply(model.scaffold.results$GW.ET.growing, model.scaffold.results$Model.Year, mean, na.rm=TRUE)

#plot checks
plot(Kcb.std, type='l')
plot(Kcb.adjusted, type='l')
plot(Kcmax, type='p')
plot(fewi, type='l')
plot(fc, type='l')
plot(fewp, type='l')
plot(fewp + fewi, type='l')