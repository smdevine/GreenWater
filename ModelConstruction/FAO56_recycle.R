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

#trial results functions and experimenting with tapply and aggregate
IrDates <- function(df, irr.n, df.output) { #only works for irr.n=5 
  years <- (min(df$year)+1):(max(df$year)-1) #could add if statment to handle partial years
  for (i in 1:length(years)) {
    df.temp <- df[which(df$year==years[i]), ]
    j <- which(df.temp$Ir > 0)
    if (length(j) >= irr.n) {
      if (years[i] > min(df$year)+1) {
        Ir.dates.add <- data.frame(Irr.1=df.temp$dates[j[1]], Irr.2=df.temp$dates[j[2]], Irr.3=df.temp$dates[j[3]], Irr.4=df.temp$dates[j[4]], Irr.5=df.temp$dates[j[5]], Irr.Last=df.temp$dates[j[length(j)]])
        #Ir.dates.add$year <- years[i]
        Ir.dates <- rbind(Ir.dates, Ir.dates.add)
        next
      } else {
        Ir.dates <- data.frame(Irr.1=df.temp$dates[j[1]], Irr.2=df.temp$dates[j[2]], Irr.3=df.temp$dates[j[3]], Irr.4=df.temp$dates[j[4]], Irr.5=df.temp$dates[j[5]], Irr.Last=df.temp$dates[j[length(j)]])
        #Ir.dates$year <- years[i]
        next
      }
    } else {
      stop(print('There is a problem with the IrDates function.  Cannot handle a water year 2004-2016 with less than 5 irrigations'))
    }
  }
  col.start <- which(colnames(df.output)=='Irr.1')
  col.end <- which(colnames(df.output)=='Irr.Last')
  df.output[which(df.output$unique_model_code==model.code), col.start:col.end] <- Ir.dates
  #print(class(Ir.dates$Irr.1))
  return(df.output)
  #print(Ir.dates)
}

IrrigationTimes <- function(x) {
  if(length(which(x > 0))==0) {
    return(NA)
  } else {
    print(names(x))
    return(which(x > 0))
  }
}

testfunction <- function(x) {
  print(x['dates'])
}
IrrigationDates <- function(x) {
  if(length(which(x[, 'Ir'] > 0))==0) {
    return(NA)
  } 
  else if (length(which(x[, 'Ir'] > 0)) >= 5) {
    return(x[ , 'dates'][which(x[, 'Ir']> 0)[1:5]])
  } else {
    return(x[ , 'dates'][which(x[, 'Ir'] >0)])
  }
}

#deep perc function before modifications on 3/15/18
DeepPercCalc <- function(df) {
  #df <- model.result[which(model.result$years==2017), ]
  #print(df$years[1])
  jan.1.index <- which(df$dates==as.Date(paste0(as.character(df$years[1]), '-01-01')))
  first.irr.index <- if (sum(df$Ir > 0) > 1 & df$doys.model[1] <= Jdev) {
    head(which(df$Ir > 0), 1)} else {
      if (sum(df$Ir > 0) == 1 & df$doys.model[1] <= Jdev) {
        which(df$Ir > 0)} else {vector()}
    }
  last.irr.index <- if (sum(df$Ir > 0) > 1 & df$doys.model[nrow(df)] >= Jharv - days.no.irr) {
    tail(which(df$Ir > 0), 1)} else {
      if (sum(df$Ir > 0) == 1 & df$doys.model[nrow(df)] >= Jharv - days.no.irr) {
        which(df$Ir > 0)} else {vector()}
    }
  jul.1.index <- which(df$dates==as.Date(paste0(as.character(df$years[1]), '-07-01'))) #if 7/1 not present, returns integer(0), which really means integer(length=0), so has length=0
  dec.31.index <- which(df$dates==as.Date(paste0(as.character(df$years[1]), '-12-31')))
  #print(df$years[1])
  if (df$dates[1] > as.Date(paste0(as.character(df$years[1]), '-01-01')) | df$dates[nrow(df)] < as.Date(paste0(as.character(df$years[nrow(df)]), '-12-31'))) { #this means they ain't a whole year's data
    if (length(last.irr.index) == 0 & length(first.irr.index) == 0) { #not sufficient coverage or irrigation dates to calc anything
      data.frame(ET.annual = NA, E.annual = NA, T.annual = NA, deep.perc.annual = NA, winter.deep.perc = NA, post.Irr1.deep.perc=NA, fall.deep.perc = NA)
    }
    else if (length(jan.1.index) != 0 & length(first.irr.index) != 0 & length(jul.1.index) == 0) { #winter deep perc only
      data.frame(ET.annual = NA, E.annual = NA, T.annual = NA, deep.perc.annual = NA, winter.deep.perc = sum(df$DPr[jan.1.index:first.irr.index]), post.Irr1.deep.perc=NA, fall.deep.perc = NA)
    }
    else if (length(jan.1.index) != 0 & length(first.irr.index) != 0 & length(jul.1.index) != 0 & length(dec.31.index) == 0) { #winter and post Irr deep perc
      data.frame(ET.annual = NA, E.annual = NA, T.annual = NA, deep.perc.annual = NA, winter.deep.perc = sum(df$DPr[jan.1.index:first.irr.index]), post.Irr1.deep.perc=if(first.irr.index < jul.1.index) {sum(df$DPr[(first.irr.index+1):jul.1.index])} else {NA}, fall.deep.perc = NA)
    }
    else if (length(jan.1.index) == 0 & length(first.irr.index) != 0 & length(jul.1.index) != 0 & length(last.irr.index) == 0) { #only Spring deep perc available
      data.frame(ET.annual = NA, E.annual = NA, T.annual = NA, deep.perc.annual = NA, winter.deep.perc = NA, post.Irr1.deep.perc=if(first.irr.index < jul.1.index) {sum(df$DPr[(first.irr.index+1):jul.1.index])} else {NA}, fall.deep.perc = NA)
    }
    else if (length(jan.1.index) == 0 & length(first.irr.index) != 0 & length(dec.31.index) != 0) { #spring and fall deep perc, assumes last irrigation if first irrigation exists
      data.frame(ET.annual = NA, E.annual = NA, T.annual = NA, deep.perc.annual = NA, winter.deep.perc = NA, post.Irr1.deep.perc=if(first.irr.index < jul.1.index) {sum(df$DPr[(first.irr.index+1):jul.1.index])} else{NA}, fall.deep.perc =  sum(df$DPr[last.irr.index:dec.31.index]))
    } else {#fall perc only
      data.frame(ET.annual = NA, E.annual = NA, T.annual = NA, deep.perc.annual = NA, winter.deep.perc = NA, post.Irr1.deep.perc=NA,  fall.deep.perc =  sum(df$DPr[last.irr.index:dec.31.index]))
    }
  } else { #entire annual coverage available
    if (length(first.irr.index)==0 & length(last.irr.index)==0) { #but no irrigations
      data.frame(ET.annual = sum(df$ETc.act), E.annual = sum(df$Ei, df$Ep), T.annual = sum(df$ETc.act, -df$Ei, -df$Ep), deep.perc.annual = sum(df$DPr), winter.deep.perc = NA, post.Irr1.deep.perc = NA, fall.deep.perc = NA)
    } else {
      data.frame(ET.annual = sum(df$ETc.act), E.annual = sum(df$Ei, df$Ep), T.annual = sum(df$ETc.act, -df$Ei, -df$Ep), deep.perc.annual = sum(df$DPr), winter.deep.perc = sum(df$DPr[jan.1.index:first.irr.index]), post.Irr1.deep.perc = if(first.irr.index < jul.1.index) {sum(df$DPr[(first.irr.index+1):jul.1.index])}else{NA}, fall.deep.perc = sum(df$DPr[last.irr.index:dec.31.index]))
    }
  }
}

#function before modification 3/16/18
WaterBalanceCalc <- function(df, stress.point=0.5*PAW) { #concept is to run by year and get growing season results relative to Jdev-Jharv period each year for the respective crop
  #df <- model.result[which(model.result$years==2004),]
  final_doy <- df$doys.model[nrow(df)]
  last.irr.index <- if (length(which(df$Ir >0)) > 1 & final_doy >= Jharv - days.no.irr) {
    tail(which(df$Ir > 0), 1)} else {
      if (length(which(df$Ir >0)) == 1 & final_doy >= Jharv - days.no.irr) {
        which(df$Ir > 0)} else {vector()}
    }
  jharv_index <- which(df$doys.model==Jharv)
  jdev_index <- which(df$doys.model==Jdev)
  final_doy_index <- nrow(df)
  if (df$doys.model[1] > Jdev | final_doy < Jharv) { #if there is not a complete growing season, can't report all data
    if (length(jharv_index)==0) { #data ends before leaf-drop, so can't get entire season or end season data
      data.frame(RAW.end.season = NA, PAW.end.season = NA, Dr.end.season = NA, P.end.season=NA, Dr.end.year = NA, GW.ET.growing = NA, Irr.app.total = NA, Irr.app.last = NA, ET.growing = NA, E.growing = NA, T.growing = NA, crop.stress.growing=NA, deep.perc.growing=NA)
    }
    else if (length(last.irr.index)==0) { #implies no data when last irrigation occurred but there is data at Jharv; thus can get end of season storage data but also need additional check for 12/31 Dr data
      data.frame(RAW.end.season = max(stress.point - df$Dr.end[jharv_index], 0), PAW.end.season = max(PAW - df$Dr.end[jharv_index], 0), Dr.end.season = df$Dr.end[jharv_index], P.end.season = NA, Dr.end.year = if(final_doy==365 | final_doy==366) {df$Dr.end[final_doy_index]} else {NA}, GW.ET.growing = NA, Irr.app.total = NA, Irr.app.last = NA, ET.growing = NA, E.growing = NA, T.growing = NA, crop.stress.growing=NA, deep.perc.growing=NA)
    } else { #implies data exists from at least when last irrigation occurred to Jharv, so can get everything but entire growing season data but also still need additional check for Dr.end.year data
      data.frame(RAW.end.season = max(stress.point - df$Dr.end[jharv_index], 0), PAW.end.season = max(PAW - df$Dr.end[jharv_index], 0), Dr.end.season = df$Dr.end[jharv_index], P.end.season = sum(df$P[last.irr.index:jharv_index]), Dr.end.year = if(final_doy==365 | final_doy==366) {df$Dr.end[final_doy_index]} else {NA}, GW.ET.growing = NA, Irr.app.total = NA, Irr.app.last = df$Ir[last.irr.index], ET.growing = NA, E.growing = NA, T.growing = NA, crop.stress.growing=NA, deep.perc.growing=NA)
    }
  } else { #entire season's coverage available
    if (length(last.irr.index)==0) { #but in the event no irrigations occurred
      data.frame(RAW.end.season = max(stress.point - df$Dr.end[jharv_index], 0), PAW.end.season = max(PAW - df$Dr.end[jharv_index], 0), Dr.end.season = df$Dr.end[jharv_index], P.end.season = NA, Dr.end.year = if(final_doy==365 | final_doy==366) {df$Dr.end[final_doy_index]} else {NA}, GW.ET.growing = sum(df$ETc.act[jdev_index:jharv_index] - df$Ir[jdev_index:jharv_index]), Irr.app.total = sum(df$Ir), Irr.app.last = NA, ET.growing = sum(df$ETc.act[jdev_index:jharv_index]), E.growing = sum(df$Ei[jdev_index:jharv_index], df$Ep[jdev_index:jharv_index]), T.growing = sum(df$ETc.act[jdev_index:jharv_index] - df$Ei[jdev_index:jharv_index] - df$Ep[jdev_index:jharv_index]), crop.stress.growing=sum(df$ETc.ns[jdev_index:jharv_index] - df$ETc.act[jdev_index:jharv_index]), deep.perc.growing=sum(df$DPr[jdev_index:jharv_index]))
    } else {
      data.frame(RAW.end.season = max(stress.point - df$Dr.end[jharv_index], 0), PAW.end.season = max(PAW - df$Dr.end[jharv_index], 0), Dr.end.season = df$Dr.end[jharv_index], P.end.season = sum(df$P[last.irr.index:jharv_index]), Dr.end.year = if(final_doy==365 | final_doy==366) {df$Dr.end[final_doy_index]} else {NA}, GW.ET.growing = sum(df$ETc.act[jdev_index:jharv_index] - df$Ir[jdev_index:jharv_index]), Irr.app.total = sum(df$Ir), Irr.app.last = df$Ir[last.irr.index], ET.growing = sum(df$ETc.act[jdev_index:jharv_index]), E.growing = sum(df$Ei[jdev_index:jharv_index], df$Ep[jdev_index:jharv_index]), T.growing = sum(df$ETc.act[jdev_index:jharv_index] - df$Ei[jdev_index:jharv_index] - df$Ep[jdev_index:jharv_index]), crop.stress.growing=sum(df$ETc.ns[jdev_index:jharv_index] - df$ETc.act[jdev_index:jharv_index]), deep.perc.growing=sum(df$DPr[jdev_index:jharv_index])) #from this, can calculate dormant season values for E, ET, and H2O.stress later
    }
  }
}