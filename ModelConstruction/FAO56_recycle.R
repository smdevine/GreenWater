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
