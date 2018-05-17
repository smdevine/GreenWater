final.resultsDir <- 'D:/Allowable_Depletion/results/Mar2018/summaries'
clean.resultsDir <- 'D:/Allowable_Depletion/results/Mar2018/clean_results' #needs to be changed
clean.QC.resultsDir <- 'D:/Allowable_Depletion/results/Mar2018/clean_QC.results'
original.resultsDir <- 'D:/Allowable_Depletion/results/Mar2018'
modelscaffoldDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/model_scaffold/run_model/Mar2018'
if (!dir.exists(file.path(original.resultsDir, 'summaries'))) {
  dir.create(file.path(original.resultsDir, 'summaries'))
}
# list.files()
# cropscape_legend <- read.csv('cropscape_legend.txt', stringsAsFactors = FALSE)
# alfalfa_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Alfalfa'] #75380 total
# grape_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Grapes']
# almond_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Almonds']
# walnut_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Walnuts']
# pistachio_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Pistachios']
# cell_numbers_of_interest <- read.csv('cellnumbers_to_modelcodes.csv', stringsAsFactors = FALSE)
# raster.model.codes <- raster('model.codes.Aug2017.tif')
P.df <- read.csv(file.path(modelscaffoldDir, 'PRISM_precip_data.csv'), stringsAsFactors = FALSE) #this is a daily summary of precip from 10/1/2003-3/8/17 from 'free' daily PRISM 4km resolution for cells of interest in California, created in download_PRISM.R script (from 3/9/18 download); blanks and negative values checked for in 'data_QA_QC.R'
#U2.df <- read.csv(file.path(modelscaffoldDir, 'SpatialCIMIS.U2.QCpass.csv'), stringsAsFactors = FALSE) #this is a daily summary of wind data from download of spatial CIMIS data, created in spatialCIMIS.R script.  No missing data except for cell 148533
#RHmin.df <- read.csv(file.path(modelscaffoldDir, 'SpatialCIMIS.RHmin.QCpass.csv'), stringsAsFactors = FALSE) #this is a daily summary of minimum relative humidity, estimated from download of spatial CIMIS Tdew and Tmax data, created in spatialCIMIS.R script.  Blanks filled on "12_08_2011" & "02_23_2018" in data_QA_QC.R, along with 2,690 >100% values corrected
ETo.df <- read.csv(file.path(modelscaffoldDir, 'SpatialCIMIS.ETo.QCpass.csv'), stringsAsFactors = FALSE) #this is a daily summary of reference ET from download of spatial CIMIS data, created in spatialCIMIS.R script.  Blanks filled on multiple days (all on "02_23_2018") in data_QA_QC.R, along with 1,633 negative and 11 zero values corrected.
area.summary <- read.csv(file.path(modelscaffoldDir, 'hectares_by_model_code2018-05-14.csv'), stringsAsFactors = FALSE)
#summarize ETo, RHmin, and U2 data by various time windows
#if output is water.year summary then winter='no' & WY.basis='yes' & growing='no'
#if output is calendar year summary then winter='no' & WY.basis='no' & growing='no'
#if output is dormant period, then winter='yes' & WY.basis='yes' & growing='no'; this will work as long as Jharv is after Oct 1
#if output is growing season only, winter='no' & WY.basis='no' & growing='yes'
ClimateAggregate <- function(climate_df, varname, winter, Jdev, Jharv, WY.basis, growing) {
  climate_df$water.year <- climate_df$year
  climate_df$water.year[which(climate_df$month >= 10)] <- climate_df$water.year[which(climate_df$month >= 10)] + 1
  if (WY.basis=='yes') {
    by.year <- split(climate_df, climate_df$water.year)
  } else {
      by.year <- split(climate_df, climate_df$year)
  }
  if (winter=='yes') { #this data.frame trimming is done on a water.year basis
    for (j in 1:length(by.year)) { #get the unnecessary rows out now
      if (Jharv < format.Date(as.Date('10_1_2005', format='%m_%d_%Y'), '%j')) {
        print('Error: Jharv (leaf-drop) is before Oct 1st')
        break
      }
      if (Jharv < by.year[[j]][1,'DOY'] | Jdev > by.year[[j]][nrow(by.year[[j]]), 'DOY']) {
        years.to.rm <- if(!exists('years.to.rm', envir = environment())){j}else{c(years.to.rm, j)}
        next
      }
      by.year[[j]] <- by.year[[j]][which(by.year[[j]][,'DOY']==Jharv):which(by.year[[j]][,'DOY']==Jdev), ] #row 1 in each of these data.frames is Oct. 1st, start of the water.year; Jharv is leaf-drop; Jdev is bloom (not including alfalfa)
    }
    if (exists('years.to.rm')) {
      by.year <- by.year[-years.to.rm] #don't need double brackets here
    }
    years <- length(by.year)
  }
  if (growing=='yes') {
    for (j in 1:length(by.year)) { #get the unnecessary rows out now
      if (Jdev < by.year[[j]][1,'DOY'] | Jharv > by.year[[j]][nrow(by.year[[j]]), 'DOY']) {
        years.to.rm <- if(!exists('years.to.rm', envir = environment())){j}else{c(years.to.rm, j)}
        next
      }
      by.year[[j]] <- by.year[[j]][which(by.year[[j]][,'DOY']==Jdev):which(by.year[[j]][,'DOY']==Jharv), ] #row 1 in each of these data.frames is Jan 1st; Jharv is leaf-drop; Jdev is bloom (not including alfalfa)
    }
    if (exists('years.to.rm')) {
      by.year <- by.year[-years.to.rm] #don't need double brackets here
    }
    years <- length(by.year)
  }
  if (growing=='no' & WY.basis =='no') { #applies to calendar year annual summaries
    for (j in 1:length(by.year)) {
      if (nrow(by.year[[j]]) < 365) {
        years.to.rm <- if(!exists('years.to.rm', envir = environment())){j}else{c(years.to.rm, j)}
        next
      }
    }
    if (exists('years.to.rm')) {
      by.year <- by.year[-years.to.rm] #don't need double brackets here
    }
    years <- length(by.year)
  }
  if (winter=='no' & WY.basis =='yes') { #applies to water.year annual summaries
    for (j in 1:length(by.year)) { 
      if (nrow(by.year[[j]]) < 365) {
        years.to.rm <- if(!exists('years.to.rm', envir = environment())){j}else{c(years.to.rm, j)}
        next
      }
    }
    if (exists('years.to.rm')) {
      by.year <- by.year[-years.to.rm] #don't need double brackets here
    }
    years <- length(by.year)
  }
  for (j in 1:length(by.year)) { #get the unnecessary columns out now, including last column which is water.year
    by.year[[j]] <- by.year[[j]][,6:(ncol(by.year[[j]])-1)]
  }
  summary <- do.call(rbind, lapply(by.year, sapply, if(varname=='ETo') {sum} else{mean}, na.rm=TRUE)) #sapply was necessary so that each "cell" of the results matrix was not returned as a list object
  summary <- t(summary)
  summary <- as.data.frame(summary)
  summary$cell_name <- rownames(summary)
  summary$CIMIScellnumber <- as.integer(gsub('cell_', '', summary$cell_name))
  colnames(summary)
  summary[[paste0(varname, '.mean.all.yrs')]] <- apply(summary[,1:years], 1, mean)
  summary
}
#test the function
# ETo.winter <- ClimateAggregate(climate_df = ETo.df, varname = 'ETo', winter = 'yes', Jdev=46, Jharv=315, WY.basis = 'yes', growing = 'no')
# ETo.growing <- ClimateAggregate(climate_df = ETo.df, varname = 'ETo', winter = 'no', Jdev=46, Jharv=315, WY.basis = 'no', growing = 'yes')
# ETo.annual <- ClimateAggregate(climate_df = ETo.df, varname = 'ETo', winter = 'no', Jdev=NA, Jharv=NA, WY.basis = 'no', growing = 'no')
# ETo.WY <- ClimateAggregate(climate_df = ETo.df, varname = 'ETo', winter = 'no', Jdev=NA, Jharv=NA, WY.basis = 'yes', growing='no')

#same function as climate aggregate with slight changes to naming conventions above
PrecipAggregate <- function(precip_df, winter, Jdev, Jharv, WY.basis, growing) {
  precip_df$water.year <- precip_df$year
  precip_df$water.year[which(precip_df$month >= 10)] <- precip_df$water.year[which(precip_df$month >= 10)] + 1
  if (WY.basis=='yes') {
    by.year <- split(precip_df, precip_df$water.year)
  } else {
    by.year <- split(precip_df, precip_df$year)
  }
  if (winter=='yes') { #this data.frame trimming is done on a water.year basis
    for (j in 1:length(by.year)) { #get the unnecessary rows out now
      if (Jharv < format.Date(as.Date('10_1_2005', format='%m_%d_%Y'), '%j')) {
        print('Error: Jharv (leaf-drop) is before Oct 1st')
        break
      }
      if (Jharv < by.year[[j]][1,'DOY'] | Jdev > by.year[[j]][nrow(by.year[[j]]), 'DOY']) {
        years.to.rm <- if(!exists('years.to.rm', envir = environment())){j}else{c(years.to.rm, j)}
        next
      }
      by.year[[j]] <- by.year[[j]][which(by.year[[j]][,'DOY']==Jharv):which(by.year[[j]][,'DOY']==Jdev), ] #row 1 in each of these data.frames is Oct. 1st, start of the water.year; Jharv is leaf-drop; Jdev is bloom (not including alfalfa)
    }
    if (exists('years.to.rm')) {
      by.year <- by.year[-years.to.rm] #don't need double brackets here
    }
    years <- length(by.year)
  }
  if (growing=='yes') {
    for (j in 1:length(by.year)) { #get the unnecessary rows out now
      if (Jdev < by.year[[j]][1,'DOY'] | Jharv > by.year[[j]][nrow(by.year[[j]]), 'DOY']) {
        years.to.rm <- if(!exists('years.to.rm', envir = environment())){j}else{c(years.to.rm, j)}
        next
      }
      by.year[[j]] <- by.year[[j]][which(by.year[[j]][,'DOY']==Jdev):which(by.year[[j]][,'DOY']==Jharv), ] #row 1 in each of these data.frames is Jan 1st; Jharv is leaf-drop; Jdev is bloom (not including alfalfa)
    }
    if (exists('years.to.rm')) {
      by.year <- by.year[-years.to.rm] #don't need double brackets here
    }
    years <- length(by.year)
  }
  if (growing=='no' & WY.basis =='no') { #applies to calendar year annual summaries
    for (j in 1:length(by.year)) {
      if (nrow(by.year[[j]]) < 365) {
        years.to.rm <- if(!exists('years.to.rm', envir = environment())){j}else{c(years.to.rm, j)}
        next
      }
    }
    if (exists('years.to.rm')) {
      by.year <- by.year[-years.to.rm] #don't need double brackets here
    }
    years <- length(by.year)
  }
  if (winter=='no' & WY.basis =='yes') { #applies to water.year annual summaries
    for (j in 1:length(by.year)) { 
      if (nrow(by.year[[j]]) < 365) {
        years.to.rm <- if(!exists('years.to.rm', envir = environment())){j}else{c(years.to.rm, j)}
        next
      }
    }
    if (exists('years.to.rm')) {
      by.year <- by.year[-years.to.rm] #don't need double brackets here
    }
    years <- length(by.year)
  }
  for (j in 1:length(by.year)) { #get the unnecessary columns out now, including last column which is water.year
    by.year[[j]] <- by.year[[j]][,6:(ncol(by.year[[j]])-1)]
  }
  summary <- do.call(rbind, lapply(by.year, sapply, sum, na.rm=TRUE)) #sapply was necessary so that each "cell" of the results matrix was not returned as a list object
  summary <- t(summary)
  summary <- as.data.frame(summary)
  summary$cell_name <- rownames(summary)
  summary$PRISMcellnumber <- as.integer(gsub('cell_', '', summary$cell_name))
  colnames(summary)
  summary[['P.mean.all.yrs']] <- apply(summary[,1:years], 1, mean)
  summary
}
#test the function
# P.winter <- PrecipAggregate(precip_df = P.df, winter='yes', Jdev=46, Jharv=315, WY.basis = 'yes', growing='no')
# P.growing <- PrecipAggregate(precip_df = P.df, winter='no', Jdev=46, Jharv=315, WY.basis = 'no', growing='yes')
# P.annual <- PrecipAggregate(precip_df = P.df, winter = 'no', Jdev=46, Jharv=315, WY.basis='no', growing='no')
# P.WY <- PrecipAggregate(precip_df = P.df, winter = 'no', Jdev=46, Jharv=315, WY.basis='yes', growing='no')

PrecipAggregateMonthly <- function(df, years) {
  df <- df[df$year %in% years,]
  by.month <- split(df, df$month)
  lapply(by.month, function(x) {
    summary <- do.call(rbind, lapply(split(x, x$year), function(y) apply(y[,6:ncol(x)], 2, sum)))
    summary <- t(summary)
    summary <- as.data.frame(summary)
    summary$cell_name <- rownames(summary)
    summary$PRISMcellnumber <- as.integer(gsub('cell_', '', summary$cell_name))
    summary
    }
  )
}
#P.monthly <- PrecipAggregateMonthly(P.df, 2004:2017)

ClimateAggregateMonthly <- function(df, years) {
  df <- df[df$year %in% years,]
  by.month <- split(df, df$month)
  lapply(by.month, function(x) {
    summary <- do.call(rbind, lapply(split(x, x$year), function(y) apply(y[,6:ncol(x)], 2, sum)))
    summary <- t(summary)
    summary <- as.data.frame(summary)
    summary$cell_name <- rownames(summary)
    summary$CIMIScellnumber <- as.integer(gsub('cell_', '', summary$cell_name))
    summary
  }
  )
}
#ETo.monthly <- ClimateAggregateMonthly(ETo.df, 2004:2017)

#this will now be run on the QC_results file for each crop x soil storage scenario
AggregateSoilPars <- function(df, paw.varname, area_summary) {
  compsums <- as.data.frame(tapply(df$comppct_r, df$unique_model_code, sum))
  colnames(compsums) <- 'compsums'
  compsums$unique_model_code <- as.integer(rownames(compsums))
  compsums$compsums <- as.numeric(compsums$compsums)
  results <- cbind(df[ ,c(paw.varname, 'TEW', 'REW', 'surface.depth', 'comppct_r')], compsums[match(df$unique_model_code, compsums$unique_model_code), ])
  var.final <- as.data.frame(tapply(results[[paw.varname]]*(results$comppct_r/results$compsums), results$unique_model_code, sum))
  colnames(var.final) <- paw.varname
  var.final$unique_model_code <- as.integer(rownames(var.final))
  var.final[[paw.varname]] <- as.numeric(var.final[[paw.varname]]) #simplify from array to numeric vector and convert to mm_H2O
  var.final <- var.final[,c(2,1)] #make unique_model_code the first column
  var.final[[gsub('cm', 'mm', paw.varname)]] <- var.final[[paw.varname]]*10
  var.final$TEW <- as.numeric(tapply(results$TEW*(results$comppct_r/results$compsums), results$unique_model_code, sum))
  var.final$REW <- as.numeric(tapply(results$REW*(results$comppct_r/results$compsums), results$unique_model_code, sum))
  var.final$surface.depth <- as.numeric(tapply(results$surface.depth*(results$comppct_r/results$compsums), results$unique_model_code, sum))
  var.final$comppct_total <- results$compsums[match(var.final$unique_model_code, results$unique_model_code)]
  var.final$mukey <- df$mukey[match(var.final$unique_model_code, df$unique_model_code)]
  var.final$comps_total <- df$n_compkeys[match(var.final$unique_model_code, df$unique_model_code)]
  var.final$comps_maj <- as.integer(tapply(results$unique_model_code, results$unique_model_code, length))
  compnames <- split(df$compname, df$unique_model_code)
  for (i in seq_along(compnames)) {
    if (length(compnames[[i]]) > 1) {
      compnames[[i]] <- paste(compnames[[i]], collapse = ', ')
    }
  }
  cokeys <- split(df$cokey, df$unique_model_code)
  for (i in seq_along(cokeys)) {
    if (length(cokeys[[i]]) > 1) {
      cokeys[[i]] <- paste(cokeys[[i]], collapse = ', ')
    }
  }
  var.final$compnames <- as.character(compnames)
  var.final$cokeys <- as.character(cokeys)
  var.final$PRISMcellnumber <- df$PRISMcellnumber[match(var.final$unique_model_code, df$unique_model_code)]
  var.final$CIMIScellnumber <- df$CIMIScellnumber[match(var.final$unique_model_code, df$unique_model_code)]
  var.final$hectares <- area_summary$hectares[match(var.final$unique_model_code, area_summary$unique_model_code)]
  rownames(var.final) <- NULL
  var.final
}

#this aggregates the model results by map unit, so that there are no duplicate unique model codes in the results (i.e. unique model codes with more than one major component have their results averaged as a component weighted average).  For this function, results for all model years are maintained, whereas in AggregateResults() below, multiple years data is compressed into a single statistic.
#going to have to skip Irr.All for now
MUAggregate <- function(df, varname) {
  year <- df$Model.Year[1]
  if (year==2003 | year==2018) { #test code to simplify results collection
    return(NULL)
  }
  #print(year)
  if (varname=='Irr.1' | varname=='Irr.Last') {
    df[[varname]][which(df[[varname]]=='1900-01-01')] <- NA
    df[[varname]] <- as.Date(df[[varname]], format='%Y-%m-%d')
    varname_doy <- paste0(varname, '.doy')
    df[[varname_doy]] <- as.integer(format.Date(df[[varname]], '%j'))
    compsums <- as.data.frame(tapply(df$comppct_r[!is.na(df[[varname]])], df$unique_model_code[!is.na(df[[varname]])], sum)) #this sums up component percentages (that have data) by unique_model_code
    colnames(compsums) <- 'compsums'
    compsums$unique_model_code <- as.integer(rownames(compsums))
    results <- cbind(df[!is.na(df[[varname]]), c(varname_doy, 'comppct_r')], compsums[match(df$unique_model_code[!is.na(df[[varname]])], compsums$unique_model_code), ]) #this eliminates rows with varname as NA and then adds the total component percentage calculated above
    var.final <- tapply(results[[varname_doy]]*(results$comppct_r/results$compsums), results$unique_model_code, sum)
    var.final <- as.data.frame(var.final)
    colnames(var.final) <- 'var.final'
    var.final$unique_model_code <- rownames(var.final)
    rownames(var.final) <- NULL
    var.final$var.final <- as.integer(var.final$var.final)
    var.final <- var.final[,c(2,1)]
    colnames(var.final)[2] <- varname_doy
    #var.final$Model.Year <- year
    var.final <- cbind(var.final, df[match(var.final$unique_model_code, df$unique_model_code), 'Model.Year'])
    colnames(var.final)[3] <- 'Model.Year'
    #rm(compsums, results)
    #gc()
    var.final
  } else {
    compsums <- as.data.frame(tapply(df$comppct_r[!is.na(df[[varname]])], df$unique_model_code[!is.na(df[[varname]])], sum))
    colnames(compsums) <- 'compsums'
    compsums$unique_model_code <- as.integer(rownames(compsums))
    results <- cbind(df[!is.na(df[[varname]]), c(varname, 'comppct_r')], compsums[match(df$unique_model_code[!is.na(df[[varname]])], compsums$unique_model_code), ])
    var.final <- tapply(results[[varname]]*(results$comppct_r/results$compsums), results$unique_model_code, sum)
    var.final <- as.data.frame(var.final)
    colnames(var.final) <- 'var.final'
    var.final$unique_model_code <- rownames(var.final)
    rownames(var.final) <- NULL
    var.final$var.final <- as.numeric(var.final$var.final)
    var.final <- var.final[,c(2,1)]
    colnames(var.final)[2] <- varname
    #var.final$Model.Year <- year
    var.final <- cbind(var.final, df[match(var.final$unique_model_code, df$unique_model_code), 'Model.Year'])
    colnames(var.final)[3] <- 'Model.Year'
    #rm(compsums, results)
    #gc()
    var.final
  }
}
#test the function with some almond results
# GW.ET.growing <- do.call(rbind, lapply(split(almond2.0m_AD50, almond2.0m_AD50$Model.Year), MUAggregate, varname='GW.ET.growing'))
# Irr.1 <- do.call(rbind, lapply(split(almond2.0m_AD50, almond2.0m_AD50$Model.Year), MUAggregate, varname='Irr.1'))
# Irr.Last <- do.call(rbind, lapply(split(almond2.0m_AD50, almond2.0m_AD50$Model.Year), MUAggregate, varname='Irr.Last'))

#get data for all years into the unique model codes
MUAggregate.AllYrs <- function(df) {
  Irr.1_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='Irr.1'))
  Irr.Last_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='Irr.Last'))
  #skipping Irr.Count for now, because no good way to aggregate by map unit
  Irr.Count_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='Irr.Count'))
  Dr.begin.year_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='Dr.begin.year'))
  Dr.begin.season_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='Dr.begin.season'))
  Dr.end.season_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='Dr.end.season'))
  P.end.season_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname="P.end.season"))
  Dr.end.year_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='Dr.end.year'))
  GW.ET.growing_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='GW.ET.growing'))
  Irr.app.total_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='Irr.app.total'))
  Irr.app.last_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='Irr.app.last'))
  ET.growing_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='ET.growing'))
  E.growing_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='E.growing'))
  T.growing_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='T.growing'))
  crop.stress.growing_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='crop.stress.growing'))
  deep.perc.growing_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='deep.perc.growing'))
  ET.annual_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='ET.annual'))
  E.annual_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='E.annual'))
  T.annual_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='T.annual'))
  crop.stress.annual_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='crop.stress.annual'))
  deep.perc.annual_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='deep.perc.annual'))
  non.irr.deep.perc_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='non.irr.deep.perc'))
  Irr1.to.last.deep.perc_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='Irr1.to.last.deep.perc'))
  fall.deep.perc_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='fall.deep.perc'))
  GW.ET.to.Irr1_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='GW.ET.to.Irr1'))
  GW.E.to.Irr1_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='GW.E.to.Irr1'))
  GW.T.to.Irr1_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='GW.T.to.Irr1'))
  IrrApp.Jan_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='IrrApp.Jan'))
  IrrApp.Feb_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='IrrApp.Feb'))
  IrrApp.Mar_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='IrrApp.Mar'))
  IrrApp.Apr_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='IrrApp.Apr'))
  IrrApp.May_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='IrrApp.May'))
  IrrApp.Jun_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='IrrApp.Jun'))
  IrrApp.Jul_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='IrrApp.Jul'))
  IrrApp.Aug_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='IrrApp.Aug'))
  IrrApp.Sep_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='IrrApp.Sep'))
  IrrApp.Oct_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='IrrApp.Oct'))
  IrrApp.Nov_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='IrrApp.Nov'))
  IrrApp.Dec_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='IrrApp.Dec'))
  ET.Jan_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='ET.Jan'))
  ET.Feb_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='ET.Feb'))
  ET.Mar_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='ET.Mar'))
  ET.Apr_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='ET.Apr'))
  ET.May_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='ET.May'))
  ET.Jun_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='ET.Jun'))
  ET.Jul_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='ET.Jul'))
  ET.Aug_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='ET.Aug'))
  ET.Sep_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='ET.Sep'))
  ET.Oct_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='ET.Oct'))
  ET.Nov_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='ET.Nov'))
  ET.Dec_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='ET.Dec'))
  E.Jan_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='E.Jan'))
  E.Feb_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='E.Feb'))
  E.Mar_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='E.Mar'))
  E.Apr_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='E.Apr'))
  E.May_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='E.May'))
  E.Jun_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='E.Jun'))
  E.Jul_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='E.Jul'))
  E.Aug_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='E.Aug'))
  E.Sep_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='E.Sep'))
  E.Oct_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='E.Oct'))
  E.Nov_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='E.Nov'))
  E.Dec_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='E.Dec'))
  DeepPerc.Jan_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='DeepPerc.Jan'))
  DeepPerc.Feb_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='DeepPerc.Feb'))
  DeepPerc.Mar_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='DeepPerc.Mar'))
  DeepPerc.Apr_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='DeepPerc.Apr'))
  DeepPerc.May_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='DeepPerc.May'))
  DeepPerc.Jun_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='DeepPerc.Jun'))
  DeepPerc.Jul_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='DeepPerc.Jul'))
  DeepPerc.Aug_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='DeepPerc.Aug'))
  DeepPerc.Sep_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='DeepPerc.Sep'))
  DeepPerc.Oct_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='DeepPerc.Oct'))
  DeepPerc.Nov_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='DeepPerc.Nov'))
  DeepPerc.Dec_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='DeepPerc.Dec'))
  CropStress.Jan_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='CropStress.Jan'))
  CropStress.Feb_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='CropStress.Feb'))
  CropStress.Mar_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='CropStress.Mar'))
  CropStress.Apr_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='CropStress.Apr'))
  CropStress.May_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='CropStress.May'))
  CropStress.Jun_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='CropStress.Jun'))
  CropStress.Jul_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='CropStress.Jul'))
  CropStress.Aug_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='CropStress.Aug'))
  CropStress.Sep_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='CropStress.Sep'))
  CropStress.Oct_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='CropStress.Oct'))
  CropStress.Nov_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='CropStress.Nov'))
  CropStress.Dec_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='CropStress.Dec'))
  result <- list(Irr.1_allyrs, Irr.Last_allyrs, Irr.Count_allyrs, Dr.begin.year_allyrs, Dr.begin.season_allyrs, Dr.end.season_allyrs, P.end.season_allyrs, Dr.end.year_allyrs, GW.ET.growing_allyrs, Irr.app.total_allyrs, Irr.app.last_allyrs, ET.growing_allyrs, E.growing_allyrs, T.growing_allyrs, crop.stress.growing_allyrs, deep.perc.growing_allyrs, ET.annual_allyrs, E.annual_allyrs, T.annual_allyrs, crop.stress.annual_allyrs, deep.perc.annual_allyrs, non.irr.deep.perc_allyrs, Irr1.to.last.deep.perc_allyrs, fall.deep.perc_allyrs, GW.ET.to.Irr1_allyrs, GW.E.to.Irr1_allyrs, GW.T.to.Irr1_allyrs, IrrApp.Jan_allyrs, IrrApp.Feb_allyrs, IrrApp.Mar_allyrs, IrrApp.Apr_allyrs, IrrApp.May_allyrs, IrrApp.Jun_allyrs, IrrApp.Jul_allyrs, IrrApp.Aug_allyrs, IrrApp.Sep_allyrs, IrrApp.Oct_allyrs, IrrApp.Nov_allyrs, IrrApp.Dec_allyrs, ET.Jan_allyrs, ET.Feb_allyrs, ET.Mar_allyrs, ET.Apr_allyrs, ET.May_allyrs, ET.Jun_allyrs, ET.Jul_allyrs, ET.Aug_allyrs, ET.Sep_allyrs, ET.Oct_allyrs, ET.Nov_allyrs, ET.Dec_allyrs, E.Jan_allyrs, E.Feb_allyrs, E.Mar_allyrs, E.Apr_allyrs, E.May_allyrs, E.Jun_allyrs, E.Jul_allyrs, E.Aug_allyrs, E.Sep_allyrs, E.Oct_allyrs, E.Nov_allyrs, E.Dec_allyrs, DeepPerc.Jan_allyrs, DeepPerc.Feb_allyrs, DeepPerc.Mar_allyrs, DeepPerc.Apr_allyrs, DeepPerc.May_allyrs, DeepPerc.Jun_allyrs, DeepPerc.Jul_allyrs, DeepPerc.Aug_allyrs, DeepPerc.Sep_allyrs, DeepPerc.Oct_allyrs, DeepPerc.Nov_allyrs, DeepPerc.Dec_allyrs, CropStress.Jan_allyrs, CropStress.Feb_allyrs, CropStress.Mar_allyrs, CropStress.Apr_allyrs, CropStress.May_allyrs, CropStress.Jun_allyrs, CropStress.Jul_allyrs, CropStress.Aug_allyrs, CropStress.Sep_allyrs, CropStress.Oct_allyrs, CropStress.Nov_allyrs, CropStress.Dec_allyrs)  
  names(result) <- c('Irr.1.doy', 'Irr.Last.doy', 'Irr.Count', 'Dr.begin.year', 'Dr.begin.season', 'Dr.end.season', 'P.end.season', 'Dr.end.year', 'GW.ET.growing', 'Irr.app.total', 'Irr.app.last', 'ET.growing', 'E.growing', 'T.growing', 'crop.stress.growing', 'deep.perc.growing', 'ET.annual', 'E.annual', 'T.annual', 'crop.stress.annual', 'deep.perc.annual', 'non.irr.deep.perc', 'Irr1.to.last.deep.perc', 'fall.deep.perc', 'GW.ET.to.Irr1', 'GW.E.to.Irr1', 'GW.T.to.Irr1', 'IrrApp.Jan', 'IrrApp.Feb', 'IrrApp.Mar', 'IrrApp.Apr', 'IrrApp.May', 'IrrApp.Jun', 'IrrApp.Jul', 'IrrApp.Aug', 'IrrApp.Sep', 'IrrApp.Oct', 'IrrApp.Nov', 'IrrApp.Dec', 'ET.Jan', 'ET.Feb', 'ET.Mar', 'ET.Apr', 'ET.May', 'ET.Jun', 'ET.Jul', 'ET.Aug', 'ET.Sep', 'ET.Oct', 'ET.Nov', 'ET.Dec', 'E.Jan', 'E.Feb', 'E.Mar', 'E.Apr', 'E.May', 'E.Jun', 'E.Jul', 'E.Aug', 'E.Sep', 'E.Oct', 'E.Nov', 'E.Dec', 'DeepPerc.Jan', 'DeepPerc.Feb', 'DeepPerc.Mar', 'DeepPerc.Apr', 'DeepPerc.May', 'DeepPerc.Jun', 'DeepPerc.Jul', 'DeepPerc.Aug', 'DeepPerc.Sep', 'DeepPerc.Oct', 'DeepPerc.Nov', 'DeepPerc.Dec', 'CropStress.Jan', 'CropStress.Feb', 'CropStress.Mar', 'CropStress.Apr', 'CropStress.May', 'CropStress.Jun', 'CropStress.Jul', 'CropStress.Aug', 'CropStress.Sep', 'CropStress.Oct', 'CropStress.Nov', 'CropStress.Dec')
  result
}

#now, distribute these results to points across all years
SetValues.AllYrs <- function(var_df, varname, main_df) {
  if (is.null(main_df$Model.Year)) {
    main_df$Model.Year <- var_df$Model.Year[1]
    main_df[[varname]] <- var_df[[varname]][match(main_df$unique_model_code, var_df$unique_model_code)]
    main_df
  } else {
    model.year <- var_df$Model.Year[1]
    subset_main_df <- main_df[which(main_df$Model.Year==model.year), ]
    subset_main_df[[varname]] <- var_df[[varname]][match(subset_main_df$unique_model_code, var_df$unique_model_code)]
    #gc()
    subset_main_df
  }
}
SetValues.AllYrs.Combined <- function(var_df, main_df) { #note that gc() is called from function defined above, which is called within this function
  for (i in seq_along(var_df)) {
    final_results <- do.call(rbind, lapply(split(var_df[[i]], var_df[[i]]$Model.Year), SetValues.AllYrs, varname=names(var_df)[i], main_df= if (i==1) {main_df} else {final_results}))
    #print(i)
  }
  gc()
  final_results
}
SetPrecipValues.AllYrs <- function(df, precip_data, colname) {
  model.year <- as.character(df$Model.Year[1])
  df[[colname]] <- precip_data[[model.year]][match(df$PRISMcellnumber, precip_data$PRISMcellnumber)]
  gc()
  df
}
SetClimateValues.AllYrs <- function(df, climate_data, colname) {
  model.year <- as.character(df$Model.Year[1])
  df[[colname]] <- climate_data[[model.year]][match(df$CIMIScellnumber, climate_data$CIMIScellnumber)]
  gc()
  df
}
#replace scenario.resultsDir with clean.resultsDir
#run the functions above to loop through each crop's results folder to get the results to points on the landscape
#write only the map-unit aggregated results for years 2004-2016 and unique model_codes, along with cell_counts of those unique model codes 
#update scenario naming so that wine.grapes can be handled
data.to.allyrs <- function(cropname, cropname2) {
  #setwd(modelscaffoldDir)
  cropscape_legend <- read.csv(file.path(modelscaffoldDir, 'cropscape_legend.txt'), stringsAsFactors = FALSE)
  cropcode <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME==cropname2]
  #P.df <- read.csv(file.path(modelscaffoldDir, 'PRISM.precip.data.updated9.13.17.csv'), stringsAsFactors = FALSE)
  #ETo.df <- read.csv(file.path(modelscaffoldDir, 'SpatialCIMIS.ETo.updated9.13.17.csv'), stringsAsFactors = FALSE)
  #U2.df <- read.csv('SpatialCIMIS.U2.updated9.13.17.csv', stringsAsFactors = F) #this is a daily summary of wind data from download of spatial CIMIS data, created in spatialCIMIS.R script.  No missing data except for cell 148533
  #RHmin.df <- read.csv('SpatialCIMIS.RHmin.updated9.13.17.csv', stringsAsFactors = F)
  area.summary <- read.csv(file.path(modelscaffoldDir, 'hectares_by_model_code2018-05-14.csv'), stringsAsFactors = FALSE)
  #setwd(file.path(clean.resultsDir, cropname))
  fnames <- list.files(file.path(clean.resultsDir, cropname))
  fnames_QC <- list.files(file.path(clean.QC.resultsDir, cropname))
  print(fnames)
  for (i in seq_along(fnames)) {
    print(i)
    scenario_name <- gsub('_FAO56results_clean.csv', '', fnames[i])
    scenario_name <- paste0('scenario_', gsub(cropname, '', scenario_name))
    scenario_name <- gsub('AD', '', scenario_name)
    scenario_name <- paste0(scenario_name, 'AD')
    model_metadata <- read.csv(file.path(original.resultsDir, paste0(cropname, '_majcomps'), scenario_name, list.files(path = file.path(original.resultsDir, paste0(cropname, '_majcomps'), scenario_name), pattern = glob2rx('*_model_metadata.csv'))), stringsAsFactors = FALSE)
    paw.varname <- model_metadata$paw.varname
    if (cropname2=='Pistachios' | cropname2=='Almonds' | cropname2=='Walnuts' | cropname2=='Grapes') {
      Jdev <- model_metadata$Jdev
      Jharv <- model_metadata$Jharv
    } else if (cropname=='alfalfa.intermountain') {
      Jdev <- model_metadata$Jini
      Jharv <- model_metadata$Jharv + (model_metadata$cycle.no - 1) * model_metadata$cycle.length + model_metadata$Lini.cycle + model_metadata$Ldev.cycle + model_metadata$Lmid.cycle + model_metadata$Lfrost.kill #30 is specified as "buffer.days" in FAO56dualcropcoeff call to IrCalc and days.no.irr, but we need to get this above Oct1st Jharv <- harvest.days[length(harvest.days)] + crop.parameters$Lini.cycle[crop.parameters$crop==cropname] + crop.parameters$Ldev.cycle[crop.parameters$crop==cropname] + crop.parameters$Lmid.cycle[crop.parameters$crop==cropname] + crop.parameters$Lfrost.kill[crop.parameters$crop==cropname]
    } else {
        Jdev <- NA
        Jharv <- NA
    }
    if (i==1) {
      ETo.annual <- ClimateAggregate(climate_df = ETo.df, varname = 'ETo', winter = 'no', Jdev=NA, Jharv=NA, WY.basis = 'no', growing = 'no')
      ETo.WY <- ClimateAggregate(climate_df = ETo.df, varname = 'ETo', winter = 'no', Jdev=NA, Jharv=NA, WY.basis = 'yes', growing='no')
      P.annual <- PrecipAggregate(precip_df = P.df, winter = 'no', Jdev=NA, Jharv=NA, WY.basis='no', growing='no')
      P.WY <- PrecipAggregate(precip_df = P.df, winter = 'no', Jdev=NA, Jharv=NA, WY.basis='yes', growing='no')
      P.monthly <- PrecipAggregateMonthly(P.df, 2004:2017) #produces list of data.frames for each month with columns by year and rows by cell name
      ETo.monthly <- ClimateAggregateMonthly(ETo.df, 2004:2017)
      if (cropname2=='Pistachios' | cropname2=='Almonds' | cropname2=='Walnuts' | cropname2=='Grapes' | cropname=='alfalfa.intermountain') {  #this block not relevant to alfalfa.CV or alfalfa.imperial
        P.winter <- PrecipAggregate(precip_df = P.df, winter='yes', Jdev=Jdev, Jharv=Jharv, WY.basis = 'yes', growing='no')
        P.growing <- PrecipAggregate(precip_df = P.df, winter='no', Jdev=Jdev, Jharv=Jharv, WY.basis = 'no', growing='yes')
        ETo.winter <- ClimateAggregate(climate_df = ETo.df, varname = 'ETo', winter = 'yes', Jdev=Jdev, Jharv=Jharv, WY.basis = 'yes', growing = 'no')
        ETo.growing <- ClimateAggregate(climate_df = ETo.df, varname = 'ETo', winter = 'no', Jdev=Jdev, Jharv=Jharv, WY.basis = 'no', growing = 'yes')
      }
    }
    #setwd(file.path(clean.resultsDir, cropname))
    model_params <- read.csv(file.path(clean.QC.resultsDir, cropname, fnames_QC[i]), stringsAsFactors = FALSE)
    soil_summary <- AggregateSoilPars(model_params, paw.varname, area.summary) #verified its tracking all unique model codes on 5/14/18
    if (!dir.exists(file.path(final.resultsDir, cropname))) {
      dir.create(file.path(final.resultsDir, cropname))
    }
    if (!dir.exists(file.path(final.resultsDir, cropname, 'MUaggregated_soildata'))) {
      dir.create(file.path(final.resultsDir, cropname, 'MUaggregated_soildata'))
    }
    write.csv(soil_summary, file.path(final.resultsDir, cropname, 'MUaggregated_soildata', paste0(scenario_name, '_soilsdata.csv')), row.names = FALSE)
    df <- read.csv(file.path(clean.resultsDir, cropname, fnames[i]), stringsAsFactors = FALSE)
    results <- MUAggregate.AllYrs(df)
    df_allyrs <- SetValues.AllYrs.Combined(results, soil_summary[,c('unique_model_code', 'CIMIScellnumber', 'PRISMcellnumber')]) #this no longer combines soil data for each year of water balance data 
    rm(results, df, soil_summary)
    gc()
    df_allyrs <- do.call(rbind, lapply(split(df_allyrs, df_allyrs$Model.Year), SetPrecipValues.AllYrs, precip_data=P.annual, colname='P.annual'))
    df_allyrs <- do.call(rbind, lapply(split(df_allyrs, df_allyrs$Model.Year), SetPrecipValues.AllYrs, precip_data=P.WY, colname='P.WY'))
    df_allyrs <- do.call(rbind, lapply(split(df_allyrs, df_allyrs$Model.Year), SetClimateValues.AllYrs, climate_data=ETo.annual, colname='ETo.annual'))
    df_allyrs <- do.call(rbind, lapply(split(df_allyrs, df_allyrs$Model.Year), SetClimateValues.AllYrs, climate_data=ETo.WY, colname='ETo.WY'))
    if (cropname2=='Pistachios' | cropname2=='Almonds' | cropname2=='Walnuts' | cropname2=='Grapes' | cropname=='alfalfa.intermountain') {
      df_allyrs <- do.call(rbind, lapply(split(df_allyrs, df_allyrs$Model.Year), SetPrecipValues.AllYrs, precip_data=P.winter, colname='P.winter')) #Jharv to Jdev
      df_allyrs <- do.call(rbind, lapply(split(df_allyrs, df_allyrs$Model.Year), SetPrecipValues.AllYrs, precip_data=P.growing, colname='P.growing')) #Jharv to Jdev
      df_allyrs <- do.call(rbind, lapply(split(df_allyrs, df_allyrs$Model.Year), SetClimateValues.AllYrs, climate_data=ETo.winter, colname='ETo.winter'))
      df_allyrs <- do.call(rbind, lapply(split(df_allyrs, df_allyrs$Model.Year), SetClimateValues.AllYrs, climate_data=ETo.growing, colname='ETo.growing'))
    } else { #this leaves alfalfa in Central and Imperial Valleys
        df_allyrs$P.winter <- NA_real_
        df_allyrs$P.growing <- df_allyrs$P.annual
        df_allyrs$ETo.winter <- NA_real_
        df_allyrs$ETo.growing <- df_allyrs$ETo.annual
    }
    df_allyrs <- do.call(rbind, lapply(split(df_allyrs, df_allyrs$Model.Year), SetPrecipValues.AllYrs, precip_data=P.monthly$`1`, colname='P.Jan'))
    df_allyrs <- do.call(rbind, lapply(split(df_allyrs, df_allyrs$Model.Year), SetPrecipValues.AllYrs, precip_data=P.monthly$`2`, colname='P.Feb'))
    df_allyrs <- do.call(rbind, lapply(split(df_allyrs, df_allyrs$Model.Year), SetPrecipValues.AllYrs, precip_data=P.monthly$`3`, colname='P.Mar'))
    df_allyrs <- do.call(rbind, lapply(split(df_allyrs, df_allyrs$Model.Year), SetPrecipValues.AllYrs, precip_data=P.monthly$`4`, colname='P.Apr'))
    df_allyrs <- do.call(rbind, lapply(split(df_allyrs, df_allyrs$Model.Year), SetPrecipValues.AllYrs, precip_data=P.monthly$`5`, colname='P.May'))
    df_allyrs <- do.call(rbind, lapply(split(df_allyrs, df_allyrs$Model.Year), SetPrecipValues.AllYrs, precip_data=P.monthly$`6`, colname='P.Jun'))
    df_allyrs <- do.call(rbind, lapply(split(df_allyrs, df_allyrs$Model.Year), SetPrecipValues.AllYrs, precip_data=P.monthly$`7`, colname='P.Jul'))
    df_allyrs <- do.call(rbind, lapply(split(df_allyrs, df_allyrs$Model.Year), SetPrecipValues.AllYrs, precip_data=P.monthly$`8`, colname='P.Aug'))
    df_allyrs <- do.call(rbind, lapply(split(df_allyrs, df_allyrs$Model.Year), SetPrecipValues.AllYrs, precip_data=P.monthly$`9`, colname='P.Sep'))
    df_allyrs <- do.call(rbind, lapply(split(df_allyrs, df_allyrs$Model.Year), SetPrecipValues.AllYrs, precip_data=P.monthly$`10`, colname='P.Oct'))
    df_allyrs <- do.call(rbind, lapply(split(df_allyrs, df_allyrs$Model.Year), SetPrecipValues.AllYrs, precip_data=P.monthly$`11`, colname='P.Nov'))
    df_allyrs <- do.call(rbind, lapply(split(df_allyrs, df_allyrs$Model.Year), SetPrecipValues.AllYrs, precip_data=P.monthly$`12`, colname='P.Dec'))
    df_allyrs <- do.call(rbind, lapply(split(df_allyrs, df_allyrs$Model.Year), SetClimateValues.AllYrs, climate_data=ETo.monthly$`1`, colname='ETo.Jan'))
    df_allyrs <- do.call(rbind, lapply(split(df_allyrs, df_allyrs$Model.Year), SetClimateValues.AllYrs, climate_data=ETo.monthly$`2`, colname='ETo.Feb'))
    df_allyrs <- do.call(rbind, lapply(split(df_allyrs, df_allyrs$Model.Year), SetClimateValues.AllYrs, climate_data=ETo.monthly$`3`, colname='ETo.Mar'))
    df_allyrs <- do.call(rbind, lapply(split(df_allyrs, df_allyrs$Model.Year), SetClimateValues.AllYrs, climate_data=ETo.monthly$`4`, colname='ETo.Apr'))
    df_allyrs <- do.call(rbind, lapply(split(df_allyrs, df_allyrs$Model.Year), SetClimateValues.AllYrs, climate_data=ETo.monthly$`5`, colname='ETo.May'))
    df_allyrs <- do.call(rbind, lapply(split(df_allyrs, df_allyrs$Model.Year), SetClimateValues.AllYrs, climate_data=ETo.monthly$`6`, colname='ETo.Jun'))
    df_allyrs <- do.call(rbind, lapply(split(df_allyrs, df_allyrs$Model.Year), SetClimateValues.AllYrs, climate_data=ETo.monthly$`7`, colname='ETo.Jul'))
    df_allyrs <- do.call(rbind, lapply(split(df_allyrs, df_allyrs$Model.Year), SetClimateValues.AllYrs, climate_data=ETo.monthly$`8`, colname='ETo.Aug'))
    df_allyrs <- do.call(rbind, lapply(split(df_allyrs, df_allyrs$Model.Year), SetClimateValues.AllYrs, climate_data=ETo.monthly$`9`, colname='ETo.Sep'))
    df_allyrs <- do.call(rbind, lapply(split(df_allyrs, df_allyrs$Model.Year), SetClimateValues.AllYrs, climate_data=ETo.monthly$`10`, colname='ETo.Oct'))
    df_allyrs <- do.call(rbind, lapply(split(df_allyrs, df_allyrs$Model.Year), SetClimateValues.AllYrs, climate_data=ETo.monthly$`11`, colname='ETo.Nov'))
    df_allyrs <- do.call(rbind, lapply(split(df_allyrs, df_allyrs$Model.Year), SetClimateValues.AllYrs, climate_data=ETo.monthly$`12`, colname='ETo.Dec'))
    df_allyrs <- cbind(df_allyrs[ ,1:7], round(df_allyrs[ ,8:ncol(df_allyrs)], 3))
    fname <- paste0(gsub('_clean.csv', '', fnames[i]), '_MUaggregated.csv')
    write.csv(df_allyrs, file.path(final.resultsDir, cropname, fname), row.names = FALSE)
    rm(df_allyrs)
    gc()
  }
}
#run the function
data.to.allyrs('alfalfa.intermountain', 'Alfalfa')
data.to.allyrs('pistachios', 'Pistachios')
data.to.allyrs('walnut.mature', 'Walnuts')
data.to.allyrs('grapes.table', 'Grapes')
data.to.allyrs('alfalfa.imperial', 'Alfalfa')
data.to.allyrs('alfalfa.CV', 'Alfalfa')
data.to.allyrs('almond.mature', 'Almonds')
data.to.allyrs('grapes.wine', 'Grapes')










