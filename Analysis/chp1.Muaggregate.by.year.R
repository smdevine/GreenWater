#TO-DO
#(1) Confirm that growing season ET is same as annual ET for alfalfa.CV and alfalfa.imperial
final.resultsDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/results/Oct2017/summaries'
clean.resultsDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/results/Oct2017/clean_results' #needs to be changed
original.resultsDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/results/Oct2017'
#pointsDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/results/data.frames/Aug2017'
modelscaffoldDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/model_scaffold/run_model/Oct2017'
if (!dir.exists(file.path(original.resultsDir, 'summaries'))) {
  dir.create(file.path(original.resultsDir, 'summaries'))
}
setwd(modelscaffoldDir)
# list.files()
# cropscape_legend <- read.csv('cropscape_legend.txt', stringsAsFactors = FALSE)
# alfalfa_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Alfalfa'] #75380 total
# grape_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Grapes']
# almond_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Almonds']
# walnut_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Walnuts']
# pistachio_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Pistachios']
# cell_numbers_of_interest <- read.csv('cellnumbers_to_modelcodes.csv', stringsAsFactors = FALSE)
# raster.model.codes <- raster('model.codes.Aug2017.tif')
P.df <- read.csv('PRISM.precip.data.updated9.13.17.csv', stringsAsFactors = F) #this is a daily summary of precip from 10/1/2003-6/25/17 from 'free' daily PRISM 4km resolution for cells of interest in California, created in download_PRISM.R script (from 6/26/17 download); blanks checked for in 'data_QA_QC.R'
U2.df <- read.csv('SpatialCIMIS.U2.updated9.13.17.csv', stringsAsFactors = F) #this is a daily summary of wind data from download of spatial CIMIS data, created in spatialCIMIS.R script.  No missing data except for cell 148533
RHmin.df <- read.csv('SpatialCIMIS.RHmin.updated9.13.17.csv', stringsAsFactors = F) #this is a daily summary of minimum relative humidity, estimated from download of spatial CIMIS Tdew and Tmax data, created in spatialCIMIS.R script.  Blanks filled on "12_08_2011" in data_QA_QC.R.  Now, no missing data except for cell 148533
ETo.df <- read.csv('SpatialCIMIS.ETo.updated9.13.17.csv', stringsAsFactors = F) #this is a daily summary of reference ET from download of spatial CIMIS data, created in spatialCIMIS.R script.  Blanks filled on multiple days in data_QA_QC.R.  Now, no missing data except for cell 148533

#read in climate summaries produced by script below
# setwd(file.path(original.resultsDir, 'climate_summaries'))
# list.files()
# prism.annual.sums <- read.csv('P.WY.summary.10.6.17.csv', stringsAsFactors = FALSE)
# ETo.summary <- read.csv('ETo.WY.summary.10.6.17.csv', stringsAsFactors = FALSE)
# RHmin.summary <- read.csv('RHmin.WY.summary.10.6.17.csv', stringsAsFactors = FALSE)
# U2.summary <- read.csv('U2.WY.summary.10.6.17.csv', stringsAsFactors = FALSE)


#get the water year P total by cell number, including 2017 for annual average
# P.df$water.year <- P.df$year
# P.df$water.year[which(P.df$month >= 10)] <- P.df$water.year[which(P.df$month >= 10)] + 1
# prism.by.year <- split(P.df, P.df$water.year)
# for (j in 1:length(prism.by.year)) { #get the unnecessary columns out now
#   prism.by.year[[j]] <- prism.by.year[[j]][,6:(ncol(prism.by.year[[j]])-1)]
# }
# prism.annual.sums <- do.call(rbind, lapply(prism.by.year, sapply, sum)) #sapply was necessary so that each "cell" of the results matrix was not returned as a list object
# prism.annual.sums <- t(prism.annual.sums)
# prism.annual.sums <- as.data.frame(prism.annual.sums)
# prism.annual.sums$cell_name <- rownames(prism.annual.sums)
# prism.annual.sums$PRISMcellnumber <- as.integer(gsub('cell_', '', prism.annual.sums$cell_name))
# colnames(prism.annual.sums)
# prism.annual.sums$mean.annual.P <- apply(prism.annual.sums[,1:14], 1, mean)
# gc()
# setwd(file.path(original.resultsDir, 'climate_summaries'))
# write.csv(prism.annual.sums, 'P.WY.summary.10.6.17.csv', row.names = FALSE) #WY refers to 'water year'

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

# ETo.summary <- ClimateAggregate(ETo.df, 'ETo', 'no') #note that 2017 does not include latter half of September
# RHmin.summary <- ClimateAggregate(RHmin.df, 'RHmin', 'no')
# U2.summary <- ClimateAggregate(U2.df, 'U2', 'no')
# setwd(file.path(original.resultsDir, 'climate_summaries'))
# write.csv(ETo.summary, 'ETo.WY.summary.10.6.17.csv', row.names = FALSE) #WY refers to 'water year'
# write.csv(RHmin.summary, 'RHmin.WY.summary.10.6.17.csv', row.names = FALSE)
# write.csv(U2.summary, 'U2.WY.summary.10.6.17.csv', row.names = FALSE)

#this aggregates results across all years by mukey and unique model code according to the 'func', such as taking the mean GW.ET.growing for each unique combination of climate, soil, and crop from 2004-2016, with major component weighted averages.
# AggregateSoilPars <- function(df, varname, func, ...) {
#   df$unique_model_code_final <- paste0(as.character(df$cokey), as.character(df$unique_model_code)) #necessary because some original unique_model_code_final were trimmed because they were too long.  better if coerced to character first
#   var.by.year <- tapply(df[[varname]], df$unique_model_code_final, func, ...)
#   mukeys <- tapply(df$mukey, df$unique_model_code_final, unique)
#   comppct_r <- tapply(df$comppct_r, df$unique_model_code_final, unique)
#   modelcode <- tapply(df$unique_model_code, df$unique_model_code_final, unique)
#   results <- cbind(var.by.year, mukeys, comppct_r, modelcode)
#   results <- as.data.frame(results)
#   compsums <- as.data.frame(tapply(results$comppct_r[!is.na(results$var.by.year)], results$modelcode[!is.na(results$var.by.year)], sum))
#   colnames(compsums) <- 'compsums'
#   compsums$modelcode <- rownames(compsums)
#   results <- merge(results, compsums, by='modelcode')
#   var.final <- tapply(results$var.by.year*(results$comppct_r/results$compsums), results$modelcode, sum, na.rm=TRUE)
#   var.final <- as.data.frame(var.final)
#   colnames(var.final) <- 'var.final'
#   var.final$unique_model_code <- rownames(var.final)
#   var.final$var.final <- as.numeric(var.final$var.final)
#   var.final <- var.final[,c(2,1)]
#   colnames(var.final)[2] <- varname
#   var.final
# }
#35287 unique walnut codes
#these have all had model_codes eliminated ahead of time where PAW, TEW, or REW is NA or equal to 0, which would have been skipped in FAO56 dual crop coeff runs
# results <- cbind(df[!is.na(df[[varname]]), c(varname, 'comppct_r')], compsums[match(df$unique_model_code[!is.na(df[[varname]])], compsums$unique_model_code), ])
AggregateSoilPars <- function(df, paw.varname, cell.counts) {
  df <- df[which(df$Model.Year==2004), ] #only need one year to get the soil stats
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
  var.final$cellcounts30m2 <- cell.counts$cell_counts30m2[match(var.final$unique_model_code, cell.counts$unique_model_code)]
  rownames(var.final) <- NULL
  var.final
}

#this aggregates the model results by map unit, so that there are no duplicate unique model codes in the results (i.e. unique model codes with more than one major component have their results averaged as a component weighted average).  For this function, results for all model years are maintained, whereas in AggregateResults() below, multiple years data is compressed into a single statistic.
MUAggregate <- function(df, varname) {
  year <- df$Model.Year[1]
  if (year==2003 | year==2017) { #test code to simplify results collection
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

#get data for all years into the points.  It works for a dataframe of points with or without multiple model years
MUAggregate.AllYrs <- function(df, cropname) {
  Irr.1_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='Irr.1'))
  Irr.Last_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='Irr.Last'))
  GW.ET.growing_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='GW.ET.growing'))
  Irr.app.total_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='Irr.app.total'))
  ET.growing_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='ET.growing'))
  E.growing_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='E.growing'))
  T.growing_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='T.growing'))
  H2O.stress_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='H2O.stress'))
  GW.E.to.Irr1_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='GW.E.to.Irr1'))
  GW.T.to.Irr1_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='GW.T.to.Irr1'))
  GW.ET.to.Irr1_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='GW.ET.to.Irr1'))
  ET.annual_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='ET.annual'))
  E.annual_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='E.annual'))
  T.annual_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='T.annual'))
  deep.perc.annual_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='deep.perc.annual'))
  winter.deep.perc_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='winter.deep.perc'))
  post.Irr1.deep.perc_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='post.Irr1.deep.perc'))
  fall.deep.perc_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='fall.deep.perc'))
  GW.capture.net_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='GW.capture.net'))
  Dr.end.season_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname='Dr.end.season'))
  RAW.end.season_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname="RAW.end.season"))
  PAW.end.season_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname="PAW.end.season"))
  P.end.season_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname="P.end.season"))
  Irr.end.storage_allyrs <- do.call(rbind, lapply(split(df, df$Model.Year), MUAggregate, varname="Irr.end.storage"))
  result <- list(Irr.1_allyrs, Irr.Last_allyrs, GW.ET.growing_allyrs, Irr.app.total_allyrs, ET.growing_allyrs, E.growing_allyrs, T.growing_allyrs, H2O.stress_allyrs, GW.E.to.Irr1_allyrs, GW.T.to.Irr1_allyrs, GW.ET.to.Irr1_allyrs, ET.annual_allyrs, E.annual_allyrs, T.annual_allyrs, deep.perc.annual_allyrs, winter.deep.perc_allyrs, post.Irr1.deep.perc_allyrs, fall.deep.perc_allyrs, GW.capture.net_allyrs, Dr.end.season_allyrs, RAW.end.season_allyrs, PAW.end.season_allyrs, P.end.season_allyrs, Irr.end.storage_allyrs)  
  names(result) <- c('Irr.1.doy', 'Irr.Last.doy', 'GW.ET.growing', 'Irr.app.total', 'ET.growing', 'E.growing', 'T.growing', 'H2O.stress', 'GW.E.to.Irr1', 'GW.T.to.Irr1', 'GW.ET.to.Irr1', 'ET.annual', 'E.annual', 'T.annual', 'deep.perc.annual', 'winter.deep.perc', 'post.Irr1.deep.perc', 'fall.deep.perc', 'GW.capture.net', 'Dr.end.season', 'RAW.end.season_allyrs', 'PAW.end.season_allyrs', 'P.end.season_allyrs', 'Irr.end.storage_allyrs')
  #rm(Irr.1_allyrs, Irr.Last_allyrs, GW.ET.growing_allyrs, Irr.app.total_allyrs, ET.growing_allyrs, E.growing_allyrs, T.growing_allyrs, H2O.stress_allyrs, GW.E.to.Irr1_allyrs, GW.T.to.Irr1_allyrs, GW.ET.to.Irr1_allyrs, ET.annual_allyrs, E.annual_allyrs, T.annual_allyrs, deep.perc.annual_allyrs, winter.deep.perc_allyrs, post.Irr1.deep.perc_allyrs, fall.deep.perc_allyrs, GW.capture.net_allyrs, Dr.end.season_allyrs, RAW.end.season_allyrs, PAW.end.season_allyrs, P.end.season_allyrs, Irr.end.storage_allyrs)
  #gc()
  if (cropname=='alfalfa.imperial') {
    result['GW.capture.net'] <- NULL
  }
  result
}

#now, distribute these results to points across all years
SetValues <- function(main_df, var_df, varname){
  main_df[[varname]] <- var_df[[varname]][match(main_df$unique_model_code, var_df$unique_model_code)]
  #gc()
  main_df
}
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
  P.df <- read.csv(file.path(modelscaffoldDir, 'PRISM.precip.data.updated9.13.17.csv'), stringsAsFactors = FALSE)
  ETo.df <- read.csv(file.path(modelscaffoldDir, 'SpatialCIMIS.ETo.updated9.13.17.csv'), stringsAsFactors = FALSE)
  #U2.df <- read.csv('SpatialCIMIS.U2.updated9.13.17.csv', stringsAsFactors = F) #this is a daily summary of wind data from download of spatial CIMIS data, created in spatialCIMIS.R script.  No missing data except for cell 148533
  #RHmin.df <- read.csv('SpatialCIMIS.RHmin.updated9.13.17.csv', stringsAsFactors = F)
  cell.counts <- read.csv(file.path(modelscaffoldDir, 'cell_counts.csv'), stringsAsFactors = FALSE)
  #setwd(file.path(clean.resultsDir, cropname))
  fnames <- list.files(file.path(clean.resultsDir, cropname))
  print(fnames)
  for (i in seq_along(fnames)) {
    print(i)
    scenario_name <- gsub('_FAO56results_clean.csv', '', fnames[i])
    scenario_name <- paste0('scenario_', gsub(cropname, '', scenario_name))
    scenario_name <- gsub('AD', '', scenario_name)
    scenario_name <- gsub('RDI.min', '', scenario_name)
    if (cropname=='grapes.wine') {
      scenario_name <- paste0(scenario_name, 'RDI.min')
    } else {
        scenario_name <- paste0(scenario_name, 'AD')
      }
    
    #setwd(file.path(original.resultsDir, paste0(cropname, '_majcomps'), scenario_name))
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
    print(Jharv)
    if (i==1) {
      ETo.annual <- ClimateAggregate(climate_df = ETo.df, varname = 'ETo', winter = 'no', Jdev=NA, Jharv=NA, WY.basis = 'no', growing = 'no')
      ETo.WY <- ClimateAggregate(climate_df = ETo.df, varname = 'ETo', winter = 'no', Jdev=NA, Jharv=NA, WY.basis = 'yes', growing='no')
      P.annual <- PrecipAggregate(precip_df = P.df, winter = 'no', Jdev=NA, Jharv=NA, WY.basis='no', growing='no')
      P.WY <- PrecipAggregate(precip_df = P.df, winter = 'no', Jdev=NA, Jharv=NA, WY.basis='yes', growing='no')
      if (cropname2=='Pistachios' | cropname2=='Almonds' | cropname2=='Walnuts' | cropname2=='Grapes' | cropname=='alfalfa.intermountain') {  #this block not relevant to alfalfa.CV or alfalfa.imperial
        P.winter <- PrecipAggregate(precip_df = P.df, winter='yes', Jdev=Jdev, Jharv=Jharv, WY.basis = 'yes', growing='no')
        P.growing <- PrecipAggregate(precip_df = P.df, winter='no', Jdev=Jdev, Jharv=Jharv, WY.basis = 'no', growing='yes')
        ETo.winter <- ClimateAggregate(climate_df = ETo.df, varname = 'ETo', winter = 'yes', Jdev=Jdev, Jharv=Jharv, WY.basis = 'yes', growing = 'no')
        ETo.growing <- ClimateAggregate(climate_df = ETo.df, varname = 'ETo', winter = 'no', Jdev=Jdev, Jharv=Jharv, WY.basis = 'no', growing = 'yes')
      }
    }
    #setwd(file.path(clean.resultsDir, cropname))
    df <- read.csv(file.path(clean.resultsDir, cropname, fnames[i]), stringsAsFactors = FALSE)
    soil_summary <- AggregateSoilPars(df, paw.varname, cell.counts)
    if (!dir.exists(file.path(final.resultsDir, cropname))) {
      dir.create(file.path(final.resultsDir, cropname))
    }
    if (!dir.exists(file.path(final.resultsDir, cropname, 'MUaggregated_soildata'))) {
      dir.create(file.path(final.resultsDir, cropname, 'MUaggregated_soildata'))
    }
    #setwd(file.path(final.resultsDir, cropname, 'MUaggregated_soildata'))
    write.csv(soil_summary, file.path(final.resultsDir, cropname, 'MUaggregated_soildata', paste0(scenario_name, '_soilsdata.csv')), row.names = FALSE)
    results <- MUAggregate.AllYrs(df, cropname)
    #df_allyrs <- df[rep(seq.int(1, nrow(df)), 15), ] #change hard coding here
    #df_allyrs$years <- rep(2003:2017, nrow(df)) #change hard coding here
    df_allyrs <- SetValues.AllYrs.Combined(results, soil_summary) #check functions above
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
    } #in order to add alfalfa.intermountain, need manual Jdev and Jharv
    df_allyrs <- cbind(df_allyrs[ ,1:18], round(df_allyrs[ ,19:ncol(df_allyrs)], 3))
    fname <- paste0(gsub('_clean.csv', '', fnames[i]), '_points_rounded.csv')
    #setwd(file.path(final.resultsDir, cropname))
    write.csv(df_allyrs, file.path(final.resultsDir, cropname, fname), row.names = FALSE)
    rm(df_allyrs)
    gc()
  }
}
#run the function
data.to.allyrs('pistachios', 'Pistachios')
data.to.allyrs('walnut.mature', 'Walnuts')
data.to.allyrs('grapes.table', 'Grapes')
data.to.allyrs('almond.mature', 'Almonds')
data.to.allyrs('alfalfa.imperial', 'Alfalfa')
data.to.allyrs('grapes.wine', 'Grapes')#redefined function here to handle grapes.wine directory naming convention using RDI.min
data.to.allyrs('alfalfa.CV', 'Alfalfa')

#need to investigate this further for file/directory naming convention

data.to.allyrs('alfalfa.intermountain', 'Alfalfa')
#these results don't inlcude the P.winter or ETo.winter columns

#this result also does not include the GW.capture.net column
 #this result does not include the 

#run for P and ETo
#grapes.table



#this is to obtain mean values across years, except for paw, which is static; almond_points_allyrs has the data by year
almond_gw_et <- AggregateResults(almond2.0m_AD50, 'GW.ET.growing', mean, na.rm=TRUE) #this produces 69,080 rows; concordance with above!
almond_paw <- AggregateResults(almond2.0m_AD50, 'z2.0m_cmH2O_modified_comp', unique)
almond_paw$paw_mm_2.0m <- almond_paw$z2.0m_cmH2O_modified_comp*10
almond_irr_app <- AggregateResults(almond2.0m_AD50, 'Irr.app.total', mean, na.rm=TRUE)
almond_et <- AggregateResults(almond2.0m_AD50, 'ET.growing', mean, na.rm=TRUE)
almond_e <- AggregateResults(almond2.0m_AD50, 'E.growing', mean, na.rm=TRUE)
almond_points <- SetPointValues(almond_points, almond_gw_et, 'GW.ET.growing')
almond_points <- SetPointValues(almond_points, almond_e, 'E.growing')
almond_points <- SetPointValues(almond_points, almond_et, 'ET.growing')
almond_points <- SetPointValues(almond_points, almond_paw, 'paw_mm_2.0m')
almond_points$paw_mm <- 10*almond_points$z2.0m_cmH2O_modified_comp
summary(lm(almond_points$GW.ET.growing ~ almond_points$paw_mm_2.0m))





