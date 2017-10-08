points.resultsDir <- 'D:/Allowable_Depletion/results'
clean.resultsDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/results/Sep2017/clean_results' #needs to be changed
original.resultsDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/results/Sep2017'
pointsDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/results/data.frames/Aug2017'
modelscaffoldDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/model_scaffold/run_model/Sep2017'
setwd(modelscaffoldDir)
list.files()
cropscape_legend <- read.csv('cropscape_legend.txt', stringsAsFactors = FALSE)
alfalfa_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Alfalfa'] #75380 total
grape_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Grapes']
almond_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Almonds']
walnut_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Walnuts']
pistachio_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Pistachios']
#cell_numbers_of_interest <- read.csv('cellnumbers_to_modelcodes.csv', stringsAsFactors = FALSE)
#raster.model.codes <- raster('model.codes.Aug2017.tif')
P.df <- read.csv('PRISM.precip.data.updated9.13.17.csv', stringsAsFactors = F) #this is a daily summary of precip from 10/1/2003-6/25/17 from 'free' daily PRISM 4km resolution for cells of interest in California, created in download_PRISM.R script (from 6/26/17 download); blanks checked for in 'data_QA_QC.R'
#U2.df <- read.csv('SpatialCIMIS.U2.updated9.13.17.csv', stringsAsFactors = F) #this is a daily summary of wind data from download of spatial CIMIS data, created in spatialCIMIS.R script.  No missing data except for cell 148533
#RHmin.df <- read.csv('SpatialCIMIS.RHmin.updated9.13.17.csv', stringsAsFactors = F) #this is a daily summary of minimum relative humidity, estimated from download of spatial CIMIS Tdew and Tmax data, created in spatialCIMIS.R script.  Blanks filled on "12_08_2011" in data_QA_QC.R.  Now, no missing data except for cell 148533
ETo.df <- read.csv('SpatialCIMIS.ETo.updated9.13.17.csv', stringsAsFactors = F) #this is a daily summary of reference ET from download of spatial CIMIS data, created in spatialCIMIS.R script.  Blanks filled on multiple days in data_QA_QC.R.  Now, no missing data except for cell 148533

#read-in all points of interest
setwd(pointsDir)
model_points <- read.csv('mukeys_cropcodes_climatecodes_AEA.csv')
model_points$mukey_cropcode <- NULL
model_points$model_code <- NULL
# sum(model_points$crop_code==75, na.rm=TRUE) #equal to 547,945.9 ha or 1,353,427 acres, an overestimate for almonds
# sum(model_points$crop_code==76, na.rm=TRUE)
# almond_points <- model_points[which(model_points$crop_code==almond_code),]
# walnut_points <- model_points[which(model_points$crop_code==walnut_code),]
# grape_points <- model_points[which(model_points$crop_code==grape_code),]
# pistachio_points <- model_points[which(model_points$crop_code==pistachio_code),]
# alfalfa_points <- model_points[which(model_points$crop_code==alfalfa_code),]

#read in climate summaries produced by script below
setwd(file.path(original.resultsDir, 'climate_summaries'))
list.files()
prism.annual.sums <- read.csv('P.WY.summary.10.6.17.csv', stringsAsFactors = FALSE)
ETo.summary <- read.csv('ETo.WY.summary.10.6.17.csv', stringsAsFactors = FALSE)
RHmin.summary <- read.csv('RHmin.WY.summary.10.6.17.csv', stringsAsFactors = FALSE)
U2.summary <- read.csv('U2.WY.summary.10.6.17.csv', stringsAsFactors = FALSE)


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

#summarize P and ETo data by water.year
#get the mean water year ETo, U2, and RHmin by cell number
ClimateAggregate <- function(climate_df, varname, modify, Jdev, Jharv) {
  climate_df$water.year <- climate_df$year
  climate_df$water.year[which(climate_df$month >= 10)] <- climate_df$water.year[which(climate_df$month >= 10)] + 1
  by.year <- split(climate_df, climate_df$water.year)
  if (modify=='yes') {
    for (j in 1:length(by.year)) { #get the unnecessary rows out now
      by.year[[j]] <- by.year[[j]][which(by.year[[j]][,5]==Jharv):which(by.year[[j]][,5]==Jdev),] #row 1 in each of these data.frames is Oct. 1st, start of the water.year; Jharv is leaf-drop; Jdev is bloom (not including alfalfa)
    }
  }
  for (j in 1:length(by.year)) { #get the unnecessary columns out now, including last column which is water.year
    by.year[[j]] <- by.year[[j]][,6:(ncol(by.year[[j]])-1)]
  }
  annual.summary <- do.call(rbind, lapply(by.year, sapply, if(varname=='ETo') {sum} else{mean}, na.rm=TRUE)) #sapply was necessary so that each "cell" of the results matrix was not returned as a list object
  annual.summary <- t(annual.summary)
  annual.summary <- as.data.frame(annual.summary)
  annual.summary$cell_name <- rownames(annual.summary)
  annual.summary$CIMIScellnumber <- as.integer(gsub('cell_', '', annual.summary$cell_name))
  colnames(annual.summary)
  annual.summary[[paste0(varname, '.mean.annual')]] <- apply(annual.summary[,1:14], 1, mean)
  annual.summary
}

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
AggregateSoilPars <- function(df, varname) {
  df <- df[which(df$Model.Year==2004), ] #only need one year to get the soil stats
  compsums <- as.data.frame(tapply(df$comppct_r, df$unique_model_code, sum))
  colnames(compsums) <- 'compsums'
  compsums$unique_model_code <- as.integer(rownames(compsums))
  compsums$compsums <- as.numeric(compsums$compsums)
  #left off here
  results <- cbind(df[ ,c(varname, 'comppct_r')], compsums[match(df$unique_model_code, compsums$unique_model_code), ])
  var.final <- tapply(results[[varname]]*(results$comppct_r/results$compsums), results$unique_model_code, sum)
  var.final <- as.data.frame(var.final)
  colnames(var.final) <- 'var.final'
  var.final$unique_model_code <- rownames(var.final)
  rownames(var.final) <- NULL
  var.final$var.final <- as.numeric(var.final$var.final)
  var.final <- var.final[,c(2,1)]
  colnames(var.final)[2] <- varname
  var.final <- cbind(var.final, df[match(var.final$unique_model_code, df$unique_model_code), 'Model.Year'])
  colnames(var.final)[3] <- 'Model.Year'
  var.final
}

#this aggregates the model results by map unit, so that there are no duplicate unique model codes in the results (i.e. unique model codes with more than one major component have their results averaged as a component weighted average).  For this function, results for all model years are maintained, whereas in AggregateResults() below, multiple years data is compressed into a single statistic.
MUAggregate <- function(df, varname) {
  #df <- almond2.0m_AD50[which(almond2.0m_AD50$Model.Year==2004),]
  year <- df$Model.Year[1]
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
    var.final
  }
}
#test the function with some almond results
# GW.ET.growing <- do.call(rbind, lapply(split(almond2.0m_AD50, almond2.0m_AD50$Model.Year), MUAggregate, varname='GW.ET.growing'))
# Irr.1 <- do.call(rbind, lapply(split(almond2.0m_AD50, almond2.0m_AD50$Model.Year), MUAggregate, varname='Irr.1'))
# Irr.Last <- do.call(rbind, lapply(split(almond2.0m_AD50, almond2.0m_AD50$Model.Year), MUAggregate, varname='Irr.Last'))

#get data for all years into the points.  It works for a dataframe of points with or without multiple model years
MUAggregate.AllYrs <- function(df) {
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
  result <- list(Irr.1_allyrs, Irr.Last_allyrs, GW.ET.growing_allyrs, Irr.app.total_allyrs, ET.growing_allyrs, E.growing_allyrs, T.growing_allyrs, H2O.stress_allyrs, GW.E.to.Irr1_allyrs, GW.T.to.Irr1_allyrs, GW.ET.to.Irr1_allyrs, ET.annual_allyrs, E.annual_allyrs, T.annual_allyrs, deep.perc.annual_allyrs, winter.deep.perc_allyrs, post.Irr1.deep.perc_allyrs, fall.deep.perc_allyrs, GW.capture.net_allyrs, Dr.end.season_allyrs) #this excludes the following data collected during model runs: "RAW.end.season" "PAW.end.season" "P.end.season" "Irr.end.storage"
  names(result) <- c('Irr.1.doy', 'Irr.Last.doy', 'GW.ET.growing', 'Irr.app.total', 'ET.growing', 'E.growing', 'T.growing', 'H2O.stress', 'GW.E.to.Irr1', 'GW.T.to.Irr1', 'GW.ET.to.Irr1', 'ET.annual', 'E.annual', 'T.annual', 'deep.perc.annual', 'winter.deep.perc', 'post.Irr1.deep.perc', 'fall.deep.perc', 'GW.capture.net', 'Dr.end.season')
  gc()
  result
}

#now, distribute these results to points across all years
SetPointValues <- function(points_df, var_df, varname){
  points_df[[varname]] <- var_df[[varname]][match(points_df$unique_model_code, var_df$unique_model_code)]
  gc()
  points_df
}
SetPointValues.AllYrs <- function(var_df, varname, points_df) {
  if (is.null(points_df$Model.Year)) {
    model.year <- var_df$Model.Year[1]
    points_df$Model.Year <- model.year
    points_df[[varname]] <- var_df[[varname]][match(points_df$unique_model_code, var_df$unique_model_code)]
    points_df
  } else {
    model.year <- var_df$Model.Year[1]
    subset_points_df <- points_df[which(points_df$Model.Year==model.year), ]
    subset_points_df[[varname]] <- var_df[[varname]][match(subset_points_df$unique_model_code, var_df$unique_model_code)]
    gc()
    subset_points_df
  }
}
SetPointValues.AllYrs.Combined <- function(df, points) { #note that gc() is called from function defined above, which is called within this function
  for (i in seq_along(df)) {
    points_results <- do.call(rbind, lapply(split(df[[i]], df[[i]]$Model.Year), SetPointValues.AllYrs, varname=names(df)[i], points_df= if (i==1) {points} else {points_results}))
    #print(i)
  }
  points_results
}
SetPointPrecipValues.AllYrs <- function(df, precip_data, colname) {
  model.year <- as.character(df$Model.Year[1])
  df[[colname]] <- precip_data[[model.year]][match(df$PRISMcellnumber, precip_data$PRISMcellnumber)]
  gc()
  df
}
SetPointClimateValues.AllYrs <- function(df, varname, climate_data) {
  model.year <- as.character(df$Model.Year[1])
  df[[varname]] <- climate_data[[model.year]][match(df$CIMIScellnumber, climate_data$CIMIScellnumber)]
  gc()
  df
}
#replace scenario.resultsDir with clean.resultsDir
#run the functions above to loop through each crop's results folder to get the results to points on the landscape
data.to.points <- function(cropname, cropcode) {
  setwd(file.path(clean.resultsDir, cropname))
  fnames <- list.files()
  print(fnames)
  for (i in 1:seq_along(fnames)) {
    print(i)
    scenario_name <- gsub('_FAO56results_clean.csv', '', fnames[i])
    scenario_name <- paste0('scenario_', gsub(cropname, '', scenario_name))
    setwd(file.path(original.resultsDir, paste0(cropname, '_majcomps'), scenario_name))
    model_metadata <- read.csv(list.files(pattern = glob2rx('*_model_metadata.csv')), stringsAsFactors = FALSE)
    paw.varname <- model_metadata$paw.varname
    if (i==1) {
      Jdev <- model_metadata$Jdev
      Jharv <- model_metadata$Jharv
      P.df$water.year <- P.df$year
      P.df$water.year[which(P.df$month >= 10)] <- P.df$water.year[which(P.df$month >= 10)] + 1
      prism.by.year <- split(P.df, P.df$water.year)
      for (j in 1:length(prism.by.year)) { #get the unnecessary rows out now
        prism.by.year[[j]] <- prism.by.year[[j]][which(prism.by.year[[j]][,5]==Jharv):which(prism.by.year[[j]][,5]==Jdev),] #this trims each data.frame to leaf-drop(Jharv) to bloom date (Jdev)
      }
      for (j in 1:length(prism.by.year)) { #get the unnecessary columns out now, including last column which is water.year
        prism.by.year[[j]] <- prism.by.year[[j]][,6:(ncol(prism.by.year[[j]])-1)]
      }
      prism.winter.sums <- do.call(rbind, lapply(prism.by.year, sapply, sum)) #sapply was necessary so that each "cell" of the results matrix was not returned as a list object
      prism.winter.sums <- t(prism.winter.sums)
      prism.winter.sums <- as.data.frame(prism.winter.sums)
      prism.winter.sums$cell_name <- rownames(prism.winter.sums)
      prism.winter.sums$PRISMcellnumber <- as.integer(gsub('cell_', '', prism.winter.sums$cell_name))
      ETo.winter.sums <- ClimateAggregate(ETo.df, 'ETo', 'yes', Jdev, Jharv) ##now, do the same for ETo
    }
    setwd(file.path(clean.resultsDir, cropname))
    df <- read.csv(fnames[i], stringsAsFactors = FALSE)
    paw <- AggregateSoilPars(df, paw.varname)
    TEW <- AggregateSoilPars(df, 'TEW')
    REW <- AggregateSoilPars(df, 'REW')
    paw$paw_mm <- paw[[paw.varname]]*10
    results <- MUAggregate.AllYrs(df)
    points_of_interest <- model_points[which(model_points$crop_code==cropcode),]
    points_oi_allyrs <- SetPointValues.AllYrs.Combined(results, points_of_interest)
    points_oi_allyrs <- do.call(rbind, lapply(split(points_oi_allyrs, points_oi_allyrs$Model.Year), SetPointPrecipValues.AllYrs, precip_data=prism.annual.sums, colname='P.annual'))
    points_oi_allyrs <- do.call(rbind, lapply(split(points_oi_allyrs, points_oi_allyrs$Model.Year), SetPointPrecipValues.AllYrs, precip_data=prism.winter.sums, colname='P.winter')) #Jharv to Jdev
    points_oi_allyrs <- do.call(rbind, lapply(split(points_oi_allyrs, points_oi_allyrs$Model.Year), SetPointClimateValues.AllYrs, varname='ETo.annual', climate_data=ETo.summary)) #Jharv to Jdev
    points_oi_allyrs <- do.call(rbind, lapply(split(points_oi_allyrs, points_oi_allyrs$Model.Year), SetPointClimateValues.AllYrs, varname='ETo.winter', climate_data=ETo.winter.sums))
    points_oi_allyrs <- do.call(rbind, lapply(split(points_oi_allyrs, points_oi_allyrs$Model.Year), SetPointClimateValues.AllYrs, varname='RHmin.mean', climate_data=RHmin.summary))
    points_oi_allyrs <- do.call(rbind, lapply(split(points_oi_allyrs, points_oi_allyrs$Model.Year), SetPointClimateValues.AllYrs, varname='U2.mean', climate_data=U2.summary))
    points_oi_allyrs <- SetPointValues(points_oi_allyrs, paw, 'paw_mm')
    points_oi_allyrs <- SetPointValues(points_oi_allyrs, TEW, 'TEW')
    points_oi_allyrs <- SetPointValues(points_oi_allyrs, TEW, 'REW')
    if (!dir.exists(file.path(points.resultsDir, cropname))) {
      dir.create(file.path(points.resultsDir, cropname))
    }
    setwd(file.path(points.resultsDir, cropname))
    points_oi_allyrs <- cbind(points_oi_allyrs[ ,1:10], round(points_oi_allyrs[ ,11:ncol(points_oi_allyrs)], 3))
    gc()
    fname <- paste0(gsub('_clean.csv', '', fnames[i]), '_points_rounded.csv')
    write.csv(points_oi_allyrs, fname, row.names = FALSE)
    gc()
  }
}

#run the function
data.to.points('walnut.mature', walnut_code)
data.to.points('pistachios', pistachio_code)
data.to.points('grapes.table', grape_code)
data.to.points('almond.mature', almond_code)

#make a box plot of green water availability by year [not finished]
gw_bp <- boxplot(GW.ET.growing ~ Model.Year, data=almond_points_allyrs, plot=FALSE)
years <- 2004:2016
for (i in 1:ncol(gw_bp$stats)) {
  df <- almond_points_allyrs[which(almond_points_allyrs$Model.Year==years[i]),]
  gw_bp$stats[,i] <- CustomBP(df$GW.ET.growing)
  
}

#make a Blue Water boxplot by year
bw_bp <- boxplot(Irr.app.total ~ Model.Year, data=almond_points_allyrs, plot=FALSE)
for (i in 1:ncol(bw_bp$stats)) {
  df <- almond_points_allyrs[which(almond_points_allyrs$Model.Year==years[i]),]
  bw_bp$stats[,i] <- CustomBP(df$Irr.app.total)
}
setwd(file.path(SepResultsDir, 'figures'))
png(paste('almond2.0m50ADbluewaterdemand.png', sep = ''), family = 'Book Antiqua', width = 7, height = 5, units = 'in', res = 600)
par(mai=c(0.9, 0.9, 0.2, 0.2))
bxp(bw_bp, outline = FALSE, boxfill='lightblue', las=2, ylab='', xlab='')
mtext(text='Year', side=1, line=3.5)
mtext(text='Irrigation applied (mm)', side=2, line=3.5)
dev.off()

#make an ET.growing boxplot
et_bp <- boxplot(ET.growing ~ Model.Year, data=almond_points_allyrs, plot=FALSE)
for (i in 1:ncol(et_bp$stats)) {
  df <- almond_points_allyrs[which(almond_points_allyrs$Model.Year==years[i]),]
  et_bp$stats[,i] <- CustomBP(df$ET.growing)
}
setwd(file.path(SepResultsDir, 'figures'))
png(paste('almond2.0m50AD.ET.growing.png', sep = ''), family = 'Book Antiqua', width = 7, height = 5, units = 'in', res = 600)
par(mai=c(0.9, 0.9, 0.2, 0.2))
bxp(et_bp, outline = FALSE, boxfill='orange', las=2, ylab='', xlab='')
mtext(text='Year', side=1, line=3.5)
mtext(text='Growing season ET (mm)', side=2, line=3.5)
dev.off()

#make an annual P boxplot
P_bp <- boxplot(annual.P ~ Model.Year, data=almond_points_allyrs, plot=FALSE)
for (i in 1:ncol(P_bp$stats)) {
  df <- almond_points_allyrs[which(almond_points_allyrs$Model.Year==years[i]),]
  P_bp$stats[,i] <- CustomBP(df$annual.P)
}
setwd(file.path(SepResultsDir, 'figures'))
png(paste('almond.annual.P.png', sep = ''), family = 'Book Antiqua', width = 7, height = 5, units = 'in', res = 600)
par(mai=c(0.9, 0.9, 0.2, 0.2))
bxp(P_bp, outline = FALSE, boxfill='blue', las=2, ylab='', xlab='')
mtext(text='Year', side=1, line=3.5)
mtext(text='Annual precipitation (mm)', side=2, line=3.5)
dev.off()

#single year combined histogram and boxplot of blue water
nf <- layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,3))
par(mar=c(3.1, 3.1, 1.1, 2.1))
boxplot(almond_points_allyrs$Irr.app.total[which(almond_points_allyrs$Model.Year==2016)], horizontal=TRUE,  outline=TRUE, frame=F, col = "lightblue")
hist(almond_points_allyrs$Irr.app.total[which(almond_points_allyrs$Model.Year==2016)], col='lightblue')

#plot of BW vs. GW
setwd(file.path(SepResultsDir, 'figures'))
png(paste('almond2.0m50AD_BWvsGW.png', sep = ''), family = 'Book Antiqua', width = 5, height = 5, units = 'in', res = 600)
par(mai=c(0.9, 0.9, 0.2, 0.2))
smoothScatter(x=almond_points_allyrs$GW.ET.growing, y=almond_points_allyrs$Irr.app.total, xlab="" , ylab="")
mtext(text='Growing season green water availability', side=1, line=3.5)
mtext(text='Blue water demand (mm)', side=2, line=3.5)
dev.off()
#stats for BW vs. GW
lm(GW.ET.growing ~ Irr.app.total, data=almond_points_allyrs)

#plot of paw vs. mean annual GW (not all years)
setwd(file.path(SepResultsDir, 'figures'))
png(paste('almond2.0m50AD_PAWvsmean.GW.png', sep = ''), family = 'Book Antiqua', width = 5, height = 5, units = 'in', res = 600)
par(mai=c(0.9, 0.9, 0.2, 0.2))
smoothScatter(x=almond_points$paw_mm_2.0m, y=almond_points$GW.ET.growing, xlab="" , ylab="", xlim=c(0, 600))
mtext(text='Soil plant available water storage (mm)', side=1, line=2)
mtext(text='Mean growing season green water availability (mm)', side=2, line=2)
dev.off()

#plot of paw vs. mean annual GW (not all years)
setwd(file.path(SepResultsDir, 'figures'))
png(paste('almond2.0m50AD_Precip.vs.mean.GW.png', sep = ''), family = 'Book Antiqua', width = 5, height = 5, units = 'in', res = 600)
par(mai=c(0.9, 0.9, 0.2, 0.2))
smoothScatter(x=almond_points_allyrs$annual.P, y=almond_points_allyrs$GW.ET.growing, xlab="" , ylab="")
mtext(text='Annual precipitation (mm)', side=1, line=2)
mtext(text='Growing season green water availability (mm)', side=2, line=2)
dev.off()

#model GW vs. P + PAW
summary(lm(GW.ET.growing ~ annual.P + I(annual.P^2) + paw_mm_2.0m + I(paw_mm_2.0m^2) + paw_mm_2.0m*annual.P + I(paw_mm_2.0m*annual.P^2), data=almond_points_allyrs))
summary(lm(GW.ET.growing ~ annual.P + I(annual.P^2) + paw_mm_2.0m + I(paw_mm_2.0m^2) + paw_mm_2.0m*annual.P + I(paw_mm_2.0m*annual.P^2) + ET.growing + I(ET.growing^2), data=almond_points_allyrs))

#create results rasters for maps
RasterBuild(almond2.0m_AD50, "GW.ET.growing", 'almondGW.ET.growing.Sep2017runs.tif', mean, na.rm=TRUE)


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





