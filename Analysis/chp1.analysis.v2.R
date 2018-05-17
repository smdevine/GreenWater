#TO-DO
# (1) Make paw rasters for allcrops datasets
# (2) Make rasters for 0.5 m root depth x 30% AD model run
resultsDir <- 'D:/Allowable_Depletion/results/Mar2018/summaries'
modelscaffoldDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/model_scaffold/run_model/Mar2018'
dissertationDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/dissertation/v2.results'
cropscape_legend <- read.csv(file.path(modelscaffoldDir, 'cropscape_legend.txt'), stringsAsFactors = FALSE)
alfalfa_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Alfalfa']
grape_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Grapes']
almond_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Almonds']
walnut_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Walnuts']
pistachio_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Pistachios']
library(raster)
library(extrafont)
library(extrafontdb)
library(spatstat)
#get each crop's results for a given scenario, rbind together and save as csv in all_crops
#note that P.growing and ETo.growing are given the values of P.annual and ETo.annual for alfalfa.imperial and alfalfa.CV
bindallresults <- function(scenario_name, paw_name) {
  cropdf <- data.frame(cropnames = c('alfalfa.intermountain', 'alfalfa.imperial', 'alfalfa.CV', 'almond.mature', 'walnut.mature', 'pistachios', 'grapes.table', 'grapes.wine'), cropcode = c(alfalfa_code, alfalfa_code, alfalfa_code, almond_code, walnut_code, pistachio_code, grape_code, grape_code))
  for (i in seq_along(cropdf$cropnames)) {
    print(i)
    cropfnames <- list.files(path = file.path(resultsDir, cropdf$cropnames[i]), pattern = glob2rx(paste0('*_FAO56results_MUaggregated.csv')))
    if (i == 1) { #as long as grapes.wine is not the first cropname
      scenario_index <- grep(scenario_name, cropfnames)
      result <- read.csv(file.path(resultsDir, cropdf$cropnames[i], cropfnames[scenario_index]), stringsAsFactors = FALSE)
      #colnames(result)[2:3] <- c(paste0(paw_name, '.cmH2O'), paste0(paw_name, '.mmH2O'))
      result$cropcode <- cropdf$cropcode[i]
      result$cropname <- cropdf$cropnames[i]
      #print(colnames(result))
    } else {
        nextdf <- read.csv(file.path(resultsDir, cropdf$cropnames[i], cropfnames[scenario_index]), stringsAsFactors = FALSE)
        #colnames(nextdf)[2:3] <- c(paste0(paw_name, '.cmH2O'), paste0(paw_name, '.mmH2O'))
        nextdf$cropcode <- cropdf$cropcode[i]
        nextdf$cropname <- cropdf$cropnames[i]
        #print(colnames(nextdf))
        result <- rbind(result, nextdf)
      }
  }
  if (!dir.exists(file.path(resultsDir, 'allcrops'))) {
    dir.create(file.path(resultsDir, 'allcrops'))
  }
  write.csv(result, file.path(resultsDir, 'allcrops', paste0('allcrops', scenario_name, '_FAO56results_MUaggregated.csv')), row.names = FALSE)
}
bindallresults('0.5mAD30', '0.5mPAW')
bindallresults('0.5mAD50', '0.5mPAW')
bindallresults('0.5mAD80', '0.5mPAW')
bindallresults('1.0mAD30', '1.0mPAW')
bindallresults('1.0mAD50', '1.0mPAW')
bindallresults('1.0mAD80', '1.0mPAW')
bindallresults('2.0mAD30', '2.0mPAW')
bindallresults('2.0mAD50', '2.0mPAW')
bindallresults('2.0mAD80', '2.0mPAW')
bindallresults('3.0mAD30', '3.0mPAW')
bindallresults('3.0mAD50', '3.0mPAW')
bindallresults('3.0mAD80', '3.0mPAW')

bindsoilresults <- function(scenario_name, paw_name) {
  cropdf <- data.frame(cropnames = c('alfalfa.intermountain', 'alfalfa.imperial', 'alfalfa.CV', 'almond.mature', 'walnut.mature', 'pistachios', 'grapes.table', 'grapes.wine'), cropcode = c(alfalfa_code, alfalfa_code, alfalfa_code, almond_code, walnut_code, pistachio_code, grape_code, grape_code))
  for (i in seq_along(cropdf$cropnames)) {
    print(i)
    soilfnames <- list.files(path = file.path(resultsDir, cropdf$cropnames[i], 'MUaggregated_soildata'), pattern = glob2rx('*_soilsdata.csv'))
    if (i == 1) { #as long as grapes.wine is not the first cropname
      scenario_index <- grep(scenario_name, cropfnames)
      result <- read.csv(file.path(resultsDir, cropdf$cropnames[i], 'MUaggregated_soildata', soilfnames[scenario_index]), stringsAsFactors = FALSE)
      colnames(result)[2:3] <- c(paste0(paw_name, '.cmH2O'), paste0(paw_name, '.mmH2O'))
      result$cropcode <- cropdf$cropcode[i]
      result$cropname <- cropdf$cropnames[i]
      #print(colnames(result))
    } else {
      nextdf <- read.csv(file.path(resultsDir, cropdf$cropnames[i], 'MUaggregated_soildata', soilfnames[scenario_index]), stringsAsFactors = FALSE)
      colnames(nextdf)[2:3] <- c(paste0(paw_name, '.cmH2O'), paste0(paw_name, '.mmH2O'))
      nextdf$cropcode <- cropdf$cropcode[i]
      nextdf$cropname <- cropdf$cropnames[i]
      #print(colnames(nextdf))
      result <- rbind(result, nextdf)
    }
  }
  if (!dir.exists(file.path(resultsDir, 'allcrops', 'MUaggregated_soildata'))) {
    dir.create(file.path(resultsDir, 'allcrops', 'MUaggregated_soildata'))
  }
  write.csv(result, file.path(resultsDir, 'allcrops', 'MUaggregated_soildata', paste0('allcrops', scenario_name, '_soilsdata.csv')), row.names = FALSE)
}
bindsoilresults('0.5mAD30', '0.5mPAW')
bindsoilresults('0.5mAD50', '0.5mPAW')
bindsoilresults('0.5mAD80', '0.5mPAW')
bindsoilresults('1.0mAD30', '1.0mPAW')
bindsoilresults('1.0mAD50', '1.0mPAW')
bindsoilresults('1.0mAD80', '1.0mPAW')
bindsoilresults('2.0mAD30', '2.0mPAW')
bindsoilresults('2.0mAD50', '2.0mPAW')
bindsoilresults('2.0mAD80', '2.0mPAW')
bindsoilresults('3.0mAD30', '3.0mPAW')
bindsoilresults('3.0mAD50', '3.0mPAW')
bindsoilresults('3.0mAD80', '3.0mPAW')
#qc check on binding
scenario_name <- '2.0mAD50'
df <- read.csv(file.path(resultsDir, 'allcrops', paste0('allcrops', scenario_name, '_FAO56results_MUaggregated.csv')), stringsAsFactors = FALSE)
df.grapes.wine <- read.csv(file.path(resultsDir, 'grapes.wine', paste0('grapes.wine', scenario_name, '_FAO56results_MUaggregated.csv')), stringsAsFactors = FALSE)
df.grapes.wine[which(df.grapes.wine$unique_model_code==100793 & df.grapes.wine$Model.Year==2008),]
df[which(df$unique_model_code==100793 & df$Model.Year==2008),]

#look at underlying allcrops data for 2.0m x 50% AD
df <- read.csv(file.path(resultsDir, 'allcrops','allcrops2.0mAD50_FAO56results_MUaggregated.csv'), stringsAsFactors = FALSE)
area.summary <- read.csv(file.path(modelscaffoldDir, 'hectares_by_model_code2018-05-14.csv'), stringsAsFactors = FALSE)
sum(!df$unique_model_code %in% area.summary$unique_model_code) #check that all codes are accounted for

#m3 per acre foot assumption: see below
#function to quantify water volumes for green water, blue water, precip
#updated to work with shapefile basis of v2 model run March 2018
volumes.calculate.AF <- function(cropname) {
  AF.to.m3 <- 43560 * (12 ^ 3) * (2.54 ^ 3) * (0.01 ^ 3)
  area.summary <- read.csv(file.path(modelscaffoldDir, 'hectares_by_model_code2018-05-14.csv'), stringsAsFactors = FALSE)
  cropfnames <- list.files(path = file.path(resultsDir, cropname), pattern = glob2rx('*_FAO56results_MUaggregated.csv'))
  soilfnames <- list.files(path = file.path(resultsDir, cropname, 'MUaggregated_soildata'), pattern = glob2rx('*_soilsdata.csv'))
  if (!dir.exists(file.path(resultsDir, cropname, 'allyrs_stats'))) {
    dir.create(file.path(resultsDir, cropname, 'allyrs_stats'))
  }
  if (!dir.exists(file.path(resultsDir, cropname, 'allyrs_stats', 'AF.summaries'))) {
    dir.create(file.path(resultsDir, cropname, 'allyrs_stats', 'AF.summaries'))
  }
  if (!dir.exists(file.path(resultsDir, cropname, 'allyrs_stats', 'TAF.summaries'))) {
    dir.create(file.path(resultsDir, cropname, 'allyrs_stats', 'TAF.summaries'))
  }
  if (!dir.exists(file.path(resultsDir, cropname, 'allyrs_stats', 'km3.summaries'))) {
    dir.create(file.path(resultsDir, cropname, 'allyrs_stats', 'km3.summaries'))
  }
  if (!dir.exists(file.path(resultsDir, cropname, 'allyrs_stats', 'depth.inch.summaries'))) {
    dir.create(file.path(resultsDir, cropname, 'allyrs_stats', 'depth.inch.summaries'))
  }
  if (!dir.exists(file.path(resultsDir, cropname, 'allyrs_stats', 'depth.mm.summaries'))) {
    dir.create(file.path(resultsDir, cropname, 'allyrs_stats', 'depth.mm.summaries'))
  }
  for (i in seq_along(cropfnames)) {
    var_df <- read.csv(file.path(resultsDir, cropname, cropfnames[i]), stringsAsFactors = FALSE)
    soils_df <- read.csv(file.path(resultsDir, cropname, 'MUaggregated_soildata', soilfnames[i]), stringsAsFactors = FALSE)
    scenario_name <- gsub('_FAO56results_MUaggregated.csv', '', cropfnames[i])
    AD.percentage <- as.integer(substr(scenario_name, nchar(scenario_name) - 1, nchar(scenario_name)))
    scenario_name <- paste0('scenario.', gsub(cropname, '', scenario_name))
    var_df$hectares <- area.summary$hectares[match(var_df$unique_model_code, area.summary$unique_model_code)]
    modeled_ha <- sum(var_df$hectares[var_df$Model.Year==2004]) #same as sum(soils_df$hectares)
    if (modeled_ha != sum(soils_df$hectares)) {
      stop('Check area discrepancy between soil summary and results files.')
    }
    volume.stats.AF <- data.frame(Model.Year=unique(var_df$Model.Year), hectares = round(modeled_ha, 0), acres = round(modeled_ha * 2.47105, 0), green.water.AF = round(tapply(var_df$GW.ET.growing * (10 / AF.to.m3) * var_df$hectares, var_df$Model.Year, sum, na.rm=TRUE), 0))
    volume.stats.AF$blue.water.AF <- round(tapply(var_df$Irr.app.total * (10 / AF.to.m3) * var_df$hectares, var_df$Model.Year, sum, na.rm=TRUE), 0)
    volume.stats.AF$ET.growing.AF <- round(tapply(var_df$ET.growing * (10 / AF.to.m3) * var_df$hectares, var_df$Model.Year, sum, na.rm=TRUE), 0)
    volume.stats.AF$E.growing.AF <- round(tapply(var_df$E.growing * (10 / AF.to.m3) * var_df$hectares, var_df$Model.Year, sum, na.rm=TRUE), 0)
    volume.stats.AF$deep.perc.annual.AF <- round(tapply(var_df$deep.perc.annual * (10 / AF.to.m3) * var_df$hectares, var_df$Model.Year, sum, na.rm=TRUE), 0)
    volume.stats.AF$crop.stress.annual.AF <- round(tapply(var_df$crop.stress.annual * (10 / AF.to.m3) * var_df$hectares, var_df$Model.Year, sum, na.rm=TRUE), 0)
    volume.stats.AF$GW.ET.to.Irr1.AF <- round(tapply(var_df$GW.ET.to.Irr1 * (10 / AF.to.m3) * var_df$hectares, var_df$Model.Year, sum, na.rm=TRUE), 0)
    volume.stats.AF$GW.E.to.Irr1.AF <- round(tapply(var_df$GW.E.to.Irr1 * (10 / AF.to.m3) * var_df$hectares, var_df$Model.Year, sum, na.rm=TRUE), 0)
    volume.stats.AF$deep.perc.growing.AF <- round(tapply(var_df$deep.perc.growing * (10 / AF.to.m3) * var_df$hectares, var_df$Model.Year, sum, na.rm=TRUE), 0)
    volume.stats.AF$non.irr.deep.perc.AF <- round(tapply(var_df$non.irr.deep.perc * (10 / AF.to.m3) * var_df$hectares, var_df$Model.Year, sum, na.rm=TRUE), 0)
    volume.stats.AF$Irr1.to.last.deep.perc.AF <- round(tapply(var_df$Irr1.to.last.deep.perc * (10 / AF.to.m3) * var_df$hectares, var_df$Model.Year, sum, na.rm=TRUE), 0)
    volume.stats.AF$fall.deep.perc.AF <- round(tapply(var_df$fall.deep.perc * (10 / AF.to.m3) * var_df$hectares, var_df$Model.Year, sum, na.rm=TRUE), 0)
    volume.stats.AF$crop.stress.growing.AF <- round(tapply(var_df$crop.stress.growing * (10 / AF.to.m3) * var_df$hectares, var_df$Model.Year, sum, na.rm=TRUE), 0)
    volume.stats.AF$ET.winter.AF <- round(tapply((var_df$ET.annual - var_df$ET.growing) * (10 / AF.to.m3) * var_df$hectares, var_df$Model.Year, sum, na.rm=TRUE), 0)
    volume.stats.AF$E.winter.AF <- round(tapply((var_df$E.annual - var_df$E.growing) * (10 / AF.to.m3) * var_df$hectares, var_df$Model.Year, sum, na.rm=TRUE), 0)
    volume.stats.AF$year.end.depletion.AF <- round(tapply(var_df$Dr.end.year * (10 / AF.to.m3) * var_df$hectares, var_df$Model.Year, sum, na.rm=TRUE), 0)
    volume.stats.AF$end.season.depletion.AF <- round(tapply(var_df$Dr.end.season * (10 / AF.to.m3) * var_df$hectares, var_df$Model.Year, sum, na.rm=TRUE), 0)
    volume.stats.AF$begin.season.depletion.AF <- round(tapply(var_df$Dr.begin.season * (10 / AF.to.m3) * var_df$hectares, var_df$Model.Year, sum, na.rm=TRUE), 0)
    volume.stats.AF$irr.app.last.AF <- round(tapply(var_df$Irr.app.last * (10 / AF.to.m3) * var_df$hectares, var_df$Model.Year, sum, na.rm=TRUE), 0)
    volume.stats.AF$P.annual.AF <- round(tapply(var_df$P.annual * (10 / AF.to.m3) * var_df$hectares, var_df$Model.Year, sum, na.rm=TRUE), 0)
    volume.stats.AF$P.end.season.AF <- round(tapply(var_df$P.end.season * (10 / AF.to.m3) * var_df$hectares, var_df$Model.Year, sum, na.rm=TRUE), 0)
    volume.stats.AF$P.growing.AF <- round(tapply(var_df$P.growing * (10 / AF.to.m3) * var_df$hectares, var_df$Model.Year, sum, na.rm=TRUE), 0)
    volume.stats.AF$P.winter.AF <- round(tapply(var_df$P.winter * (10 / AF.to.m3) * var_df$hectares, var_df$Model.Year, sum, na.rm=TRUE), 0)
    volume.stats.AF$AWS.storage.AF <- round(sum(soils_df[,3] * (10 / AF.to.m3) * soils_df$hectares), 0)
    volume.stats.AF$AD.storage.AF <- round(sum(soils_df[,3] * (AD.percentage / 100) * (10 / AF.to.m3) * soils_df$hectares), 0)
    volume.stats.AF$TEW.storage.AF <- round(sum(soils_df$TEW * (10 / AF.to.m3) * soils_df$hectares), 0)
    volume.stats.AF$REW.storage.AF <- round(sum(soils_df$REW * (10 / AF.to.m3) * soils_df$hectares), 0)
    volume.stats.AF$GW_to_ET <- round(volume.stats.AF$green.water.AF / volume.stats.AF$ET.growing.AF, 2)
    volume.stats.AF$GW_to_P <- round(volume.stats.AF$green.water.AF / volume.stats.AF$P.annual.AF, 2)
    volume.stats.AF$irr.count.wtd.avg <- round(tapply(var_df$Irr.Count * var_df$hectares, var_df$Model.Year, sum) / modeled_ha, 1)
    volume.stats.AF$irr.1.doy.wtd.avg <- round(tapply(var_df$Irr.1.doy * var_df$hectares, var_df$Model.Year, sum) / modeled_ha, 1)
    volume.stats.AF$last.irr.doy.wtd.avg <- round(tapply(var_df$Irr.Last.doy * var_df$hectares, var_df$Model.Year, sum) / modeled_ha, 1)
    volume.stats.TAF <- data.frame(Model.Year=volume.stats.AF$Model.Year, round(volume.stats.AF[,4:30] / 1000, 1))
    colnames(volume.stats.TAF)[2:ncol(volume.stats.TAF)] <- gsub('AF', 'TAF', colnames(volume.stats.TAF)[2:ncol(volume.stats.TAF)])
    volume.stats.km3 <- data.frame(Model.Year = volume.stats.AF$Model.Year, round(volume.stats.AF[,4:30] * (AF.to.m3/(1000^3)), 3))
    colnames(volume.stats.km3)[2:ncol(volume.stats.km3)] <- gsub('AF', 'km3', colnames(volume.stats.km3)[2:ncol(volume.stats.km3)])
    depth.stats.inches <- data.frame(Model.Year = volume.stats.AF$Model.Year, round(volume.stats.AF[,4:30] / (modeled_ha * 2.47105) * 12, 2)) 
    colnames(depth.stats.inches)[2:ncol(depth.stats.inches)] <- gsub('AF', 'inches', colnames(depth.stats.inches)[2:ncol(depth.stats.inches)])
    depth.stats.mm <- data.frame(Model.Year = volume.stats.AF$Model.Year, round(volume.stats.AF[,4:30] * (AF.to.m3 / (modeled_ha * 10)), 2))
    colnames(depth.stats.mm)[2:ncol(depth.stats.mm)] <- gsub('AF', 'mm', colnames(depth.stats.mm)[2:ncol(depth.stats.mm)])
    write.csv(volume.stats.AF, file.path(resultsDir, cropname, 'allyrs_stats', 'AF.summaries', paste0(scenario_name, '.AFsummary.by.year.csv')), row.names = FALSE)
    write.csv(volume.stats.TAF, file.path(resultsDir, cropname, 'allyrs_stats', 'TAF.summaries', paste0(scenario_name, '.TAFsummary.by.year.csv')), row.names = FALSE)
    write.csv(volume.stats.km3, file.path(resultsDir, cropname, 'allyrs_stats', 'km3.summaries', paste0(scenario_name, '.km3summary.by.year.csv')), row.names = FALSE)
    write.csv(depth.stats.inches, file.path(resultsDir, cropname, 'allyrs_stats', 'depth.inch.summaries', paste0(scenario_name, '.inches.summary.by.year.csv')), row.names = FALSE)
    write.csv(depth.stats.mm, file.path(resultsDir, cropname, 'allyrs_stats', 'depth.mm.summaries', paste0(scenario_name, '.mm.summary.by.year.csv')), row.names = FALSE)
  }
}
volumes.calculate.AF('alfalfa.intermountain')
volumes.calculate.AF('alfalfa.imperial')
volumes.calculate.AF('alfalfa.CV')
volumes.calculate.AF('almond.mature')
volumes.calculate.AF('walnut.mature')
volumes.calculate.AF('pistachios')
volumes.calculate.AF('grapes.table')
volumes.calculate.AF('grapes.wine')
volumes.calculate.AF('allcrops')

#get 2005-2017 totals for these calculated volumes
combine.scenarios <- function(cropname, year_start, year_end) {
  AF.to.km3 <- 43560 * (12 ^ 3) * (2.54 ^ 3) * (0.01 ^ 3) * (0.001 ^ 3)
  fnames_full <- list.files(path=file.path(resultsDir, cropname, 'allyrs_stats', 'AF.summaries'), pattern=glob2rx(pattern='*.csv'), full.names = TRUE)
  fnames <- list.files(path=file.path(resultsDir, cropname, 'allyrs_stats', 'AF.summaries'), pattern=glob2rx(pattern='*.csv'))
  scenario.AF.summary <- as.data.frame(matrix(data=NA, nrow=length(fnames), ncol=20))
  colnames(scenario.AF.summary) <- c('root.depth', 'allowable.depletion', 'AD.inches', 'irr.count', 'irr.1.doy', 'last.irr.doy', 'green.water', 'blue.water', 'evaporation', 'deep.percolation', 'crop.stress.annual', 'dormant.ET', 'precipitation', 'crop.stress.growing', 'non.irr.deep.perc', 'Irr1.to.last.deep.perc', 'fall.deep.perc', 'delta.S.model.span', 'P.balance.error', 'P.balance.error.perc')
  for (i in seq_along(fnames)) {
    df <- read.csv(fnames_full[i], stringsAsFactors = FALSE)
    start_yr <- which(df$Model.Year==year_start)
    stop_yr <- which(df$Model.Year==year_end)
    fname <- fnames[i]
    scenario.AF.summary$root.depth[i] <- as.numeric(substr(fname, 10, 12))
    scenario.AF.summary$allowable.depletion[i] <- as.numeric(substr(fname, 16, 17))
    scenario.AF.summary$AD.inches[i] <- round(df$AD.storage.AF[1]/df$acres[1] * 12, 2)
    scenario.AF.summary$irr.count[i] <- round(mean(df$irr.count.wtd.avg), 1)
    scenario.AF.summary$irr.1.doy[i] <- round(mean(df$irr.1.doy.wtd.avg), 1)
    scenario.AF.summary$last.irr.doy[i] <- round(mean(df$last.irr.doy.wtd.avg), 1)
    scenario.AF.summary$green.water[i] <- sum(df$green.water.AF[start_yr:stop_yr]) #rows 2:14 are years 2005-2017
    scenario.AF.summary$blue.water[i] <- sum(df$blue.water.AF[start_yr:stop_yr])
    scenario.AF.summary$evaporation[i] <- sum(df$E.growing.AF[start_yr:stop_yr])
    scenario.AF.summary$deep.percolation[i] <- sum(df$deep.perc.annual.AF[start_yr:stop_yr])
    scenario.AF.summary$crop.stress.annual[i] <- sum(df$crop.stress.annual.AF[start_yr:stop_yr])
    scenario.AF.summary$dormant.ET[i] <- sum(df$ET.winter.AF[start_yr:stop_yr])
    scenario.AF.summary$precipitation[i] <- sum(df$P.annual.AF[start_yr:stop_yr])
    scenario.AF.summary$crop.stress.growing[i] <- sum(df$crop.stress.growing.AF[start_yr:stop_yr])
    scenario.AF.summary$non.irr.deep.perc[i] <- sum(df$non.irr.deep.perc.AF[start_yr:stop_yr])
    scenario.AF.summary$Irr1.to.last.deep.perc[i] <- sum(df$Irr1.to.last.deep.perc.AF[start_yr:stop_yr])
    scenario.AF.summary$fall.deep.perc[i] <- sum(df$fall.deep.perc.AF[start_yr:stop_yr])
    scenario.AF.summary$delta.S.model.span[i] <- df$year.end.depletion.AF[stop_yr] - df$year.end.depletion.AF[start_yr-1]
  }
  scenario.AF.summary$P.balance.error <- scenario.AF.summary$green.water + scenario.AF.summary$deep.percolation + scenario.AF.summary$dormant.ET - scenario.AF.summary$precipitation
  scenario.AF.summary$P.balance.error.perc <- round(100 * (scenario.AF.summary$P.balance.error / scenario.AF.summary$precipitation), 2)
  scenario.AF.summary <- scenario.AF.summary[order(scenario.AF.summary$AD.inches), ]
  scenario.MAF.summary <- cbind(scenario.AF.summary[,1:6], round(scenario.AF.summary[7:(ncol(scenario.AF.summary) - 1)] / 10^6, 3), scenario.AF.summary['P.balance.error.perc'])
  scenario.TAF.summary <- cbind(scenario.AF.summary[,1:6], round(scenario.AF.summary[7:(ncol(scenario.AF.summary) - 1)] / 10^3, 0), scenario.AF.summary['P.balance.error.perc'])
  scenario.km3.summary <- cbind(scenario.AF.summary[,1:6], round(scenario.AF.summary[7:(ncol(scenario.AF.summary) - 1)] * AF.to.km3, 3), scenario.AF.summary['P.balance.error.perc'])
  scenario.km3.summary$AD.inches <- scenario.km3.summary$AD.inches * 25.4
  colnames(scenario.km3.summary)[which(colnames(scenario.km3.summary) == 'AD.inches')] <- 'AD.mm'
  write.csv(scenario.MAF.summary, file=file.path(dissertationDir, 'tables', 'MAF.summaries', paste0(cropname, '.all.scenarios.MAF.summary.csv')), row.names=FALSE)
  write.csv(scenario.TAF.summary, file=file.path(dissertationDir, 'tables', 'TAF.summaries', paste0(cropname, '.all.scenarios.TAF.summary.csv')), row.names=FALSE)
  write.csv(scenario.km3.summary, file=file.path(dissertationDir, 'tables', 'km3.summaries', paste0(cropname, 'all.scenario.km3.summary.csv')), row.names=FALSE)
}
combine.scenarios('alfalfa.intermountain', 2005, 2017)
combine.scenarios('almond.mature')
combine.scenarios('alfalfa.imperial')
combine.scenarios('alfalfa.CV')
combine.scenarios('walnut.mature')
combine.scenarios('pistachios')
combine.scenarios('grapes.table')
combine.scenarios('grapes.wine')
combine.scenarios('allcrops')

#combine variables across crops and scenarios from stats summaries in mm
#connected summaries with allyrs_stats_v4, which is only 2005-2016 results
make.crop.water.depth.table <- function(scenario_name, stat) {
  cropnames <- c('almond.mature', 'alfalfa.intermountain', 'alfalfa.imperial', 'alfalfa.CV', 'walnut.mature', 'pistachios', 'grapes.table', 'grapes.wine')
  paw_varname_end <- ifelse(grepl('alfalfa', cropnames), '_mmH2O_unmodified_comp', '_mmH2O_modified_comp')
  paw_varnames <- paste0('z', substr(scenario_name, 1, 4), paw_varname_end) 
  varnames <- c('cropname', 'GW.ET.growing', 'Irr.app.total', 'E.growing', 'deep.perc.annual', 'post.Irr1.deep.perc', 'H2O.stress', 'P.annual', 'ETo.growing', 'TEW', 'PAW')
  varnames.v2 <- c('cropname', 'GW.ET.growing', 'Irr.app.total', 'E.growing', 'deep.perc.annual', 'post.Irr1.deep.perc', 'H2O.stress', 'P.annual', 'ETo.annual', 'TEW', 'PAW')
  var.summary <- as.data.frame(matrix(data=NA, nrow=length(cropnames), ncol=length(varnames)))
  colnames(var.summary) <- varnames
  var.summary$cropname <- cropnames
  for (i in seq_along(cropnames)) {
    paw_varname <- paw_varnames[i]
    varnames[length(varnames)] <- paw_varname
    varnames.v2[length(varnames.v2)] <- paw_varname
    if (cropnames[i]=='alfalfa.CV' | cropnames[i]=='alfalfa.imperial') {
      varnames.temp <- varnames.v2
    } else {varnames.temp <- varnames}
    if (cropnames[i]=='grapes.wine') {
      scenario_name_wine <- paste0(substr(scenario_name, 1, 4), if(substr(scenario_name, 7, 8)=='30') {'RDI.min0.8'} else if(substr(scenario_name, 7, 8)=='50') {'RDI.min0.5'} else if(substr(scenario_name, 7, 8)=='80'){'RDI.min0.2'})
      fname <- paste0(cropnames[i], scenario_name_wine, '_FAO56_summarystats_', '2005_2016', '.csv')
    } else {
      fname <- paste0(cropnames[i], scenario_name, '_FAO56_summarystats_', '2005_2016', '.csv') #example fname almond.mature0.5mAD30_FAO56_summarystats_2005_2016.csv
    }
    fname_full <- file.path(resultsDir, cropnames[i], 'allyrs_stats_v4', fname)
    df <- read.csv(fname_full, stringsAsFactors = FALSE)
    var.summary[i, 2:length(varnames.temp)] <- df[match(varnames.temp[2:length(varnames.temp)], df$varname), stat]
  }
  write.csv(var.summary, file=file.path(dissertationDir, 'tables', paste0(scenario_name, '.', stat, '.crop.water.balance.mm.csv')), row.names=FALSE)
  var.summary
}
#make.crop.water.depth.table('2.0mAD50', 'Median')
#make.crop.water.depth.table('1.0mAD50', 'Median')
#make.crop.water.depth.table('0.5mAD30', 'Median')
#re-run 2/27/18 for NRCS Chico presentation
make.crop.water.depth.table('2.0mAD50', 'Mean')
make.crop.water.depth.table('1.0mAD50', 'Mean')
make.crop.water.depth.table('0.5mAD30', 'Mean')

#font_import() #only needs to be done once?
CustomBP <- function(x) {
  lower.x <- quantile(x, 0.1, na.rm = TRUE)
  q1.x <- quantile(x, 0.25, na.rm = TRUE)
  med.x <- median(x, na.rm = TRUE)
  q3.x <- quantile(x, 0.75, na.rm = TRUE)
  upper.x <- quantile(x, 0.9, na.rm = TRUE)
  c(lower.x, q1.x, med.x, q3.x, upper.x)
}

#plotting function
#to-do take out Irr and ETo boxplot for all scenarios; only needs to be done once for each crop unlike GW and BW
#re-ran almond figures 11/15 to get larger font
#re-ran all figures 12/11 to get years 2005-2016 and 
MakeBPs <- function(cropname, years=2004:2016) {
  make.bp <- function(bp_name, yaxis_lab, varname, bxfill) {
    png(file.path(resultsDir, cropname, 'figures', scenario_name, paste0('allyrs.boxplots', varname, '.png', sep = '')), family = 'Book Antiqua', width = 7, height = 5, units = 'in', res = 600)
    par(mai=c(0.9, 0.9, 0.2, 0.2))
    bxp(bp_name, outline = FALSE, boxfill=bxfill, las=2, ylab='', xlab='', cex.axis=1.2)
    mtext(text='Year', side=1, line=3.5, cex = 1.2)
    mtext(text=yaxis_lab, side=2, line=3.5, cex = 1.2)
    dev.off()
  }
  define.bp.stats <- function(bp_name, varname) { #this is to replace default boxplot stats with custom function
    for (i in 1:ncol(bp_name$stats)) { 
      df <- var_df[which(var_df$Model.Year==years[i]),]
      bp_name$stats[,i] <- CustomBP(rep(df[[varname]], times=df$cellcounts30m2))
    }
  }
  loadfonts(quiet=TRUE, device='win')
  fnames <- list.files(path=file.path(resultsDir, cropname), pattern = glob2rx('*.csv'))
  for (j in seq_along(fnames)) {
    var_df <- read.csv(file.path(resultsDir, cropname, fnames[j]), stringsAsFactors = FALSE)
    scenario_name <- gsub('_FAO56results_points_rounded.csv', '', fnames[j])
    scenario_name <- paste0('scenario_', gsub(cropname, '', scenario_name))
    bp_GW <- boxplot(GW.ET.growing ~ Model.Year, data=var_df, plot=FALSE)
    bp_BW <- boxplot(Irr.app.total ~ Model.Year, data=var_df, plot=FALSE)
    bp_P <- boxplot(P.WY ~ Model.Year, data=var_df, plot=FALSE)
    bp_ETo <- boxplot(ETo.WY ~ Model.Year, data=var_df, plot=FALSE)
    define.bp.stats(bp_GW, 'GW.ET.growing')
    define.bp.stats(bp_BW, 'Irr.app.total')
    define.bp.stats(bp_P, 'P.WY')
    define.bp.stats(bp_ETo, 'ETo.WY')
    if (!dir.exists(file.path(resultsDir, cropname, 'figures'))) {
      dir.create(file.path(resultsDir, cropname, 'figures'))
    }
    if (!dir.exists(file.path(resultsDir, cropname, 'figures', scenario_name))) {
      dir.create(file.path(resultsDir, cropname, 'figures', scenario_name))
    }
    make.bp(bp_name = bp_GW, yaxis_lab = 'Growing season green water (mm)', varname = 'GW.ET.growing', bxfill = 'green')
    make.bp(bp_name = bp_BW, yaxis_lab = 'Irrigation [blue water] demand (mm)', varname = 'Irr.app.total', bxfill = 'lightblue')
    make.bp(bp_name = bp_P, yaxis_lab = 'Water year precipitation (mm)', varname = 'P.WY', bxfill = bxfill = 'blue')
    make.bp(bp_name = bp_ETo, yaxis_lab = 'Reference evapotranspiration (mm)', varname = 'ETo.WY', bxfill = 'orange')
  } #bp_name, yaxis_lab, varname, bxfill
}

#run the function to make boxplots of P, ETo, green, and blue water by year for each crop
MakeBPs('alfalfa.intermountain')
MakeBPs('walnut.mature')
MakeBPs('almond.mature')
MakeBPs('pistachios')
MakeBPs('grapes.table')
MakeBPs('grapes.wine')
MakeBPs('alfalfa.CV')
MakeBPs('alfalfa.imperial')
MakeBPs('allcrops')

#updated boxplot function for green and blue water plots on 12/12 for presentation purposes
#goal is to get 4 boxplots on a page for each crop x scenario
#only done for 2.0 m root depth x 50% allowable depletion
MakeBPs.v2 <- function(cropname, years=2005:2016) {
  make.bp <- function(bp_name, yaxis_lab, xaxis=FALSE, plotxaxis='n', varname, bxfill, scenario_name) {
    
    if (plotxaxis == 'n') {
      png(file.path(dissertationDir, 'figures', 'boxplots', cropname, scenario_name, paste0(scenario_name, varname, '.png', sep = '')), family = 'Book Antiqua', width = 7.5, height = 2.2, units = 'in', res = 600)
      par(mai=c(0.1, 0.6, 0.15, 0.1))
    } else {
        png(file.path(dissertationDir, 'figures', 'boxplots', cropname, scenario_name, paste0(scenario_name, varname, '.png', sep = '')), family = 'Book Antiqua', width = 7.5, height = 2.7, units = 'in', res = 600)
        par(mai=c(0.6, 0.6, 0.15, 0.1))
      }
    bxp(bp_name, outline = FALSE, boxfill=bxfill, ylab='', cex.axis=1, boxwex=0.6, lwd=0.8, xaxt=plotxaxis) #ylim=c(0,400))
    if (xaxis) {
      mtext(text='Year', side=1, line=2, cex = 1, las=0)
    }
    mtext(text=yaxis_lab, side=2, line=2, cex = 1, las=0)
    dev.off()
  }
  define.bp.stats <- function(bp_name, varname) { #this is to replace default boxplot stats with custom function
    for (i in 1:ncol(bp_name$stats)) { 
      df <- var_df[which(var_df$Model.Year==years[i]),]
      bp_name$stats[,i] <- CustomBP(rep(df[[varname]], times=df$cellcounts30m2))
    }
  }
  loadfonts(quiet=TRUE, device='win')
  fnames <- list.files(path=file.path(resultsDir, cropname), pattern = glob2rx('*.csv'))
  for (j in 5) { #seq_along(fnames)) { note: 5 refers to 2.0 m x 50% AD scenario
    var_df <- read.csv(file.path(resultsDir, cropname, fnames[j]), stringsAsFactors = FALSE)
    var_df <- var_df[which(var_df$Model.Year %in% years),]
    var_df$GW.ET.growing[which(var_df$GW.ET.growing<0)] <- 0 #convert negative values to 0
    scenario_name <- gsub('_FAO56results_points_rounded.csv', '', fnames[j])
    scenario_name <- paste0('scenario_', gsub(cropname, '', scenario_name))
    bp_GW <- boxplot(GW.ET.growing ~ Model.Year, data=var_df, plot=FALSE)
    bp_BW <- boxplot(Irr.app.total ~ Model.Year, data=var_df, plot=FALSE)
    bp_P <- boxplot(P.WY ~ Model.Year, data=var_df, plot=FALSE)
    bp_ETo <- boxplot(ETo.WY ~ Model.Year, data=var_df, plot=FALSE)
    define.bp.stats(bp_GW, 'GW.ET.growing')
    define.bp.stats(bp_BW, 'Irr.app.total')
    define.bp.stats(bp_P, 'P.WY')
    define.bp.stats(bp_ETo, 'ETo.WY')
    if (!dir.exists(file.path(dissertationDir, 'figures', 'boxplots', cropname))) {
      dir.create(file.path(dissertationDir, 'figures', 'boxplots', cropname))
    }
    # if (!dir.exists(file.path(dissertationDir, 'figures', 'boxplots', cropname, 'green.water'))) {
    #   dir.create(file.path(dissertationDir, 'figures', 'boxplots', cropname, 'green.water'))
    # }
    # if (!dir.exists(file.path(dissertationDir, 'figures', 'boxplots', cropname, 'blue.water'))) {
    #   dir.create(file.path(dissertationDir, 'figures', 'boxplots', cropname, 'blue.water'))
    # }
    if (!dir.exists(file.path(dissertationDir, 'figures', 'boxplots', cropname, scenario_name))) {
      dir.create(file.path(dissertationDir, 'figures', 'boxplots', cropname, scenario_name))
    }
    make.bp(bp_name = bp_GW, yaxis_lab = 'Green water (mm)', varname = 'GW.ET.growing', bxfill = 'green', scenario_name = scenario_name)
    make.bp(bp_name = bp_BW, yaxis_lab = 'Blue water (mm)', varname = 'Irr.app.total', bxfill = 'lightblue', scenario_name = scenario_name)
    make.bp(bp_name = bp_P, yaxis_lab = 'Water year P (mm)', xaxis = TRUE, plotxaxis = 's', varname = 'P.WY', bxfill = 'blue', scenario_name = scenario_name)
    make.bp(bp_name = bp_ETo, yaxis_lab = 'Reference ET (mm)', varname = 'ETo.WY', bxfill = 'orange', scenario_name = scenario_name)
  } #bp_name, yaxis_lab, varname, bxfill
}
MakeBPs.v2('alfalfa.intermountain')
MakeBPs.v2('walnut.mature')
MakeBPs.v2('almond.mature')
MakeBPs.v2('pistachios')
MakeBPs.v2('grapes.table')
MakeBPs.v2('grapes.wine')
MakeBPs.v2('alfalfa.CV')
MakeBPs.v2('alfalfa.imperial')

#build a raster based on a single results filename and variable of interest
raster.model.codes <- raster(file.path(modelscaffoldDir, 'model.codes.Aug2017.tif')) #'model.codes.Aug2017.CA.TA.tif'))
cell_numbers_to_codes <- read.csv(file.path(modelscaffoldDir, 'cellnumbers_to_modelcodes.csv'), stringsAsFactors = FALSE)
RasterBuild <- function(readraster=FALSE, readcellnums=FALSE, cropname, fname, years) {
  if (!dir.exists(file.path(rasterResultsDir, cropname))) {
    dir.create(file.path(rasterResultsDir, cropname))
  }
  if (!dir.exists(file.path(rasterResultsDir, cropname, 'figures'))) {
    dir.create(file.path(rasterResultsDir, cropname, 'figures'))
  }
  rasterize.result.statistic <- function(varname, func, funcname, years) {
    raster.result <- raster.model.codes
    var_df2 <- var_df[which(var_df$Model.Year %in% years),]
    var_df_summary <- data.frame(varname = tapply(var_df2[[varname]], var_df2$unique_model_code, func))
    colnames(var_df_summary) <- paste0(varname, '.', funcname)
    var_df_summary$unique_model_code <- row.names(var_df_summary)
    raster.result[cell_numbers_to_codes$cell_numbers_of_interest] <- var_df_summary[[paste0(varname, '.', funcname)]][match(cell_numbers_to_codes$unique_model_codes, var_df_summary$unique_model_code)]
    if (!dir.exists(file.path(rasterResultsDir, cropname, 'figures', scenario_name, 'rasters', varname))) {
      dir.create(file.path(rasterResultsDir, cropname, 'figures', scenario_name, 'rasters', varname))
    }
    writeRaster(raster.result, file.path(rasterResultsDir, cropname, 'figures', scenario_name, 'rasters', varname, paste0(varname, '.', funcname, '.tif')), format='GTiff')
    removeTmpFiles(h=0.0001)
  }
  rasterize.result.annual <- function(varname, year) {
    raster.result <- raster.model.codes
    var_df_annual <- var_df[var_df$Model.Year==year, ]
    raster.result[cell_numbers_to_codes$cell_numbers_of_interest] <- var_df_annual[[varname]][match(cell_numbers_to_codes$unique_model_codes, var_df_annual$unique_model_code)]
    if (!dir.exists(file.path(rasterResultsDir, cropname, 'figures', scenario_name, 'rasters', varname))) {
      dir.create(file.path(rasterResultsDir, cropname, 'figures', scenario_name, 'rasters', varname))
    }
    writeRaster(raster.result, file.path(rasterResultsDir, cropname, 'figures', scenario_name, 'rasters', varname, paste0(varname, '.', as.character(year), '.tif')), format='GTiff')
    removeTmpFiles(h=0.0001)
  }
  if (readraster) {
    raster.model.codes <- raster(file.path(modelscaffoldDir, 'model.codes.Aug2017.tif'))
  }
  if (readcellnums) {
    cell_numbers_to_codes <- read.csv(file.path(modelscaffoldDir, 'cellnumbers_to_modelcodes.csv'), stringsAsFactors = FALSE)
  }
  var_df <- read.csv(file.path(resultsDir, cropname, fname), stringsAsFactors = FALSE)
  scenario_name <- gsub('_FAO56results_points_rounded.csv', '', fname)
  scenario_name <- paste0('scenario_', gsub(cropname, '', scenario_name))
  print(scenario_name)
  if (!dir.exists(file.path(rasterResultsDir, cropname, 'figures', scenario_name))) {
    dir.create(file.path(rasterResultsDir, cropname, 'figures', scenario_name))
  }
  if (!dir.exists(file.path(rasterResultsDir, cropname, 'figures', scenario_name, 'rasters'))) {
    dir.create(file.path(rasterResultsDir, cropname, 'figures', scenario_name, 'rasters'))
  }
  # rasterize.result.statistic(varname = 'GW.ET.growing', func = mean, 'mean', years = years)
  # rasterize.result.statistic(varname = 'Irr.app.total', func = mean, 'mean', years = years)
  # rasterize.result.statistic(varname = 'E.growing', func = mean, 'mean', years = years)
  # rasterize.result.statistic(varname = 'deep.perc.annual', func = max, 'max', years = years)
  # rasterize.result.statistic(varname = 'Irr.1.doy', func = median, 'median')
  rasterize.result.statistic(varname='cropcode', func = unique, funcname = 'CS', years = years) #CS stands for CropScape
  # rasterize.result.statistic(varname='P.annual', func=mean, 'mean', years = years)
  # rasterize.result.statistic(varname='ETo.annual', func=mean, 'mean', years = years)
  # rasterize.result.statistic(varname = 'X1.0mPAW.mmH2O', func=median, 'SSURGO', years = years)
  # rasterize.result.statistic(varname='ETo.annual', func=mean, 'mean', years = years)
  # rasterize.result.statistic(varname = 'TEW', func=median, 'SSURGO', years = years)
  # rasterize.result.statistic(varname='ETo.growing', func=mean, 'mean', years = years)
}
#last run on 1.31.18 to export to Jan2018.AEA
RasterBuild(readraster=FALSE, readcellnums=FALSE, cropname = 'allcrops', fname = 'allcrops0.5mAD30_FAO56results_points_rounded.csv', years=2005:2016)
RasterBuild(readraster=FALSE, readcellnums=FALSE, cropname = 'allcrops', fname = 'allcrops1.0mAD50_FAO56results_points_rounded.csv', years=2005:2016)
RasterBuild(readraster=FALSE, readcellnums=FALSE, cropname = 'allcrops', fname = 'allcrops2.0mAD50_FAO56results_points_rounded.csv', years=2005:2016)
#confirm cell counts with valid crop codes vs. counts of cells with valid GW results
crop.codes <- raster(file.path(rasterResultsDir, 'allcrops', 'figures', 'scenario_2.0mAD50', 'rasters', 'cropcode', 'cropcode.CS.tif'))
sum(!is.na(crop.codes))
#18,039,013 have crop codes in USGS AEA
#  have crop codes in CA Teale Albers, because resolution changed after projection
sum(var_df$cellcounts30m2[var_df$Model.Year==2005 & !is.na(var_df$GW.ET.growing)]) #18,039,219 cells have data

#read-in raster.model.codes to global environment if you so choose
#function updates include option to convert negative GW to zero (not recommened as of 1/2018)
raster.model.codes <- raster(file.path(modelscaffoldDir, 'model.codes.Aug2017.CA.TA.tif'))
cell_numbers_to_codes <- read.csv(file.path(modelscaffoldDir, 'test_cellnumbers_to_modelcodes.CA.TA.csv'), stringsAsFactors = FALSE)
RasterBuild.v2 <- function(readraster=FALSE, readcellnums=FALSE, cropname, convert.negative.GW) {
  if (!dir.exists(file.path(rasterResultsDir, cropname))) {
    dir.create(file.path(rasterResultsDir, cropname))
  }
  if (!dir.exists(file.path(rasterResultsDir, cropname, 'figures'))) {
    dir.create(file.path(rasterResultsDir, cropname, 'figures'))
  }
  rasterize.result.annual <- function(varname, year) {
    raster.result <- raster.model.codes
    var_df_annual <- var_df[var_df$Model.Year==year, ]
    raster.result[cell_numbers_to_codes$cell_numbers_of_interest] <- var_df_annual[[varname]][match(cell_numbers_to_codes$unique_model_codes, var_df_annual$unique_model_code)]
    if (!dir.exists(file.path(rasterResultsDir, cropname, 'figures', scenario_name, 'rasters', varname))) {
      dir.create(file.path(rasterResultsDir, cropname, 'figures', scenario_name, 'rasters', varname))
    }
    writeRaster(raster.result, file.path(rasterResultsDir, cropname, 'figures', scenario_name, 'rasters', varname, paste0(varname, '.', as.character(unique(var_df$Model.Year)[j]), '.tif')), format='GTiff')
    removeTmpFiles(h=0.0001)
  }
  rasterize.result.statistic <- function(varname, func, funcname, years) {
    raster.result <- raster.model.codes
    var_df2 <- var_df[which(var_df$Model.Year %in% years),]
    var_df_summary <- data.frame(varname = tapply(var_df2[[varname]], var_df2$unique_model_code, func))
    colnames(var_df_summary) <- paste0(varname, '.', funcname)
    var_df_summary$unique_model_code <- row.names(var_df_summary)
    raster.result[cell_numbers_to_codes$cell_numbers_of_interest] <- var_df_summary[[paste0(varname, '.', funcname)]][match(cell_numbers_to_codes$unique_model_codes, var_df_summary$unique_model_code)]
    if (!dir.exists(file.path(rasterResultsDir, cropname, 'figures', scenario_name, 'rasters', varname))) {
      dir.create(file.path(rasterResultsDir, cropname, 'figures', scenario_name, 'rasters', varname))
    }
    writeRaster(raster.result, file.path(rasterResultsDir, cropname, 'figures', scenario_name, 'rasters', varname, paste0(varname, '.', funcname, '.tif')), format='GTiff')
    removeTmpFiles(h=0.0001)
  }
  if (readraster) {
    raster.model.codes <- raster(file.path(modelscaffoldDir, 'model.codes.Aug2017.tif'))
  }
  if (readcellnums) {
    cell_numbers_to_codes <- read.csv(file.path(modelscaffoldDir, 'cellnumbers_to_modelcodes.csv'), stringsAsFactors = FALSE)
  }
  cropfnames <- list.files(path = file.path(resultsDir, cropname), pattern = glob2rx('*.csv'))
  for (i in 1:(length(cropfnames)-1)) { #seq_along(cropfnames)) { #because 3.0m x 80AD ok
    var_df <- read.csv(file.path(resultsDir, cropname, cropfnames[i]), stringsAsFactors = FALSE)
    if (convert.negative.GW) {
      var_df$GW.ET.growing[var_df$GW.ET.growing < 0] <- 0
    }
    scenario_name <- gsub('_FAO56results_points_rounded.csv', '', cropfnames[i])
    AD.percent <- as.integer(substr(scenario_name, nchar(scenario_name)-1, nchar(scenario_name)))
    scenario_name <- paste0('scenario_', gsub(cropname, '', scenario_name))
    if (!grepl('mmH2O', colnames(var_df)[3])) {
      stop("check input data.frame because PAW not in 3rd column")
    }
    #var_df$AD.less.Dr.end.season <- ((AD.percent / 100) * var_df[,3]) - var_df$Dr.end.season #positive is mm surplus water at end of season compared to target
    #var_df$Dr.begin.season <- pmax(var_df$Dr.end.season - var_df$GW.capture.net, 0)
    #var_df$AD.less.Dr.begin.season <-  ((AD.percent /100) * var_df[,3]) - var_df$Dr.begin.season
    if (!dir.exists(file.path(rasterResultsDir, cropname, 'figures', scenario_name))) {
      dir.create(file.path(rasterResultsDir, cropname, 'figures', scenario_name))
    }
    if (!dir.exists(file.path(rasterResultsDir, cropname, 'figures', scenario_name, 'rasters'))) {
      dir.create(file.path(rasterResultsDir, cropname, 'figures', scenario_name, 'rasters'))
    }
    for (j in seq_along(unique(var_df$Model.Year))) {
      rasterize.result.annual(varname = 'GW.ET.growing', year = unique(var_df$Model.Year)[j])
      #raster.result.annual(varname = '')
      #rasterize.result.annual(varname = 'Irr.app.total', year = unique(var_df$Model.Year)[j])
      #rasterize.result.annual(varname = 'Irr.1.doy', year = unique(var_df$Model.Year)[j])
      #rasterize.result.annual(varname = 'deep.perc.annual', year = unique(var_df$Model.Year)[j])
    }
    rasterize.result.statistic(varname = 'GW.ET.growing', func = mean, 'mean', years = 2005:2016)
    rasterize.result.statistic(varname = 'Irr.app.total', func = mean, 'mean', years = 2005:2016)
    rasterize.result.statistic(varname = 'E.growing', func = mean, 'mean', years = 2005:2016)
    rasterize.result.statistic(varname = 'deep.perc.annual', func = max, 'max', years = 2005:2016)
    # rasterize.result.statistic(varname = 'GW.ET.growing', func = median, 'median')
    # rasterize.result.statistic(varname = 'E.growing', func = median, 'median')
    # rasterize.result.statistic(varname = 'Irr.app.total', func = median, 'median')
    # rasterize.result.statistic(varname = 'Irr.1.doy', func = median, 'median')
    # rasterize.result.statistic(varname = 'deep.perc.annual', func = median, 'median')
    # rasterize.result.statistic(varname = 'GW.ET.growing', func = min, 'min')
    # rasterize.result.statistic(varname = 'E.growing', func = min, 'min')
    # rasterize.result.statistic(varname = 'Irr.app.total', func = min, 'min')
    # rasterize.result.statistic(varname = 'Irr.1.doy', func = min, 'min')
    # rasterize.result.statistic(varname = 'deep.perc.annual', func = min, 'min')
    # rasterize.result.statistic(varname = 'GW.ET.growing', func = max, 'max')
    # rasterize.result.statistic(varname = 'E.growing', func = max, 'max')
    # rasterize.result.statistic(varname = 'Irr.app.total', func = max, 'max')
    # rasterize.result.statistic(varname = 'Irr.1.doy', func = max, 'max')
    # rasterize.result.statistic(varname = 'deep.perc.annual', func = max, 'max')
    # rasterize.result.statistic(varname = colnames(var_df)[3], func = unique, 'PAW')
  }
}
RasterBuild.v2(cropname = 'allcrops', convert.negative.GW = FALSE)
# RasterBuild.v2(cropname = 'alfalfa.intermountain', convert.negative.GW = FALSE)
# RasterBuild.v2(cropname = 'walnut.mature', convert.negative.GW = FALSE)
# RasterBuild.v2(cropname = 'pistachios', convert.negative.GW = FALSE)
# RasterBuild.v2(cropname = 'almond.mature', convert.negative.GW = FALSE)
# RasterBuild.v2(cropname = 'alfalfa.CV', convert.negative.GW = FALSE)
# RasterBuild.v2(cropname = 'alfalfa.imperial', convert.negative.GW = FALSE)
# RasterBuild.v2(cropname = 'grapes.table', convert.negative.GW = FALSE)
# RasterBuild.v2(cropname = 'grapes.wine', convert.negative.GW = FALSE)

#read in summary data for each scenario to summarize data with the help of cell counts
#updated 11.29.17 for allcrops directory
collect.stats <- function(cropname) {
  setwd(file.path(resultsDir, cropname))
  fnames <- list.files(pattern = glob2rx('*.csv'))
  for (j in seq_along(fnames)) { #2:length(fnames)) { #
    summary.fname <- gsub('results_points_rounded.csv', '', fnames[j])
    setwd(file.path(resultsDir, cropname))
    result <- read.csv(fnames[j], stringsAsFactors = FALSE)
    result <- result[ ,-2] #don't need the PAW data twice in different units
    if (cropname=='allcrops') {
      result <- result[ ,!(names(result) %in% c('unique_model_code', 'mukey', 'compnames', 'cokeys', 'PRISMcellnumber', 'CIMIScellnumber', 'Model.Year', 'cropcode', 'cropname'))]
    } else {result <- result[ ,!(names(result) %in% c('unique_model_code', 'mukey', 'compnames', 'cokeys', 'PRISMcellnumber', 'CIMIScellnumber', 'Model.Year'))]} #don't need summary stats for these columns
    varnames <- colnames(result)
    varnames <- varnames[-which(varnames=='cellcounts30m2')]
    summary_result <- as.data.frame(matrix(data=NA, nrow=length(varnames), ncol=12))
    colnames(summary_result) <- c('varname', 'Min', 'Qu0.005', 'Qu0.025', 'Qu0.25', 'Median', 'Mean', 'Qu0.75', 'Qu0.975', 'Qu0.995', 'Max', 'StDev')
    row.names(summary_result) <- varnames
    for (i in seq_along(result)) {
      if (colnames(result)[i]=='cellcounts30m2') {
        next
      }
      varname <- colnames(result)[i]
      data.expanded <- rep(result[,i], times=result$cellcounts30m2)
      stats.data <- data.frame(varname=varname, Min=min(data.expanded, na.rm=TRUE), Qu0.005=quantile(data.expanded, 0.005, na.rm = TRUE), Qu0.025=quantile(data.expanded, 0.025, na.rm = TRUE), Qu0.25=quantile(data.expanded, 0.25, na.rm = TRUE), Median=median(data.expanded, na.rm = TRUE), Mean=mean(data.expanded, na.rm = TRUE), Qu0.75=quantile(data.expanded, 0.75, na.rm = TRUE), Qu0.975=quantile(data.expanded, 0.975, na.rm = TRUE), Qu0.995=quantile(data.expanded, 0.995, na.rm = TRUE), Max=max(data.expanded, na.rm = TRUE), StDev=sd(data.expanded, na.rm = TRUE), row.names = NULL)
      stats.data[,2:ncol(stats.data)] <-round(stats.data[,2:ncol(stats.data)], 1)
      stats.data$varname <- as.character(stats.data$varname)
      summary_result[varname,] <- stats.data #indexing by row.name here
    }
    if (!dir.exists(file.path(resultsDir, cropname, 'allyrs_stats'))) {
      dir.create(file.path(resultsDir, cropname, 'allyrs_stats'))
    }
    setwd(file.path(resultsDir, cropname, 'allyrs_stats'))
    write.csv(summary_result, paste0(summary.fname, '_summarystats.csv'), row.names = FALSE)
  }
}
collect.stats('walnut.mature')
collect.stats('almond.mature')
collect.stats('pistachios')
collect.stats('grapes.table')
collect.stats('grapes.wine')
#these results don't inlcude the P.winter or ETo.winter columns
collect.stats('alfalfa.CV')
collect.stats('alfalfa.intermountain')
#this result also does not include the GW.capture.net column
collect.stats('alfalfa.imperial')
collect.stats('allcrops')

#function to collect stats about 20, 40, 60, and 80th percentiles
#do only for years 2005-2016, see version below that expands stats collection
collect.stats.v2 <- function(cropname) {
  setwd(file.path(resultsDir, cropname))
  fnames <- list.files(pattern = glob2rx('*.csv'))
  for (j in length(fnames)) { #2:length(fnames)) { #
    summary.fname <- gsub('results_points_rounded.csv', '', fnames[j])
    setwd(file.path(resultsDir, cropname))
    result <- read.csv(fnames[j], stringsAsFactors = FALSE)
    result <- result[ ,-2] #don't need the PAW data twice in different units
    if (cropname=='allcrops') {
      result <- result[ ,!(names(result) %in% c('unique_model_code', 'mukey', 'compnames', 'cokeys', 'PRISMcellnumber', 'CIMIScellnumber', 'Model.Year', 'cropcode', 'cropname'))]
    } else {result <- result[ ,!(names(result) %in% c('unique_model_code', 'mukey', 'compnames', 'cokeys', 'PRISMcellnumber', 'CIMIScellnumber', 'Model.Year'))]} #don't need summary stats for these columns
    varnames <- colnames(result)
    varnames <- varnames[-which(varnames=='cellcounts30m2')]
    summary_result <- as.data.frame(matrix(data=NA, nrow=length(varnames), ncol=5))
    colnames(summary_result) <- c('varname', 'Qu0.2', 'Qu0.4', 'Qu0.6', 'Qu0.8')
    row.names(summary_result) <- varnames
    for (i in seq_along(result)) {
      if (colnames(result)[i]=='cellcounts30m2') {
        next
      }
      varname <- colnames(result)[i]
      data.expanded <- rep(result[,i], times=result$cellcounts30m2)
      stats.data <- data.frame(varname=varname, Qu0.2=quantile(data.expanded, 0.2, na.rm = TRUE), Qu0.4=quantile(data.expanded, 0.4, na.rm = TRUE), Qu0.6=quantile(data.expanded, 0.6, na.rm = TRUE), Qu0.8=quantile(data.expanded, 0.8, na.rm = TRUE), row.names = NULL)
      stats.data[,2:ncol(stats.data)] <-round(stats.data[,2:ncol(stats.data)], 1)
      stats.data$varname <- as.character(stats.data$varname)
      summary_result[varname,] <- stats.data #indexing by row.name here
    }
    if (!dir.exists(file.path(resultsDir, cropname, 'allyrs_stats_v2'))) {
      dir.create(file.path(resultsDir, cropname, 'allyrs_stats_v2'))
    }
    setwd(file.path(resultsDir, cropname, 'allyrs_stats_v2'))
    write.csv(summary_result, paste0(summary.fname, '_summarystats_v2.csv'), row.names = FALSE)
  }
}
collect.stats.v2('walnut.mature')
collect.stats.v2('almond.mature')
collect.stats.v2('pistachios')
collect.stats.v2('grapes.table')
collect.stats.v2('grapes.wine')
collect.stats.v2('alfalfa.CV')
collect.stats.v2('alfalfa.intermountain')
#this result also does not include the GW.capture.net column
collect.stats.v2('alfalfa.imperial')
collect.stats.v2('allcrops')

#do only for years 2005-2016
collect.stats.v3 <- function(cropname, years) {
  fnames <- list.files(path = file.path(resultsDir, cropname), pattern = glob2rx('*.csv'), full.names = FALSE)
  fnames_full <- list.files(path = file.path(resultsDir, cropname), pattern = glob2rx('*.csv'), full.names = TRUE)
  for (j in seq_along(fnames)) { #seq_along(fnames))
    summary.fname <- gsub('results_points_rounded.csv', '', fnames[j])
    result <- read.csv(fnames_full[j], stringsAsFactors = FALSE)
    result <- result[ ,-2] #don't need the PAW data twice in different units
    result <- result[which(result$Model.Year %in% years), ]
    print(nrow(result))
    if (cropname=='allcrops') {
      result <- result[ ,!(names(result) %in% c('unique_model_code', 'mukey', 'compnames', 'cokeys', 'PRISMcellnumber', 'CIMIScellnumber', 'Model.Year', 'cropcode', 'cropname'))]
    } else {result <- result[ ,!(names(result) %in% c('unique_model_code', 'mukey', 'compnames', 'cokeys', 'PRISMcellnumber', 'CIMIScellnumber', 'Model.Year'))]} #don't need summary stats for these columns
    varnames <- colnames(result)
    varnames <- varnames[-which(varnames=='cellcounts30m2')]
    summary_result <- as.data.frame(matrix(data=NA, nrow=length(varnames), ncol=5))
    colnames(summary_result) <- c('varname', 'Qu0.2', 'Qu0.4', 'Qu0.6', 'Qu0.8')
    row.names(summary_result) <- varnames
    for (i in seq_along(result)) {
      if (colnames(result)[i]=='cellcounts30m2') {
        next
      }
      varname <- colnames(result)[i]
      data.expanded <- rep(result[,i], times=result$cellcounts30m2)
      stats.data <- data.frame(varname=varname, Qu0.2=quantile(data.expanded, 0.2, na.rm = TRUE), Qu0.4=quantile(data.expanded, 0.4, na.rm = TRUE), Qu0.6=quantile(data.expanded, 0.6, na.rm = TRUE), Qu0.8=quantile(data.expanded, 0.8, na.rm = TRUE), row.names = NULL)
      stats.data[,2:ncol(stats.data)] <- round(stats.data[,2:ncol(stats.data)], 1)
      stats.data$varname <- as.character(stats.data$varname)
      summary_result[varname,] <- stats.data #indexing by row.name here
    }
    if (!dir.exists(file.path(resultsDir, cropname, 'allyrs_stats_v3'))) {
      dir.create(file.path(resultsDir, cropname, 'allyrs_stats_v3'))
    }
    write.csv(summary_result, file.path(resultsDir, cropname, 'allyrs_stats_v3', paste0(summary.fname, '_summarystats_', as.character(years), '.csv')), row.names = FALSE)
  }
}
collect.stats.v3('allcrops', 2005) #run on 1/30/18 to get quantile breaks for maps
collect.stats.v3('allcrops', 2014)

#do only for years 2005-2016 for more complete set of stats
collect.stats.v4 <- function(cropname, years, fname_yrs) {
  fnames <- list.files(path = file.path(resultsDir, cropname), pattern = glob2rx('*.csv'), full.names = FALSE)
  fnames_full <- list.files(path = file.path(resultsDir, cropname), pattern = glob2rx('*.csv'), full.names = TRUE)
  for (j in seq_along(fnames)) { #seq_along(fnames))
    summary.fname <- gsub('results_points_rounded.csv', '', fnames[j])
    result <- read.csv(fnames_full[j], stringsAsFactors = FALSE)
    result <- result[ ,-2] #don't need the PAW data twice in different units
    result <- result[which(result$Model.Year %in% years), ]
    print(nrow(result))
    if (cropname=='allcrops') {
      result <- result[ ,!(names(result) %in% c('unique_model_code', 'mukey', 'compnames', 'cokeys', 'PRISMcellnumber', 'CIMIScellnumber', 'Model.Year', 'cropcode', 'cropname'))]
    } else {result <- result[ ,!(names(result) %in% c('unique_model_code', 'mukey', 'compnames', 'cokeys', 'PRISMcellnumber', 'CIMIScellnumber', 'Model.Year'))]} #don't need summary stats for these columns
    varnames <- colnames(result)
    varnames <- varnames[-which(varnames=='cellcounts30m2')]
    summary_result <- as.data.frame(matrix(data=NA, nrow=length(varnames), ncol=16))
    colnames(summary_result) <- c('varname', 'Min', 'Qu0.005', 'Qu0.025', 'Qu0.2', 'Qu0.25', 'Qu0.4', 'Median', 'Mean', 'Qu0.6', 'Qu0.75', 'Qu0.8', 'Qu0.975', 'Qu0.995', 'Max', 'StDev')
    row.names(summary_result) <- varnames
    for (i in seq_along(result)) {
      if (colnames(result)[i]=='cellcounts30m2') {
        next
      }
      varname <- colnames(result)[i]
      data.expanded <- rep(result[,i], times=result$cellcounts30m2)
      stats.data <- data.frame(varname=varname, Min=min(data.expanded, na.rm=TRUE), Qu0.005=quantile(data.expanded, 0.005, na.rm = TRUE), Qu0.025=quantile(data.expanded, 0.025, na.rm = TRUE), Qu0.2=quantile(data.expanded, 0.2, na.rm = TRUE), Qu0.25=quantile(data.expanded, 0.25, na.rm = TRUE), Qu0.4=quantile(data.expanded, 0.4, na.rm = TRUE), Median=median(data.expanded, na.rm = TRUE), Mean=mean(data.expanded, na.rm = TRUE), Qu0.6=quantile(data.expanded, 0.6, na.rm = TRUE), Qu0.75=quantile(data.expanded, 0.75, na.rm = TRUE), Qu0.8=quantile(data.expanded, 0.8, na.rm = TRUE), Qu0.975=quantile(data.expanded, 0.975, na.rm = TRUE), Qu0.995=quantile(data.expanded, 0.995, na.rm = TRUE), Max=max(data.expanded, na.rm = TRUE), StDev=sd(data.expanded, na.rm = TRUE), row.names = NULL)
      stats.data[,2:ncol(stats.data)] <- round(stats.data[,2:ncol(stats.data)], 1)
      stats.data$varname <- as.character(stats.data$varname)
      summary_result[varname,] <- stats.data #indexing by row.name here
    }
    if (!dir.exists(file.path(resultsDir, cropname, 'allyrs_stats_v4'))) {
      dir.create(file.path(resultsDir, cropname, 'allyrs_stats_v4'))
    }
    write.csv(summary_result, file.path(resultsDir, cropname, 'allyrs_stats_v4', paste0(summary.fname, '_summarystats_', fname_yrs, '.csv')), row.names = FALSE)
  }
}
collect.stats.v4('alfalfa.intermountain', 2005:2016, '2005_2016')
collect.stats.v4('walnut.mature', 2005:2016, '2005_2016')
collect.stats.v4('almond.mature', 2005:2016, '2005_2016')
collect.stats.v4('pistachios', 2005:2016, '2005_2016')
collect.stats.v4('grapes.table', 2005:2016, '2005_2016')
collect.stats.v4('grapes.wine', 2005:2016, '2005_2016')
collect.stats.v4('alfalfa.CV', 2005:2016, '2005_2016')
collect.stats.v4('alfalfa.imperial', 2005:2016, '2005_2016')
collect.stats.v4('allcrops', 2005:2016, '2005_2016')


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


#playing with Dr trends across rooting assumptions
setwd(file.path(SepResultsDir, 'almond.mature_majcomps/scenario_2.0m50AD'))
fnames <- list.files()
crop.soil.WB <- read.csv('almond.mature2.0mAD50_101839_13095536_2017-09-15.csv', stringsAsFactors = FALSE)
# plot(as.Date(crop.soil.WB$dates), crop.soil.WB$Dr.end, type='l') #needs to be smoothed
# first100 <- head(crop.soil.WB$Dr.end, 100)
# for (i in 1:length(crop.soil.WB$Dr.end)) {
#   if (i > 4) {
#     crop.soil.WB$Dr.end[i] <- mean(crop.soil.WB$Dr.end[(i-4):(i+5)])
#   } else {next}
# }
# first100
# head(crop.soil.WB$Dr.end, 100)
plot(as.Date(crop.soil.WB$dates), crop.soil.WB$Dr.end, type='l', col='green')
dataWY2005 <- crop.soil.WB[which(crop.soil.WB$dates=='2004-10-01'):which(crop.soil.WB$dates=='2005-06-01'), ]
dataWY2011 <- crop.soil.WB[which(crop.soil.WB$dates=='2010-10-01'):which(crop.soil.WB$dates=='2011-06-01'), ]
dataWY2014 <- crop.soil.WB[which(crop.soil.WB$dates=='2013-10-01'):which(crop.soil.WB$dates=='2014-06-01'), ]
dataWY2016 <- crop.soil.WB[which(crop.soil.WB$dates=='2015-10-01'):which(crop.soil.WB$dates=='2016-06-01'), ]
dataWY2017 <- crop.soil.WB[which(crop.soil.WB$dates=='2016-10-01'):which(crop.soil.WB$dates=='2017-06-01'), ]
dataWY2005_365 <- crop.soil.WB[which(crop.soil.WB$dates=='2004-10-01'):which(crop.soil.WB$dates=='2005-09-30'), ]
dataWY2014_365 <- crop.soil.WB[which(crop.soil.WB$dates=='2013-10-01'):which(crop.soil.WB$dates=='2014-09-30'), ]
#format.Date(as.Date(dataWY2011$dates), '%b-%d')
plot(1:length(dataWY2014$Dr.end), dataWY2014$Dr.end, type='l', col='blue', xlab='Dates', ylab='Soil water depletion (mm)', axes=FALSE)
# now tell it that annotations will be rotated by 90* (see ?par)
par(las=2)
# now draw the first axis
axis(1, at=seq(from=1, to=length(dataWY2014$dates), by=21), labels=format.Date(as.Date(dataWY2014$dates), '%b-%d')[seq(from=1, to=length(dataWY2014$dates), by=21)])
#lines(1:length(dataWY2011$Dr.end), dataWY2011$Dr.end, type='l', col='green')
lines(1:length(dataWY2017$Dr.end), dataWY2017$Dr.end, type='l', col='turquoise')
lines(1:length(dataWY2016$Dr.end), dataWY2016$Dr.end, type='l', col='orange')
lines(1:length(dataWY2005$Dr.end), dataWY2005$Dr.end, type='l', col='darkgreen')

#plot whole year's data
plot(1:length(dataWY2014_365$Dr.end), dataWY2014_365$Dr.end, type='l', col='blue', xlab='', ylab='Soil water depletion (mm)', axes=FALSE)
axis(1, at=seq(from=1, to=length(dataWY2005_365$dates), by=31), labels=format.Date(as.Date(dataWY2005_365$dates), '%b')[seq(from=1, to=length(dataWY2005_365$dates), by=31)])
axis(2)
lines(1:length(dataWY2005_365$Dr.end), dataWY2005_365$Dr.end, type='l', col='green')
setwd(file.path(resultsDir, 'rasters/Sep2017'))
almond_gw_et_raster <- raster('almondGW.ET.growing.Sep2017runs.tif')



#some green water modeling
#better to do all of this modelling with the annual data itself; not means;
#use almond_GW_ET_allyrs produced above in place of almond_points
#merging P with almond_points first
almond_points <- SetPointPrecipValues_MeanAnnual(almond_points)
mean(almond_points$mean.annual.P) #281 mm
mean(almond_points$GW.ET.growing, na.rm=TRUE) #171 mm
almond_points$GW.ET.growing[which(almond_points$GW.ET.growing < 0)] <- 0
summary(almond_points$GW.ET.growing)
summary(almond_points$GW.ET.growing/almond_points$mean.annual.P)
hist(almond_points$GW.ET.growing/almond_points$mean.annual.P)
almond_points$GW.ET.to.P <- almond_points$GW.ET.growing/almond_points$mean.annual.P
summary(lm(almond_points$GW.ET.to.P ~ almond_points$paw_mm))
rows.to.sample <- sample(1:nrow(almond_points), 0.02*nrow(almond_points))
#plot of GW.ET:P vs. 
plot(almond_points$paw_mm[rows.to.sample], almond_points$GW.ET.to.P[rows.to.sample])
plot(almond_points$paw_mm[rows.to.sample], almond_points$GW.ET.growing[rows.to.sample])
paw_sd <- sd(almond_points$paw_mm, na.rm = TRUE)
paw_median <- median(almond_points$paw_mm, na.rm=TRUE)
p_sd <- sd(almond_points$mean.annual.P)
p_mean <- mean(almond_points$mean.annual.P)
almond_points$paw_symbol <- NA
almond_points$paw_symbol[which(!is.na(almond_points$paw_mm))] <-rgb(0, 197, 255, maxColorValue = 255)
almond_points$paw_symbol[which(almond_points$paw_mm < (paw_median - 0.43*paw_sd))] <- rgb(190, 232, 255, maxColorValue = 255)
almond_points$paw_symbol[which(almond_points$paw_mm > (paw_median + 0.43*paw_sd))] <- rgb(0, 77, 168, maxColorValue = 255)
plot(almond_points$mean.annual.P[rows.to.sample], almond_points$GW.ET.growing[rows.to.sample], xlab='mean annual P (mm)', ylab='mean green water ET (mm)', col=almond_points$paw_symbol[rows.to.sample])
plot(almond_points$paw_mm[rows.to.sample], almond_points$E.growing[rows.to.sample])
summary(almond_points$GW.ET.growing/almond_points$ET.growing)
hist(almond_points$GW.ET.growing/almond_points$ET.growing)
summary(lm(GW.ET.growing ~ mean.annual.P + paw_mm + ET.growing, data=almond_points))
summary(lm(GW.ET.growing ~ mean.annual.P + I(mean.annual.P^2) + paw_mm, data=almond_points))
summary(lm(GW.ET.growing ~ mean.annual.P + I(mean.annual.P^2) + paw_mm + I(paw_mm^2), data=almond_points))
summary(lm(GW.ET.growing ~ mean.annual.P, data=almond_points))
summary(lm(GW.ET.growing ~ mean.annual.P + I(mean.annual.P^2) + paw_mm + paw_mm*mean.annual.P, data=almond_points))
summary(lm(GW.ET.growing ~ mean.annual.P + I(mean.annual.P^2) + paw_mm + I(paw_mm^2) + paw_mm*mean.annual.P, data=almond_points))
summary(lm(GW.ET.growing ~ mean.annual.P + paw_mm + paw_mm*mean.annual.P, data=almond_points))
summary(lm(GW.ET.growing ~ mean.annual.P + I(mean.annual.P^2) + paw_mm + I(paw_mm^2) + ET.growing + I(ET.growing^2) + paw_mm*mean.annual.P, data=almond_points))
summary(lm(GW.ET.growing ~ mean.annual.P + I(mean.annual.P^2) + paw_mm + I(paw_mm^2) + paw_mm*mean.annual.P + I(paw_mm*mean.annual.P^2), data=almond_points))


#code for working with irrigation dates
#now, aggregate mean date to 1st irrigation for green water ET by mukey & model.code

almond2.0m_AD50$Irr.1[which(almond2.0m_AD50$Irr.1=='1900-01-01')] <- NA
almond2.0m_AD50$Irr.1 <- as.Date(almond2.0m_AD50$Irr.1, format='%Y-%m-%d')
almond2.0m_AD50$Irr.1.doy <- as.integer(format.Date(almond2.0m_AD50$Irr.1, '%j'))
first.irr.mean <- tapply(almond2.0m_AD50$Irr.1.doy, almond2.0m_AD50$unique_model_code_final, mean, na.rm=TRUE)
mukeys <- tapply(almond2.0m_AD50$mukey, almond2.0m_AD50$unique_model_code_final, unique)
comppct_r <- tapply(almond2.0m_AD50$comppct_r, almond2.0m_AD50$unique_model_code_final, unique)
modelcode <- tapply(almond2.0m_AD50$unique_model_code, almond2.0m_AD50$unique_model_code_final, unique)
results <- cbind(first.irr.mean, mukeys, comppct_r, modelcode)
results <- as.data.frame(results)
compsums <- as.data.frame(tapply(results$comppct_r[!is.na(results$first.irr.mean)], results$modelcode[!is.na(results$first.irr.mean)], sum))
colnames(compsums) <- 'compsums'
compsums$modelcode <- rownames(compsums)
results <- merge(results, compsums, by='modelcode')
first.irr.mean <- tapply(results$first.irr.mean*(results$comppct_r/results$compsums), results$modelcode, sum, na.rm=TRUE)
length(unique(results$modelcode)) #193820 results
first.irr.mean <- as.data.frame(first.irr.mean)
colnames(first.irr.mean) <- 'irr1.doy'
first.irr.mean$unique_model_code <- rownames(first.irr.mean)
first.irr.mean$irr1.doy <- as.integer(first.irr.mean$irr1.doy)
dim(first.irr.mean) #193,820  model codes with data
first.irr.mean <- first.irr.mean[,c(2,1)]
system.time(raster.irr1.meandoy <- subs(raster.model.codes, first.irr.mean, by=1, which=2, filename='irr1.meandoy.Aug2017runs.tif', format='GTiff'))
setwd(file.path(resultsDir, 'rasters/Aug2017'))
writeRaster(raster.irr1.meandoy, 'irr1.meandoy.Aug2017runs.tif', format='GTiff') #this took 65 minutes
cellStats(raster.irr1.meandoy, stat='mean', na.rm=TRUE) #mean is DOY 122
