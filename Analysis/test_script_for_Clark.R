resultsDir <- 'D:/Allowable_Depletion/results/test/summaries'
dissertationDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/dissertation/test'
volumes.calculate.AF <- function(cropname) {
  AF.to.m3 <- 43560 * (12 ^ 3) * (2.54 ^ 3) * (0.01 ^ 3)
  area.summary <- read.csv(file.path(resultsDir, 'hectares_by_model_code2018-05-14.csv'), stringsAsFactors = FALSE)
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
    volume.stats.AF$irr.1.doy.wtd.avg <- round(tapply(var_df$Irr.1.doy * var_df$hectares, var_df$Model.Year, sum, na.rm = TRUE) / modeled_ha, 1)
    volume.stats.AF$last.irr.doy.wtd.avg <- round(tapply(var_df$Irr.Last.doy * var_df$hectares, var_df$Model.Year, sum, na.rm = TRUE) / modeled_ha, 1)
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
  scenario.AF.summary <- as.data.frame(matrix(data=NA, nrow=length(fnames), ncol=21))
  colnames(scenario.AF.summary) <- c('root.depth', 'allowable.depletion', 'AD.inches', 'irr.count', 'irr.1.doy', 'last.irr.doy', 'green.water', 'blue.water', 'ET.growing', 'evaporation.growing', 'deep.percolation.annual', 'crop.stress.annual', 'dormant.ET', 'precipitation', 'crop.stress.growing', 'non.irr.deep.perc', 'Irr1.to.last.deep.perc', 'fall.deep.perc', 'delta.S.model.span', 'P.balance.error', 'P.balance.error.perc')
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
    scenario.AF.summary$ET.growing[i] <- sum(df$ET.growing.AF)
    scenario.AF.summary$evaporation.growing[i] <- sum(df$E.growing.AF[start_yr:stop_yr])
    scenario.AF.summary$deep.percolation.annual[i] <- sum(df$deep.perc.annual.AF[start_yr:stop_yr])
    scenario.AF.summary$crop.stress.annual[i] <- sum(df$crop.stress.annual.AF[start_yr:stop_yr])
    scenario.AF.summary$dormant.ET[i] <- sum(df$ET.winter.AF[start_yr:stop_yr])
    scenario.AF.summary$precipitation[i] <- sum(df$P.annual.AF[start_yr:stop_yr])
    scenario.AF.summary$crop.stress.growing[i] <- sum(df$crop.stress.growing.AF[start_yr:stop_yr])
    scenario.AF.summary$non.irr.deep.perc[i] <- sum(df$non.irr.deep.perc.AF[start_yr:stop_yr])
    scenario.AF.summary$Irr1.to.last.deep.perc[i] <- sum(df$Irr1.to.last.deep.perc.AF[start_yr:stop_yr])
    scenario.AF.summary$fall.deep.perc[i] <- sum(df$fall.deep.perc.AF[start_yr:stop_yr])
    scenario.AF.summary$delta.S.model.span[i] <- df$year.end.depletion.AF[stop_yr] - df$year.end.depletion.AF[start_yr - 1]
  }
  scenario.AF.summary$P.balance.error <- scenario.AF.summary$green.water + scenario.AF.summary$deep.percolation.annual + scenario.AF.summary$dormant.ET - scenario.AF.summary$precipitation
  scenario.AF.summary$P.balance.error.perc <- round(100 * (scenario.AF.summary$P.balance.error / scenario.AF.summary$precipitation), 2)
  scenario.AF.summary$GW_to_P <- round(scenario.AF.summary$green.water / scenario.AF.summary$precipitation, 2)
  scenario.AF.summary$GW_to_ET <- round(scenario.AF.summary$green.water / scenario.AF.summary$ET.growing, 2)
  scenario.AF.summary <- scenario.AF.summary[order(scenario.AF.summary$AD.inches), ]
  scenario.MAF.summary <- cbind(scenario.AF.summary[,1:6], round(scenario.AF.summary[7:(ncol(scenario.AF.summary) - 3)] / 10^6, 3), scenario.AF.summary[c('P.balance.error.perc', 'GW_to_P', 'GW_to_ET')])
  scenario.TAF.summary <- cbind(scenario.AF.summary[,1:6], round(scenario.AF.summary[7:(ncol(scenario.AF.summary) - 3)] / 10^3, 0), scenario.AF.summary[c('P.balance.error.perc', 'GW_to_P', 'GW_to_ET')])
  scenario.km3.summary <- cbind(scenario.AF.summary[,1:6], round(scenario.AF.summary[7:(ncol(scenario.AF.summary) - 3)] * AF.to.km3, 3), scenario.AF.summary[c('P.balance.error.perc', 'GW_to_P', 'GW_to_ET')])
  scenario.km3.summary$AD.inches <- scenario.km3.summary$AD.inches * 25.4
  colnames(scenario.km3.summary)[which(colnames(scenario.km3.summary) == 'AD.inches')] <- 'AD.mm'
  if (!dir.exists(file.path(dissertationDir, 'tables'))) {
    dir.create(file.path(dissertationDir, 'tables'))
  }
  if (!dir.exists(file.path(dissertationDir, 'tables', 'AF.summaries'))) {
    dir.create(file.path(dissertationDir, 'tables', 'AF.summaries'))
  }
  if (!dir.exists(file.path(dissertationDir, 'tables', 'MAF.summaries'))) {
    dir.create(file.path(dissertationDir, 'tables', 'MAF.summaries'))
  }
  if (!dir.exists(file.path(dissertationDir, 'tables', 'TAF.summaries'))) {
    dir.create(file.path(dissertationDir, 'tables', 'TAF.summaries'))
  }
  if (!dir.exists(file.path(dissertationDir, 'tables', 'km3.summaries'))) {
    dir.create(file.path(dissertationDir, 'tables', 'km3.summaries'))
  }
  write.csv(scenario.AF.summary, file=file.path(dissertationDir, 'tables', 'AF.summaries', paste0(cropname, '.all.scenarios.AF.summary.csv')), row.names=FALSE)
  write.csv(scenario.MAF.summary, file=file.path(dissertationDir, 'tables', 'MAF.summaries', paste0(cropname, '.all.scenarios.MAF.summary.csv')), row.names=FALSE)
  write.csv(scenario.TAF.summary, file=file.path(dissertationDir, 'tables', 'TAF.summaries', paste0(cropname, '.all.scenarios.TAF.summary.csv')), row.names=FALSE)
  write.csv(scenario.km3.summary, file=file.path(dissertationDir, 'tables', 'km3.summaries', paste0(cropname, 'all.scenario.km3.summary.csv')), row.names=FALSE)
}

combine.scenarios('alfalfa.intermountain', 2005, 2017)
combine.scenarios('almond.mature', 2005, 2017)
combine.scenarios('alfalfa.imperial', 2005, 2017)
combine.scenarios('alfalfa.CV', 2005, 2017)
combine.scenarios('walnut.mature', 2005, 2017)
combine.scenarios('pistachios', 2005, 2017)
combine.scenarios('grapes.table', 2005, 2017)
combine.scenarios('grapes.wine', 2005, 2017)
combine.scenarios('allcrops', 2005, 2017)

#combine variables across crops and scenarios from stats summaries in mm
combine.crops.by.scenario <- function(root.depth, AD, years) {
  modeled_ha_by.crop <- read.csv(file.path(dissertationDir, 'model_stats', 'modeled_ha_by.crop.csv'), stringsAsFactors = FALSE)
  cropnames <- c('almond.mature', 'alfalfa.intermountain', 'alfalfa.imperial', 'alfalfa.CV', 'walnut.mature', 'pistachios', 'grapes.table', 'grapes.wine')
  varnames <- c('cropname', 'allowable.depletion', "irr.count", "irr.1.doy", "last.irr.doy", "green.water", "blue.water", "ET.growing", "evaporation.growing", "deep.percolation.annual", "crop.stress.annual", "dormant.ET", "precipitation", "crop.stress.growing", "non.irr.deep.perc", "Irr1.to.last.deep.perc", "fall.deep.perc", "delta.S.model.span", "P.balance.error", "P.balance.error.perc", "GW_to_P", "GW_to_ET")
  var.summary <- as.data.frame(matrix(data=NA, nrow=length(cropnames), ncol=length(varnames)))
  colnames(var.summary) <- varnames
  for (i in seq_along(cropnames)) {
    fname.path <- file.path(dissertationDir, 'tables', 'AF.summaries', paste0(cropnames[i], '.all.scenarios.AF.summary.csv'))
    result <- read.csv(fname.path, stringsAsFactors = FALSE)
    var.summary[i,] <- cbind(cropname = as.character(cropnames[i]), result[which(result$root.depth==root.depth & result$allowable.depletion==AD), 3:ncol(result)], stringsAsFactors=FALSE)
  }
  var.summary.inches <- var.summary  
  var.summary.inches$acres <- modeled_ha_by.crop$acres[match(var.summary.inches$cropname, modeled_ha_by.crop$cropname)]
  var.summary.inches[,6:19] <- round((var.summary.inches[,6:19] / var.summary.inches$acres) * (12 / length(years)), 2) #conversion to inches yr-1
  var.summary.inches$modeled_years <- paste0(years[1], '_', years[length(years)])
  var.summary.mm <- var.summary
  var.summary.mm$acres <- modeled_ha_by.crop$acres[match(var.summary.mm$cropname, modeled_ha_by.crop$cropname)]
  var.summary.mm[,6:19] <- round((var.summary.mm[,6:19] / var.summary.mm$acres) * (12 * 25.4 / length(years)), 2) #conversion to mm yr-1
  var.summary.mm$acres <- NULL
  var.summary.mm$hectares <- modeled_ha_by.crop$hectares[match(var.summary.mm$cropname, modeled_ha_by.crop$cropname)]
  var.summary.mm$allowable.depletion <- var.summary.mm$allowable.depletion * 25.4
  var.summary.mm$modeled_years <- paste0(years[1], '_', years[length(years)])
  if (!dir.exists(file.path(dissertationDir, 'tables'))) {
    dir.create(file.path(dissertationDir, 'tables'))
  }
  if (!dir.exists(file.path(dissertationDir, 'tables', 'crop.comparisons'))) {
    dir.create(file.path(dissertationDir, 'tables', 'crop.comparisons'))
  }
  if (!dir.exists(file.path(dissertationDir, 'tables', 'crop.comparisons', 'by.depth.inches'))) {
    dir.create(file.path(dissertationDir, 'tables', 'crop.comparisons'))
  }
  if (!dir.exists(file.path(dissertationDir, 'tables', 'crop.comparisons', 'by.depth.mm'))) {
    dir.create(file.path(dissertationDir, 'tables', 'crop.comparisons', 'by.depth.mm'))
  }
  write.csv(var.summary.inches, file.path(dissertationDir, 'tables', 'crop.comparisons', 'by.depth.inches', paste0(as.character(root.depth), 'm_AD', as.character(AD), '.inches.mean.annual.comparisons.csv')), row.names = FALSE)
  write.csv(var.summary.mm, file.path(dissertationDir, 'tables', 'crop.comparisons', 'by.depth.mm', paste0(as.character(root.depth), 'm_AD', as.character(AD), '.mm.mean.annual.comparisons.csv')), row.names = FALSE)
}
combine.crops.by.scenario(root.depth = 0.5, AD = 30, years = 2005:2017)
combine.crops.by.scenario(root.depth = 0.5, AD = 50, years = 2005:2017)
combine.crops.by.scenario(root.depth = 0.5, AD = 80, years = 2005:2017)
combine.crops.by.scenario(root.depth = 1.0, AD = 30, years = 2005:2017)
combine.crops.by.scenario(root.depth = 1.0, AD = 50, years = 2005:2017)
combine.crops.by.scenario(root.depth = 1.0, AD = 80, years = 2005:2017)
combine.crops.by.scenario(root.depth = 2.0, AD = 30, years = 2005:2017)
combine.crops.by.scenario(root.depth = 2.0, AD = 50, years = 2005:2017)
combine.crops.by.scenario(root.depth = 2.0, AD = 80, years = 2005:2017)
combine.crops.by.scenario(root.depth = 3.0, AD = 30, years = 2005:2017)
combine.crops.by.scenario(root.depth = 3.0, AD = 50, years = 2005:2017)
combine.crops.by.scenario(root.depth = 3.0, AD = 80, years = 2005:2017)

#create some 2005-2017 allcrop model summaries for merging with shapefile
result.summarize <- function(df, years, cropDir) {
  var_df <- read.csv(file.path(resultsDir, 'allcrops', df), stringsAsFactors=FALSE)
  var_df2 <- var_df[which(var_df$Model.Year %in% years),]
  var_df_summary <- data.frame(unique_model_code = unique(var_df2$unique_model_code)[order(unique(var_df2$unique_model_code))], GW.mean = as.numeric(tapply(var_df2[['GW.ET.growing']], var_df2$unique_model_code, mean)), BW.mean = as.numeric(tapply(var_df2[['Irr.app.total']], var_df2$unique_model_code, mean)), ET.growing = as.numeric(tapply(var_df2[['ET.growing']], var_df2$unique_model_code, mean)), E.growing.mean = as.numeric(tapply(var_df2[['E.growing']], var_df2$unique_model_code, mean)),  DP.max = as.numeric(tapply(var_df2[['deep.perc.annual']], var_df2$unique_model_code, max)), Irr1.mean = as.numeric(tapply(var_df2[['Irr.1.doy']], var_df2$unique_model_code, mean, na.rm = TRUE)), IrrCount.mean = as.numeric(tapply(var_df2[['Irr.Count']], var_df2$unique_model_code, mean, na.rm = TRUE)), IrrLast.doy = as.numeric(tapply(var_df2[['Irr.Last.doy']], var_df2$unique_model_code, mean, na.rm = TRUE)), ETo.growing = as.numeric(tapply(var_df2[['ETo.growing']], var_df2$unique_model_code, mean)), ETo.annual = as.numeric(tapply(var_df2[['ETo.annual']], var_df2$unique_model_code, mean)), P.annual = as.numeric(tapply(var_df2[['P.annual']], var_df2$unique_model_code, mean)))
  fname <- paste0(gsub('.csv', '', df), '_', as.character(years[1]), '_', as.character(years[length(years)]), '_synthesis.csv')
  if (!dir.exists(file.path(resultsDir, cropDir, '2005_2017_by_model_code'))) {
    dir.create(file.path(resultsDir, cropDir, '2005_2017_by_model_code'))
  }
  write.csv(var_df_summary, file.path(resultsDir, cropDir, '2005_2017_by_model_code', fname), row.names = FALSE)
  var_df_summary
}
results_0.5mAD30 <- result.summarize('allcrops0.5mAD30_FAO56results_MUaggregated.csv', 2005:2017, 'allcrops')
results_1.0mAD50 <- result.summarize('allcrops1.0mAD50_FAO56results_MUaggregated.csv', 2005:2017, 'allcrops')
results_2.0mAD50 <- result.summarize('allcrops2.0mAD50_FAO56results_MUaggregated.csv', 2005:2017, 'allcrops')