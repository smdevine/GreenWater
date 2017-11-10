#TO-DO
# (1) add spatial CIMIS ETo data to almond_points_allyrs
# (2) test green water availability models for almond_points_allyrs
resultsDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/results/Oct2017/summaries'
modelscaffoldDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/model_scaffold/run_model/Oct2017'
#if so desired
#model.scaffold <- read.csv(file.path(modelscaffoldDir, 'model_scaffold_majcomps.v2.csv'), stringsAsFactors = F)
rasterResultsDir <- 'D:/Allowable_Depletion/results/Oct2017/summaries'
setwd(modelscaffoldDir)
cropscape_legend <- read.csv('cropscape_legend.txt', stringsAsFactors = FALSE)
alfalfa_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Alfalfa'] #75380 total
grape_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Grapes']
almond_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Almonds']
walnut_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Walnuts']
pistachio_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Pistachios']
library(raster)
library(extrafont)
library(extrafontdb)
#font_import() #only needs to be done once?
CustomBP <- function(x){
  lower.x <- quantile(x, 0.05, na.rm = TRUE)
  q1.x <- quantile(x, 0.25, na.rm = TRUE)
  med.x <- median(x, na.rm = TRUE)
  q3.x <- quantile(x, 0.75, na.rm = TRUE)
  upper.x <- quantile(x, 0.95, na.rm = TRUE)
  c(lower.x, q1.x, med.x, q3.x, upper.x)
}

#plotting function
#to-do take out Irr and ETo boxplot for all scenarios; only needs to be done once for each crop unlike GW and BW
MakeBPs <- function(cropname, years=2004:2016) {
  make.bp <- function(bp_name, yaxis_lab, varname, bxfill) {
    png(file.path(resultsDir, cropname, 'figures', scenario_name, paste0('allyrs.boxplots', varname, '.png', sep = '')), family = 'Book Antiqua', width = 7, height = 5, units = 'in', res = 600)
    par(mai=c(0.9, 0.9, 0.2, 0.2))
    bxp(bp_name, outline = FALSE, boxfill=bxfill, las=2, ylab='', xlab='')
    mtext(text='Year', side=1, line=3.5)
    mtext(text=yaxis_lab, side=2, line=3.5)
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
    make.bp(bp_GW, 'Growing season green water (mm)', 'GW.ET.growing', 'green')
    make.bp(bp_BW, 'Irrigation [blue water] demand (mm)', 'Irr.app.total', 'lightblue')
    make.bp(bp_P, 'Water year precipitation (mm)', 'P.WY', 'blue')
    make.bp(bp_ETo, 'Reference evapotranspiration (mm)', 'ETo.WY', 'orange')
  }
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
#build a raster based for a single filename and variable
RasterBuild <- function(cropname, fname, varname, func, funcname) {
  if (!dir.exists(file.path(rasterResultsDir, cropname))) {
    dir.create(file.path(rasterResultsDir, cropname))
  }
  if (!dir.exists(file.path(rasterResultsDir, cropname, 'figures'))) {
    dir.create(file.path(rasterResultsDir, cropname, 'figures'))
  }
  if (!dir.exists(file.path(rasterResultsDir, cropname, 'figures', scenario_name))) {
    dir.create(file.path(rasterResultsDir, cropname, 'figures', scenario_name))
  }
  if (!dir.exists(file.path(rasterResultsDir, cropname, 'figures', scenario_name, 'rasters'))) {
    dir.create(file.path(rasterResultsDir, cropname, 'figures', scenario_name, 'rasters'))
  }
  rasterize.result.statistic <- function(varname, func, funcname) {
    raster.result <- raster.model.codes
    var_df_summary <- data.frame(varname = tapply(var_df[[varname]], var_df$unique_model_code, func))
    colnames(var_df_summary) <- paste0(varname, '.', funcname)
    var_df_summary$unique_model_code <- row.names(var_df_summary)
    raster.result[cell_numbers_to_codes$cell_numbers_of_interest] <- var_df_summary[[paste0(varname, '.', funcname)]][match(cell_numbers_to_codes$unique_model_codes, var_df_summary$unique_model_code)]
    if (!dir.exists(file.path(rasterResultsDir, cropname, 'figures', scenario_name, 'rasters', varname))) {
      dir.create(file.path(rasterResultsDir, cropname, 'figures', scenario_name, 'rasters', varname))
    }
    writeRaster(raster.result, file.path(rasterResultsDir, cropname, 'figures', scenario_name, 'rasters', varname, paste0(varname, '.', funcname, '.tif')), format='GTiff')
    removeTmpFiles(h=0.0001)
  }
  var_df <- read.csv(file.path(resultsDir, cropname, fname), stringsAsFactors = FALSE)
  scenario_name <- gsub('_FAO56results_points_rounded.csv', '', cropfnames[i])
  scenario_name <- paste0('scenario_', gsub(cropname, '', scenario_name))
  rasterize.result.statistic(varname = varname, func = func, funcname = funcname)
}

raster.model.codes <- raster(file.path(modelscaffoldDir, 'model.codes.Aug2017.tif'))
cell_numbers_to_codes <- read.csv(file.path(modelscaffoldDir, 'cellnumbers_to_modelcodes.csv'), stringsAsFactors = FALSE)
RasterBuild.v2 <- function(readraster=FALSE, readcellnums=FALSE, cropname) {
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
  rasterize.result.statistic <- function(varname, func, funcname) {
    raster.result <- raster.model.codes
    var_df_summary <- data.frame(varname = tapply(var_df[[varname]], var_df$unique_model_code, func))
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
  for (i in seq_along(cropfnames)) { #seq_along(cropfnames)) {
    var_df <- read.csv(file.path(resultsDir, cropname, cropfnames[i]), stringsAsFactors = FALSE)
    scenario_name <- gsub('_FAO56results_points_rounded.csv', '', cropfnames[i])
    scenario_name <- paste0('scenario_', gsub(cropname, '', scenario_name))
    if (!dir.exists(file.path(rasterResultsDir, cropname, 'figures', scenario_name))) {
      dir.create(file.path(rasterResultsDir, cropname, 'figures', scenario_name))
    }
    if (!dir.exists(file.path(rasterResultsDir, cropname, 'figures', scenario_name, 'rasters'))) {
      dir.create(file.path(rasterResultsDir, cropname, 'figures', scenario_name, 'rasters'))
    }
    for (j in seq_along(unique(var_df$Model.Year))) {
      rasterize.result.annual(varname = 'GW.ET.growing', year = unique(var_df$Model.Year)[j])
      rasterize.result.annual(varname = 'Irr.app.total', year = unique(var_df$Model.Year)[j])
      rasterize.result.annual(varname = 'Irr.1.doy', year = unique(var_df$Model.Year)[j])
      rasterize.result.annual(varname = 'deep.perc.annual', year = unique(var_df$Model.Year)[j])
    }
    rasterize.result.statistic(varname = 'GW.ET.growing', func = median, 'median')
    rasterize.result.statistic(varname = 'E.growing', func = median, 'median')
    rasterize.result.statistic(varname = 'Irr.app.total', func = median, 'median')
    rasterize.result.statistic(varname = 'Irr.1.doy', func = median, 'median')
    rasterize.result.statistic(varname = 'deep.perc.annual', func = median, 'median')
    rasterize.result.statistic(varname = 'GW.ET.growing', func = min, 'min')
    rasterize.result.statistic(varname = 'E.growing', func = min, 'min')
    rasterize.result.statistic(varname = 'Irr.app.total', func = min, 'min')
    rasterize.result.statistic(varname = 'Irr.1.doy', func = min, 'min')
    rasterize.result.statistic(varname = 'deep.perc.annual', func = min, 'min')
    rasterize.result.statistic(varname = 'GW.ET.growing', func = max, 'max')
    rasterize.result.statistic(varname = 'E.growing', func = max, 'max')
    rasterize.result.statistic(varname = 'Irr.app.total', func = max, 'max')
    rasterize.result.statistic(varname = 'Irr.1.doy', func = max, 'max')
    rasterize.result.statistic(varname = 'deep.perc.annual', func = max, 'max')
    rasterize.result.statistic(varname = colnames(var_df)[3], func = unique, 'PAW')
  }
}
RasterBuild.v2(cropname = 'alfalfa.intermountain')
RasterBuild.v2(cropname = 'walnut.mature')
RasterBuild.v2(cropname = 'pistachios')
RasterBuild.v2(cropname = 'almond.mature')
RasterBuild.v2(cropname = 'alfalfa.CV')
RasterBuild.v2(cropname = 'alfalfa.imperial')
RasterBuild.v2(cropname = 'grapes.table')
RasterBuild.v2(cropname = 'grapes.wine')

#read in summary data for each scenario to summarize data with the help of cell counts
collect.stats <- function(cropname) {
  setwd(file.path(resultsDir, cropname))
  fnames <- list.files(pattern = glob2rx('*.csv'))
  for (j in seq_along(fnames)) {
    summary.fname <- gsub('results_points_rounded.csv', '', fnames[j])
    setwd(file.path(resultsDir, cropname))
    result <- read.csv(fnames[j], stringsAsFactors = FALSE)
    result <- result[ ,-2] #don't need the PAW data twice in different units
    result <- result[ ,!(names(result) %in% c('unique_model_code', 'mukey', 'compnames', 'cokeys', 'PRISMcellnumber', 'CIMIScellnumber', 'Model.Year'))] #don't need summary stats for these columns
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

#function to quantify water volumes for green water, blue water, precip
volumes.calculate <- function(cropname) {
  cropfnames <- list.files(path = file.path(resultsDir, cropname), pattern = glob2rx('*_FAO56results_points_rounded.csv'))
  if (!dir.exists(file.path(resultsDir, cropname, 'allyrs_stats'))) {
    dir.create(file.path(resultsDir, cropname, 'allyrs_stats'))
  }
  if (!dir.exists(file.path(resultsDir, cropname, 'allyrs_stats', 'AF.summaries'))) {
    dir.create(file.path(resultsDir, cropname, 'allyrs_stats', 'AF.summaries'))
  }
  for (i in seq_along(cropfnames)) {
    var_df <- read.csv(file.path(resultsDir, cropname, cropfnames[i]), stringsAsFactors = FALSE)
    scenario_name <- gsub('_FAO56results_points_rounded.csv', '', cropfnames[i])
    scenario_name <- paste0('scenario.', gsub(cropname, '', scenario_name))
    volume.stats <- round(data.frame(green.water.AF = tapply(var_df$GW.ET.growing * (9 / 12334.8)*var_df$cellcounts30m2, var_df$Model.Year, sum, na.rm=TRUE)), 0)
    volume.stats$blue.water.AF <- round(tapply(var_df$Irr.app.total * (9 / 12334.8) * var_df$cellcounts30m2, var_df$Model.Year, sum, na.rm=TRUE), 0)
    volume.stats$ET.growing.AF <- round(tapply(var_df$ET.growing * (9 / 12334.8) * var_df$cellcounts30m2, var_df$Model.Year, sum, na.rm=TRUE), 0)
    volume.stats$acres <- round(sum(var_df$cellcounts30m2[var_df$Model.Year==2004] * 900 / 10000) * 2.47105, 0)
    volume.stats$GW.growing.inches <- round((volume.stats$green.water.AF / volume.stats$acres) * 12, 1)
    volume.stats$BW.growing.inches <- round((volume.stats$blue.water.AF / volume.stats$acres) * 12, 1)
    volume.stats$ET.growing.inches <- round((volume.stats$ET.growing.AF /volume.stats$acres) * 12, 1)
    volume.stats$GW_to_ET <- round(volume.stats$green.water.AF / volume.stats$ET.growing.AF, 2)
    volume.stats$precip.WY.AF <- round(tapply(var_df$P.WY * (9 / 12334.8) * var_df$cellcounts30m2, var_df$Model.Year, sum, na.rm=TRUE), 0)
    volume.stats$GW_to_P <- round(volume.stats$green.water.AF / volume.stats$precip.WY.AF, 2)
    volume.stats$deep.perc.annual.AF <- round(tapply(var_df$deep.perc.annual * (9 / 12334.8) * var_df$cellcounts30m2, var_df$Model.Year, sum, na.rm=TRUE), 0)
    volume.stats$ET.winter.AF <- round(tapply((var_df$ET.annual - var_df$ET.growing) * (9 / 12334.8) * var_df$cellcounts30m2, var_df$Model.Year, sum, na.rm=TRUE), 0)
    volume.stats$E.growing.AF <- round(tapply(var_df$E.growing * (9 / 12334.8) * var_df$cellcounts30m2, var_df$Model.Year, sum, na.rm=TRUE), 0)
    volume.stats$E.winter.AF <- round(tapply((var_df$E.annual - var_df$E.growing) * (9 / 12334.8) * var_df$cellcounts30m2, var_df$Model.Year, sum, na.rm=TRUE), 0)
    volume.stats$Model.Year <- as.integer(rownames(volume.stats))
    write.csv(volume.stats, file.path(resultsDir, cropname, 'allyrs_stats/AF.summaries', paste0(scenario_name, '.AFsummary.by.year.csv')), row.names = FALSE)
  }
}
volumes.calculate('alfalfa.intermountain')
volumes.calculate('alfalfa.imperial')
volumes.calculate('alfalfa.CV')
volumes.calculate('almond.mature')
volumes.calculate('walnut.mature')
volumes.calculate('pistachios')
volumes.calculate('grapes.table')
volumes.calculate('grapes.wine')

#get each crop's results for a given scenario, rbind together and save as csv in all_crops
bindallresults <- function(scenario_name, paw_name) {
  cropdf <- data.frame(cropnames = c('alfalfa.intermountain', 'alfalfa.imperial', 'alfalfa.CV', 'almond.mature', 'walnut.mature', 'pistachios', 'grapes.table', 'grapes.wine'), cropcode = c(alfalfa_code, alfalfa_code, alfalfa_code, almond_code, walnut_code, pistachio_code, grape_code, grape_code))
  for (i in seq_along(cropdf$cropnames)) {
    print(i)
    cropfnames <- list.files(path = file.path(resultsDir, cropdf$cropnames[i]), pattern = glob2rx(paste0('*_FAO56results_points_rounded.csv')))
    if (i == 1) { #as long as grapes.wine is not the first cropname
      scenario_index <- grep(scenario_name, cropfnames)
    }
    if(cropdf$cropnames[i]=='grapes.wine') {
      cropfnames <- cropfnames[c(3, 2, 1, 6, 5, 4, 9, 8, 7)] #because the RDImin0.2 scenario is closest to the 80% AD scenario for the other crops...
    }
    if (i == 1) {
      result <- read.csv(file.path(resultsDir, cropdf$cropnames[i], cropfnames[scenario_index]), stringsAsFactors = FALSE)
      if (cropdf$cropnames[i]=='alfalfa.imperial') {
        result$GW.capture.net <- NA
        result <- result[c(1:34, 40, 35, 36:39)]
        result$P.winter <- NA
        result$P.growing <- result$P.annual
        result$ETo.winter <- NA
        result$ETo.growing <- result$ETo.annual
      }
      if (cropdf$cropnames[i]=='alfalfa.CV') {
        result$P.winter <- NA
        result$P.growing <- result$P.annual
        result$ETo.winter <- NA
        result$ETo.growing <- result$ETo.annual
      }
      colnames(result)[2:3] <- c(paste0(paw_name, '.cmH2O'), paste0(paw_name, '.mmH2O'))
      result$cropcode <- cropdf$cropcode[i]
      result$cropname <- cropdf$cropnames[i] 
    # print(ncol(result))
      print(colnames(result))
    } else {
        nextdf <- read.csv(file.path(resultsDir, cropdf$cropnames[i], cropfnames[scenario_index]), stringsAsFactors = FALSE)
        if (cropdf$cropnames[i]=='alfalfa.imperial') {
          nextdf$GW.capture.net <- NA
          nextdf <- nextdf[c(1:34, 40, 35, 36:39)]
          nextdf$P.winter <- NA
          nextdf$P.growing <- nextdf$P.annual
          nextdf$ETo.winter <- NA
          nextdf$ETo.growing <- nextdf$ETo.annual
        }
        if (cropdf$cropnames[i]=='alfalfa.CV') {
          nextdf$P.winter <- NA
          nextdf$P.growing <- nextdf$P.annual
          nextdf$ETo.winter <- NA
          nextdf$ETo.growing <- nextdf$ETo.annual
        }
      # print(ncol(nextdf))
        colnames(nextdf)[2:3] <- c(paste0(paw_name, '.cmH2O'), paste0(paw_name, '.mmH2O'))
        nextdf$cropcode <- cropdf$cropcode[i]
        nextdf$cropname <- cropdf$cropnames[i]
        print(colnames(nextdf))
        result <- rbind(result, nextdf)
      }
  }
  if (!dir.exists(file.path(resultsDir, 'all_crops'))) {
  dir.create(file.path(resultsDir, 'all_crops'))
  }
  write.csv(result, file.path(resultsDir, 'all_crops', paste0('allcrops', scenario_name, '_FAO56results_points_rounded.csv')), row.names = FALSE)
}
bindallresults('1.0mAD30', '1.0mPAW')
bindallresults('1.0mAD50', '1.0mPAW')
bindallresults('1.0mAD80', '1.0mPAW')
bindallresults('2.0mAD30', '2.0mPAW')
bindallresults('2.0mAD50', '2.0mPAW')
bindallresults('2.0mAD80', '2.0mPAW')
bindallresults('3.0mAD30', '3.0mPAW')
bindallresults('3.0mAD50', '3.0mPAW')
bindallresults('3.0mAD80', '3.0mPAW')

bindallresults('')
#get total GW for each crop x soil storage scenario
#this needs to be updated to handle not having grapes.wine in same order [DONE]
cropnames_list <- c('alfalfa.intermountain', 'alfalfa.imperial', 'alfalfa.CV', 'almond.mature', 'walnut.mature', 'pistachios', 'grapes.table', 'grapes.wine')
final.summary.GW <- data.frame(cropnames=cropnames_list, z1.0m.AD30=rep(NA, 8), z1.0m.AD50=rep(NA, 8), z1.0m.AD80=rep(NA, 8), z2.0m.AD30=rep(NA, 8), z2.0m.AD50=rep(NA, 8), z2.0m.AD80=rep(NA, 8), z3.0m.AD30=rep(NA, 8), z3.0m.AD50=rep(NA, 8), z3.0m.AD80=rep(NA, 8))
for (i in seq_along(cropnames_list)) {
  fnames <- list.files(file.path(path = file.path(resultsDir, cropnames_list[i], 'allyrs_stats/AF.summaries')), pattern = glob2rx('*.AFsummary.by.year.csv'))
  if(cropnames_list[i]=='grapes.wine'){
    fnames <- fnames[c(3, 2, 1, 6, 5, 4, 9, 8, 7)] #because the RDImin0.2 scenario is closest to the 80% AD scenario for the other crops...
  }
  for (j in seq_along(fnames)) {
    af.df <- read.csv(file.path(resultsDir, cropnames_list[i], 'allyrs_stats/AF.summaries', fnames[j]))
    final.summary.GW[i, j + 1] <- sum(af.df$green.water.AF)
  }
}
final.summary.GW
lapply(final.summary.GW, class)
lapply(final.summary.GW[,2:ncol(final.summary.GW)], sum)

#get GW sums by year for all crops combined for a given soil storage scenario
#revise to do all scenarios at once
scenario_name <- '3.0mAD80'
cropnames_list <- c('alfalfa.intermountain', 'alfalfa.imperial', 'alfalfa.CV', 'almond.mature', 'walnut.mature', 'pistachios', 'grapes.table', 'grapes.wine')
GW.by.year <- data.frame(year=2004:2016, alfalfa.intermountain = rep(NA, 13), alfalfa.imperial = rep(NA, 13), alfalfa.CV = rep(NA, 13), almond.mature = rep(NA, 13), walnut.mature = rep(NA, 13), pistachios = rep(NA, 13), grapes.table = rep(NA, 13), grapes.wine = rep(NA, 13))
for (i in seq_along(cropnames_list)) {
  fname <- list.files(path = file.path(resultsDir, cropnames_list[i], 'allyrs_stats/AF.summaries'), pattern = glob2rx(paste0('*', if (cropnames_list[i]=='grapes.wine') {'2.0mRDI.min0.5'} else{scenario_name}, '.AFsummary.by.year.csv')))
  af.df <- read.csv(file.path(resultsDir, cropnames_list[i], 'allyrs_stats/AF.summaries', fname))
  GW.by.year[,i+1] <- af.df$green.water.AF
}
GW.by.year$total.GW.growing <- apply(GW.by.year[,2:ncol(GW.by.year)], 1, sum)
max(GW.by.year$total.GW.growing)
min(GW.by.year$total.GW.growing)
mean(GW.by.year$total.GW.growing)
sum(GW.by.year$total.GW.growing)
sum(GW.by.year$total.GW.growing[order(GW.by.year$total.GW.growing, decreasing = TRUE)][1:4])/sum(GW.by.year$total.GW.growing)
sum(GW.by.year$total.GW.growing[order(GW.by.year$total.GW.growing, decreasing = TRUE)][1:4])

#get E sums by year for all crops combined for a given soil storage scenario
scenario_name <- '3.0mAD50'
cropnames_list <- c('alfalfa.intermountain', 'alfalfa.imperial', 'alfalfa.CV', 'almond.mature', 'walnut.mature', 'pistachios', 'grapes.table', 'grapes.wine')
E.by.year <- data.frame(year=2004:2016, alfalfa.intermountain = rep(NA, 13), alfalfa.imperial = rep(NA, 13), alfalfa.CV = rep(NA, 13), almond.mature = rep(NA, 13), walnut.mature = rep(NA, 13), pistachios = rep(NA, 13), grapes.table = rep(NA, 13), grapes.wine = rep(NA, 13))
for (i in seq_along(cropnames_list)) {
  fname <- list.files(path = file.path(resultsDir, cropnames_list[i], 'allyrs_stats/AF.summaries'), pattern = glob2rx(paste0('*', if (cropnames_list[i]=='grapes.wine') {'2.0mRDI.min0.5'} else{scenario_name}, '.AFsummary.by.year.csv')))
  af.df <- read.csv(file.path(resultsDir, cropnames_list[i], 'allyrs_stats/AF.summaries', fname))
  E.by.year[,i+1] <- af.df$E.growing.AF
}
E.by.year$E.growing.total <- apply(E.by.year[,2:ncol(E.by.year)], 1, sum)
mean(E.by.year$E.growing.total)

#get irr sums by year for all crops combined for a given soil storage scenario
scenario_name <- '3.0mAD80'
cropnames_list <- c('alfalfa.intermountain', 'alfalfa.imperial', 'alfalfa.CV', 'almond.mature', 'walnut.mature', 'pistachios', 'grapes.table', 'grapes.wine')
BW.by.year <- data.frame(year=2004:2016, alfalfa.intermountain = rep(NA, 13), alfalfa.imperial = rep(NA, 13), alfalfa.CV = rep(NA, 13), almond.mature = rep(NA, 13), walnut.mature = rep(NA, 13), pistachios = rep(NA, 13), grapes.table = rep(NA, 13), grapes.wine = rep(NA, 13))
for (i in seq_along(cropnames_list)) {
  fname <- list.files(path = file.path(resultsDir, cropnames_list[i], 'allyrs_stats/AF.summaries'), pattern = glob2rx(paste0('*', if (cropnames_list[i]=='grapes.wine') {'2.0mRDI.min0.5'} else{scenario_name}, '.AFsummary.by.year.csv')))
  af.df <- read.csv(file.path(resultsDir, cropnames_list[i], 'allyrs_stats/AF.summaries', fname))
  BW.by.year[,i+1] <- af.df$blue.water.AF
}
BW.by.year$BW.growing.total <- apply(BW.by.year[,2:ncol(BW.by.year)], 1, sum)
min(BW.by.year$BW.growing.total)
mean(BW.by.year$BW.growing.total)
max(BW.by.year$BW.growing.total)
sum(BW.by.year$BW.growing.total)


#make a hist with the expanded data
hist <- hist(data.expanded, main=NULL, xlab = varname)

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


#rasterize model_points_sp, setting values to 'unique_model_code'
setwd(file.path(resultsDir, 'data.frames/Aug2017'))
model_points <- read.csv('mukeys_cropcodes_climatecodes_AEA.csv')
model_points_sp <- model_points
coordinates(model_points_sp) <- c("longitude_AEA", "latitude_AEA")
proj4string(model_points_sp) <- '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs' #this is Albers Equal Area coordinates)
latmax <- ymax(model_points_sp)
latmin <- ymin(model_points_sp)
lonmax <- xmax(model_points_sp)
lonmin <- xmin(model_points_sp)
model_points_sp <- merge(model_points_sp, allcrops_GW_ET, by='unique_model_code')
#test$meanGW.mm.year <- as.numeric(test$meanGW.mm.year)


#get cell numbers of interest to see if this works faster
cell_numbers_of_interest <- Which(!is.na(raster.model.codes), cells = TRUE)
unique_model_codes <- raster.model.codes[cell_numbers_of_interest]
cellnums_to_modelcode <- cbind(cell_numbers_of_interest, unique_model_codes)
length(cellnums_to_modelcode)
setwd('C:/Users/smdevine/Desktop/Allowable_Depletion/model_scaffold/run_model/Aug2017')
write.csv(cellnums_to_modelcode, 'cellnumbers_to_modelcodes.csv', row.names = FALSE)
cell_numbers_of_interest <- read.csv('cellnumbers_to_modelcodes.csv', stringsAsFactors = FALSE)


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
