library(raster)
resultsDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/results/Oct2017/summaries'
modelscaffoldDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/model_scaffold/run_model/Oct2017'
#if so desired
#model.scaffold <- read.csv(file.path(modelscaffoldDir, 'model_scaffold_majcomps.v2.csv'), stringsAsFactors = F)
rasterResultsDir <- 'D:/Allowable_Depletion/results/Oct2017/summaries'
list.files(path=rasterResultsDir)
list.files(path=file.path(rasterResultsDir, 'allcrops', 'figures', 'scenario_2.0mAD50'))
list.files(path=file.path(rasterResultsDir, 'allcrops', 'figures', 'scenario_2.0mAD50', 'rasters', 'GW.ET.growing'))
gw.median.z2.0AD50 <- raster(file.path(rasterResultsDir, 'allcrops', 'figures', 'scenario_2.0mAD50', 'rasters', 'GW.ET.growing', 'GW.ET.growing.median.tif'))
gw.median.z2.0AD50
gw.median.z2.0AD50[gw.median.z2.0AD50 < 0] <- 0 #change negative GW values to 0.  negative GW values indicate that part of the irrigation was needed to overcome a dormant period water deficit
cellStats(gw.median.z2.0AD50*(9 / 12334.8), stat='sum') #1988704 acre-feet
bw.median.z2.0AD50 <- raster(file.path(rasterResultsDir, 'allcrops', 'figures', 'scenario_2.0mAD50', 'rasters', 'GW.ET.growing', 'GW.ET.growing.median.tif'))
gw.2007.z2.0AD50 <- raster(file.path(rasterResultsDir, 'allcrops', 'figures', 'scenario_2.0mAD50', 'rasters', 'GW.ET.growing', 'GW.ET.growing.2007.tif'))
gw.2007.z2.0AD50[gw.2007.z2.0AD50 < 0] <- 0
cellStats(gw.2007.z2.0AD50*(9 / 12334.8), stat='sum') #1154671 acre-feet

gw.scenario <- 'scenario_2.0mAD50'
varname <- 'GW.ET.growing'
gw.by.year <- list.files(path=file.path(rasterResultsDir, 'allcrops', 'figures', 'scenario_2.0mAD50', 'rasters', 'GW.ET.growing'), pattern = glob2rx('*.tif'), full.names = TRUE)
gw.by.year.2005.2016 <- gw.by.year[2:13]
gw.by.year.2005.2016 <- stack(x=gw.by.year.2005.2016)
gw.std.dev.2005.2016 <- calc(gw.by.year.2005.2016, sd)
