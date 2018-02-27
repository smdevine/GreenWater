library(raster)
resultsDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/results/Oct2017/summaries'
modelscaffoldDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/model_scaffold/run_model/Oct2017'
spatialDir <- 'C:/Users/smdevine/Desktop/SpatialData'
#if so desired
#model.scaffold <- read.csv(file.path(modelscaffoldDir, 'model_scaffold_majcomps.v2.csv'), stringsAsFactors = F)
rasterResultsDir <- 'D:/Allowable_Depletion/results/Jan2018.AEA/summaries'
rasterResultsDir2 <- 'D:/Allowable_Depletion/results/Oct2017/summaries'
list.files(path=rasterResultsDir)
list.files(path=file.path(rasterResultsDir, 'allcrops', 'figures', 'scenario_2.0mAD50'))
list.files(path=file.path(rasterResultsDir, 'allcrops', 'figures', 'scenario_2.0mAD50', 'rasters'))
list.files(path=file.path(rasterResultsDir, 'allcrops', 'figures', 'scenario_2.0mAD50', 'rasters', 'GW.ET.growing'))
gw.median.z2.0AD50 <- raster(file.path(rasterResultsDir, 'allcrops', 'figures', 'scenario_2.0mAD50', 'rasters', 'GW.ET.growing', 'GW.ET.growing.median.tif'))
gw.median.z2.0AD50
gw.median.z2.0AD50[gw.median.z2.0AD50 < 0] <- 0 #change negative GW values to 0.  negative GW values indicate that part of the irrigation was needed to overcome a dormant period water deficit
cellStats(gw.median.z2.0AD50*(9 / 12334.8), stat='sum') #1988704 acre-feet
bw.median.z2.0AD50 <- raster(file.path(rasterResultsDir, 'allcrops', 'figures', 'scenario_2.0mAD50', 'rasters', 'GW.ET.growing', 'GW.ET.growing.median.tif'))
gw.2007.z2.0AD50 <- raster(file.path(rasterResultsDir, 'allcrops', 'figures', 'scenario_2.0mAD50', 'rasters', 'GW.ET.growing', 'GW.ET.growing.2007.tif'))
gw.2007.z2.0AD50[gw.2007.z2.0AD50 < 0] <- 0
cellStats(gw.2007.z2.0AD50*(9 / 12334.8), stat='sum') #1154671 acre-feet

#get the standard deviation of annual green water availability for 2 m x 50% AD
gw.scenario <- 'scenario_2.0mAD50'
varname <- 'GW.ET.growing'
gw.by.year <- list.files(path=file.path(rasterResultsDir, 'allcrops', 'figures', 'scenario_2.0mAD50', 'rasters', 'GW.ET.growing'), pattern = glob2rx('*.tif'), full.names = TRUE)
gw.by.year.2005.2016 <- gw.by.year[2:13]
gw.by.year.2005.2016 <- stack(x=gw.by.year.2005.2016)
gw.std.dev.2005.2016 <- calc(gw.by.year.2005.2016, sd)
writeRaster(gw.std.dev.2005.2016, filename = 'D:/Allowable_Depletion/results/Oct2017/summaries/allcrops/figures/scenario_2.0mAD50/rasters/GW.ET.growing/GW.ET.growing.stdev.mm.2005.2016.tif', options='window')

#get the total green water map (2005-2016)
gw.scenario <- 'scenario_0.5mAD30'
varname <- 'GW.ET.growing'
gw.by.year <- list.files(path=file.path(rasterResultsDir, 'allcrops', 'figures', gw.scenario, 'rasters', varname), pattern = glob2rx('*.tif'), full.names = TRUE)
gw.by.year
gw.by.year.2005.2016 <- gw.by.year[2:13]
gw.by.year.2005.2016 <- stack(x=gw.by.year.2005.2016)
#beginCluster(n=4)
gw.total.2005.2016 <- calc(gw.by.year.2005.2016, sum, progress='window') #this 
cellStats(gw.total.2005.2016*(9 / 12334.8), stat='sum') #26.07 MAF; this compares to 26.2 MAF when negative annual GW is first converted to 0 before summing
endCluster()
writeRaster(gw.total.2005.2016, filename = file.path(rasterResultsDir, 'allcrops', 'figures', gw.scenario, 'rasters', varname, 'GW.ET.growing.total.mm.2005.2016.tif'), options='window')

#get the total blue water map (2005-2016)
bw.by.year <- list.files(path=file.path(rasterResultsDir, 'allcrops', 'figures', gw.scenario, 'rasters', 'Irr.app.total'), pattern = glob2rx('*.tif'), full.names = TRUE)
bw.by.year
bw.by.year.2005.2016 <- bw.by.year[2:13]
bw.by.year.2005.2016 <- stack(bw.by.year.2005.2016)
#beginCluster(n=4)
bw.total.2005.2016 <- calc(bw.by.year.2005.2016, sum, progress='window')
writeRaster(bw.total.2005.2016, filename = file.path(rasterResultsDir, 'allcrops', 'figures', gw.scenario, 'rasters', 'Irr.app.total', 'Irr.app.total.mm.2005.2016.tif'), options='window')
#endCluster()

#get the annual average blue water map (2005-2016)
gw.scenario <- 'scenario_3.0mAD50'
bw.total.2005.2016 <- raster(file.path(rasterResultsDir, 'allcrops', 'figures', gw.scenario, 'rasters', 'Irr.app.total', 'Irr.app.total.mm.2005.2016.tif'))
bw.annual.avg <- calc(bw.total.2005.2016, fun=function(x) {x / 12})
writeRaster(bw.annual.avg, filename = file.path(rasterResultsDir, 'allcrops', 'figures', gw.scenario, 'rasters', 'Irr.app.total', 'Irr.app.total.mm.annual.avg.tif'))

#get the annual avg blue water differences between soil storage scenarios
bw.total.0.5mAD30 <- raster(file.path(rasterResultsDir, 'allcrops', 'figures', 'scenario_0.5mAD30', 'rasters', 'Irr.app.total', 'Irr.app.total.mm.annual.avg.tif'))
bw.total.1.0mAD50 <- raster(file.path(rasterResultsDir, 'allcrops', 'figures', 'scenario_1.0mAD50', 'rasters', 'Irr.app.total', 'Irr.app.total.mm.annual.avg.tif'))
bw.total.2.0mAD50 <- raster(file.path(rasterResultsDir, 'allcrops', 'figures', 'scenario_2.0mAD50', 'rasters', 'Irr.app.total', 'Irr.app.total.mm.annual.avg.tif'))
bw.total.3.0mAD50 <- raster(file.path(rasterResultsDir, 'allcrops', 'figures', 'scenario_3.0mAD50', 'rasters', 'Irr.app.total', 'Irr.app.total.mm.annual.avg.tif'))
bw.total.stack <- stack(bw.total.0.5mAD30, bw.total.1.0mAD50, bw.total.2.0mAD50, bw.total.3.0mAD50)
names(bw.total.stack) <- c('bw.total.0.5mAD30', 'bw.total.1.0mAD50', 'bw.total.2.0mAD50', 'bw.total.3.0mAD50')
bw.1.0less0.5 <- calc(bw.total.stack, fun=function(x) {x[2] - x[1]}, filename=file.path(rasterResultsDir, 'allcrops', 'figures', 'comparisons', 'bw.1.0AD50less0.5AD30.tif'), progress='window')
bw.2.0less1.0 <- calc(gw.total.stack, fun=function(x) {x[3] - x[2]}, filename=file.path(rasterResultsDir, 'allcrops', 'figures', 'comparisons', 'bw.2.0AD50less1.0AD50.tif'), progress='window')
bw.3.0less2.0 <- calc(gw.total.stack, fun=function(x) {x[4] - x[3]}, filename=file.path(rasterResultsDir, 'allcrops', 'figures', 'comparisons', 'bw.3.0AD50less2.0AD50.tif'), progress='window')

#get the total green water map (2005-2016) after first converting negative GW to 0 within the chp1.analysis.R script
gw.scenario <- 'scenario_0.5mAD30'
varname <- 'GW.ET.growing'
gw.by.year.revised <- list.files(path=file.path(rasterResultsDir, 'allcrops', 'figures', gw.scenario, 'rasters', 'GW.ET.growing', 'revised.results'), pattern = glob2rx('*.tif'), full.names = TRUE)
print(gw.by.year.revised)
gw.by.year.revised.stack <- stack(x=gw.by.year.revised)
beginCluster(n=4)
system.time(gw.total.2005.2016.revised <- calc(gw.by.year.revised.stack, sum, progress='window')) #this 
cellStats(gw.total.2005.2016.revised * (9 / 12334.8), stat='sum') #26.206252 MAF; this compares to 26.2 MAF when negative annual GW is first converted to 0 before summing
writeRaster(gw.total.2005.2016.revised, filename = file.path(rasterResultsDir, 'allcrops', 'figures', gw.scenario, 'rasters', 'GW.ET.growing', 'revised.results', 'revised.GW.ET.growing.total.mm.2005.2016.tif'), options='window')

#this process changed on 12/22 and altered below this block
#get the growing season ET and then the green water:growing season ET using the revised GW files where negative annual GW results were converted to 0; growing season ET = blue water + green water, where green water negative values are not converted to 0 to get accurate growing season ET
gw.total.2005.2016.revised <- raster(file.path(rasterResultsDir, 'allcrops', 'figures', gw.scenario, 'rasters', 'GW.ET.growing', 'revised.results', 'revised.GW.ET.growing.total.mm.2005.2016.tif'))
gw.total.2005.2016 <- raster(file.path(rasterResultsDir, 'allcrops', 'figures', gw.scenario, 'rasters', 'GW.ET.growing','GW.ET.growing.total.mm.2005.2016.tif'))
bw.total.2005.2016 <- raster(file.path(rasterResultsDir, 'allcrops', 'figures', gw.scenario, 'rasters', 'Irr.app.total', 'Irr.app.total.mm.2005.2016.tif'))
total.stack <- stack(gw.total.2005.2016.revised, gw.total.2005.2016, bw.total.2005.2016)
#total.stack$growing.et.total.2005.2016 <- calc(total.stack, sum, progress='window')
gw.to.et.ratio <- calc(total.stack, fun=function(x) {x[1] / (x[2] + x[3]) }, progress='window')
writeRaster(gw.to.et.ratio, filename=file.path(rasterResultsDir, 'allcrops', 'figures', gw.scenario, 'rasters', 'GW.ET.growing', 'revised.results', 'revised.GW.ET.to.growing.ET.2005.2016.tif'), options='window') #range is ???
gw.to.et.ratio <- raster(file.path(rasterResultsDir, 'allcrops', 'figures', gw.scenario, 'rasters', 'GW.ET.growing', 'revised.results', 'revised.GW.ET.to.growing.ET.2005.2016.tif'))
hist(gw.to.et.ratio)

#12/22/17 revised code
#get the revised cumulative gw et:growing season et ratio using negative annual GW values in the numerator also when present
gw.scenario <- 'scenario_0.5mAD30'
gw.total.2005.2016 <- raster(file.path(rasterResultsDir, 'allcrops', 'figures', gw.scenario, 'rasters', 'GW.ET.growing', 'GW.ET.growing.sum.tif'))
bw.total.2005.2016 <- raster(file.path(rasterResultsDir2, 'allcrops', 'figures', gw.scenario, 'rasters', 'Irr.app.total', 'Irr.app.total.mm.2005.2016.tif'))
total.stack <- stack(gw.total.2005.2016, bw.total.2005.2016)
#total.stack$growing.et.total.2005.2016 <- calc(total.stack, sum, progress='window')
gw.to.et.ratio <- calc(total.stack, fun=function(x) {x[1] / (x[1] + x[2]) }, filename=file.path(rasterResultsDir, 'allcrops', 'figures', gw.scenario, 'rasters', 'GW.ET.to.growing.ET.2005.2016.tif'), progress='window')
#writeRaster(gw.to.et.ratio, filename=file.path(rasterResultsDir, 'allcrops', 'figures', gw.scenario, 'rasters', 'GW.ET.to.growing.ET.2005.2016.tif'), options='window') #range is ???
#gw.to.et.ratio <- raster(file.path(rasterResultsDir, 'allcrops', 'figures', gw.scenario, 'rasters', 'GW.ET.growing', 'GW.ET.to.growing.ET.2005.2016.tif'))

#get the standard deviation of annual green water availability for 2 m x 50% AD for rasters with negative values converted to 0 first
gw.scenario <- 'scenario_2.0mAD50'
varname <- 'GW.ET.growing'
gw.by.year.revised <- list.files(path=file.path(rasterResultsDir, 'allcrops', 'figures', gw.scenario, 'rasters', 'GW.ET.growing', 'revised.results'), pattern = glob2rx('*.tif'), full.names = TRUE)
print(gw.by.year.revised) #to see which ones we need
gw.by.year.revised.stack <- stack(x=gw.by.year.revised[1:12])
beginCluster(n=4)
gw.std.dev.2005.2016 <- calc(gw.by.year.revised.stack, sd)
endCluster()
writeRaster(gw.std.dev.2005.2016, filename = file.path(rasterResultsDir, 'allcrops', 'figures', gw.scenario, 'rasters', 'GW.ET.growing', 'revised.results', 'revisedGW.ET.growing.stdev.mm.2005.2016.tif'))

#get quantiles of various rasters for improving ArcGIS's estimates for plots
gw.scenario <- 'scenario_1.0mAD50'
gw.to.et.ratio0.5mAD30 <- raster(file.path(rasterResultsDir, 'allcrops', 'figures', 'scenario_0.5mAD30', 'rasters', 'GW.ET.growing', 'revised.results', 'revised.GW.ET.to.growing.ET.2005.2016.tif'))
gw.to.et.ratio1.0mAD50 <- raster(file.path(rasterResultsDir, 'allcrops', 'figures', 'scenario_1.0mAD50', 'rasters', 'GW.ET.growing', 'revised.results', 'revised.GW.ET.to.growing.ET.2005.2016.tif'))
gw.to.et.ratio2.0mAD50 <- raster(file.path(rasterResultsDir, 'allcrops', 'figures', 'scenario_2.0mAD50', 'rasters', 'GW.ET.growing', 'revised.results', 'revised.GW.ET.to.growing.ET.2005.2016.tif'))
gw.to.et.ratio3.0mAD50 <- raster(file.path(rasterResultsDir, 'allcrops', 'figures', 'scenario_3.0mAD50', 'rasters', 'GW.ET.growing', 'revised.results', 'revised.GW.ET.to.growing.ET.2005.2016.tif'))
gw.to.et.ratio.stack <- stack(gw.to.et.ratio0.5mAD30, gw.to.et.ratio1.0mAD50, gw.to.et.ratio2.0mAD50, gw.to.et.ratio3.0mAD50)
gw.to.et.quantiles <- lapply(gw.to.et.ratio.stack, quantile, probs=c(0.05, 0.1, 0.2, 0.4, 0.5, 0.6, 0.8, 0.9, 0.95), na.rm=TRUE, type=7)
gw.to.et.quantiles0. <- quantile(gw.to.et.ratio0.5mAD30, probs=c(0.05, 0.1, 0.2, 0.4, 0.5, 0.6, 0.8, 0.9, 0.95), na.rm=TRUE, type=7)
total.gw.quantiles <- quantile(gw.total.2005.2016.revised, probs=c(0.2, 0.4, 0.6, 0.8), na.rm=TRUE, type=7)
total.gw.quantiles.v2 <- quantile(gw.total.2005.2016.revised, probs=c(0.2, 0.4, 0.6, 0.8), na.rm=TRUE, type=5)

eto.mean <- raster(file.path(rasterResultsDir, 'allcrops', 'figures', 'scenario_2.0mAD50', 'rasters', 'ETo.annual', 'ETo.annual.mean.tif'))
eto.mean.quantiles <- quantile(eto.mean, probs=c(0.2, 0.4, 0.6, 0.8), na.rm=TRUE, type=7)
p.mean <- raster(file.path(rasterResultsDir, 'allcrops', 'figures', 'scenario_2.0mAD50', 'rasters', 'P.annual', 'P.annual.mean.tif'))
p.mean.quantiles <- quantile(p.mean, probs=c(0.2, 0.4, 0.6, 0.8), na.rm=TRUE, type=7)
paw <- raster(file.path(rasterResultsDir, 'allcrops', 'figures', 'scenario_2.0mAD50', 'rasters', 'PAW', 'PAW2.0m.mmH2O.SSURGO.tif'))
paw.quantiles <- quantile(paw, probs=c(0.2, 0.4, 0.6, 0.8), na.rm=TRUE, type=7)
gw.annual.quantiles <- quantile(gw.by.year.revised.stack, probs=c(0.2, 0.4, 0.6, 0.8), na.rm=TRUE, type=7) #this failed; was calculated by collect.stats.v2 in chp1.analysis.R
gw.std.dev.2005.2016 <- raster(file.path(rasterResultsDir, 'allcrops', 'figures', 'scenario_2.0mAD50', 'rasters', 'GW.ET.growing', 'revised.results', 'revisedGW.ET.growing.stdev.mm.2005.2016.tif'))
gw.std.dev.quantiles <- quantile(gw.std.dev.2005.2016, probs=c(0.2, 0.4, 0.6, 0.8), na.rm=TRUE, type=7)

#get differences between soil storage scenarios
#get the total green water map (2005-2016) after first converting negative GW to 0 within the chp1.analysis.R script
gw.scenario <- 'scenario_3.0mAD50'
gw.by.year.revised <- list.files(path=file.path(rasterResultsDir, 'allcrops', 'figures', gw.scenario, 'rasters', 'GW.ET.growing', 'revised.results'), pattern = glob2rx('*.tif'), full.names = TRUE)
print(gw.by.year.revised)
gw.by.year.revised.stack <- stack(x=gw.by.year.revised)
beginCluster(n=4)
system.time(gw.total.2005.2016.revised <- calc(gw.by.year.revised.stack, sum, progress='window')) #this 
#cellStats(gw.total.2005.2016.revised * (9 / 12334.8), stat='sum') #26.206252 MAF; this compares to 26.2 MAF when negative annual GW is first converted to 0 before summing
writeRaster(gw.total.2005.2016.revised, filename = file.path(rasterResultsDir, 'allcrops', 'figures', gw.scenario, 'rasters', 'GW.ET.growing', 'revised.results', 'revised.GW.ET.growing.total.mm.2005.2016.tif'), options='window')


gw.total.0.5mAD30 <- raster(file.path(rasterResultsDir, 'allcrops', 'figures', 'scenario_0.5mAD30', 'rasters', 'GW.ET.growing', 'revised.results', 'revised.GW.ET.growing.total.mm.2005.2016.tif'))
gw.total.1.0mAD50 <- raster(file.path(rasterResultsDir, 'allcrops', 'figures', 'scenario_1.0mAD50', 'rasters', 'GW.ET.growing', 'revised.results', 'revised.GW.ET.growing.total.mm.2005.2016.tif'))
gw.total.2.0mAD50 <- raster(file.path(rasterResultsDir, 'allcrops', 'figures', 'scenario_2.0mAD50', 'rasters', 'GW.ET.growing', 'revised.results', 'revised.GW.ET.growing.total.mm.2005.2016.tif'))
gw.total.3.0mAD50 <- raster(file.path(rasterResultsDir, 'allcrops', 'figures', 'scenario_3.0mAD50', 'rasters', 'GW.ET.growing', 'revised.results', 'revised.GW.ET.growing.total.mm.2005.2016.tif'))
gw.total.stack <- stack(gw.total.0.5mAD30, gw.total.1.0mAD50, gw.total.2.0mAD50, gw.total.3.0mAD50)
names(gw.total.stack) <- c('gw.total.0.5mAD30', 'gw.total.1.0mAD50', 'gw.total.2.0mAD50', 'gw.total.3.0mAD50')
gw.1.0less0.5 <- calc(gw.total.stack, fun=function(x) {x[2] - x[1]}, filename=file.path(rasterResultsDir, 'allcrops', 'figures', 'comparisons', 'gw.1.0AD50less0.5AD30.tif'), progress='window')
gw.2.0less1.0 <- calc(gw.total.stack, fun=function(x) {x[3] - x[2]}, filename=file.path(rasterResultsDir, 'allcrops', 'figures', 'comparisons', 'gw.2.0AD50less1.0AD50.tif'), progress='window')
gw.3.0less2.0 <- calc(gw.total.stack, fun=function(x) {x[4] - x[3]}, filename=file.path(rasterResultsDir, 'allcrops', 'figures', 'comparisons', 'gw.3.0AD50less2.0AD50.tif'), progress='window')
gw.2.0less0.5 <- calc(gw.total.stack, fun=function(x) {x[3] - x[1]}, filename=file.path(rasterResultsDir, 'allcrops', 'figures', 'comparisons', 'gw.2.0AD50less0.5AD30.tif'), progress='window')

#get annual avg effect of different soil storage scenarios on annual green water availability
#determine if call to beginCluster has any effect and if calc speeds up raster math
system.time(gw.1.0less0.5_annual <- (gw.1.0less0.5 / 12)) #72.5 seconds
system.time(writeRaster(gw.1.0less0.5_annual, filename = file.path(rasterResultsDir, 'allcrops', 'figures', 'comparisons', 'gw.1.0AD50less0.5AD30.annual.avg.tif'), progress='window'))
beginCluster(4)
system.time(gw.2.0less1.0_annual <- calc(gw.2.0less1.0, fun=function(x) {x / 12})) #66.5 seconds
system.time(writeRaster(gw.2.0less1.0_annual, filename=file.path(rasterResultsDir, 'allcrops', 'figures', 'comparisons', 'gw.2.0AD50less1.0AD50.annual.avg.tif')))
system.time(gw.3.0less2.0_annual <- (gw.3.0less2.0 / 12))
system.time(writeRaster(gw.3.0less2.0_annual, filename=file.path(rasterResultsDir, 'allcrops', 'figures', 'comparisons', 'gw.3.0AD50less2.0AD50.annual.avg.tif')))
endCluster()
system.time(gw.2.0less0.5_annual <- calc(gw.2.0less0.5, fun = function(x) {x / 12})) #conclucion: cluster call has no effect; calc(x, fun=function(x) {x /12}) is slightly faster than x/12
writeRaster(gw.2.0less0.5_annual, file.path(rasterResultsDir, 'allcrops', 'figures', 'comparisons', 'gw.2.0AD50less0.5AD30.annual.avg.tif'))

#new approach to get annual average effect differences using means
#TO-DO: write this as a function in chp1.analysis.R
gw.0.5mAD30.mean <- raster(file.path(rasterResultsDir, 'allcrops', 'figures', 'scenario_0.5mAD30', 'rasters', 'GW.ET.growing', 'GW.ET.growing.mean.tif'))
gw.1.0mAD50.mean <- raster(file.path(rasterResultsDir, 'allcrops', 'figures', 'scenario_1.0mAD50', 'rasters', 'GW.ET.growing', 'GW.ET.growing.mean.tif'))
gw.2.0mAD50.mean <- raster(file.path(rasterResultsDir, 'allcrops', 'figures', 'scenario_2.0mAD50', 'rasters', 'GW.ET.growing', 'GW.ET.growing.mean.tif'))
#gw.3.0mAD50.mean <- raster(file.path(rasterResultsDir, 'allcrops', 'figures', 'scenario_3.0mAD50', 'rasters', 'GW.ET.growing', 'GW.ET.growing.mean.tif'))
gw.annual.mean.stack <- stack(gw.0.5mAD30.mean, gw.1.0mAD50.mean, gw.2.0mAD50.mean) #, gw.3.0mAD50.mean)
gw.1.0less0.5_annual <- calc(gw.annual.mean.stack, fun = function(x) {x[2] - x[1]}, filename=file.path(rasterResultsDir, 'allcrops', 'figures', 'comparisons', 'gw.1.0AD50less0.5AD30.tif'), progress='window')
gw.2.0less1.0_annual <- calc(gw.annual.mean.stack, fun = function(x) {x[3] - x[2]}, filename=file.path(rasterResultsDir, 'allcrops', 'figures', 'comparisons', 'gw.2.0AD50less1.0AD50.tif'), progress='window')
gw.3.0less2.0_annual <- calc(gw.annual.mean.stack, fun = function(x) {x[4] - x[3]}, filename=file.path(rasterResultsDir, 'allcrops', 'figures', 'comparisons', 'gw.3.0AD50less2.0AD50.tif'), progress='window')
gw.2.0less0.5_annual <- calc(gw.annual.mean.stack, fun = function(x) {x[3] - x[1]}, filename=file.path(rasterResultsDir, 'allcrops', 'figures', 'comparisons', 'gw.2.0AD50less0.5AD30.tif'), progress='window')

#annual average effect differences using means for blue water
bw.0.5mAD30.mean <- raster(file.path(rasterResultsDir, 'allcrops', 'figures', 'scenario_0.5mAD30', 'rasters', 'Irr.app.total', 'Irr.app.total.mean.tif'))
bw.1.0mAD50.mean <- raster(file.path(rasterResultsDir, 'allcrops', 'figures', 'scenario_1.0mAD50', 'rasters', 'Irr.app.total', 'Irr.app.total.mean.tif'))
bw.2.0mAD50.mean <- raster(file.path(rasterResultsDir, 'allcrops', 'figures', 'scenario_2.0mAD50', 'rasters', 'Irr.app.total', 'Irr.app.total.mean.tif'))
#bw.3.0mAD50.mean <- raster(file.path(rasterResultsDir, 'allcrops', 'figures', 'scenario_3.0mAD50', 'rasters', 'Irr.app.total', 'Irr.app.total.mean.tif'))
bw.annual.mean.stack <- stack(bw.0.5mAD30.mean, bw.1.0mAD50.mean, bw.2.0mAD50.mean) #, gw.3.0mAD50.mean)
bw.1.0less0.5_annual <- calc(bw.annual.mean.stack, fun = function(x) {x[2] - x[1]}, filename=file.path(rasterResultsDir, 'allcrops', 'figures', 'comparisons', 'bw.1.0AD50less0.5AD30.tif'), progress='window')
bw.2.0less1.0_annual <- calc(bw.annual.mean.stack, fun = function(x) {x[3] - x[2]}, filename=file.path(rasterResultsDir, 'allcrops', 'figures', 'comparisons', 'bw.2.0AD50less1.0AD50.tif'), progress='window')
bw.3.0less2.0_annual <- calc(bw.annual.mean.stack, fun = function(x) {x[4] - x[3]}, filename=file.path(rasterResultsDir, 'allcrops', 'figures', 'comparisons', 'bw.3.0AD50less2.0AD50.tif'), progress='window')
bw.0.5less2.0_annual <- calc(bw.annual.mean.stack, fun = function(x) {x[1] - x[3]}, filename=file.path(rasterResultsDir, 'allcrops', 'figures', 'comparisons', 'bw.0.5AD30less2.0AD50.tif'), progress='window')
quantile(bw.0.5less2.0_annual, probs=c(0.01, 0.05, 0.2, 0.4, 0.6, 0.8, 0.95, 0.99), na.rm=TRUE, type=7)

#project CA counties to California Teale Albers for plotting purposes
CA.counties <- shapefile(file.path(spatialDir, 'government_units', 'county_nrcs_a_ca.shp'))
CA.counties.CA.TA <- spTransform(CA.counties, crs('+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs'))
plot(CA.counties.CA.TA)
shapefile(x=CA.counties.CA.TA, filename=file.path(spatialDir, 'government_units', 'CA_TA_projection', 'counties.CA.TA.shp'))
CA.counties.AEA <- shapefile(file.path(spatialDir, 'government_units', 'AEA_projection', 'CA_counties_AEA.shp'))
plot(CA.counties.AEA)
CV_box <- spPolygons(rbind(c(-2313800, 2221300), c(-2020150, 2221300), c(-2020150, 1564000), c(-2313800, 1564000)), crs='+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs')
plot(CV_box, add=T)

#project CA counties to AEA
#definition from http://spatialreference.org/ref/esri/usa-contiguous-albers-equal-area-conic/proj4/: +proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs
#definition from reading in CA 2015 Cropscape raster: +proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs
CA.counties <- shapefile(file.path(spatialDir, 'government_units', 'county_nrcs_a_ca.shp'))
#CA.counties.AEA <- spTransform(CA.counties, crs('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs'))
CA.counties.AEA <- spTransform(CA.counties, crs('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'))
shapefile(x=CA.counties.AEA, filename=file.path(spatialDir, 'government_units', 'AEA_projection', 'CA.counties.AEA.shp'), overwrite=TRUE)

#project AEA extent frames to CA_TA
CA.counties.CA.TA <- shapefile(file.path(spatialDir, 'government_units', 'CA_TA_projection', 'counties.CA.TA.shp'))
plot(CA.counties.CA.TA)
CV_box_CA.TA <- spTransform(CV_box, crs('+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs'))
plot(CV_box_CA.TA, add=TRUE)
