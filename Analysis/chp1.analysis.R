library(raster)
options(digits = 22)
max_modified <- function(x) {
  if(all(is.na(x))) {
    return(NA)
  }
  else {max(x, na.rm = TRUE)}
}
min_modified <- function(x) {
  if(all(is.na(x))) {
    return(NA)
  }
  else {min(x, na.rm = TRUE)}
}
modelscaffoldDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/model_scaffold/run_model/Sep2017'
setwd(modelscaffoldDir)
cell_numbers_of_interest <- read.csv('cellnumbers_to_modelcodes.csv', stringsAsFactors = FALSE)
raster.model.codes <- raster('model.codes.Aug2017.tif')
prism.data <- read.csv('PRISM.precip.data.updated9.13.17.csv', stringsAsFactors = FALSE)
resultsDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/results'
scenario.resultsDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/results/scenario results/Sep2017' #this is for most recent runs starting Sept 15 2017; August runs results are in a directory up
setwd(scenario.resultsDir)
list.files()
almond2.0m_AD50 <- read.csv('almond.mature2.0mAD50_FAO56results.csv', stringsAsFactors = FALSE)
almond2.0m_AD50$full_matrix_rownum <- NULL
dim(almond2.0m_AD50) #1206045 rows
walnut2.0m_AD50 <- read.csv('walnut.mature2.0mAD50_FAO56results.csv', stringsAsFactors = FALSE)
walnut2.0m_AD50$full_matrix_rownum <- NULL
dim(walnut2.0m_AD50)
pistachio2.0m_AD50 <- read.csv('pistachios2.0mAD50_FAO56results.csv', stringsAsFactors = FALSE)
dim(pistachio2.0m_AD50)
grapes.wine2.0m_0.2minRDI <- read.csv('grapes.wine2.0mRDI.min0.2_FAO56results.csv', stringsAsFactors = FALSE)
mean(grapes.wine2.0m_0.2minRDI$GW.ET.growing , na.rm = TRUE) #134.931 mm
hist(grapes.wine2.0m_0.2minRDI$GW.ET.growing)
sum(grapes.wine2.0m_0.2minRDI$GW.ET.growing < 0, na.rm = TRUE) #38591 scenario years less than 0
grapes.wine2.0m_0.2minRDI$H2O.stress <- NULL
dim(grapes.wine2.0m_0.2minRDI)
allcrops2.0m_AD50 <- rbind(almond2.0m_AD50, walnut2.0m_AD50, pistachio2.0m_AD50, grapes.wine2.0m_0.2minRDI)
dim(allcrops2.0m_AD50) #3,432,480 rows
length(unique(allcrops2.0m_AD50$unique_model_code)) #200,090 unique model codes

#this aggregates results across all years by mukey and unique model code
AggregateResults <- function(df, varname, func, ...) {
  var.by.year <- tapply(df[[varname]], df$unique_model_code_final, func, ...)
  mukeys <- tapply(df$mukey, df$unique_model_code_final, unique)
  comppct_r <- tapply(df$comppct_r, df$unique_model_code_final, unique)
  modelcode <- tapply(df$unique_model_code, df$unique_model_code_final, unique)
  results <- cbind(var.by.year, mukeys, comppct_r, modelcode)
  results <- as.data.frame(results)
  compsums <- as.data.frame(tapply(results$comppct_r[!is.na(results$var.by.year)], results$modelcode[!is.na(results$var.by.year)], sum))
  colnames(compsums) <- 'compsums'
  compsums$modelcode <- rownames(compsums)
  results <- merge(results, compsums, by='modelcode')
  var.final <- tapply(results$var.by.year*(results$comppct_r/results$compsums), results$modelcode, sum, na.rm=TRUE)
  var.final <- as.data.frame(var.final)
  colnames(var.final) <- 'var.final'
  var.final$unique_model_code <- rownames(var.final)
  var.final$var.final <- as.numeric(var.final$var.final)
  var.final <- var.final[,c(2,1)]
  colnames(var.final)[2] <- varname
  var.final
}
almond_gw_et <- AggregateResults(almond2.0m_AD50, 'GW.ET.growing', mean, na.rm=TRUE)
almond_paw <- AggregateResults(almond2.0m_AD50, 'z2.0m_cmH2O_modified_comp', unique)

setwd(file.path(resultsDir, 'data.frames/Aug2017'))
model_points <- read.csv('mukeys_cropcodes_climatecodes_AEA.csv')
almond_points <- model_points[which(model_points$crop_code==75),]
almond_gw_et <- merge(almond_points, almond_gw_et, by='unique_model_code')
almond_gw_et <- merge(almond_gw_et, almond_paw, by='unique_model_code')
plot(almond_gw_et$GW.ET.growing, almond_gw_et$z2.0m_cmH2O_modified_comp)
storage_vs_GW <- lm(almond_gw_et$GW.ET.growing ~ almond_gw_et$z2.0m_cmH2O_modified_comp)
summary(storage_vs_GW)

#get the mean water year P by cell number, excluding 2017
prism.data$water.year <- prism.data$year
prism.data$water.year[which(prism.data$months >= 10)] <- prism.data$water.year[which(prism.data$months >= 10)] + 1
prism.by.year <- split(prism.data, prism.data$water.year)
for (j in 1:length(prism.by.year)) { #get the unnecessary columns out now
  prism.by.year[[j]] <- prism.by.year[[j]][,6:(ncol(prism.by.year[[j]])-1)]
}
prism.annual.sums <- do.call(rbind, lapply(prism.by.year, sapply, sum)) #sapply was necessary so that the "cell" of the matrix was not returned as a list object
prism.annual.sums <- t(prism.annual.sums)
prism.annual.sums <- as.data.frame(prism.annual.sums)
prism.annual.sums$cell_name <- rownames(prism.annual.sums)
prism.annual.sums$PRISMcellnumber <- as.integer(gsub('cell_', '', prism.annual.sums$cell_name))
colnames(prism.annual.sums)
prism.annual.sums$mean.annual.P <- apply(prism.annual.sums[,1:15], 1, mean)

#now, merge with results above
almond_gw_et <- merge(almond_gw_et, prism.annual.sums[,c('mean.annual.P', 'PRISMcellnumber')], by='PRISMcellnumber')
mean(almond_gw_et$mean.annual.P)
almond_gw_et$GW.ET.growing[which(almond_gw_et$GW.ET.growing < 0)] <- 0
summary(almond_gw_et$GW.ET.growing/almond_gw_et$mean.annual.P)
almond_gw_et$paw_mm <- 10*almond_gw_et$z2.0m_cmH2O_modified_comp
GW_vs_P2_AWS <- lm(GW.ET.growing ~ mean.annual.P + I(mean.annual.P^2) + paw_mm, data=almond_gw_et)
GW_vs_P <- lm(GW.ET.growing ~ mean.annual.P, data=almond_gw_et)
summary(GW_vs_P)
GW_vs_P2_AWS_interact <- lm(GW.ET.growing ~ mean.annual.P + I(mean.annual.P^2) + paw_mm + paw_mm*mean.annual.P, data=almond_gw_et)
summary(GW_vs_P2_AWS_interact)
GW_vs_P2_AWS2_interact <- lm(GW.ET.growing ~ mean.annual.P + I(mean.annual.P^2) + paw_mm + I(paw_mm^2) + paw_mm*mean.annual.P, data=almond_gw_et)
summary(GW_vs_P2_AWS2_interact)
GW_vs_P_AWS_interact <- lm(GW.ET.growing ~ mean.annual.P + paw_mm + paw_mm*mean.annual.P, data=almond_gw_et)
summary(GW_vs_P_AWS_interact)
#builds a raster based upon aggregation process above
RasterBuild <- function(df, varname, rasterfname, func, ...) {
  var.by.year <- tapply(df[[varname]], df$unique_model_code_final, func, ...)
  mukeys <- tapply(df$mukey, df$unique_model_code_final, unique)
  comppct_r <- tapply(df$comppct_r, df$unique_model_code_final, unique)
  modelcode <- tapply(df$unique_model_code, df$unique_model_code_final, unique)
  results <- cbind(var.by.year, mukeys, comppct_r, modelcode)
  results <- as.data.frame(results)
  compsums <- as.data.frame(tapply(results$comppct_r[!is.na(results$var.by.year)], results$modelcode[!is.na(results$var.by.year)], sum))
  colnames(compsums) <- 'compsums'
  compsums$modelcode <- rownames(compsums)
  results <- merge(results, compsums, by='modelcode')
  var.final <- tapply(results$var.by.year*(results$comppct_r/results$compsums), results$modelcode, sum, na.rm=TRUE)
  var.final <- as.data.frame(var.final)
  colnames(var.final) <- 'var.final'
  var.final$unique_model_code <- rownames(var.final)
  var.final$var.final <- as.numeric(var.final$var.final)
  var.final <- var.final[,c(2,1)]
  raster.result <- raster.model.codes
  raster.result[cell_numbers_of_interest$cell_numbers_of_interest] <- var.final$var.final[match(cell_numbers_of_interest$unique_model_codes, var.final$unique_model_code)]
  setwd(file.path(resultsDir, 'rasters/Sep2017'))
  rasterOptions(progress = 'window')
  writeRaster(raster.result, rasterfname, format='GTiff')
}

RasterBuild(almond2.0m_AD50, "GW.ET.growing", 'almondGW.ET.growing.Sep2017runs.tif', mean, na.rm=TRUE)
setwd(file.path(resultsDir, 'rasters/Sep2017'))
almond_gw_et_raster <- raster('almondGW.ET.growing.Sep2017runs.tif')

RasterBuild("ET.growing", 'ET.growing.Aug2017runs.tif', mean, na.rm=TRUE)
RasterBuild("Irr.app.total", 'Irr.app.total.Aug2017runs.tif', mean, na.rm=TRUE)
RasterBuild("E.growing", 'E.growing.Aug2017runs.tif', mean, na.rm=TRUE)
RasterBuild("GW.capture.net", 'GW.capture.net.Aug2017runs.tif', mean, na.rm=TRUE)
RasterBuild("deep.perc.annual", 'deep.perc.annual.Aug2017runs.tif', mean, na.rm=TRUE)
RasterBuild("z2.0m_cmH2O_modified_comp", 'paw.cmH2O.Aug2017runs.tif', unique)
RasterBuild("TEW", 'TEW.surface.Aug2017runs.tif', unique)
RasterBuild("Dr.end.season", 'Dr.end.season.Aug2017runs.tif', mean, na.rm=TRUE)

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
raster.model.codes <- raster(xmn=(lonmin-15), xmx=(lonmax+15), ymn=(latmin-15), ymx=(latmax+15), resolution=30, crs='+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs') #add or subtract 15 because coordinates are centers of the raster cells from which these were derived
rasterOptions(progress = 'window')
raster.model.codes <- rasterize(x=model_points_sp, y=raster.model.codes, field='unique_model_code', fun=function(x,...) {min(x)})
setwd(file.path(resultsDir, 'rasters/Aug2017'))
writeRaster(raster.model.codes, 'model.codes.Aug2017.tif', format='GTiff')
raster.model.codes <- raster('model.codes.Aug2017.tif')
raster.maxGW <- subs(raster.model.codes, max_GW_ET, by=1, which=2) #this is much faster than rasterizing points
writeRaster(raster.maxGW, 'maxGW.ET.Aug2017runs.tif', format='GTiff')

#get cell numbers of interest to see if this works faster
cell_numbers_of_interest <- Which(!is.na(raster.model.codes), cells = TRUE)
unique_model_codes <- raster.model.codes[cell_numbers_of_interest]
cellnums_to_modelcode <- cbind(cell_numbers_of_interest, unique_model_codes)
length(cellnums_to_modelcode)
setwd('C:/Users/smdevine/Desktop/Allowable_Depletion/model_scaffold/run_model/Aug2017')
write.csv(cellnums_to_modelcode, 'cellnumbers_to_modelcodes.csv', row.names = FALSE)
cell_numbers_of_interest <- read.csv('cellnumbers_to_modelcodes.csv', stringsAsFactors = FALSE)
# or like this (and this also returns the cell values ( p[, 3] ): 
p <- rasterToPoints(ras, fun=function(x){x>50}) 
cellFromXY(ras, p[,1:2])

#formula for converting from mm GW to acre-feet GW
sum(!is.na(model_points_sp$maxGW.mm.year))*900*mean(model_points_sp$maxGW.mm.year, na.rm=TRUE)/(1000*1233.48) #2,465,077 acre-feet for 2,935,306 acres
#acre-feet of green water
table(model_points_sp$crop_code[which(is.na(model_points_sp$maxGW.mm.year))])



#code for working with irrigation dates
#now, aggregate mean date to 1st irrigation for green water ET by mukey & model.code
allcrops2.0m_AD50$Irr.1[which(allcrops2.0m_AD50$Irr.1=='1900-01-01')] <- NA
test <- allcrops2.0m_AD50$Irr.1
test <- as.integer(test) #this coerces text to NA
date_integer_indices <- which(!is.na(test))
allcrops2.0m_AD50$Irr.1[date_integer_indices] <- as.character(as.Date(as.integer(allcrops2.0m_AD50$Irr.1[date_integer_indices]), origin='1970-01-01'))
allcrops2.0m_AD50$Irr.1[which(allcrops2.0m_AD50$Irr.1=='1900-01-01')] <- NA
allcrops2.0m_AD50$Irr.1 <- as.Date(allcrops2.0m_AD50$Irr.1, format='%Y-%m-%d')
class(allcrops2.0m_AD50$Irr.1)
allcrops2.0m_AD50$Irr.1.doy <- as.integer(format.Date(allcrops2.0m_AD50$Irr.1, '%j'))
first.irr.mean <- tapply(allcrops2.0m_AD50$Irr.1.doy, allcrops2.0m_AD50$unique_model_code_final, mean, na.rm=TRUE)
mukeys <- tapply(allcrops2.0m_AD50$mukey, allcrops2.0m_AD50$unique_model_code_final, unique)
comppct_r <- tapply(allcrops2.0m_AD50$comppct_r, allcrops2.0m_AD50$unique_model_code_final, unique)
modelcode <- tapply(allcrops2.0m_AD50$unique_model_code, allcrops2.0m_AD50$unique_model_code_final, unique)
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

#almond data exploration
sum(almond_gw_et$GW.ET.growing < 0, na.rm = TRUE) #only 596 c
hist(almond_gw_et$GW.ET.growing)

#almond data exploration -- this is results in the model scaffold structure; it has not been joined back to spatial representation
sum(almond2.0m_AD50$GW.ET.growing < 0, na.rm = TRUE) #2543 less than 0;
summary(almond2.0m_AD50$GW.ET.growing)
hist(almond2.0m_AD50$GW.ET.growing)
mean(almond2.0m_AD50$GW.ET.growing, na.rm = TRUE)
hist(tapply(almond2.0m_AD50$GW.ET.growing, almond2.0m_AD50$Model.Year, mean, na.rm=TRUE))
hist(tapply(almond2.0m_AD50$Irr.app.total, almond2.0m_AD50$Model.Year, mean, na.rm=TRUE))
plot(tapply(almond2.0m_AD50$Irr.app.total, almond2.0m_AD50$Model.Year, mean, na.rm=TRUE), tapply(almond2.0m_AD50$GW.ET.growing, almond2.0m_AD50$Model.Year, mean, na.rm=TRUE))
plot(tapply(almond2.0m_AD50$ET.growing, almond2.0m_AD50$Model.Year, mean, na.rm=TRUE), tapply(almond2.0m_AD50$Irr.app.total, almond2.0m_AD50$Model.Year, mean, na.rm=TRUE))
plot(2003:2017, tapply(almond2.0m_AD50$ET.growing, almond2.0m_AD50$Model.Year, mean, na.rm=TRUE))
plot(tapply(almond2.0m_AD50$z2.0m_cmH2O_modified_comp, almond2.0m_AD50$unique_model_code_final, mean, na.rm=TRUE), tapply(almond2.0m_AD50$GW.ET.growing, almond2.0m_AD50$unique_model_code_final, mean, na.rm=TRUE))
almond2.0m_paw <- as.data.frame(tapply(almond2.0m_AD50$z2.0m_cmH2O_modified_comp, almond2.0m_AD50$unique_model_code_final, unique))
colnames(almond2.0m_paw) <- 'PAW_cm'
hist(almond2.0m_paw$PAW_cm)
boxplot()
head(which(almond2.0m_AD50$GW.ET.growing < 0))
almond2.0m_AD50[476990,]
n <- 25
mean(almond2.0m_AD50$E.annual[(n*15-14):(n*15)], na.rm = TRUE)

##data exploration
mean(model.scaffold.results$GW.ET.growing, na.rm=TRUE)
hist(model.scaffold.results$GW.ET.growing)
hist(tapply(model.scaffold.results$GW.ET.growing, model.scaffold.results$Model.Year, mean, na.rm=TRUE))
hist(tapply(model.scaffold.results$Irr.app.total, model.scaffold.results$Model.Year, mean, na.rm=TRUE))
plot(tapply(model.scaffold.results$Irr.app.total, model.scaffold.results$Model.Year, mean, na.rm=TRUE), tapply(model.scaffold.results$GW.ET.growing, model.scaffold.results$Model.Year, mean, na.rm=TRUE))
plot(tapply(model.scaffold.results$ET.growing, model.scaffold.results$Model.Year, mean, na.rm=TRUE), tapply(model.scaffold.results$Irr.app.total, model.scaffold.results$Model.Year, mean, na.rm=TRUE))
plot(2003:2017, tapply(model.scaffold.results$ET.growing, model.scaffold.results$Model.Year, mean, na.rm=TRUE))
(240/25.4)/12*1000000
plot(tapply(model.scaffold.results$z1.0m_cmH2O_modified_comp, model.scaffold.results$unique_model_code_final, mean, na.rm=TRUE), tapply(model.scaffold.results$GW.ET.growing, model.scaffold.results$unique_model_code_final, mean, na.rm=TRUE))
