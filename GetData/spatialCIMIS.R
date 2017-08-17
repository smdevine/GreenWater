#see "Daily reference evapotranspiration for California using satellite imagery and weather station measurement interpolation" by Hart et al. 2009
library(raster)
library(rgdal)
library(XML)
library(httr)
#NOTE: 1/1/2003-2/19/2003, 3/7/2003-3/9/2003, 4/9/2003-4/10/2003, 6/15/2003, 6/17/2003, 6/19/2003-6/22/2003, 6/24/2003, and a number more dates before 9/30/2003 are missing. 2004 water year to the present is good to go.
californiaDir <- 'C:/Users/smdevine/Desktop/SpatialData/CA_counties/government_units'
cellsofinterestDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/model_scaffold/run_model/Aug2017'
#below is specific to downloading data
ca_ta <- showP4(showWKT("+init=epsg:3310"))
spatialCIMISdir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/SpatialCIMIS'
#'C:/Users/smdevine/Desktop/Allowable_Depletion/SpatialCIMIS'
startyear <- '2003'
endyear <- '2017'
startdate <- strptime(paste0("10/01/", startyear), '%m/%d/%Y')
enddate <- strptime(paste0("06/13/", endyear), '%m/%d/%Y')
datesequence <- seq.Date(from=as.Date(startdate), to=as.Date(enddate), by='day')
base_url <- 'http://cimis.casil.ucdavis.edu/cimis/'
varofinterest <- 'U2.asc.gz'
varDir <- 'U2'
#i <- 1
for (i in 1:length(datesequence)) {
  day <- format.Date(datesequence[i], '%d')
  mnth <- format.Date(datesequence[i], '%m')
  yr <- format.Date(datesequence[i], '%Y')
  if (file.exists(file.path(spatialCIMISdir, varDir)) == FALSE) {
    dir.create(file.path(spatialCIMISdir, varDir))
  }
  if (file.exists(file.path(spatialCIMISdir, varDir, yr)) == FALSE) {
    dir.create(file.path(spatialCIMISdir, varDir, yr))
  }
  url_download <- paste0(base_url, yr, '/', mnth, '/', day, '/', varofinterest)
  setwd(file.path(spatialCIMISdir, varDir, yr))
  error_catch <- try(download.file(url_download, destfile = varofinterest, quiet = TRUE, mode = 'wb'))
  if (class(error_catch)=='try-error') {
    print(error_catch)
    next
  }
  cimis_table <- read.table(varofinterest, skip=6, na.strings="-9999")
#this two step operation is much faster than the "for loop" solution
  cimis_table <- t(cimis_table)
  cimis_values <- as.vector(cimis_table)
  cimis_raster <- raster(ncol=510, nrow=560, xmx=610000, xmn=-410000, ymn=-660000, ymx=460000, crs=ca_ta, vals=cimis_values)
  writeRaster(cimis_raster, filename = paste0(varDir, yr, mnth, day, '.tif'), format='GTiff', overwrite=TRUE) #each Gtiff will be 1092 KB
  file.remove(varofinterest)
}

#plot some files to check
yr <- 2017
setwd(file.path(spatialCIMISdir, yr))
raster_fnames <- list.files(pattern = glob2rx('*.tif'))
for(i in 1:100) {
  cimis_raster <- raster(raster_fnames[i])
  plot(cimis_raster, main=raster_fnames[i])
}
setwd(californiaDir)
list.files()
california <- shapefile("california_CA_TA.shp")
plot(california, add=T)

#calculate relative humidity
if (file.exists(file.path(spatialCIMISdir, 'minRH')) == FALSE) {
  dir.create(file.path(spatialCIMISdir, 'minRH'))
}
startyear <- '2003'
endyear <- '2017'
startdate <- strptime(paste0("10/01/", startyear), '%m/%d/%Y')
enddate <- strptime(paste0("06/13/", endyear), '%m/%d/%Y')
datesequence <- seq.Date(from=as.Date(startdate), to=as.Date(enddate), by='day')
i <- 90
for (i in 1:length(datesequence)) {
  day <- format.Date(datesequence[i], '%d')
  mnth <- format.Date(datesequence[i], '%m')
  yr <- format.Date(datesequence[i], '%Y')
  #setwd(file.path(spatialCIMISdir, 'Tmin', yr))
  #Tmin <- raster(paste0('Tmin', yr, mnth, day, '.tif'))
  setwd(file.path(spatialCIMISdir, 'Tmax', yr))
  Tmax <- raster(paste0('Tmax', yr, mnth, day, '.tif'))
  setwd(file.path(spatialCIMISdir, 'Tdew', yr))
  Tdew <- raster(paste0('Tdew', yr, mnth, day, '.tif'))
  Ea <- calc(Tdew, fun = function(x){0.6108*exp(17.27*x/(x+237.3))})
  Es <- calc(Tmax, fun= function(x){0.6108*exp(17.27*x/(x+237.3))})
  #Es <- overlay(x=Tmin, y=Tmax, fun=function(x,y){0.6108/2*(exp(17.27*x/(x+237.3))+exp(17.27*y/(y+237.3)))}) #this is if one desires the avg daily RH
  minRH <- overlay(x=Ea, y=Es, fun=function(x,y){100*x/y}) #with the above calculations for Ea and Es, this is an approximation of the daily minimum RH
  if (file.exists(file.path(spatialCIMISdir, 'minRH', yr)) == FALSE) {
    dir.create(file.path(spatialCIMISdir, 'minRH', yr))
  }
  setwd(file.path(spatialCIMISdir, 'minRH', yr))
  writeRaster(minRH, filename = paste0('minRH_', yr, mnth, day, '.tif'), format='GTiff', overwrite=TRUE)
}

#get SpatialCIMIS into matrix for cells of interest
#file naming convention for U2 rasters forgot '_' between 'U2' and date; hence the if/else statement below
SpCIMISExtract <- function(varname) {
  setwd(cellsofinterestDir)
  cellsofinterest <- read.csv("CIMIS_cells_unique.csv")
  cellsofinterest <- cellsofinterest[order(cellsofinterest$CIMIS_cells), ]
  cellsofinterest_names <- paste0('cell_', as.character(cellsofinterest))
#varname <- 'U2'
  startyear <- '2003'
  endyear <- '2017'
  startdate <- strptime(paste0("10/1/", startyear), '%m/%d/%Y')
  enddate <- strptime(paste0("6/13/", endyear), '%m/%d/%Y')
  datesequence <- seq.Date(from=as.Date(startdate), to=as.Date(enddate), by='day')
  cimis_data <- as.data.frame(matrix(nrow=length(datesequence), ncol=(length(cellsofinterest)+5)))
  colnames(cimis_data) <- c('dates', 'month', 'day', 'year', 'DOY', cellsofinterest_names)
  cimis_data$dates <- format.Date(datesequence, '%m_%d_%Y')
  cimis_data$month <- as.integer(format.Date(datesequence, '%m'))
  cimis_data$day <- as.integer(format.Date(datesequence, '%d'))
  cimis_data$year <- as.integer(format.Date(datesequence, '%Y'))
  cimis_data$DOY <- as.integer(format.Date(datesequence, '%j'))
#i <- 1
  for (i in 1:length(datesequence)) {
    day <- format.Date(datesequence[i], '%d')
    mnth <- format.Date(datesequence[i], '%m')
    yr <- format.Date(datesequence[i], '%Y')
    setwd(file.path(spatialCIMISdir, varname, yr))
    if (varname == 'U2') {
      spCIMIS <- raster(paste0(varname, yr, mnth, day, '.tif'))
    } else {spCIMIS <- raster(paste0(varname, '_', yr, mnth, day, '.tif'))}
    cimis_data[i, 6:ncol(cimis_data)] <- extract(spCIMIS, cellsofinterest)
    print(i)
  }
  setwd(cellsofinterestDir)
#write.csv(cimis_data, paste0('SpatialCIMIS_', varname, '_data.csv'), row.names = F)
#cimis_data <- read.csv('SpatialCIMIS_data.csv') #this has 86,600,954 cells
  cimis_data2 <- cbind(cimis_data[ ,1:5], round(cimis_data[ ,6:ncol(cimis_data)], 3)) #cimis_data['cell_number'] preserves data.frame class
  write.csv(cimis_data2, paste0('SpatialCIMIS_', varname, '_rounded.csv'), row.names=F)
}
SpCIMISExtract('U2')
SpCIMISExtract('minRH')
#parallel execution with foreach
library(foreach)
library(doSNOW)
cl<-makeCluster(2, type = 'SOCK') #change the number to your desired number of CPU cores  
clusterExport(cl, list=c("cellsofinterestDir", 'spatialCIMISdir', 'SpCIMISExtract'))
clusterCall(cl, function() library(raster))
registerDoSNOW(cl)
foreach(i=1:2) %dopar% {  
  varname <- c('U2', 'minRH')
  SpCIMISExtract(varname = varname[i])
}
stopCluster(cl)
#explore other datasets in the Spatial CIMIS database
setwd(californiaDir)
california <- shapefile("california_CA_TA.shp")
i <- 1
day <- format.Date(datesequence[i], '%d')
mnth <- format.Date(datesequence[i], '%m')
yr <- format.Date(datesequence[i], '%Y')
fname <- 'U2.asc.gz'
url_download <- paste0(base_url, yr, '/', mnth, '/', day, '/', fname)
setwd(file.path(spatialCIMISdir, 'testing'))
download.file(url_download, destfile = fname, quiet = TRUE, mode = 'wb')
cimis_table <- read.table(fname, skip=6, na.strings="-9999") 
#this two step operation is much faster than the "for loop" solution
cimis_table <- t(cimis_table)
cimis_values <- as.vector(cimis_table)
cimis_raster <- raster(ncol=510, nrow=560, xmx=610000, xmn=-410000, ymn=-660000, ymx=460000, crs=ca_ta, vals=cimis_values)
summary(cimis_raster)
plot(cimis_raster)
plot(california, add=T)
writeRaster(cimis_raster, filename = paste0(fname, yr, mnth, day, '.tif'), format='GTiff', overwrite=TRUE) #each Gtiff will be 1092 KB
file.remove(fname)
#'K.asc.gz' is on a 0-1 scale (#clear sky factor)
#'Rnl.asc.gz' is -8.66 - 1.26 scale for 1/1/2016 #net long wave radiation
#'Rs.asc.gz' is 1.03-14.05 scale for 1/1/2016 #solar radiation
#'Rso.asc.gz' is 8.48-14.24 scale for 1/1/2016 #clear sky solar radiation
#'Tdew.asc.gz' is -28.5 - 4.9 scale for 1/1/2016 #dew point temperature at 1.5m
#'Tn.asc.gz' is -36.2 - 18.3 scale for 1/1/2016 #daily minimum air temperature at 1.5m
#'Tx.asc.gz' is -15.8 - 22.8 scale for 1/1/2016 #daily maximum air temperature at 1.5m
#'U2.asc.gz" is 0 - 3.46 m/s #wind speed at 2m

#as noted in first 6 rows of table of raw data in "ETO.asc.gz"
#              V1      V2
#1           ncols     510
#2           nrows     560
#3       xllcorner -410000
#4       yllcorner -660000
#5        cellsize    2000
#6    NODATA_value   -9999

showP4(showWKT("+init=epsg:3310"))
showEPSG(proj4string(california)) #can't find it this way
proj4string(cimis_raster) #NAD83 datum is implied by 
projInfo(type='datum')