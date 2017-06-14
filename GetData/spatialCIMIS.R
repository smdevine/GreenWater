#see "Daily reference evapotranspiration for California using satellite imagery and weather station measurement interpolation" by Hart et al. 2009
library(raster)
library(rgdal)
library(XML)
library(httr)
#NOTE: 1/1/2003-2/19/2003, 3/7/2003-3/9/2003, 4/9/2003-4/10/2003, 6/15/2003, 6/17/2003, 6/19/2003-6/22/2003, 6/24/2003, and a number more dates before 9/30/2003 are missing. 2004 water year to the present is good to go.
californiaDir <- 'C:/Users/smdevine/Desktop/SpatialData/CA_counties/government_units'
spatialCIMISdir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/SpatialCIMIS'
cellsofinterestDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/results/model_scaffold'
ca_ta <- showP4(showWKT("+init=epsg:3310"))
startyear <- '2003'
endyear <- '2017'
startdate <- strptime(paste0("10/1/", startyear), '%m/%d/%Y')
enddate <- strptime(paste0("06/01/", endyear), '%m/%d/%Y')
datesequence <- seq.Date(from=as.Date(startdate), to=as.Date(enddate), by='day')
base_url <- 'http://cimis.casil.ucdavis.edu/cimis/'
varofinterest <- 'ETO.asc.gz'
varDir <- 'ETo'
i <- 1
for (i in 1:length(datesequence)) {
  day <- format.Date(datesequence[i], '%d')
  mnth <- format.Date(datesequence[i], '%m')
  yr <- format.Date(datesequence[i], '%Y')
  if (file.exists(file.path(spatialCIMISdir, yr)) == FALSE) {
    dir.create(file.path(spatialCIMISdir, yr))
  }
  url_download <- paste0(base_url, yr, '/', mnth, '/', day, '/', 'ETo.asc.gz')
  setwd(file.path(spatialCIMISdir, varDir, yr))
  error_catch <- try(download.file(url_download, destfile = varofinterest, quiet = TRUE, mode = 'wb'))
  if (class(error_catch)=='try-error') {
    next
  }
  cimis_table <- read.table("ETo.asc.gz", skip=6, na.strings="-9999")
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

#get SpatialCIMIS into matrix for cells of interest
setwd(cellsofinterestDir)
list.files()
cellsofinterest <- read.csv("CIMIS_cells_unique.csv")
cellsofinterest <- cellsofinterest[order(cellsofinterest$CIMIS_cells), ]
startyear <- '2003'
endyear <- '2017'
startdate <- strptime(paste0("10/1/", startyear), '%m/%d/%Y')
enddate <- strptime(paste0("6/1/", endyear), '%m/%d/%Y')
datesequence <- seq.Date(from=as.Date(startdate), to=as.Date(enddate), by='day')
datesequence_colnames <- format.Date(datesequence, '%b_%d_%Y')
cimis_data <- as.data.frame(matrix(nrow=length(cellsofinterest), ncol=(length(datesequence)+1)))
colnames(cimis_data) <- c('cell_number', datesequence_colnames)
cimis_data$cell_number <- cellsofinterest
i <- 1
for (i in 1:length(datesequence)) {
  day <- format.Date(datesequence[i], '%d')
  mnth <- format.Date(datesequence[i], '%m')
  yr <- format.Date(datesequence[i], '%Y')
  setwd(file.path(spatialCIMISdir, yr))
  spCIMIS <- raster(paste0('ETo_', yr, mnth, day, '.tif'))
  cimis_data[ ,i+1] <- extract(spCIMIS, cellsofinterest)
  print(i)
}
setwd(cellsofinterestDir)
write.csv(cimis_data, 'SpatialCIMIS_data.csv', row.names = F)
cimis_data <- read.csv('SpatialCIMIS_data.csv') #this has 86,600,954 cells
cimis_data2 <- cbind(cimis_data['cell_number'], round(cimis_data[ ,2:ncol(cimis_data)], 3)) #cimis_data['cell_number'] preserves data.frame class
write.csv(cimis_data2, 'SpatialCIMIS_data_rounded.csv', row.names=F)

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