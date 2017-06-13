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
endyear <- '2003'
startdate <- strptime(paste0("7/8/", startyear), '%m/%d/%Y')
enddate <- strptime(paste0("12/31/", endyear), '%m/%d/%Y')
datesequence <- seq.Date(from=as.Date(startdate), to=as.Date(enddate), by='day')
base_url <- 'http://cimis.casil.ucdavis.edu/cimis/'
i <- 1
for (i in 1:length(datesequence)) {
  day <- format.Date(datesequence[i], '%d')
  mnth <- format.Date(datesequence[i], '%m')
  yr <- format.Date(datesequence[i], '%Y')
  if (file.exists(file.path(spatialCIMISdir, yr)) == FALSE) {
    dir.create(file.path(spatialCIMISdir, yr))
  }
  url_download <- paste0(base_url, yr, '/', mnth, '/', day, '/', 'ETo.asc.gz')
  setwd(file.path(spatialCIMISdir, yr))
  error_catch <- try(download.file(url_download, destfile = "ETO.asc.gz", quiet = TRUE, mode = 'wb'))
  if (class(error_catch)=='try-error') {
    next
  }
  cimis_table <- read.table("ETo.asc.gz", skip=6, na.strings="-9999")
#this two step operation is much faster than the "for loop" solution
  cimis_table <- t(cimis_table)
  cimis_values <- as.vector(cimis_table)
  cimis_raster <- raster(ncol=510, nrow=560, xmx=610000, xmn=-410000, ymn=-660000, ymx=460000, crs=ca_ta, vals=cimis_values)
  writeRaster(cimis_raster, filename = paste0('ETo_', yr, mnth, day, '.tif'), format='GTiff', overwrite=TRUE) #each Gtiff will be 1092 KB
  file.remove("ETo.asc.gz")
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
