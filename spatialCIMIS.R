library(raster)
library(rgdal)
library(XML)
library(httr)
#NOTE: 1/1/2003-2/19/2003 and 3/7/2003-3/9/2003 are missing
californiaDir <- 'C:/Users/smdevine/Desktop/SpatialData/CA_counties/government_units'
spatialCIMISdir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/SpatialCIMIS'
ca_ta <- showP4(showWKT("+init=epsg:3310"))
startyear <- '2017'
endyear <- '2017'
startdate <- strptime(paste0("1/1/", startyear), '%m/%d/%Y')
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
  download.file(url_download, destfile = "ETO.asc.gz", quiet = TRUE, mode = 'wb')
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
