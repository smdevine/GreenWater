#initial work to preprocess soil map units to a raster aligned with Cropscape was done on O'Geen lab spatial analysis desktop in ArcMap and R
#ls() #lists objects in the working session
library(raster)
library(rgdal)
options(digits = 22, scipen = 999)
#library(SpaDES)
#rasterOptions(chunksize = 2e+07, maxmemory = 2e+08, progress='window')
rasterOptions(progress = 'window')
#memory.limit(size = 10000)
mainDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion'
cropscape <- file.path(mainDir, 'CA_cropscape2015')
mu_dir <- file.path(mainDir, 'soils_data/spatial')
mu_data_dir <- file.path(mainDir, 'soils_data/results/paw_check')
results <- file.path(mainDir, 'results')
cropscape_results <- file.path(results, 'rasters/cropscape')
#cropscape_export <- 'E:/CA_cropscape2015/tiles'
soil_results <- file.path(results, 'rasters/soils')
# results_max <- file.path(soil_results, 'max')
# results_min <- file.path(soil_results, 'min')
# results_wtdavg <- file.path(soil_results, 'wtd_avg')
# setwd(mu_dir)
# mu_raster <- raster('farmland_mu_albers.tif')
setwd(cropscape)
cropscape_raster <- raster('cropscape_masked_ag.tif')
# names(cropscape_raster) <- 'cropscape'
setwd(cropscape)
cropscape_legend <- read.csv('cropscape_legend.txt', stringsAsFactors = FALSE)
alfalfa <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Alfalfa']
grapes <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Grapes']
almonds <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Almonds']
walnuts <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Walnuts']

#split cropscape raster into four tiles and then select crops of interest for masking soil rasters
splitRaster(cropscape_raster, 2, 2, path=file.path(cropscape_results, 'main_tiles'))
setwd(file.path(cropscape_results, 'main_tiles'))
tiles <- list.files(file.path(cropscape_results, 'main_tiles'), pattern = glob2rx('*.grd'))
crops <- c('alfalfa', 'grapes', 'almonds', 'walnuts')
crop_codes <- c(alfalfa, grapes, almonds, walnuts)
for (j in 2:4) {
  for (i in 1:4) {
    setwd(file.path(cropscape_results, 'main_tiles'))
    tile <- raster(tiles[i])
    tile[tile$cropscape!=crop_codes[j]] <- NA #replace 1 with j
    setwd(file.path(cropscape_export, crops[j])) #replace 1 with j
    writeRaster(tile, paste(crops[j], '_tile', i, '.tif', sep = ''), format='GTiff') #replace 1 with j
    removeTmpFiles()
  }
}
#it was realized that saving the files as grd files after this process was taking 1.6 GB per file! so, grd files were read back in and saved as tif files

#save main tiles as tif files
setwd(file.path(cropscape_results, 'main_tiles'))
tile <- raster(tiles[4])
setwd(file.path(cropscape_results, 'main_tiles/GTiff'))
writeRaster(tile, 'cropscape_tile4.tif', format='GTiff')

#save masked crop tiles as tif files
crops <- c('alfalfa', 'grapes', 'almonds', 'walnuts')
crop_codes <- c(alfalfa, grapes, almonds, walnuts)
for (j in 1:4) {
  setwd(file.path(cropscape_export, crops[j])) #replace 1 with j
  tiles <- list.files(pattern = glob2rx('*.grd'))
  for (i in 1:4) {
    tile <- raster(tiles[i])
    setwd(file.path(cropscape_export, crops[j])) #replace 1 with j
    writeRaster(tile, paste(crops[j], '_tile', i, '.tif', sep = ''), format='GTiff') #replace 1 with j
    removeTmpFiles()
  }
}

#save study_crop tiles as tif files
setwd(file.path(cropscape_export, 'study_crops'))
tiles <- list.files(pattern = glob2rx('*.grd'))
tile <- raster(tiles[4])
writeRaster(tile, 'study_crops_tile4.tif', format='GTiff')

#split soils mu raster into 4 tiles before further processing
splitRaster(mu_raster, 2, 2, path=file.path(soil_results, 'main_tiles'))

#save soils mu raster tiles as tif files to save memory
setwd(file.path(soil_results, 'main_tiles'))
tiles <- list.files(pattern = glob2rx('*.grd'))
tile <- raster(tiles[4])
writeRaster(tile, 'farmland_mu_albers_tile4.tif', format='GTiff')

#mask each soil mu tile by crop and then run subs to add additional layers to the stack for max, min, and wtdavg
#read in soils data
setwd(mu_data_dir)
mu_data <- read.csv('mu_aggregated_data_CA_ag.csv', stringsAsFactors = FALSE)
crops <- c('alfalfa', 'grapes', 'almonds', 'walnuts')
for (j in 2:4) {
  setwd(file.path(cropscape_results, crops[j])) #replace 1 with j
  crop_tiles <- list.files(pattern = glob2rx('*.tif'))
  for (i in 1:4) {
    setwd(file.path(cropscape_results, crops[j]))
    crop_tile <- raster(crop_tiles[i]) #change back to i
    setwd(file.path(soil_results, 'main_tiles')) 
    soil_tiles <- list.files(pattern = glob2rx('*.tif'))
    soil_tile <- raster(soil_tiles[i]) #change back to i
    setwd(file.path(soil_results, crops[j])) #change back to j
    soil_tile <- mask(soil_tile, crop_tile, filename=paste('mu_', crops[j], '_tile', i, '.tif', sep = ''), format='GTiff')
    subs(soil_tile, mu_data, by='mukey', which=paste('AD_', crops[j], '_cmH2O_min', sep = ''), filename=paste('AD_', crops[j], '_cmH20_min_tile', i, '.tif', sep = ''), format='GTiff')
    subs(soil_tile, mu_data, by='mukey', which=paste('AD_', crops[j], '_cmH2O_max', sep = ''), filename=paste('AD_', crops[j], '_cmH20_max_tile', i, '.tif', sep = ''), format='GTiff')
    subs(soil_tile, mu_data, by='mukey', which=paste('AD_', crops[j], '_cmH2O_wtdavg', sep = ''), filename=paste('AD_', crops[j], '_cmH20_wtdavg_tile', i, '.tif', sep = ''), format='GTiff')
    #removeTmpFiles(h=0.0001)
    #showTmpFiles()
  }
}

#test of merging AD results for each crop by tile; in each folder: max is 1-4; min is 5-8; wtd_avg is 9-12; mukeys are 13-16; for some reason NAvalue has become -1.7e+308
crops <- c('alfalfa', 'grapes', 'almonds', 'walnuts')
tile_num <- 3
tile_list <- vector("list", length = 4)
var_type <- 'wtdavg'
adjuster <- 8
for (j in 1:4) {
  setwd(file.path(soil_results, crops[j]))
  AD_tiles <- list.files(pattern = glob2rx('*.tif'))
  AD_tile <- raster(AD_tiles[tile_num+adjuster])
  AD_tile
  tile_list[j] <- AD_tile
}
AD_all <- stack(tile_list) #doesn't save time to convert this to a brick next before summing
AD_sum <- sum(AD_all, na.rm = TRUE)
setwd(file.path(results, paste('rasters/final/', var_type, sep = '')))
writeRaster(AD_sum, paste('AD_', var_type, '_cmH2O_tile', tile_num, '.tif', sep=''), format='GTiff')
showTmpFiles()
removeTmpFiles(h=0.0001)
showTmpFiles()
setwd('C:/Users/smdevine/Desktop/SpatialData/FRAP')
FRAP <- raster('frap_veg.bil')
CA_TA <- crs(FRAP)
test <- projectRaster(AD_sum, crs = CA_TA)

#read-in results rasters and sum up AD values by setting var_type[j]
var_type <- c('max', 'min', 'wtdavg')
setwd(file.path(results, paste('rasters/final/', var_type[3], sep = '')))
raster_fnames <- list.files(pattern = glob2rx('*.tif'))
tile1 <- raster(raster_fnames[1])
tile1_af <- tile1*(9/1233.489)
cellStats(tile1_af, sum, na.rm=TRUE)
#[1] 427546.3 max AD
#[1] 231588.3 min AD
#[1] 347605 wtd avg AD
tile2 <- raster(raster_fnames[2])
tile2_af <- tile2*(9/1233.489)
cellStats(tile2_af, sum, na.rm=TRUE)
#[1] 641074.2 max AD
#[1] 344717.3 min AD
#[1] 519023.5 wtd avg AD
tile3 <- raster(raster_fnames[3])
tile3_af <- tile3*(9/1233.489)
cellStats(tile3_af, sum, na.rm=TRUE)
#[1] 102961.6 max AD
#[1] 57689.06 min AD
#[1] 77297.62 wtd avg AD
tile4 <- raster(raster_fnames[4])
tile4_af <- tile4*(9/1233.489)
cellStats(tile4_af, sum, na.rm=TRUE)
#[1] 14778.01 max AD
#[1] 7096.104 min AD
# wtd avg AD
showTmpFiles()
removeTmpFiles(h=0.0001)

#read-in cropfiles to calculate some stats
setwd(file.path(cropscape_results, 'study_crops'))
cropsfnames <- list.files(pattern=glob2rx('*.tif'))
crops_tile1 <- raster(cropsfnames[1])
crops_tile1[!is.na(crops_tile1$study_crops_tile1)] <- 1 #change to 1 if not NA
ha_tile1 <- cellStats(crops_tile1, sum, na.rm=TRUE)*900/10000 #number of hectares, because each cell is 30 x 30 m
ha_tile1*2.47105
#1,771,685 acres in tile 1
crops_tile2 <- raster(cropsfnames[2])
crops_tile2[!is.na(crops_tile2$study_crops_tile2)] <- 1
ha_tile2 <- cellStats(crops_tile2, sum, na.rm=TRUE)*900/10000
ha_tile2*2.47105
#1,809,155 acres in tile 2
crops_tile3 <- raster(cropsfnames[3])
crops_tile3[!is.na(crops_tile3$study_crops_tile3)] <- 1
ha_tile3 <- cellStats(crops_tile3, sum, na.rm=TRUE)*900/10000
ha_tile3*2.47105
#221098.2 acres in tile 3
crops_tile4 <- raster(cropsfnames[4])
crops_tile4[!is.na(crops_tile4$study_crops_tile4)] <- 1
ha_tile4 <- cellStats(crops_tile4, sum, na.rm=TRUE)*900/10000
ha_tile4*2.47105
#27829.56 acres in tile 4

#read in crops rasters by crop to calculate acreages
cropname <- 'walnuts'
cropcode <- walnuts
setwd(file.path(cropscape_results, cropname))
crop_fnames <- list.files(pattern=glob2rx('*.tif'))
crop_tile1 <- raster(crop_fnames[1])
crop_ha_tile1 <- cellStats(crop_tile1, sum, na.rm=TRUE)/cropcode*(900/10000)
crop_ha_tile1*2.47105
crop_tile2 <- raster(crop_fnames[2])
crop_ha_tile2 <- cellStats(crop_tile2, sum, na.rm=TRUE)/cropcode*(900/10000)
crop_ha_tile2*2.47105
crop_tile3 <- raster(crop_fnames[3])
crop_ha_tile3 <- cellStats(crop_tile3, sum, na.rm=TRUE)/cropcode*(900/10000)
crop_ha_tile3*2.47105
crop_tile4 <- raster(crop_fnames[4])
crop_ha_tile4 <- cellStats(crop_tile4, sum, na.rm=TRUE)/cropcode*(900/10000)
crop_ha_tile4*2.47105

