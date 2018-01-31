#on 8/18, merged relevant scrip from 'merge_cropscape_soils_data.R' so model_scaffold
#can work as a standalone to set up the model parameter matrix
#on 1/23, projected model.codes.Aug2017.tif to California Teale Albers for plotting purposes
#CA Teale Albers reference: http://www.spatialreference.org/ref/sr-org/10/
library(raster)
library(rgdal)
options(digits = 22, scipen = 999)
rasterOptions(progress = 'window')
mainDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion'
cropscape <- file.path(mainDir, 'CA_cropscape2015')
mu_dir <- file.path(mainDir, 'soils_data/spatial')
mu_data_dir <- file.path(mainDir, 'soils_data/results/paw_check')
results <- file.path(mainDir, 'results')
cropscape_results <- file.path(results, 'rasters/cropscape')
soil_results <- file.path(results, 'rasters/soils')
californiaDir <- 'C:/Users/smdevine/Desktop/SpatialData/CA_counties/government_units'
spatialCIMIS <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/SpatialCIMIS'
PRISMDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/PRISMdaily'
modelscaffoldDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/model_scaffold/run_model/Oct2017' #latest model scaffold directory
###this follows some raster processing in merge_cropscape_soils_data.R
###purpose of this script is to determine number of unique soil and crop scenarios and construct a model matrix that will be more memory efficient than a raster based model, since the majority of the cells in the raster are not of interest
setwd(mu_dir)
mu_raster <- raster('farmland_mu_albers.tif')
#split soils mu raster into 4 tiles before further processing
if (!dir.exists(file.path(soil_results, 'main_tiles'))) {
  dir.create(file.path(soil_results, 'main_tiles'))
}
splitRaster(mu_raster, 2, 2, path=file.path(soil_results, 'main_tiles')) #these
#files were subsequently deleted after converting to tif

#save soils mu raster tiles as tif files to save memory

setwd(file.path(soil_results, 'main_tiles'))
tiles <- list.files(pattern = glob2rx('*.grd'))
for (i in 1:4) {
  tile <- raster(tiles[i])
  writeRaster(tile, paste0('farmland_mu_albers_tile', as.character(i), '.tif'), format='GTiff')
}

#isolate crops of interest
setwd(cropscape)
cropscape_raster <- raster('cropscape_masked_ag.tif')
cropscape_legend <- read.csv('cropscape_legend.txt', stringsAsFactors = FALSE)
alfalfa <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Alfalfa']
grapes <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Grapes']
almonds <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Almonds']
walnuts <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Walnuts']
pistachios <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Pistachios']
tomatoes <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Tomatoes']
wheat <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Winter Wheat']

#split cropscape raster into four tiles and then select crops of interest for masking soil rasters
splitRaster(cropscape_raster, 2, 2, path=file.path(cropscape_results, 'main_tiles'))
setwd(file.path(cropscape_results, 'main_tiles'))
tiles <- list.files(file.path(cropscape_results, 'main_tiles'), pattern = glob2rx('*.grd'))
crops <- c('alfalfa', 'grapes', 'almonds', 'walnuts', 'pistachios', 'tomatoes', 'wheat')
crop_codes <- c(alfalfa, grapes, almonds, walnuts, pistachios, tomatoes, wheat)
for (i in 1:4) { #refers to 4 tiles
  setwd(file.path(cropscape_results, 'main_tiles'))
  study.crops.raster <- raster(tiles[i])
  study.crops.raster[!(study.crops.raster$cropscape_masked_ag %in% crop_codes)] <- NA
  if (!dir.exists(file.path(cropscape_results, 'study_crops'))) {
    dir.create(file.path(cropscape_results, 'study_crops'))
  }
  setwd(file.path(cropscape_results, 'study_crops'))
  writeRaster(study.crops.raster, paste0('study_crops_tile', as.character(i), '.tif'), format='GTiff')
}

#first part saves new mukey rasters that are masked by crops of interest
setwd(file.path(cropscape_results, 'study_crops')) #replace 1 with j
crop_tiles <- list.files(pattern = glob2rx('*.tif'))
setwd(file.path(soil_results, 'main_tiles')) 
soil_tiles <- list.files(pattern = glob2rx('*.tif'))
if (!dir.exists(file.path(soil_results, 'study_crops'))) {
  dir.create(file.path(soil_results, 'study_crops'))
}
for (i in 1:4) {
  setwd(file.path(cropscape_results, 'study_crops'))
  crop_tile <- raster(crop_tiles[i]) #change back to i
  setwd(file.path(soil_results, 'main_tiles'))
  soil_tile <- raster(soil_tiles[i]) #change back to i
  setwd(file.path(soil_results, 'study_crops')) #change back to j
  soil_tile <- mask(soil_tile, crop_tile, filename=paste('mu_cropmasked_tile', i, '.tif', sep = ''), format='GTiff')
  removeTmpFiles(h=0.0001)
  #showTmpFiles()
}
setwd(file.path(soil_results, 'study_crops'))
soil_cropmasked_tiles <- list.files(pattern = glob2rx('*.tif'))

#check acreages of these tiles to confirm that they match crop acreages
# soil_tile <- raster(soil_cropmasked_tiles[i])
# soil_temp <- soil_tile
# soil_temp[!is.na(soil_temp$mu_cropmasked_tile3)] <- 1
# cellStats(soil_temp, sum, na.rm=TRUE)

#plot the study crops if so desired; need to work on raster legend
setwd(file.path(cropscape_results, 'alfalfa'))
crop_tiles <- list.files(pattern = glob2rx('*.tif'))
crop_tile1 <- raster(crop_tiles[1])
extent1 <- extent(crop_tile1)
crop_tile2 <- raster(crop_tiles[2])
extent2 <- extent(crop_tile2)
crop_tile3 <- raster(crop_tiles[3])
extent3 <- extent(crop_tile3)
crop_tile4 <- raster(crop_tiles[4])
extent4 <- extent(crop_tile4)

setwd(californiaDir)
california <- shapefile("california_CA_TA.shp")
california_aea <- spTransform(california, crs(crop_tile1))
plot(california_aea)
plot(crop_tile1, add=T, legend=F, col='green')
plot(crop_tile2, add=T, legend=F, col='green')
plot(crop_tile3, add=T, legend=F, col='green')
plot(crop_tile4, add=T, legend=F, col='green')
# plot(extent1, add=T)
# plot(extent2, add=T)
# plot(extent3, add=T)
# plot(extent4, add=T)

#create a spatial points dataframe out of the raster cells of interest with mukeys, cropcodes, and coordinates
setwd(californiaDir)
california <- shapefile("california_CA_TA.shp")
setwd(file.path(soil_results, 'study_crops/Aug2017'))
soil_cropmasked_tiles <- list.files(pattern = glob2rx('*.tif'))
mu_t1 <- raster('mu_cropmasked_tile1.tif')
mu_t2 <- raster('mu_cropmasked_tile2.tif')
mu_t3 <- raster('mu_cropmasked_tile3.tif')
mu_t4 <- raster('mu_cropmasked_tile4.tif')
mu_all <- mosaic(mu_t1, mu_t2, mu_t3, mu_t4, fun=min)
setwd(file.path(soil_results, 'study_crops/Aug2017/merged_rasters'))
writeRaster(mu_all, 'mu_cropmasked_all.tif')
mu_all <- raster('mu_cropmasked_all.tif')

setwd(file.path(cropscape_results, 'study_crops'))
crop_tiles <- list.files(pattern = glob2rx('*.tif'))
for (i in 1:4) {
  setwd(file.path(soil_results, 'study_crops'))
  soil_tile <- raster(soil_cropmasked_tiles[i])
  setwd(file.path(cropscape_results, 'study_crops'))
  crop_tile  <- raster(crop_tiles[i])
  cells <- Which(!is.na(soil_tile), cells=TRUE)
  coordinates <- xyFromCell(soil_tile, cells) #get coordinates of the center of raster cells
  mukeys <- values(soil_tile)
  mukeys <- mukeys[!is.na(mukeys)]
  crop_codes <- values(crop_tile)
  crop_codes <- crop_codes[!is.na(crop_codes)]
  mukey_crop_df <- data.frame(mukey=mukeys, crop_code=crop_codes, longitude_AEA=coordinates[,1], latitude_AEA=coordinates[,2])
  if (i==1) {
    results_df <- mukey_crop_df
    next
  }
  results_df <- rbind(results_df, mukey_crop_df)
}
#could possibly just have used as.data.frame(na.rm=TRUE) to simplify much of above
setwd(file.path(results, 'data.frames', 'Aug2017'))
write.csv(results_df, 'mukeys_cropcodes_AEA.csv', row.names = FALSE)
#read in this csv file
results_df <- read.csv('mukeys_cropcodes_AEA.csv')
head(results_df)
lapply(results_df, class) #get the classes of each df column
results_df$mukey_cropcode <- as.numeric(paste0(results_df$mukey, results_df$crop_code))
nrow(results_df)
#17,220,605 rows in the data.frame (these are the number of 30 x 30 m raster cells of interest)
#now, 21,679,100
length(unique(results_df$mukey))
#5,225 unique mukeys
#now, 5,620
length(unique(results_df$mukey_cropcode))
#13,309 unique crop x mukey combinations
#now, 20,605
mukeys_cropcodes_AEA <- results_df
coordinates(mukeys_cropcodes_AEA) <- c("longitude_AEA", "latitude_AEA")
proj4string(mukeys_cropcodes_AEA) <- '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs' #this is Albers Equal Area coordinates
results_CA_TA <- spTransform(mukeys_cropcodes_AEA, crs(california)) #California Teale Albers coordinates for getting SpatialCimis cell numbers
results_df_CA_TA <- as.data.frame(results_CA_TA)
head(results_df_CA_TA)
colnames(results_df_CA_TA)[4:5] <- c('longitude_CA_TA', 'latitude_CA_TA')
setwd(file.path(results, 'data.frames'))
write.csv(results_df_CA_TA, 'mukeys_cropcodes_CA_TA.csv', row.names = FALSE)
mukeys_cropcodes_longlatNAD83 <- spTransform(mukeys_cropcodes_AEA, "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0") #as defined from a prism raster
mukeys_cropcodes_longlatNAD83_df <- as.data.frame(mukeys_cropcodes_longlatNAD83)
colnames(mukeys_cropcodes_longlatNAD83_df)[4:5] <- c('longitude_NAD83', 'latitude_NAD83')
#setwd('C:/Users/smdevine/Desktop/Allowable_Depletion/results/shapefiles')
#shapefile(mukeys_cropcodes_longlatNAD83, 'mukeys_cropcodes_longlatNAD83.shp')
#mukeys_cropcodes_longlatNAD83 <- shapefile('mukeys_cropcodes_longlatNAD83.shp') #this is much slower than reading in the csv file
setwd(file.path(results, 'data.frames'))
write.csv(mukeys_cropcodes_longlatNAD83_df, 'mukeys_cropcodes_longlatNAD83.csv')

#read in CA_TA projection now and get cell numbers from
setwd(file.path(results, 'data.frames', 'Aug2017'))
mukeys_cropcodes_CA_TA <- read.csv('mukeys_cropcodes_CA_TA.csv')
coordinates(mukeys_cropcodes_CA_TA) <- c('longitude_CA_TA', 'latitude_CA_TA')
crs(mukeys_cropcodes_CA_TA) <- '+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0' #this is California Teale Albers, as defined by California state boundary shapefile
setwd(file.path(spatialCIMIS, 'U2/2004'))
spatialCIMIS <- raster('U220040101.tif')
CIMIScellnumbers <- as.integer(cellFromXY(object=spatialCIMIS, xy=mukeys_cropcodes_CA_TA))
summary(CIMIScellnumbers)
CIMIScellunique <- unique(CIMIScellnumbers) #17,341 CIMIS cells need sampling; 18,633
setwd(file.path(mainDir, 'model_scaffold/run_model/Aug2017'))
CIMIScellunique_df <- data.frame(CIMIS_cells=CIMIScellunique)
write.csv(CIMIScellunique_df, 'CIMIS_cells_unique.csv', row.names = F)
#get cell numbers from PRISM for each point of interest
setwd(file.path(PRISMDir, '2003/PRISM_ppt_stable_4kmD2_20031001_bil'))
prism <- raster('PRISM_ppt_stable_4kmD2_20031001_bil.bil')
PRISMcellnumbers <- as.integer(cellFromXY(object = prism, xy = mukeys_cropcodes_longlatNAD83))
summary(PRISMcellnumbers)
PRISMcellunique <- unique(PRISMcellnumbers) #5338 PRISM cells need sampling; 5701 need sampling
PRISMcellunique_df <- data.frame(PRISM_cells=PRISMcellunique) 
setwd(file.path(mainDir, 'model_scaffold/run_model/Aug2017'))
write.csv(PRISMcellunique_df, 'PRISM_cells_unique.csv', row.names = F)

#add PRISM and CIMIS raster cell numbers to find unique combinations of soil, crop, and climate
lapply(results_df, class)
results_df$PRISMcellnumber <- PRISMcellnumbers
results_df$CIMIScellnumber <- CIMIScellnumbers
results_df$model_code <- paste0(results_df$mukey, results_df$crop_code, results_df$PRISMcellnumber, results_df$CIMIScellnumber)
#results_df$model_code <- as.numeric(results_df$model_code) #set options(digits=22, scipen=999) before writing this to csv.  However, excel will round all number after 15 digits if opening the csv.  DANGER! long integers are being changed somehow, which creates the impression there are less than 200k unique combinations!  conclusion is that these should be kept in character class to preserve integrity
model_codes <- unique(results_df$model_code) #242,714 unique combinations of soil, crop, and climate out of 17,220,605 where soil is map unit; now, 341,207 unique combinations
unique_model_code <- 100001:(100000+length(model_codes)) #create a dummy code for the model matrix
model_codes_matrix <- data.frame(model_code=model_codes, unique_model_code=unique_model_code)
final_results_df <- merge(results_df, model_codes_matrix, by='model_code')
head(final_results_df, 20) #check that codes maintained integrity
#final_results_df$model_code <- NULL #only used this for writing csv
setwd(file.path(results, 'data.frames'))
write.csv(final_results_df, 'mukeys_cropcodes_climatecodes_AEA.csv', row.names = FALSE)
cells_to_model <- match(unique_model_code, final_results_df$unique_model_code)
model_scaffold <- final_results_df[cells_to_model, ]
head(model_scaffold)
model_scaffold$model_code <- NULL
model_scaffold$mukey_cropcode <- NULL
#model_scaffold$full_matrix_rownum <- rownames(model_scaffold)
#model_scaffold$longitude_AEA <- NULL
#model_scaffold$latitude_AEA <- NULL
setwd(file.path(mainDir, 'model_scaffold'))
write.csv(model_scaffold, 'model_scaffold_codes8.17.17.csv', row.names = F) #this destroys the model code numbers when read back in, so they were deleted to avoid future confusion. they can simply be re-created by paste0(results_df$mukey, results_df$crop_code, results_df$PRISMcellnumber, results_df$CIMIScellnumber) in this order
model_scaffold <- read.csv('model_scaffold_codes8.17.17.csv')

#rasterization testing
setwd(file.path(results, 'data.frames'))
model_points <- read.csv('mukeys_cropcodes_climatecodes_AEA.csv')
model_points_sp <- model_points
coordinates(model_points_sp) <- c("longitude_AEA", "latitude_AEA")
proj4string(model_points_sp) <- '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs' #this is Albers Equal Area coordinates)
latmax <- ymax(model_points_sp)
latmin <- ymin(model_points_sp)
lonmax <- xmax(model_points_sp)
lonmin <- xmin(model_points_sp)
raster_mukeys <- raster(xmn=(lonmin-15), xmx=(lonmax+15), ymn=(latmin-15), ymx=(latmax+15), resolution=30, crs='+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs') #add or subtract 15 because coordinates are centers of the raster cells from which these were derived
raster_mukeys <- rasterize(x=model_points_sp, y=raster_mukeys, field='mukey') #appears to be working but will take 2-3 hours

#lines 252-278 was originally in "summer2017analysis.R" but makes more sense to be here
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

#create matrix of cell numbers of interest and model codes
cell_numbers_of_interest <- Which(!is.na(raster.model.codes), cells = TRUE)
unique_model_codes <- raster.model.codes[cell_numbers_of_interest]
cellnums_to_modelcode <- cbind(cell_numbers_of_interest, unique_model_codes)
length(cellnums_to_modelcode)
write.csv(file.path(modelscaffoldDir, 'cellnumbers_to_modelcodes.csv'), 'cellnumbers_to_modelcodes.csv', row.names = FALSE)
cell_numbers_to_codes <- read.csv(file.path(modelscaffoldDir, 'cellnumbers_to_modelcodes.csv'), stringsAsFactors = FALSE)
lapply(cell_numbers_to_codes, class)
length(unique(cell_numbers_to_codes$unique_model_codes)) #341,207 unique codes

#project model.codes.Aug2017.tif to California Teale Albers for plotting purposes
model.codes.AEA <- raster(file.path(modelscaffoldDir, 'model.codes.Aug2017.tif'))
model.codes.CA.TA <- projectRaster(from=model.codes.AEA, res = 30, crs = crs('+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs'), method = 'ngb', filename = file.path(modelscaffoldDir, 'model.codes.Aug2017.CA.TA.v2.tif'), progress='window') #ngb stands for nearest neighbor; appropriate for this categorical data
plot(model.codes.CA.TA)
writeRaster(model.codes.CA.TA, filename = file.path(modelscaffoldDir, 'model.codes.Aug2017.CA.TA.tif'))

#test that we still have the same number of cell numbers of interest after projection
#get matrix of cell numbers of interest
cell_numbers_of_interest.CA.TA <- Which(!is.na(model.codes.CA.TA), cells = TRUE)
unique_model_codes <- model.codes.CA.TA[cell_numbers_of_interest.CA.TA]
cellnums_to_modelcode.CA.TA <- cbind(cell_numbers_of_interest.CA.TA, unique_model_codes)
#setwd('C:/Users/smdevine/Desktop/Allowable_Depletion/model_scaffold/run_model/Oct2017')
write.csv(cellnums_to_modelcode.CA.TA, file.path(modelscaffoldDir,'test.v2_cellnumbers_to_modelcodes.CA.TA.csv'), row.names = FALSE)
cellnums_modelcode_CA.TA <- read.csv(file.path(modelscaffoldDir,'test_cellnumbers_to_modelcodes.CA.TA.csv'))
length(unique(cellnums_modelcode_CA.TA$unique_model_codes)) #339,303 unique codes, loss of 1,904 due to projection

#alternative approach to to building raster for California Teale Albers (in-progress 1/30/18)
#follows lines approx. 252-278 above
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