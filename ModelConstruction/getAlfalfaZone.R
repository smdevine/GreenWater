library(raster)
resultsDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/results'
zonesDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/maps'
modelScaffoldDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/model_scaffold/run_model/Aug2017'
setwd(file.path(resultsDir, 'data.frames/Aug2017'))
model_points <- read.csv('mukeys_cropcodes_climatecodes_AEA.csv')
model_points_sp <- model_points
coordinates(model_points_sp) <- c("longitude_AEA", "latitude_AEA")
proj4string(model_points_sp) <- '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs' #this is Albers Equal Area coordinates
setwd(zonesDir)
list.files()
alfalfa.zones <- shapefile('alfalfa.zones.shp')
alfalfa.zones <- spTransform(alfalfa.zones, proj4string(alfalfa_points_sp))
alfalfa_points_sp <- model_points_sp[which(model_points_sp$crop_code==36),]
zones <- over(alfalfa_points_sp, alfalfa.zones)
alfalfa_points_sp$zone <- zones$name
summary(as.factor(alfalfa_points_sp$zone))
unique.codes <- unique(alfalfa_points_sp$unique_model_code)
alfalfa_points_sp_df <- as.data.frame(alfalfa_points_sp)
alfalfa_codes_zones <- alfalfa_points_sp_df[match(unique.codes, alfalfa_points_sp_df$unique_model_code), c('unique_model_code', 'zone')]
setwd(modelScaffoldDir)
write.csv(alfalfa_codes_zones, 'alfalfa_zones_codes.csv', row.names = FALSE)

#Central Valley Imperial Valley   Intermountain         NA's 
    #3218637          906837          782290          108635