library(raster)
zonesDir <- 'C:/Users/smdevine/Desktop/SpatialData/ecozones/ca_eco_l3/AEA_projection'
modelScaffoldDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/model_scaffold/run_model/Aug2017'
SepmodelScaffoldDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/model_scaffold/run_model/Sep2017'
setwd(file.path(resultsDir, 'data.frames/Aug2017'))
model_points <- read.csv('mukeys_cropcodes_climatecodes_AEA.csv')
model_points_sp <- model_points
coordinates(model_points_sp) <- c("longitude_AEA", "latitude_AEA")
proj4string(model_points_sp) <- '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs' #this is Albers Equal Area coordinates
setwd(zonesDir)
list.files()
ecozones_l3 <- shapefile('ca_eco_l3_AEA.shp')
ecozones_l3 <- spTransform(ecozones_l3, crs(model_points_sp)) #this does not really change the projection of the underlying data; it was already re-projected in ArcGIS
grape_points_sp <- model_points_sp[which(model_points_sp$crop_code==69),]
zones <- over(grape_points_sp, ecozones_l3) 
dim(zones) #same dim as grape_points_sp
head(zones)
summary(as.factor(zones$US_L3NAME))
#Cascades    Central California Foothills and Coastal Mountains 
#229         1019756 
#Central California Valley      Coast Range 
#3158597                        1470 
#Klamath Mountains/California High North Coast Range    Mojave Basin and Range 
#1550                                                   14 
#Sierra Nevada                             Sonoran Basin and Range 
#131                                       44128 
#Southern California Mountains             Southern California/Northern Baja Coast 
#137                                       1134
grape.zones <- zones$US_L3NAME
summary(as.factor(grape.zones)) #4665 Cropscape grape cells lie outside of three remaining ecozones: (1) Central California Foothills and Coastal Mountains (2) Central California Valley and (3) Sonoran Basin and Range
grape.zones <- as.data.frame(grape.zones, stringsAsFactors=FALSE)
grape.zones$grape.zones[which(grape.zones$grape.zones!='Central California Foothills and Coastal Mountains' & grape.zones$grape.zones!='Central California Valley' & grape.zones$grape.zones!='Sonoran Basin and Range')] <- 'Not of interest'
summary(as.factor(grape.zones$grape.zones))
grape.zones$unique_model_code <- grape_points_sp$unique_model_code
unique.codes <- unique(grape.zones$unique_model_code)
unique.grape.zones <- grape.zones$grape.zones[match(unique.codes, grape.zones$unique_model_code)]
grape.zones.codes <- data.frame(unique_model_code=unique.codes, grape.zone=unique.grape.zones)
summary(as.factor(grape.zones.codes$grape.zone))
setwd(SepmodelScaffoldDir)
write.csv(grape.zones.codes, 'grape_zones_codes.csv', row.names = FALSE)

#check for discrepancies when adding zones to points
grape_points_sp$zone <- grape.zones.codes$grape.zone[match(grape_points_sp$unique_model_code, grape.zones.codes$unique_model_code)]
summary(as.factor(grape_points_sp$zone))
#Central California Foothills and Coastal Mountains  Central California Valley 
#1019732                                             3158573 
#Not of interest                            Sonoran Basin and Range 
#4713                                              44128 
#CV discrepancy
3158597-3158573 #24 cells
#foothills and coastal mountains discrepancy
1019756-1019732 #24 cells
#no discrepancy in Sonoran Basin and Range