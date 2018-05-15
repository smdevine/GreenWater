#this is a complete re-working of the model scaffold based on new crops dataset from LandIQ
#combines code from model_scaffold.R, model_consruction.R, getAlfalfaZone.R, getGrapeZone.R into one script
#still maintains dependency on PRISM, SpatialCIMIS, and SSURGO download scripts, as well as SSURGO data aggregration script
#generally the same approach except now the fundamental unit of interest is a polygon (or vector in GIS terminology) as opposed to a raster cell, since the crop dataset is a shapefile
#note that two GIS operations were performed outside of R in ArcMap 10.5 due to rgeos errors
library(raster)
library(rgdal)
library(rgeos)
mainDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion'
spatialCIMIS <- file.path(mainDir,'SpatialCIMIS')
CropsDir <- file.path(mainDir, 'LandIQ.crops')
PRISMDir <- file.path(mainDir, 'PRISMdaily')
SoilMUDir <- file.path(mainDir, 'soils_data/spatial')
modelscaffoldDir <- file.path(mainDir, 'model_scaffold/run_model/Mar2018')
soilcomptempDir <- file.path(mainDir, 'model_scaffold/soil_climate_crop/by_componentMar7.2018')
SoilsDataDir <- file.path(mainDir, 'soils_data/results/spring2018model')
CropZonesDir <- 'C:/Users/smdevine/Desktop/SpatialData/ecozones/ca_eco_l3'

#cropscape codes for historical consistency with new LandIQ crops dataset
cropscape_legend <- read.csv('C:/Users/smdevine/Desktop/Allowable_Depletion/CA_cropscape2015/cropscape_legend.txt', stringsAsFactors = FALSE)
alfalfa_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Alfalfa']
grapes_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Grapes']
almonds_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Almonds']
walnuts_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Walnuts']
pistachios_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Pistachios']

dwr.crops <- shapefile(file.path(CropsDir, 'i15_Crop_Mapping_2014_Final_LandIQonAtlas.shp'))
#from ESRI projection description
#NAD_1983_2011_California_Teale_Albers
#WKID: 6414 Authority: EPSG

#Projection: Albers
#False_Easting: 0.0
#False_Northing: -4000000.0
#Central_Meridian: -120.0
#Standard_Parallel_1: 34.0
#Standard_Parallel_2: 40.5
#Latitude_Of_Origin: 0.0
#Linear Unit: Meter (1.0)

#Geographic Coordinate System: GCS_NAD_1983_2011
#Angular Unit: Degree (0.0174532925199433)
#Prime Meridian: Greenwich (0.0)
#Datum: D_NAD_1983_2011
#Spheroid: GRS_1980
#Semimajor Axis: 6378137.0
#Semiminor Axis: 6356752.314140356
#Inverse Flattening: 298.257222101
crs(dwr.crops)
unique(dwr.crops$Crop2014)
perennials.of.interest <- dwr.crops[dwr.crops$Crop2014 %in% c('Alfalfa and Alfalfa Mixtures', 'Almonds', 'Grapes', 'Pistachios', 'Walnuts'),]
unique(perennials.of.interest$Crop2014)
nrow(perennials.of.interest) #140,819 fields with these five crops
tapply(perennials.of.interest$Acres, perennials.of.interest$Crop2014, sum)
#Alfalfa and Alfalfa Mixtures   Almonds     Grapes   Pistachios  Walnuts 
#930131.1                      1127945.9    904566.3   342769.4  370362.6
shapefile(x=perennials.of.interest, filename=file.path(CropsDir, 'LandIQ.crops.of.interest.shp'))
#rasterize(x=perennials.of.interest, field='Crop2014')
perennials.of.interest <- shapefile(file.path(CropsDir, 'LandIQ.crops.of.interest.shp'))
perennials.of.interest$cropcode <- NA
perennials.of.interest$cropcode[perennials.of.interest$Crop2014=='Alfalfa and Alfalfa Mixtures'] <- alfalfa_code
perennials.of.interest$cropcode[perennials.of.interest$Crop2014=='Almonds'] <- almonds_code
perennials.of.interest$cropcode[perennials.of.interest$Crop2014=='Grapes'] <- grapes_code
perennials.of.interest$cropcode[perennials.of.interest$Crop2014=='Walnuts'] <- walnuts_code
perennials.of.interest$cropcode[perennials.of.interest$Crop2014=='Pistachios'] <- pistachios_code
unique(perennials.of.interest$cropcode)
crs(perennials.of.interest)
crs(perennials.of.interest) <- '+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs' #get the right label back on, according to http://spatialreference.org/ref/sr-org/10/proj4/
shapefile(x=perennials.of.interest, filename=file.path(CropsDir, 'LandIQ.crops.of.interest.shp'), overwrite=TRUE)
perennials.of.interest #140,819 features

#test intersect function to split crops polygons by soil type
soils.CA.TA <- shapefile(file.path(SoilMUDir, 'mapunits_CA_TA.shp'))
crs(soils.CA.TA)
#crs(soils.CA.TA) <- '+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs'
nrow(soils.CA.TA) #457307 mapunit polygons for entire state
sum(area(perennials.of.interest))/10000*2.47105 #3,675,695 acres
sum(perennials.of.interest$Acres) #3,675,775
perennials.soils <- intersect(perennials.of.interest, soils.CA.TA) #doesn't work: Error in RGEOSBinPredFunc(spgeom1, spgeom2, byid, func) : rgeos_binpredfunc_prepared: maximum returned dense matrix size exceeded

#new CA soil mapunits file projected to California Teale Albers on 3/6/2018 using ArcMap 10.5
#intersect between crops and soil mapunits performed in ArcMap 10.5 on 3/6/2018 as a result
crops.soils <- shapefile(file.path(CropsDir, 'crops_soils_v2.shp'))
colnames(data.frame(crops.soils))
lapply(as.data.frame(crops.soils), class)
nrow(crops.soils) #now, 323,422 features
length(unique(crops.soils$mukey)) #only 5,301 mukeys
crops.soils$Acres <- area(crops.soils)/10000*2.47105
sum(crops.soils$Acres) #now, 3,674,350 acres, loss of 1345 acres
#crops.soils$crop.soils.code <- paste0(crops.soils$cropcode, crops.soils$mukey)
#length(unique(crops.soils$crop.soils.code)) #9916 unique mapunit x crop combinations
max(crops.soils$Acres) #is 580.9 acres; use gCentroids to get centroids
#crops.soils$FID_combined <- paste0(crops.soils$FID_LandIQ, crops.soils$FID_farmla)
#length(unique(crops.soils$FID_combined))
centroids <- gCentroid(crops.soils, byid = TRUE)
centroids.sp.df <- SpatialPointsDataFrame(centroids, data.frame(crops.soils))
nrow(centroids.sp.df)
plot(crops.soils[1,])
plot(centroids.sp.df[1,], add=TRUE)
plot(crops.soils[2,])
plot(centroids.sp.df[2,], add=TRUE)
ssurgo_spatialver <- data.frame(areasymbol=unique(crops.soils$areasymbol), spatialver=crops.soils$spatialver[match(unique(crops.soils$areasymbol), crops.soils$areasymbol)])
ssurgo_spatialver <- ssurgo_spatialver[order(ssurgo_spatialver$areasymbol),]
write.csv(ssurgo_spatialver, file.path(modelscaffoldDir, 'ssurgo_spatialver.csv'), row.names = FALSE)

#add PRISM and CIMIS raster cell numbers to find unique combinations of soil, crop, and climate
#merge with climate data from SpatialCIMIS
#export unique raster cell numbers for both datasets to build csv based database from rasters
spatialCIMIS <- raster(file.path(spatialCIMIS, 'U2/2004', 'U220040101.tif'))
CIMIScellnumbers <- as.integer(cellFromXY(object=spatialCIMIS, xy=centroids.sp.df))
summary(CIMIScellnumbers)
length(unique(CIMIScellnumbers)) #13,060 CIMIS cells need sampling
CIMIScellunique_df <- data.frame(CIMIS_cells=unique(CIMIScellnumbers))
#write.csv(CIMIScellunique_df, file.path(modelscaffoldDir, 'CIMIS_cells_unique.csv'), row.names = F)

#get cell numbers from PRISM for each point of interest
prism <- raster(file.path(PRISMDir, '2003/PRISM_ppt_stable_4kmD2_20031001_bil', 'PRISM_ppt_stable_4kmD2_20031001_bil.bil'))
#project crop x soil centroids to PRISM projection
centroids.sp.geo.df <- spTransform(centroids.sp.df, crs(prism))
PRISMcellnumbers <- as.integer(cellFromXY(object = prism, xy = centroids.sp.geo.df))
length(unique(PRISMcellnumbers)) #4546 PRISM cells need sampling
PRISMcellunique_df <- data.frame(PRISM_cells=unique(PRISMcellnumbers))
#write.csv(PRISMcellunique_df, file.path(modelscaffoldDir, 'PRISM_cells_unique.csv'), row.names = F)

#write unique soil survey area names to file for running the soilDB query in 'download_SSURGO_allCA.R'
write.csv(data.frame(areasymbol=unique(crops.soils$areasymbol)), file.path(modelscaffoldDir, 'soil.areasymbols.csv'), row.names=FALSE)

#get grape zones for each grape field
list.files(CropZonesDir)
grape.zones <- shapefile(file.path(CropZonesDir, 'ca_eco_l3.shp'))
centroids.sp.aea.df <- spTransform(centroids.sp.df, crs(grape.zones))
grape.zone.names <- over(centroids.sp.aea.df, grape.zones) #do this for all crop x soil centroids to make merge with crop.soils easier; grape.zone.names changed to NA below if not a grape field 
dim(grape.zone.names)
#summary(as.factor(grape.zone.names$US_L3NAME))
#Cascades 
# 44 
#Central California Foothills and Coastal Mountains 
#69164 
#Central California Valley 
#58582 
#Coast Range 
#1852 
#Eastern Cascades Slopes and Foothills 
#5 
#Klamath Mountains/California High North Coast Range 
#601 
#Mojave Basin and Range 
#19 
#Sierra Nevada 
#1374 
#Sonoran Basin and Range 
#1371 
#Southern California Mountains 
#174 
#Southern California/Northern Baja Coast 
#2381 
grape.zone.names <- grape.zone.names['US_L3NAME']
grape.zone.names$grape.zone.name <- ifelse(grape.zone.names$US_L3NAME == 'Cascades' | grape.zone.names$US_L3NAME == 'Central California Foothills and Coastal Mountains' | grape.zone.names$US_L3NAME == 'Coast Range' | grape.zone.names$US_L3NAME == 'Eastern Cascades Slopes and Foothills' | grape.zone.names$US_L3NAME == 'Klamath Mountains/California High North Coast Range' | grape.zone.names$US_L3NAME == 'Sierra Nevada' | grape.zone.names$US_L3NAME == 'Southern California Mountains' | grape.zone.names$US_L3NAME == 'Southern California/Northern Baja Coast', 'Coast, Foothills, and Mountains', ifelse(grape.zone.names$US_L3NAME == 'Central California Valley', 'Central Valley', 'Southern Desert')) #thus, Sonoran and Mojave Basin and Range are grouped into 'Southern Desert'
grape.zone.names$grape.zone.name[centroids.sp.aea.df$cropcode!='69'] <- NA #maintain all ecozone names to add back to crop.soils
summary(as.factor(grape.zone.names$grape.zone.name))
summary(as.factor(grape.zone.names$US_L3NAME))
crops.soils$grape.zone <- grape.zone.names$grape.zone.name
crops.soils$ecozone_L3NAME <- grape.zone.names$US_L3NAME
sum(!is.na(crops.soils$grape.zone))
sum(crops.soils$cropcode=='69')

#add alfalfa zone
alfalfa.zones <- shapefile(file.path(CropZonesDir, 'alfalfa.zones.shp'))
centroids.sp.aea.df <- spTransform(centroids.sp.df, crs(alfalfa.zones))
alfalfa.zone.names <- over(centroids.sp.aea.df, alfalfa.zones)
alfalfa.zone.names$name[centroids.sp.aea.df$cropcode!='36'] <- NA
crops.soils$alfalfa.zone <- alfalfa.zone.names$name
sum(!is.na(crops.soils$alfalfa.zone))
sum(crops.soils$cropcode=='36')
sum(crops.soils$Acres[crops.soils$cropcode=='36']) #928,710.5
sum(crops.soils$Acres[crops.soils$cropcode=='36' & !is.na(crops.soils$alfalfa.zone)]) #874815, so excludes 53,896 or about 5.8% of LandIQ identified alfalfa

#now on to the model_scaffold
model_scaffold <- crops.soils
model_scaffold$cropcode <- as.integer(model_scaffold$cropcode)
lapply(data.frame(model_scaffold), class)
model_scaffold$PRISMcellnumber <- PRISMcellnumbers
model_scaffold$CIMIScellnumber <- CIMIScellnumbers
model_scaffold$model_code <- paste0(model_scaffold$mukey, model_scaffold$cropcode, model_scaffold$PRISMcellnumber, model_scaffold$CIMIScellnumber)
#model_scaffold$model_code <- as.numeric(model_scaffold$model_code) #set options(digits=22, scipen=999) before writing this to csv.  However, excel will round all number after 15 digits if opening the csv.  DANGER! long integers are being changed somehow, which creates the impression there are less than 200k unique combinations!  conclusion is that these should be kept in character class to preserve integrity
length(unique(model_scaffold$model_code)) #98,980 unique combinations of soil, crop, and climate out of 323,422 of soil x crop polygons
model_codes <- unique(model_scaffold$model_code)
unique_model_codes <- 100001:(100000+length(model_codes)) #create a dummy code for the model matrix
model_codes_matrix <- data.frame(model_code=model_codes, unique_model_code=unique_model_codes)

#add the dummy unique_model_code to the shapefile and write to disk
model_scaffold_final <- merge(model_scaffold, model_codes_matrix, by='model_code')
nrow(model_scaffold_final)
head(model_scaffold_final, 20) #check manually that codes maintained integrity
#model_scaffold_final$crop.soils.code <- NULL
model_scaffold_final$latitude_NAD83 <- coordinates(centroids.sp.geo.df)[,2]
model_scaffold_final$longitude_NAD83 <- coordinates(centroids.sp.geo.df)[,1]
shapefile(model_scaffold_final, file.path(modelscaffoldDir, 'shapefiles', 'model_scaffold.3.6.18.shp'), overwrite=TRUE) #still in same projection as original LandIQ shapefile 'i15_Crop_Mapping_2014_Final_LandIQonAtlas.shp'; field_names abbreviated due to shapefile contraints
write.csv(data.frame(model_scaffold_final), file.path(modelscaffoldDir, 'model_scaffold_allfields.3.8.18.csv'), row.names = FALSE) #this is a full copy of the model scaffold that contains duplicate unique climate x crop x map unit combinations

#get hectares by unique model code
model_scaffold_final <- read.csv(file.path(modelscaffoldDir, 'model_scaffold_allfields.3.8.18.csv'), stringsAsFactors = FALSE) #323422 fields x mapunit combos
model_scaffold_final$hectares <- model_scaffold_final$Acres / 2.47105
sum(model_scaffold_final$hectares) #1,486,959 hectares
length(unique(model_scaffold_final$unique_model_code)) #98,980 unique model codes
area_summary <- as.data.frame(tapply(model_scaffold_final$hectares, model_scaffold_final$unique_model_code, sum))
colnames(area_summary) <- 'hectares'
area_summary$unique_model_code <- rownames(area_summary)
write.csv(area_summary, file.path(modelscaffoldDir, paste0('hectares_by_model_code', Sys.Date(), '.csv')), row.names = FALSE)
crop_summary <- data.frame(hectares=tapply(model_scaffold_final$hectares, model_scaffold_final$Crop2014, sum))
crop_summary$crop <- rownames(crop_summary)
write.csv(crop_summary, file.path(modelscaffoldDir, paste0('hectares_by_crop', Sys.Date(), '.csv')), row.names = FALSE)

#create model_scaffold for the only the unique model codes and write to disk as csv
#maintain this order in columns: (1)mukey	(2)crop_code	(3)longitude_AEA	(4)latitude_AEA	(5)PRISMcellnumber	(6)CIMIScellnumber	(7)unique_model_code
#IMPORTANT: some "unique_model_codes" will show up in the model_scaffold more than once as some climate x mapunit x crop combinations have more than one major component as part of the mapuunit; in the end, results will be percent component averaged

unique_indices <- match(unique_model_codes, model_scaffold_final$unique_model_code)
model_scaffold_unique <- data.frame(model_scaffold_final[unique_indices, c('mukey', 'cropcode', 'longitude_NAD83', 'latitude_NAD83', 'PRISMcellnumber', 'CIMIScellnumber', 'unique_model_code', 'alfalfa.zone', 'grape.zone', 'ecozone_L3NAME')])
head(model_scaffold_unique)
write.csv(model_scaffold_unique, file.path(modelscaffoldDir, 'model_scaffold_codes3.8.18.csv'), row.names = FALSE) #this destroys the model code numbers when read back in, so they were deleted to avoid future confusion. they can simply be re-created by paste0(model_scaffold_unique$mukey, model_scaffold_unique$crop_code, model_scaffold_unique$PRISMcellnumber, model_scaffold_unique$CIMIScellnumber) in this order
model_scaffold <- read.csv(file.path(modelscaffoldDir, 'model_scaffold_codes3.8.18.csv'), stringsAsFactors = FALSE)
dim(model_scaffold)
#head(model_scaffold)
#dim(model_scaffold) #242,714 rows
#lapply(model_scaffold, class)
#length(unique(model_scaffold$mukey)) #5,225 unique mukeys
#get number of cokeys per mukey (can rewrite as function to end up with n set of model matrices for each scenario of allowable depletion assumptions and rooting depth)
#list.files()
soil_comp_data <- read.csv(file.path(SoilsDataDir, "CA_all_comps_summary_dbmodified_FINAL2018-03-08.csv"), stringsAsFactors = FALSE) #re-ran soils aggregation on 9/12 to include 3 m root zone PAW estimate
dim(soil_comp_data)
head(soil_comp_data)
compkeys_n <- function(x) {length(unique(x))}
soilcomps_n <- as.data.frame(tapply(soil_comp_data$cokey, soil_comp_data$mukey, compkeys_n))
colnames(soilcomps_n) <- 'n_compkeys'
soilcomps_n$mukey <- as.integer(rownames(soilcomps_n))
rownames(soilcomps_n) <- NULL
soilcomps_n$n_compkeys <- as.integer(soilcomps_n$n_compkeys)
#summary(soilcomps_n$n_compkeys)
model_scaffold2 <- merge(model_scaffold, soilcomps_n, by='mukey')
maxcomps <- max(model_scaffold2$n_compkeys)
#dim(model_scaffold2) #reduced to 242,688 rows, so 26 model codes lost
#length(unique(model_scaffold2$mukey)) #2 mukeys lost by above merge
#nrow(soilcomps_n) #there were 8,803 mukeys
#i <- which(model_scaffold$mukey %in% soilcomps_n$mukey)
#missing_mukeys <- model_scaffold[-i,]
#missing_mukeys #it was determined that these mukeys cover 1,285 ha of ag land in CA, but there is no component data for them in the SSURGO database, so that is why these mukeys don't exist at the component level
#summary(model_scaffold2$n_compkeys)
#head(model_scaffold2)
soil_comp_data <- soil_comp_data[order(soil_comp_data$mukey, soil_comp_data$comppct_r, soil_comp_data$cokey, decreasing=c(F, T, F)), ] #this works as confirmed by writing to csv below
colnames(soil_comp_data)
soil_comp_data <- soil_comp_data[ ,c(1:5,17,27:32,39:ncol(soil_comp_data))]

# missing_mukeys <- model_scaffold2$mukey[!(model_scaffold2$mukey %in% soil_comp_data$mukey)]
# mukeys <- model_scaffold2$mukey[model_scaffold2$mukey %in% soil_comp_data$mukey]
#setwd(model_scaffoldDir)
#write.csv(soil_comp_data, 'soil_comp_data_sorted.csv', row.names=F)
for (i in seq_len(maxcomps)) {
  soilcomp_rownums <- match(model_scaffold2$mukey, soil_comp_data$mukey)
  temp <- soil_comp_data[soilcomp_rownums, ]
  temp$mukey <- NULL
  temp2 <- cbind(model_scaffold2, temp)
  write.csv(temp2, file.path(soilcomptempDir, paste0('model_scaffold_comp', as.character(i), '.csv')), row.names=F) #save the file for modeling purposes later
  model_scaffold2 <- model_scaffold2[-which(model_scaffold2$n_compkeys==i), ] #now get rid of model codes with i number of cokeys.  they don't need to be included in additional model scaffolds
  soilcomp_rownums <- unique(soilcomp_rownums)
  soil_comp_data <- soil_comp_data[-soilcomp_rownums, ] #get rid of the cokeys already covered
}

fnames <- list.files(path=soilcomptempDir, pattern = glob2rx('*csv'), full.names = TRUE)
master.file <- do.call(rbind, lapply(fnames, read.csv, stringsAsFactors = FALSE))
dim(master.file)
#1,177,027 unique soil components, climate, and crop for original raster based approach; 1,681,860 with additional crops added 9/12/18); now with vector based LandIQ 5 perennial crops layer: 476,007 unique combinations (this total includes minor components)
sum(master.file$mukey=='462096' & master.file$compname=='Peltier' & master.file$comppct_r==85) #74 instances need to be fixed
master.file$majcompflag[which(master.file$mukey=='462096' & master.file$compname=='Peltier' & master.file$comppct_r==85)] <- 'Yes'
sum(master.file$majcompflag=='Yes') #112,136 major component only; 112,210 after fixing
sum(master.file$comppct_r>=15, na.rm = TRUE) #116,251 >15% component only
sum(master.file$majcompflag=='Yes' & master.file$SSURGO_awc_data=='Yes') #112,210 are major components though 116,251 are >= 15% of mapunits (verified 3/9/18); of these, 109372 originally had data
summary(as.factor(master.file$majcompflag)) #confirmed no NAs
summary(master.file$comppct_r[master.file$majcompflag=='Yes'])
hist(master.file$comppct_r[master.file$majcompflag=='Yes'])
model_scaffold_majcomps <- master.file[master.file$majcompflag=='Yes', ]
dim(model_scaffold_majcomps)
colnames(model_scaffold_majcomps)
length(unique(model_scaffold_majcomps$unique_model_code)) #98,966 unique map unit x climate x crop combinations
length(unique(model_scaffold_final$unique_model_code)) #98,980 unique map unit x climate x crop combinations, lost 
model_scaffold_majcomps$test_code <- paste0(model_scaffold_majcomps$cokey, model_scaffold_majcomps$cropcode, model_scaffold_majcomps$PRISMcellnumber, model_scaffold_majcomps$CIMIScellnumber)
length(unique(model_scaffold_majcomps$test_code)) #confirmed 112,210 unique major soil component x climate x crop combinations
model_scaffold_majcomps$test_code <- NULL
lapply(model_scaffold_majcomps, class)
length(unique(model_scaffold_final$mukey)) #5301
length(unique(model_scaffold_majcomps$mukey)) #5299, missing 1 now after fix
unique(model_scaffold_final$mukey)[!(unique(model_scaffold_final$mukey) %in% unique(model_scaffold_majcomps$mukey))] #these are the missing ones: "784250" "462096"
sum(crops.soils$Acres[crops.soils$mukey=='784250']) #6.2 acres this is water in SSURGO
sum(crops.soils$Acres[crops.soils$mukey=='462096']) #6000 acres: this is 
data.frame(crops.soils)[crops.soils$mukey=='462096',] #mistake is that Peltier (85% of mapunit) is not flagged as a major component; fixed above

#write unique model code scaffold to disk for running FAO56_dualcropcoeff.R
write.csv(model_scaffold_majcomps, file.path(modelscaffoldDir, 'model_scaffold_majcomps.v3.csv'), row.names = FALSE) #v1 included alfalfa.zone column; v2 includes both alfalfa and grape zone column; v3 also includes column identifying whether or not column originally had data

#read this back in for some further refinement of the scaffold
model.scaffold <- read.csv(file.path(modelscaffoldDir, 'model_scaffold_majcomps.v3.csv'), stringsAsFactors = FALSE)
dim(model.scaffold) #112210 rows
alfalfa_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Alfalfa'] #75380 total
grape_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Grapes']
almond_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Almonds']
walnut_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Walnuts']
pistachio_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Pistachios']
sum(model.scaffold$cropcode==grape_code) #37,488
sum(model.scaffold$cropcode==alfalfa_code) #26,993
sum(model.scaffold$cropcode==almond_code) #27,672
sum(model.scaffold$cropcode==walnut_code) #13,411
sum(model.scaffold$cropcode==pistachio_code) #6,946
summary(as.factor(model.scaffold$alfalfa.zone[model.scaffold$cropcode==alfalfa_code]))
#Central Valley Imperial Valley   Intermountain       NA's 
#   14590            6044            4300            1759 
#thus, 1759 NAs may not be run for alfalfa
sum(model.scaffold$cropcode==alfalfa_code & model.scaffold$ecozone_L3NAME=='Sonoran Basin and Range' & is.na(model.scaffold$alfalfa.zone)) #only 61 NAs in Sonoran Basin and Range
summary(as.factor(model.scaffold$ecozone_L3NAME[model.scaffold$cropcode==alfalfa_code & is.na(model.scaffold$alfalfa.zone)]))
sum(model.scaffold$cropcode==grape_code & is.na(model.scaffold$grape.zone)) #0
summary(as.factor(model.scaffold$grape.zone[model.scaffold$cropcode==grape_code]))
#Central Valley   Coast, Foothills, and Mountains   Southern Desert
#16193                           20975                    320

summary(as.factor(model.scaffold$ecozone_L3NAME[model.scaffold$cropcode==almond_code]))
summary(as.factor(model.scaffold$ecozone_L3NAME[model.scaffold$cropcode==walnut_code]))
summary(as.factor(model.scaffold$ecozone_L3NAME[model.scaffold$cropcode==pistachio_code]))

#conclusion is to clean up model_scaffold to remove AWC NAs and AWC zeroes, skip Southern Desert grapes and alfalfa zone NAs
#save this modified model.scaffold with date tagged for march 2018 model run
model.scaffold <- model.scaffold[!(model.scaffold$cropcode==alfalfa_code & is.na(model.scaffold$alfalfa.zone)),] #take out unique alfalfa soil & climate combinations outside of Intermountain, Central Valley, or Low Desert (Imperial Valley) regions, removing 1759
model.scaffold <- model.scaffold[!(model.scaffold$cropcode==grape_code & model.scaffold$grape.zone=='Southern Desert'),] #this removes 320
model.scaffold <- model.scaffold[!(is.na(model.scaffold$z1.5m_cmH2O_unmodified_comp)), ] #removes another 1290
model.scaffold <- model.scaffold[!(model.scaffold$z1.5m_cmH2O_unmodified_comp == 0), ] #removes another 1071
model.scaffold <- model.scaffold[!is.na(model.scaffold$TEW),] #removes another 714
sum(is.na(model.scaffold$z1.5m_cmH2O_unmodified_comp))
sum(model.scaffold$z1.5m_cmH2O_unmodified_comp==0)
sum(is.na(model.scaffold$TEW))
sum(model.scaffold$REW==0)
sum(model.scaffold$TEW==0)
sum(model.scaffold$TEW < model.scaffold$REW) #16 have this state; fixed in FAO56 model run
dim(model.scaffold) #reduced to 107,561 unique crop x soil x climate combinations to model
summary(as.factor(model.scaffold$cropcode))
model.scaffold <- model.scaffold[order(model.scaffold$unique_model_code, model.scaffold$cokey),]
write.csv(model.scaffold, file.path(modelscaffoldDir, paste0('model_scaffold_majcomps', Sys.Date(),'.csv')), row.names = FALSE)

#counts of major component by unique_model_code
summary(as.factor(tapply(model.scaffold$cokey, model.scaffold$unique_model_code, length)))
#84,304 have one major component; 10,101 have 2 major components; 1,001 have 3 majors; 13 have 4 majors

#double check grape.zone and alfalfa.zone counts match cropcode counts now
sum(model.scaffold$cropcode==36)
sum(!is.na(model.scaffold$alfalfa.zone))
sum(model.scaffold$cropcode==69)
sum(!is.na(model.scaffold$grape.zone))

#investigate NAs and 0's
mukey_AD_isNA <- unique(model_scaffold_majcomps$mukey[which(is.na(model_scaffold_majcomps$z1.5m_cmH2O_unmodified_comp))]) #119 mukeys are NA for awc
cokey_AD_isNA <- unique(model_scaffold_majcomps$cokey[which(is.na(model_scaffold_majcomps$z1.5m_cmH2O_unmodified_comp))]) #120 cokeys
compnames_NA <- soil_comp_data$compname[match(cokey_AD_isNA, soil_comp_data$cokey)] #confirmed that these 22 component names are all components for which we shouldn't expect any data
(unique(compnames_NA))

#investigate problems with alfalfa and grape zones