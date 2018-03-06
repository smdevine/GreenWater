library(raster)
library(rgdal)
spatialCIMIS <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/SpatialCIMIS'
CropsDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/LandIQ.crops'
PRISMDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/PRISMdaily'
SoilMUDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/soils_data/spatial'
modelscaffoldDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/model_scaffold/run_model/Mar2018' #this is a complete re-working of the model scaffold based on new crops dataset from LandIQ

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

#test erase then cover function to split crops polygons by soil type
soils.CA.TA <- shapefile(file.path(SoilMUDir, 'farmland_mapunits_2016.shp'))
crs(soils.CA.TA)
crs(soils.CA.TA) <- '+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs'
soils.CA.TA #188,750 polygon
sum(area(perennials.of.interest))/10000*2.47105 #3,675,695 acres
sum(perennials.of.interest$Acres) #3,675,775
perennials.soils <- intersect(perennials.of.interest, soils.CA.TA) #doesn't work: Error in RGEOSBinPredFunc(spgeom1, spgeom2, byid, func) : rgeos_binpredfunc_prepared: maximum returned dense matrix size exceeded

#intersect between crops and soil mapunits performed in ArcMap 10.3 as a result
crops.soils <- shapefile(file.path(CropsDir, 'crops.soils.shp'))
#get rid of pointless columns
crops.soils$hectares <- NULL
crops.soils$mukey_num <- NULL
crops.soils$acres_1 <- NULL
lapply(as.data.frame(crops.soils), class)
nrow(crops.soils) #now, 313681 features
length(unique(crops.soils$mukey)) #only 4795 mukeys
crops.soils$Acres <- area(crops.soils)/10000*2.47105
sum(crops.soils$Acres) #now, 3,577,618 acres, loss of 98157 or 2.7%
#conclusion is that I need to wait for the whole of CA mapunit shapefile
crops.soils$crop.soils.code <- paste0(crops.soils$cropcode, crops.soils$mukey)
length(unique(crops.soils$crop.soils.code)) #only 9337 unique mapunit x crop combinations
max(crops.soils$Acres) #is 580.8 acres; use gCentroids to get centroids
crops.soils$FID_combined <- paste0(crops.soils$FID_LandIQ, crops.soils$FID_farmla)
length(unique(crops.soils$FID_combined))
centroids <- gCentroid(crops.soils, byid = TRUE) 
centroids.sp.df <- SpatialPointsDataFrame(centroids, as.data.frame(crops.soils))
nrow(centroids.sp.df)
mukeys_cropcodes_CA_TA <- centroids.sp.df

#add PRISM and CIMIS raster cell numbers to find unique combinations of soil, crop, and climate
#merge with climate data from SpatialCIMIS
#export unique raster cell numbers for both datasets to build csv based database from rasters
spatialCIMIS <- raster(file.path(spatialCIMIS, 'U2/2004', 'U220040101.tif'))
CIMIScellnumbers <- as.integer(cellFromXY(object=spatialCIMIS, xy=mukeys_cropcodes_CA_TA))
summary(CIMIScellnumbers)
length(CIMIScellnumbers)
length(unique(CIMIScellnumbers))
#CIMIScellunique <- unique(CIMIScellnumbers) #17,341 CIMIS cells need sampling; 18,633
#setwd(file.path(mainDir, 'model_scaffold/run_model/Aug2017'))
#CIMIScellunique_df <- data.frame(CIMIS_cells=CIMIScellunique)
#write.csv(CIMIScellunique_df, 'CIMIS_cells_unique.csv', row.names = F)

#get cell numbers from PRISM for each point of interest
setwd(file.path(PRISMDir, '2003/PRISM_ppt_stable_4kmD2_20031001_bil'))
prism <- raster('PRISM_ppt_stable_4kmD2_20031001_bil.bil')
PRISMcellnumbers <- as.integer(cellFromXY(object = prism, xy = mukeys_cropcodes_longlatNAD83))
summary(PRISMcellnumbers)
PRISMcellunique <- unique(PRISMcellnumbers) #5338 PRISM cells need sampling; 5701 need sampling
PRISMcellunique_df <- data.frame(PRISM_cells=PRISMcellunique) 
setwd(file.path(mainDir, 'model_scaffold/run_model/Aug2017'))
write.csv(PRISMcellunique_df, 'PRISM_cells_unique.csv', row.names = F)


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