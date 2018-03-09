#this is a complete re-working of the model scaffold based on new crops dataset from LandIQ
#combines code from model_scaffold.R, model_consruction.R, ....
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
crops.soils$crop.soils.code <- paste0(crops.soils$cropcode, crops.soils$mukey)
length(unique(crops.soils$crop.soils.code)) #9916 unique mapunit x crop combinations
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
write.csv(CIMIScellunique_df, file.path(modelscaffoldDir, 'CIMIS_cells_unique.csv'), row.names = F)

#get cell numbers from PRISM for each point of interest
prism <- raster(file.path(PRISMDir, '2003/PRISM_ppt_stable_4kmD2_20031001_bil', 'PRISM_ppt_stable_4kmD2_20031001_bil.bil'))
#project crop x soil centroids to PRISM projection
centroids.sp.geo.df <- spTransform(centroids.sp.df, crs(prism))
PRISMcellnumbers <- as.integer(cellFromXY(object = prism, xy = centroids.sp.geo.df))
length(unique(PRISMcellnumbers)) #4546 PRISM cells need sampling
PRISMcellunique_df <- data.frame(PRISM_cells=unique(PRISMcellnumbers))
write.csv(PRISMcellunique_df, file.path(modelscaffoldDir, 'PRISM_cells_unique.csv'), row.names = F)

#write unique soil survey area names to file for running the soilDB query in 'download_SSURGO_allCA.R'
write.csv(data.frame(areasymbol=unique(crops.soils$areasymbol)), file.path(modelscaffoldDir, 'soil.areasymbols.csv'), row.names=FALSE)

#now on to the model_scaffold
model_scaffold <- crops.soils
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
model_scaffold_final$crop.soils.code <- NULL
model_scaffold_final$latitude_NAD83 <- coordinates(centroids.sp.geo.df)[,2]
model_scaffold_final$longitude_NAD83 <- coordinates(centroids.sp.geo.df)[,1]
shapefile(model_scaffold_final, file.path(modelscaffoldDir, 'shapefiles', 'model_scaffold.3.6.18.shp'), overwrite=TRUE) #still in same projection as original LandIQ shapefile 'i15_Crop_Mapping_2014_Final_LandIQonAtlas.shp'; field_names abbreviated due to shapefile contraints

#create model_scaffold for the only the unique model codes and write to disk as csv
#maintain this order in columns: (1)mukey	(2)crop_code	(3)longitude_AEA	(4)latitude_AEA	(5)PRISMcellnumber	(6)CIMIScellnumber	(7)unique_model_code

unique_indices <- match(unique_model_codes, model_scaffold_final$unique_model_code)
model_scaffold_unique <- data.frame(model_scaffold_final[unique_indices, c('mukey', 'cropcode', 'longitude_NAD83', 'latitude_NAD83', 'PRISMcellnumber', 'CIMIScellnumber', 'unique_model_code')])
head(model_scaffold_unique)
write.csv(model_scaffold_unique, file.path(modelscaffoldDir, 'model_scaffold_codes3.6.18.csv'), row.names = FALSE) #this destroys the model code numbers when read back in, so they were deleted to avoid future confusion. they can simply be re-created by paste0(model_scaffold_unique$mukey, model_scaffold_unique$crop_code, model_scaffold_unique$PRISMcellnumber, model_scaffold_unique$CIMIScellnumber) in this order
model_scaffold <- read.csv(file.path(modelscaffoldDir, 'model_scaffold_codes3.6.18.csv'), stringsAsFactors = FALSE)

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
soil_comp_data <- soil_comp_data[ ,c(1:5, 27:32, 39:ncol(soil_comp_data))]

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
master.file <- do.call(rbind, lapply(fnames, read.csv))
dim(master.file)
#1,177,027 unique soil components, climate, and crop for original raster based approach; 1,681,860 with additional crops added 9/12/18); now with vector based LandIQ 5 perennial crops layer: 476,007 unique combinations (includes minor components)
sum(master.file$majcompflag=='Yes') #112,136 major component only
sum(master.file$comppct_r>=15, na.rm = TRUE) #116,251 >15% component only
j <- which(master.file$majcompflag=='Yes' & master.file$SSURGO_awc_data=='Yes')
length(j) #112,136 are major components though 116,251 are >= 15% of mapunits (verified 3/7/18); of these, 109,298 originally had data
summary(as.factor(master.file$majcompflag)) #confirmed no NAs
summary(master.file$comppct_r[master.file$majcompflag=='Yes'])
hist(master.file$comppct_r[master.file$majcompflag=='Yes'])
#left off here 3/7/18 5:27 pm
model_scaffold_majcomps <- master.file[which(master.file$comppct_r >= 15), ]
dim(model_scaffold_majcomps)

#add alfalfa zones to model scaffold
setwd(file.path(model_scaffoldDir, 'run_model/Sep2017'))
alfalfa_zones_codes <- read.csv('alfalfa_zones_codes.csv', stringsAsFactors = FALSE)
model_scaffold_majcomps$alfalfa.zone <- alfalfa_zones_codes$zone[match(model_scaffold_majcomps$unique_model_code, alfalfa_zones_codes$unique_model_code)]
grape_zones_codes <- read.csv('grape_zones_codes.csv', stringsAsFactors = FALSE)
model_scaffold_majcomps$grape.zone <- grape_zones_codes$grape.zone[match(model_scaffold_majcomps$unique_model_code, grape_zones_codes$unique_model_code)]
write.csv(model_scaffold_majcomps, file.path(model_scaffoldDir, 'model_scaffold_majcomps.v3.csv'), row.names = F) #v1 included alfalfa.zone column; v2 includes both alfalfa and grape zone column; v3 also includes column identifying whether or not column originally had data

#investigate NAs and 0's
mukey_AD_isNA <- unique(model_scaffold2$mukey[which(is.na(model_scaffold2$allowable_depletion))])
cokey_AD_isNA <- unique(model_scaffold2$cokey[which(is.na(model_scaffold2$allowable_depletion))])
compnames_NA <- soil_comp_data$compname[match(cokey_AD_isNA, soil_comp_data$cokey)] #confirmed that these are all components for which we shouldn't expect any data
setwd(soildataDir)
list.files()
mu_area <- read.csv('farmland_mu_area.csv')
mu_area_rows <- match(mukey_AD_isNA, mu_area$mukey)
mu_area <- mu_area[mu_area_rows,]
sum(mu_area$hectares)
#the total ag area of these mukeys are 70,641 hectares; however the raster cells with these should be less, because CropScape really shouldn't be identifying crops in these areas
tapply(model_scaffold2$allowable_depletion, model_scaffold2$crop_code, mean, na.rm=TRUE)
model_scaffold2$n_compkeys[which(model_scaffold2$mukey=='455489')]