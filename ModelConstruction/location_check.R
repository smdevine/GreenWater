library(raster)
mainDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion'
coords <- SpatialPoints(cbind(-2045640, 1740060), proj4string = crs('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs')) #this is Albers Equal Area coordinates
spTransform(coords, "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")
#result is: -115.653, 33.13325
# -1809120	1305900

#convert from AEA to CA_TA
spTransform(coords, crs('+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs'))
#central valley upper right corner -30448.53, 293859.2 (x, y)
#central valley lower left corner -151421.3, -410200.4 (x, y)

#test crs of raster
test <- raster('D:/Allowable_Depletion/results/Dec2017.check/summaries/allcrops/figures/scenario_2.0mAD50/rasters/GW.ET.growing/GW.ET.growing.mean.tif')
crs(test) #this matches AEA definition above
cropscape <- raster('C:/Users/smdevine/Desktop/Allowable_Depletion/CA_cropscape2015/CDL_2015_06.tif')
crs(cropscape) #good, I didn't do anything stupid
projection(cropscape)

#original crs of soil map units
mapunits <- shapefile(file.path(mainDir, 'soils_data', 'spatial', 'farmland_mapunits_2016.shp'))
#crs is +proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0 (this is California Teale Albers)