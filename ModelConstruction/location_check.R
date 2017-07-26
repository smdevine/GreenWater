library(raster)
coords <- SpatialPoints(cbind(-1809120, 1305900), proj4string = crs('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs')) #this is Albers Equal Area coordinates)
spTransform(coords, "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")
#result is: -115.653, 33.13325
# -1809120	1305900
