library(raster)
coords <- SpatialPoints(cbind(-2253330, 1848300), proj4string = crs('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs')) #this is Albers Equal Area coordinates)
spTransform(coords, "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")
