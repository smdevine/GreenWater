options(digits = 22)
scenario.resultsDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/results/scenario results'
setwd(file.path(scenario.resultsDir, '50% allowable depletion'))
list.files()
almond2.0m_AD50 <- read.csv('almond.mature2.0mAD50_FAO56results.csv', stringsAsFactors = FALSE)
walnut2.0m_AD50 <- read.csv('walnut.mature2.0mAD50_FAO56results.csv', stringsAsFactors = FALSE)
pistachio2.0m_AD50 <- read.csv('pistachios2.0mAD50_FAO56results.csv', stringsAsFactors = FALSE)
setwd(file.path(scenario.resultsDir, 'wine grapes'))
list.files()
grapes.wine2.0m_0.2minRDI <- read.csv('grapes.wine2.0mRDI.min0.2_FAO56results.csv', stringsAsFactors = FALSE)
almond_GW_ET <- tapply(almond2.0m_AD50$GW.ET.growing, list(almond2.0m_AD50$unique_model_code, almond2.0m_AD50$cokey), mean, na.rm=TRUE)




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


##data exploration
mean(model.scaffold.results$GW.ET.growing, na.rm=TRUE)
hist(model.scaffold.results$GW.ET.growing)
hist(tapply(model.scaffold.results$GW.ET.growing, model.scaffold.results$Model.Year, mean, na.rm=TRUE))
hist(tapply(model.scaffold.results$Irr.app.total, model.scaffold.results$Model.Year, mean, na.rm=TRUE))
plot(tapply(model.scaffold.results$Irr.app.total, model.scaffold.results$Model.Year, mean, na.rm=TRUE), tapply(model.scaffold.results$GW.ET.growing, model.scaffold.results$Model.Year, mean, na.rm=TRUE))
plot(tapply(model.scaffold.results$ET.growing, model.scaffold.results$Model.Year, mean, na.rm=TRUE), tapply(model.scaffold.results$Irr.app.total, model.scaffold.results$Model.Year, mean, na.rm=TRUE))
plot(2003:2017, tapply(model.scaffold.results$ET.growing, model.scaffold.results$Model.Year, mean, na.rm=TRUE))
(240/25.4)/12*1000000
plot(tapply(model.scaffold.results$z1.0m_cmH2O_modified_comp, model.scaffold.results$unique_model_code_final, mean, na.rm=TRUE), tapply(model.scaffold.results$GW.ET.growing, model.scaffold.results$unique_model_code_final, mean, na.rm=TRUE))