library(raster)
options(digits = 22)
max_modified <- function(x) {
  if(all(is.na(x))) {
    return(NA)
  }
  else {max(x, na.rm = TRUE)}
}
modelscaffoldDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/model_scaffold/run_model/Aug2017'
JulmodelscaffoldDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/model_scaffold/run_model/July2017'
resultsDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/results'
scenario.resultsDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/results/scenario results'
setwd(modelscaffoldDir)
list.files()
almond_Jul_Aug_code_converter <- read.csv("almond_Jul_Aug_code_converter.csv", stringsAsFactors = FALSE)
walnut_Jul_Aug_code_converter <- read.csv("walnut_Jul_Aug_code_converter.csv", stringsAsFactors = FALSE)
colnames(almond_Jul_Aug_code_converter)[1] <- 'Aug.model.code'
write.csv(almond_Jul_Aug_code_converter, 'almond_Jul_Aug_code_converter.csv', row.names = FALSE)
setwd(scenario.resultsDir)
list.files()
almond2.0m_AD50 <- read.csv('almond.mature2.0mAD50_FAO56results.csv', stringsAsFactors = FALSE)
almond2.0m_AD50$full_matrix_rownum <- NULL
dim(almond2.0m_AD50) #1206030 rows
almond2.0m_AD50$revised.code <- NA
dim(almond2.0m_AD50)
almond2.0m_AD50$revised.code <- almond_Jul_Aug_code_converter$Aug.model.code[match(almond2.0m_AD50$unique_model_code, almond_Jul_Aug_code_converter$Jul.model.code)]
almond2.0m_AD50$unique_model_code <- almond2.0m_AD50$revised.code
almond2.0m_AD50$revised.code <- NULL
walnut2.0m_AD50 <- read.csv('walnut.mature2.0mAD50_FAO56results.csv', stringsAsFactors = FALSE)
walnut2.0m_AD50$full_matrix_rownum <- NULL
dim(walnut2.0m_AD50)
walnut2.0m_AD50$revised.code <- NA
walnut2.0m_AD50$revised.code <- walnut_Jul_Aug_code_converter$Aug.model.code[match(walnut2.0m_AD50$unique_model_code, walnut_Jul_Aug_code_converter$Jul.model.code)]
walnut2.0m_AD50$unique_model_code <- walnut2.0m_AD50$revised.code
walnut2.0m_AD50$revised.code <- NULL
pistachio2.0m_AD50 <- read.csv('pistachios2.0mAD50_FAO56results.csv', stringsAsFactors = FALSE)
dim(pistachio2.0m_AD50)
grapes.wine2.0m_0.2minRDI <- read.csv('grapes.wine2.0mRDI.min0.2_FAO56results.csv', stringsAsFactors = FALSE)
grapes.wine2.0m_0.2minRDI$H2O.stress <- NULL
dim(grapes.wine2.0m_0.2minRDI)
allcrops2.0m_AD50 <- rbind(almond2.0m_AD50, walnut2.0m_AD50, pistachio2.0m_AD50, grapes.wine2.0m_0.2minRDI)
dim(allcrops2.0m_AD50) #3,432,480 rows
length(unique(allcrops2.0m_AD50$unique_model_code)) #200,090 unique model codes

#now, aggregate mean annual green water ET by mukey & model.code
allcrops2.0m_AD50$unique_model_code_final <- paste0(as.character(allcrops2.0m_AD50$unique_model_code), as.character(allcrops2.0m_AD50$cokey))
allcrops_GW_ET <- tapply(allcrops2.0m_AD50$GW.ET.growing, allcrops2.0m_AD50$unique_model_code_final, mean, na.rm=TRUE)
allcrops_mukeys <- tapply(allcrops2.0m_AD50$mukey, allcrops2.0m_AD50$unique_model_code_final, unique)
allcrops_comppct_r <- tapply(allcrops2.0m_AD50$comppct_r, allcrops2.0m_AD50$unique_model_code_final, unique)
allcrops_modelcode <- tapply(allcrops2.0m_AD50$unique_model_code, allcrops2.0m_AD50$unique_model_code_final, unique)
allcrops_results <- cbind(allcrops_GW_ET, allcrops_mukeys, allcrops_comppct_r, allcrops_modelcode)
allcrops_results <- as.data.frame(allcrops_results)
allcrops_compsums <- as.data.frame(tapply(allcrops_results$allcrops_comppct_r[!is.na(allcrops_results$allcrops_GW_ET)], allcrops_results$allcrops_modelcode[!is.na(allcrops_results$allcrops_GW_ET)], sum))
colnames(allcrops_compsums) <- 'compsums'
allcrops_compsums$allcrops_modelcode <- rownames(allcrops_compsums)
allcrops_results <- merge(allcrops_results, allcrops_compsums, by='allcrops_modelcode')
allcrops_GW_ET <- tapply(allcrops_results$allcrops_GW_ET*(allcrops_results$allcrops_comppct_r/allcrops_results$compsums), allcrops_results$allcrops_modelcode, sum, na.rm=TRUE)
length(unique(allcrops_results$allcrops_modelcode)) #193820 results
allcrops_GW_ET <- as.data.frame(allcrops_GW_ET)
colnames(allcrops_GW_ET) <- 'meanGW.mm.year'
allcrops_GW_ET$unique_model_code <- rownames(allcrops_GW_ET)
allcrops_GW_ET$meanGW.mm.year <- as.numeric(allcrops_GW_ET$meanGW.mm.year)
dim(allcrops_GW_ET) #193,820  model codes with data

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
raster.maxGW <- subs(raster.model.codes, max_GW_ET, by=1, which=2) #this is much faster than rasterizing points
writeRaster(raster.maxGW, 'maxGW.ET.Aug2017runs.tif', format='GTiff')

#merge mean annual GreenWater ET with spatial points data.frame that has the unique_model_codes for all points of interest
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
raster_gw_mean2004_2016 <- raster(xmn=(lonmin-15), xmx=(lonmax+15), ymn=(latmin-15), ymx=(latmax+15), resolution=30, crs='+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs') #add or subtract 15 because coordinates are centers of the raster cells from which these were derived
rasterOptions(progress = 'window')
raster_gw_mean2004_2016 <- rasterize(x=model_points_sp, y=raster_gw_mean2004_2016, field='meanGW.mm.year', fun=function(x,...) {mean(x)}) #this latter addition to the rasterize function (defining fun is to deal with NAs present in meanGW.mm.year)

sum(!is.na(model_points_sp$meanGW.mm.year)) #13204255 cells with data
sum(!is.na(model_points_sp$meanGW.mm.year))*900/10000 #1,188,383 hectares -or- 2935306 acres with data
model_points_subset <- model_points_sp[which(model_points_sp$crop_code==almonds | model_points_sp$crop_code==walnuts | model_points_sp$crop_code==pistachios | model_points_sp$crop_code==grapes), ] #13272386 cells that are almonds, walnuts, pistachios, or grapes, so 68131 with no data for these crops
length(unique(model_points_subset$unique_model_code)) #200,217 unique model codes
mean(model_points_sp$meanGW.mm.year, na.rm=TRUE) #mean green water ET across almonds, walnuts, pistachios, and wine grapes
sum(!is.na(model_points_sp$meanGW.mm.year))*900*mean(model_points_sp$meanGW.mm.year, na.rm=TRUE)/(1000*1233.48) #1,407,218 acre-feet for 2,935,306 acres
#acre-feet of green water
table(model_points_sp$crop_code[which(is.na(model_points_sp$meanGW.mm.year))])

#now, aggregate max annual green water ET by mukey & model.code
allcrops2.0m_AD50$unique_model_code_final <- paste0(as.character(allcrops2.0m_AD50$unique_model_code), as.character(allcrops2.0m_AD50$cokey))
max_GW_ET <- tapply(allcrops2.0m_AD50$GW.ET.growing, allcrops2.0m_AD50$unique_model_code_final, max_modified)
max_mukeys <- tapply(allcrops2.0m_AD50$mukey, allcrops2.0m_AD50$unique_model_code_final, unique)
max_comppct_r <- tapply(allcrops2.0m_AD50$comppct_r, allcrops2.0m_AD50$unique_model_code_final, unique)
max_modelcode <- tapply(allcrops2.0m_AD50$unique_model_code, allcrops2.0m_AD50$unique_model_code_final, unique)
max_results <- cbind(max_GW_ET, max_mukeys, max_comppct_r, max_modelcode)
max_results <- as.data.frame(max_results)
max_compsums <- as.data.frame(tapply(max_results$max_comppct_r[!is.na(max_results$max_GW_ET)], max_results$max_modelcode[!is.na(max_results$max_GW_ET)], sum))
colnames(max_compsums) <- 'compsums'
max_compsums$max_modelcode <- rownames(max_compsums)
max_results <- merge(max_results, max_compsums, by='max_modelcode')
max_GW_ET <- tapply(max_results$max_GW_ET*(max_results$max_comppct_r/max_results$compsums), max_results$max_modelcode, sum, na.rm=TRUE)
length(unique(max_results$max_modelcode)) #193820 results
max_GW_ET <- as.data.frame(max_GW_ET)
colnames(max_GW_ET) <- 'maxGW.mm.year'
max_GW_ET$unique_model_code <- rownames(max_GW_ET)
max_GW_ET$maxGW.mm.year <- as.numeric(max_GW_ET$maxGW.mm.year)
dim(max_GW_ET) #193,820  model codes with data
max_GW_ET <- max_GW_ET[,c(2,1)]

#merge max GW ET ('allcrops_GW_ET' above) with spatial points data.frame (read-in above)
model_points_sp$maxGW.mm.year <- NA
model_points_sp$maxGW.mm.year <- max_GW_ET$maxGW.mm.year[match(model_points_sp$unique_model_code, max_GW_ET$unique_model_code)]
#model_points_sp <- merge(model_points_sp, max_GW_ET, by='unique_model_code')
#test$meanGW.mm.year <- as.numeric(test$meanGW.mm.year)
raster_gw_max2004_2016 <- raster(xmn=(lonmin-15), xmx=(lonmax+15), ymn=(latmin-15), ymx=(latmax+15), resolution=30, crs='+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs') #add or subtract 15 because coordinates are centers of the raster cells from which these were derived
rasterOptions(progress = 'window')
raster_gw_max2004_2016 <- rasterize(x=model_points_sp, y=raster_gw_max2004_2016, field='maxGW.mm.year', fun=function(x,...) {mean(x)}) #this latter addition to the rasterize function (defining fun is to deal with NAs present in maxGW.mm.year)
sum(!is.na(model_points_sp$maxGW.mm.year)) #13204255 cells with data
sum(!is.na(model_points_sp$maxGW.mm.year))*900/10000 #1,188,383 hectares -or- 2935306 acres with data
model_points_subset <- model_points_sp[which(model_points_sp$crop_code==almonds | model_points_sp$crop_code==walnuts | model_points_sp$crop_code==pistachios | model_points_sp$crop_code==grapes), ] #13272386 cells that are almonds, walnuts, pistachios, or grapes, so 68131 with no data for these crops
length(unique(model_points_subset$unique_model_code)) #200,217 unique model codes
mean(model_points_sp$maxGW.mm.year, na.rm=TRUE) #mean green water ET across almonds, walnuts, pistachios, and wine grapes
sum(!is.na(model_points_sp$maxGW.mm.year))*900*mean(model_points_sp$maxGW.mm.year, na.rm=TRUE)/(1000*1233.48) #2,465,077 acre-feet for 2,935,306 acres
#acre-feet of green water
table(model_points_sp$crop_code[which(is.na(model_points_sp$maxGW.mm.year))])

#convert unique model codes in July results to August codes
setwd(modelscaffoldDir)
list.files()
model.scaffold.Aug <- read.csv("model_scaffold_majcomps.csv", stringsAsFactors = FALSE)
model.scaffold.Aug <- model.scaffold.Aug[-which(is.na(model.scaffold.Aug$crop_code)),]
dim(model.scaffold.Aug)

setwd(JulmodelscaffoldDir)
list.files()
model.scaffold.Jul <- read.csv('model_scaffold_majcomps.csv', stringsAsFactors = FALSE)
model.scaffold.Jul <- model.scaffold.Jul[-which(is.na(model.scaffold.Jul$crop_code)),]
dim(model.scaffold.Jul)

model.scaffold.Jul.Aug.merged <- merge(model.scaffold.Aug[,c('cokey', 'PRISMcellnumber', 'CIMIScellnumber', 'crop_code', 'unique_model_code')], model.scaffold.Jul[,c('cokey', 'PRISMcellnumber', 'CIMIScellnumber', 'crop_code', 'unique_model_code')], by=c('cokey', 'PRISMcellnumber', 'CIMIScellnumber', 'crop_code'))
almond_Jul_Aug_code_converter <- model.scaffold.Jul.Aug.merged[which(model.scaffold.Jul.Aug.merged$crop_code==75),]
walnut_Jul_Aug_code_converter <- model.scaffold.Jul.Aug.merged[which(model.scaffold.Jul.Aug.merged$crop_code==76),]
setwd(modelscaffoldDir)
almond_Jul_Aug_code_converter <- almond_Jul_Aug_code_converter[,c(5,6)]
colnames(almond_Jul_Aug_code_converter)[1] <- 'Aug.model.code'
colnames(almond_Jul_Aug_code_converter)[2] <- 'Jul.model.code'
write.csv(almond_Jul_Aug_code_converter, 'almond_Jul_Aug_code_converter.csv', row.names = FALSE)
walnut_Jul_Aug_code_converter <- walnut_Jul_Aug_code_converter[,c(5,6)]
colnames(walnut_Jul_Aug_code_converter)[1] <- 'Aug.model.code'
colnames(walnut_Jul_Aug_code_converter)[2] <- 'Jul.model.code'
write.csv(walnut_Jul_Aug_code_converter, 'walnut_Jul_Aug_code_converter.csv', row.names = FALSE)
head(model.scaffold.Jul.Aug.merged)
dim(model.scaffold.Jul.Aug.merged)
summary(model.scaffold.Jul$crop_code)
summary(model.scaffold.Aug$crop_code)
summary(model.scaffold.Jul.Aug.merged$crop_code)
unique(model.scaffold.Jul.Aug.merged$crop_code)
sum(model.scaffold.Aug$crop_code==36, na.rm = TRUE)
sum(model.scaffold.Jul$crop_code==36, na.rm = TRUE)
sum(model.scaffold.Jul.Aug.merged$crop_code==36, na.rm = TRUE)

model.scaffold.subset <- model.scaffold[which(model.scaffold$crop_code==almonds | model.scaffold$crop_code==walnuts | model.scaffold$crop_code==pistachios | model.scaffold$crop_code==grapes), ]
length(unique(model.scaffold.subset$unique_model_code)) #200,092 unique model codes
class(model_points_subset)
mukey_isNA <- unique(model_points_subset$mukey[which(is.na(model_points_subset$meanGW.mm.year))]) #3192 are NA
summary(model_points_subset[model_points_subset$mukey==1403414,])
model_points_subset
model_points_subset$crop_code[model_points_subset$mukey==1403414]
model.scaffold.subset[model.scaffold.subset$unique_model_code==359805,] #this model code should have produced a result
summary(model_points_subset[model_points_subset$mukey==455824,])
model_points_subset$unique_model_code[model_points_subset$mukey==455824 & is.na(model_points_subset$meanGW.mm.year)]
model.scaffold.subset[model.scaffold.subset$unique_model_code==348831,]
allcrops2.0m_AD50[which(allcrops2.0m_AD50$unique_model_code==348831), ]


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