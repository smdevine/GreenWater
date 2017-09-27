#TO-DO
# (1) add spatial CIMIS ETo data to almond_points_allyrs
# (2) test green water availability models for almond_points_allyrs

library(raster)
library(extrafont)
library(extrafontdb)
#font_import() #only needs to be done once?
options(digits = 22)
max_modified <- function(x) {
  if(all(is.na(x))) {
    return(NA)
  }
  else {max(x, na.rm = TRUE)}
}
min_modified <- function(x) {
  if(all(is.na(x))) {
    return(NA)
  }
  else {min(x, na.rm = TRUE)}
}

#this aggregates the model results by map unit, so that there are no duplicate unique model codes in the results (i.e. unique model codes with more than one major component have their results averaged as a component weighted average).  For this function, results for all model years are maintained, whereas in AggregateResults() below, multiple years data is compressed into a single statistic.
MUAggregate <- function(df, varname) {
  #df <- almond2.0m_AD50[which(almond2.0m_AD50$Model.Year==2004),]
  year <- df$Model.Year[1]
  print(year)
  compsums <- as.data.frame(tapply(df$comppct_r[!is.na(df[[varname]])], df$unique_model_code[!is.na(df[[varname]])], sum))
  colnames(compsums) <- 'compsums'
  compsums$unique_model_code <- as.integer(rownames(compsums))
  results <- cbind(df[!is.na(df[[varname]]), c(varname, 'comppct_r')], compsums[match(df$unique_model_code[!is.na(df[[varname]])], compsums$unique_model_code), ])
  var.final <- tapply(results[[varname]]*(results$comppct_r/results$compsums), results$unique_model_code, sum)
  var.final <- as.data.frame(var.final)
  colnames(var.final) <- 'var.final'
  var.final$unique_model_code <- rownames(var.final)
  rownames(var.final) <- NULL
  var.final$var.final <- as.numeric(var.final$var.final)
  var.final <- var.final[,c(2,1)]
  colnames(var.final)[2] <- varname
  #var.final$Model.Year <- year
  var.final <- cbind(var.final, df[match(var.final$unique_model_code, df$unique_model_code), 'Model.Year'])
  colnames(var.final)[3] <- 'Model.Year'
  var.final
}

#this aggregates results across all years by mukey and unique model code according to the 'func', such as taking the mean GW.ET.growing for each unique combination of climate, soil, and crop from 2004-2016, with major component weighted averages
AggregateResults <- function(df, varname, func, ...) {
  var.by.year <- tapply(df[[varname]], df$unique_model_code_final, func, ...)
  mukeys <- tapply(df$mukey, df$unique_model_code_final, unique)
  comppct_r <- tapply(df$comppct_r, df$unique_model_code_final, unique)
  modelcode <- tapply(df$unique_model_code, df$unique_model_code_final, unique)
  results <- cbind(var.by.year, mukeys, comppct_r, modelcode)
  results <- as.data.frame(results)
  compsums <- as.data.frame(tapply(results$comppct_r[!is.na(results$var.by.year)], results$modelcode[!is.na(results$var.by.year)], sum))
  colnames(compsums) <- 'compsums'
  compsums$modelcode <- rownames(compsums)
  results <- merge(results, compsums, by='modelcode')
  var.final <- tapply(results$var.by.year*(results$comppct_r/results$compsums), results$modelcode, sum, na.rm=TRUE)
  var.final <- as.data.frame(var.final)
  colnames(var.final) <- 'var.final'
  var.final$unique_model_code <- rownames(var.final)
  var.final$var.final <- as.numeric(var.final$var.final)
  var.final <- var.final[,c(2,1)]
  colnames(var.final)[2] <- varname
  var.final
}

#function to set point values according to the stats produced by 'Aggregate Result
SetPointValues <- function(points_df, var_df, varname){
  points_df[[varname]] <- var_df[[varname]][match(points_df$unique_model_code, var_df$unique_model_code)]
  points_df
}
SetPointPrecipValues_MeanAnnual <- function(points_df){
  points_df$mean.annual.P <- prism.annual.sums$mean.annual.P[match(points_df$PRISMcellnumber, prism.annual.sums$PRISMcellnumber)]
  points_df
}
SetPointValues.AllYrs <- function(var_df, varname, points_df) {
  if (is.null(points_df$Model.Year)) {
    model.year <- var_df$Model.Year[1]
    points_df$Model.Year <- model.year
    points_df[[varname]] <- var_df[[varname]][match(points_df$unique_model_code, var_df$unique_model_code)]
    points_df
  } else {
    model.year <- var_df$Model.Year[1]
    subset_points_df <- points_df[which(points_df$Model.Year==model.year),]
    subset_points_df[[varname]] <- var_df[[varname]][match(subset_points_df$unique_model_code, var_df$unique_model_code)]
    subset_points_df
  }
}

SetPointPrecipValues.AllYrs <- function(df){
  model.year <- as.character(df$Model.Year[1])
  df$annual.P <- prism.annual.sums[[model.year]][match(df$PRISMcellnumber, prism.annual.sums$PRISMcellnumber)]
  df
}

CustomBP <- function(x){
  stats.x <- summary(x) #in this order: (1)Min. (2)1st Qu.  (3)Median    (4)Mean (5)3rd Qu.   (6) Max.    NA's 
  min.x <- stats.x[1]
  q1.x <- stats.x[2]
  med.x <- stats.x[3]
  mean.x <- stats.x[4]
  q3.x <- stats.x[5]
  max.x <- stats.x[6]
  sd.x <- sd(x, na.rm = TRUE)
  c(max(mean.x - 1.96*sd.x, min.x), q1.x, med.x, q3.x, min(mean.x + 1.96*sd.x, max.x))
}

#plotting function
MakeBP <- function(varname, var_df, fname_header, bxfill, yaxis_lab, years=2004:2016) {
  loadfonts(quiet=TRUE, device='win')
  bp <- boxplot(varname ~ Model.Year, data=var_df, plot=FALSE)
  for (i in 1:ncol(bp$stats)) {
    df <- var_df[which(var_df$Model.Year==years[i]),]
    bp$stats[,i] <- CustomBP(df[[varname]])
    setwd(file.path(SepResultsDir, 'figures'))
    png(paste0(fname_header, '.', varname, '.png', sep = ''), family = 'Book Antiqua', width = 7, height = 5, units = 'in', res = 600)
    par(mai=c(0.9, 0.9, 0.2, 0.2))
    bxp(bp, outline = FALSE, boxfill=bxfill, las=2, ylab='', xlab='')
    mtext(text='Year', side=1, line=3.5)
    mtext(text=yaxis_lab, side=2, line=3.5)
    dev.off()
    
  }
}

#playing with Dr trends across rooting assumptions
setwd(file.path(SepResultsDir, 'almond.mature_majcomps/scenario_2.0m50AD'))
fnames <- list.files()
crop.soil.WB <- read.csv('almond.mature2.0mAD50_101839_13095536_2017-09-15.csv', stringsAsFactors = FALSE)
# plot(as.Date(crop.soil.WB$dates), crop.soil.WB$Dr.end, type='l') #needs to be smoothed
# first100 <- head(crop.soil.WB$Dr.end, 100)
# for (i in 1:length(crop.soil.WB$Dr.end)) {
#   if (i > 4) {
#     crop.soil.WB$Dr.end[i] <- mean(crop.soil.WB$Dr.end[(i-4):(i+5)])
#   } else {next}
# }
# first100
# head(crop.soil.WB$Dr.end, 100)
plot(as.Date(crop.soil.WB$dates), crop.soil.WB$Dr.end, type='l', col='green')
dataWY2005 <- crop.soil.WB[which(crop.soil.WB$dates=='2004-10-01'):which(crop.soil.WB$dates=='2005-06-01'), ]
dataWY2011 <- crop.soil.WB[which(crop.soil.WB$dates=='2010-10-01'):which(crop.soil.WB$dates=='2011-06-01'), ]
dataWY2014 <- crop.soil.WB[which(crop.soil.WB$dates=='2013-10-01'):which(crop.soil.WB$dates=='2014-06-01'), ]
dataWY2016 <- crop.soil.WB[which(crop.soil.WB$dates=='2015-10-01'):which(crop.soil.WB$dates=='2016-06-01'), ]
dataWY2017 <- crop.soil.WB[which(crop.soil.WB$dates=='2016-10-01'):which(crop.soil.WB$dates=='2017-06-01'), ]
dataWY2005_365 <- crop.soil.WB[which(crop.soil.WB$dates=='2004-10-01'):which(crop.soil.WB$dates=='2005-09-30'), ]
dataWY2014_365 <- crop.soil.WB[which(crop.soil.WB$dates=='2013-10-01'):which(crop.soil.WB$dates=='2014-09-30'), ]
#format.Date(as.Date(dataWY2011$dates), '%b-%d')
plot(1:length(dataWY2014$Dr.end), dataWY2014$Dr.end, type='l', col='blue', xlab='Dates', ylab='Soil water depletion (mm)', axes=FALSE)
# now tell it that annotations will be rotated by 90* (see ?par)
par(las=2)
# now draw the first axis
axis(1, at=seq(from=1, to=length(dataWY2014$dates), by=21), labels=format.Date(as.Date(dataWY2014$dates), '%b-%d')[seq(from=1, to=length(dataWY2014$dates), by=21)])
#lines(1:length(dataWY2011$Dr.end), dataWY2011$Dr.end, type='l', col='green')
lines(1:length(dataWY2017$Dr.end), dataWY2017$Dr.end, type='l', col='turquoise')
lines(1:length(dataWY2016$Dr.end), dataWY2016$Dr.end, type='l', col='orange')
lines(1:length(dataWY2005$Dr.end), dataWY2005$Dr.end, type='l', col='darkgreen')

#plot whole year's data
plot(1:length(dataWY2014_365$Dr.end), dataWY2014_365$Dr.end, type='l', col='blue', xlab='', ylab='Soil water depletion (mm)', axes=FALSE)
axis(1, at=seq(from=1, to=length(dataWY2005_365$dates), by=31), labels=format.Date(as.Date(dataWY2005_365$dates), '%b')[seq(from=1, to=length(dataWY2005_365$dates), by=31)])
axis(2)
lines(1:length(dataWY2005_365$Dr.end), dataWY2005_365$Dr.end, type='l', col='green')


#build a raster based upon data aggregation process above in AggregateResults func.
RasterBuild <- function(df, varname, rasterfname, func, ...) {
  var.by.year <- tapply(df[[varname]], df$unique_model_code_final, func, ...)
  mukeys <- tapply(df$mukey, df$unique_model_code_final, unique)
  comppct_r <- tapply(df$comppct_r, df$unique_model_code_final, unique)
  modelcode <- tapply(df$unique_model_code, df$unique_model_code_final, unique)
  results <- cbind(var.by.year, mukeys, comppct_r, modelcode)
  results <- as.data.frame(results)
  compsums <- as.data.frame(tapply(results$comppct_r[!is.na(results$var.by.year)], results$modelcode[!is.na(results$var.by.year)], sum))
  colnames(compsums) <- 'compsums'
  compsums$modelcode <- rownames(compsums)
  results <- merge(results, compsums, by='modelcode')
  var.final <- tapply(results$var.by.year*(results$comppct_r/results$compsums), results$modelcode, sum, na.rm=TRUE)
  var.final <- as.data.frame(var.final)
  colnames(var.final) <- 'var.final'
  var.final$unique_model_code <- rownames(var.final)
  var.final$var.final <- as.numeric(var.final$var.final)
  var.final <- var.final[,c(2,1)]
  raster.result <- raster.model.codes
  raster.result[cell_numbers_of_interest$cell_numbers_of_interest] <- var.final$var.final[match(cell_numbers_of_interest$unique_model_codes, var.final$unique_model_code)]
  setwd(file.path(resultsDir, 'rasters/Sep2017'))
  rasterOptions(progress = 'window')
  writeRaster(raster.result, rasterfname, format='GTiff')
}

RasterBuild(almond2.0m_AD50, "GW.ET.growing", 'almondGW.ET.growing.Sep2017runs.tif', mean, na.rm=TRUE)
setwd(file.path(resultsDir, 'rasters/Sep2017'))
almond_gw_et_raster <- raster('almondGW.ET.growing.Sep2017runs.tif')

RasterBuild("ET.growing", 'ET.growing.Aug2017runs.tif', mean, na.rm=TRUE)
RasterBuild("Irr.app.total", 'Irr.app.total.Aug2017runs.tif', mean, na.rm=TRUE)
RasterBuild("E.growing", 'E.growing.Aug2017runs.tif', mean, na.rm=TRUE)
RasterBuild("GW.capture.net", 'GW.capture.net.Aug2017runs.tif', mean, na.rm=TRUE)
RasterBuild("deep.perc.annual", 'deep.perc.annual.Aug2017runs.tif', mean, na.rm=TRUE)
RasterBuild("z2.0m_cmH2O_modified_comp", 'paw.cmH2O.Aug2017runs.tif', unique)
RasterBuild("TEW", 'TEW.surface.Aug2017runs.tif', unique)
RasterBuild("Dr.end.season", 'Dr.end.season.Aug2017runs.tif', mean, na.rm=TRUE)


#some GW modeling
#better to do all of this modelling with the annual data itself; not means;
#use almond_GW_ET_allyrs produced above in place of almond_points
#merging P with almond_points first
almond_points <- SetPointPrecipValues_MeanAnnual(almond_points)
mean(almond_points$mean.annual.P) #281 mm
mean(almond_points$GW.ET.growing, na.rm=TRUE) #171 mm
almond_points$GW.ET.growing[which(almond_points$GW.ET.growing < 0)] <- 0
summary(almond_points$GW.ET.growing)
summary(almond_points$GW.ET.growing/almond_points$mean.annual.P)
hist(almond_points$GW.ET.growing/almond_points$mean.annual.P)
almond_points$GW.ET.to.P <- almond_points$GW.ET.growing/almond_points$mean.annual.P
summary(lm(almond_points$GW.ET.to.P ~ almond_points$paw_mm))
rows.to.sample <- sample(1:nrow(almond_points), 0.02*nrow(almond_points))
#plot of GW.ET:P vs. 
plot(almond_points$paw_mm[rows.to.sample], almond_points$GW.ET.to.P[rows.to.sample])
plot(almond_points$paw_mm[rows.to.sample], almond_points$GW.ET.growing[rows.to.sample])
paw_sd <- sd(almond_points$paw_mm, na.rm = TRUE)
paw_median <- median(almond_points$paw_mm, na.rm=TRUE)
p_sd <- sd(almond_points$mean.annual.P)
p_mean <- mean(almond_points$mean.annual.P)
almond_points$paw_symbol <- NA
almond_points$paw_symbol[which(!is.na(almond_points$paw_mm))] <-rgb(0, 197, 255, maxColorValue = 255)
almond_points$paw_symbol[which(almond_points$paw_mm < (paw_median - 0.43*paw_sd))] <- rgb(190, 232, 255, maxColorValue = 255)
almond_points$paw_symbol[which(almond_points$paw_mm > (paw_median + 0.43*paw_sd))] <- rgb(0, 77, 168, maxColorValue = 255)
plot(almond_points$mean.annual.P[rows.to.sample], almond_points$GW.ET.growing[rows.to.sample], xlab='mean annual P (mm)', ylab='mean green water ET (mm)', col=almond_points$paw_symbol[rows.to.sample])
plot(almond_points$paw_mm[rows.to.sample], almond_points$E.growing[rows.to.sample])
summary(almond_points$GW.ET.growing/almond_points$ET.growing)
hist(almond_points$GW.ET.growing/almond_points$ET.growing)
summary(lm(GW.ET.growing ~ mean.annual.P + paw_mm + ET.growing, data=almond_points))
summary(lm(GW.ET.growing ~ mean.annual.P + I(mean.annual.P^2) + paw_mm, data=almond_points))
summary(lm(GW.ET.growing ~ mean.annual.P + I(mean.annual.P^2) + paw_mm + I(paw_mm^2), data=almond_points))
summary(lm(GW.ET.growing ~ mean.annual.P, data=almond_points))
summary(lm(GW.ET.growing ~ mean.annual.P + I(mean.annual.P^2) + paw_mm + paw_mm*mean.annual.P, data=almond_points))
summary(lm(GW.ET.growing ~ mean.annual.P + I(mean.annual.P^2) + paw_mm + I(paw_mm^2) + paw_mm*mean.annual.P, data=almond_points))
summary(lm(GW.ET.growing ~ mean.annual.P + paw_mm + paw_mm*mean.annual.P, data=almond_points))
summary(lm(GW.ET.growing ~ mean.annual.P + I(mean.annual.P^2) + paw_mm + I(paw_mm^2) + ET.growing + I(ET.growing^2) + paw_mm*mean.annual.P, data=almond_points))
summary(lm(GW.ET.growing ~ mean.annual.P + I(mean.annual.P^2) + paw_mm + I(paw_mm^2) + paw_mm*mean.annual.P + I(paw_mm*mean.annual.P^2), data=almond_points))



#build a raster for a single year's data


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
raster.model.codes <- raster('model.codes.Aug2017.tif')
raster.maxGW <- subs(raster.model.codes, max_GW_ET, by=1, which=2) #this is much faster than rasterizing points
writeRaster(raster.maxGW, 'maxGW.ET.Aug2017runs.tif', format='GTiff')

#get cell numbers of interest to see if this works faster
cell_numbers_of_interest <- Which(!is.na(raster.model.codes), cells = TRUE)
unique_model_codes <- raster.model.codes[cell_numbers_of_interest]
cellnums_to_modelcode <- cbind(cell_numbers_of_interest, unique_model_codes)
length(cellnums_to_modelcode)
setwd('C:/Users/smdevine/Desktop/Allowable_Depletion/model_scaffold/run_model/Aug2017')
write.csv(cellnums_to_modelcode, 'cellnumbers_to_modelcodes.csv', row.names = FALSE)
cell_numbers_of_interest <- read.csv('cellnumbers_to_modelcodes.csv', stringsAsFactors = FALSE)
# or like this (and this also returns the cell values ( p[, 3] ): 
p <- rasterToPoints(ras, fun=function(x){x>50}) 
cellFromXY(ras, p[,1:2])

#formula for converting from mm GW to acre-feet GW
sum(!is.na(model_points_sp$maxGW.mm.year))*900*mean(model_points_sp$maxGW.mm.year, na.rm=TRUE)/(1000*1233.48) #2,465,077 acre-feet for 2,935,306 acres
#acre-feet of green water
table(model_points_sp$crop_code[which(is.na(model_points_sp$maxGW.mm.year))])



#code for working with irrigation dates
#now, aggregate mean date to 1st irrigation for green water ET by mukey & model.code
allcrops2.0m_AD50$Irr.1[which(allcrops2.0m_AD50$Irr.1=='1900-01-01')] <- NA
test <- allcrops2.0m_AD50$Irr.1
test <- as.integer(test) #this coerces text to NA
date_integer_indices <- which(!is.na(test))
allcrops2.0m_AD50$Irr.1[date_integer_indices] <- as.character(as.Date(as.integer(allcrops2.0m_AD50$Irr.1[date_integer_indices]), origin='1970-01-01'))
allcrops2.0m_AD50$Irr.1[which(allcrops2.0m_AD50$Irr.1=='1900-01-01')] <- NA
allcrops2.0m_AD50$Irr.1 <- as.Date(allcrops2.0m_AD50$Irr.1, format='%Y-%m-%d')
class(allcrops2.0m_AD50$Irr.1)
allcrops2.0m_AD50$Irr.1.doy <- as.integer(format.Date(allcrops2.0m_AD50$Irr.1, '%j'))
first.irr.mean <- tapply(allcrops2.0m_AD50$Irr.1.doy, allcrops2.0m_AD50$unique_model_code_final, mean, na.rm=TRUE)
mukeys <- tapply(allcrops2.0m_AD50$mukey, allcrops2.0m_AD50$unique_model_code_final, unique)
comppct_r <- tapply(allcrops2.0m_AD50$comppct_r, allcrops2.0m_AD50$unique_model_code_final, unique)
modelcode <- tapply(allcrops2.0m_AD50$unique_model_code, allcrops2.0m_AD50$unique_model_code_final, unique)
results <- cbind(first.irr.mean, mukeys, comppct_r, modelcode)
results <- as.data.frame(results)
compsums <- as.data.frame(tapply(results$comppct_r[!is.na(results$first.irr.mean)], results$modelcode[!is.na(results$first.irr.mean)], sum))
colnames(compsums) <- 'compsums'
compsums$modelcode <- rownames(compsums)
results <- merge(results, compsums, by='modelcode')
first.irr.mean <- tapply(results$first.irr.mean*(results$comppct_r/results$compsums), results$modelcode, sum, na.rm=TRUE)
length(unique(results$modelcode)) #193820 results
first.irr.mean <- as.data.frame(first.irr.mean)
colnames(first.irr.mean) <- 'irr1.doy'
first.irr.mean$unique_model_code <- rownames(first.irr.mean)
first.irr.mean$irr1.doy <- as.integer(first.irr.mean$irr1.doy)
dim(first.irr.mean) #193,820  model codes with data
first.irr.mean <- first.irr.mean[,c(2,1)]
system.time(raster.irr1.meandoy <- subs(raster.model.codes, first.irr.mean, by=1, which=2, filename='irr1.meandoy.Aug2017runs.tif', format='GTiff'))
setwd(file.path(resultsDir, 'rasters/Aug2017'))
writeRaster(raster.irr1.meandoy, 'irr1.meandoy.Aug2017runs.tif', format='GTiff') #this took 65 minutes
cellStats(raster.irr1.meandoy, stat='mean', na.rm=TRUE) #mean is DOY 122

#almond data exploration
sum(almond_gw_et$GW.ET.growing < 0, na.rm = TRUE) #only 596 c
hist(almond_gw_et$GW.ET.growing)

#almond data exploration -- this is results in the model scaffold structure; it has not been joined back to spatial representation
sum(almond2.0m_AD50$GW.ET.growing < 0, na.rm = TRUE) #2543 less than 0;
summary(almond2.0m_AD50$GW.ET.growing)
hist(almond2.0m_AD50$GW.ET.growing)
mean(almond2.0m_AD50$GW.ET.growing, na.rm = TRUE)
hist(tapply(almond2.0m_AD50$GW.ET.growing, almond2.0m_AD50$Model.Year, mean, na.rm=TRUE))
hist(tapply(almond2.0m_AD50$Irr.app.total, almond2.0m_AD50$Model.Year, mean, na.rm=TRUE))
plot(tapply(almond2.0m_AD50$Irr.app.total, almond2.0m_AD50$Model.Year, mean, na.rm=TRUE), tapply(almond2.0m_AD50$GW.ET.growing, almond2.0m_AD50$Model.Year, mean, na.rm=TRUE))
plot(tapply(almond2.0m_AD50$ET.growing, almond2.0m_AD50$Model.Year, mean, na.rm=TRUE), tapply(almond2.0m_AD50$Irr.app.total, almond2.0m_AD50$Model.Year, mean, na.rm=TRUE))
plot(2003:2017, tapply(almond2.0m_AD50$ET.growing, almond2.0m_AD50$Model.Year, mean, na.rm=TRUE))
plot(tapply(almond2.0m_AD50$z2.0m_cmH2O_modified_comp, almond2.0m_AD50$unique_model_code_final, mean, na.rm=TRUE), tapply(almond2.0m_AD50$GW.ET.growing, almond2.0m_AD50$unique_model_code_final, mean, na.rm=TRUE))
almond2.0m_paw <- as.data.frame(tapply(almond2.0m_AD50$z2.0m_cmH2O_modified_comp, almond2.0m_AD50$unique_model_code_final, unique))
colnames(almond2.0m_paw) <- 'PAW_cm'
hist(almond2.0m_paw$PAW_cm)
boxplot()
head(which(almond2.0m_AD50$GW.ET.growing < 0))
almond2.0m_AD50[476990,]
n <- 25
mean(almond2.0m_AD50$E.annual[(n*15-14):(n*15)], na.rm = TRUE)

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
