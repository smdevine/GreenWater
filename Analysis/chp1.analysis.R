#TO-DO
# (1) add spatial CIMIS ETo data to almond_points_allyrs
# (2) test green water availability models for almond_points_allyrs
points.resultsDir <- 'D:/Allowable_Depletion/results'
library(raster)
library(extrafont)
library(extrafontdb)
#font_import() #only needs to be done once?

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
MakeBP <- function(varname, var_df, fname_header, bxfill, yaxis_lab, dirname, years=2004:2016) {
  loadfonts(quiet=TRUE, device='win')
  bp <- boxplot(varname ~ Model.Year, data=var_df, plot=FALSE)
  for (i in 1:ncol(bp$stats)) {
    df <- var_df[which(var_df$Model.Year==years[i]),]
    bp$stats[,i] <- CustomBP(df[[varname]])
    
    setwd(file.path(SepResultsDir, 'figures', dirname))
    png(paste0(fname_header, '.', varname, '.png', sep = ''), family = 'Book Antiqua', width = 7, height = 5, units = 'in', res = 600)
    par(mai=c(0.9, 0.9, 0.2, 0.2))
    bxp(bp, outline = FALSE, boxfill=bxfill, las=2, ylab='', xlab='')
    mtext(text='Year', side=1, line=3.5)
    mtext(text=yaxis_lab, side=2, line=3.5)
    dev.off()
  }
}

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

#read in summary data for each scenario to summarize data with the help of cell counts
collect.stats <- function(cropname) {
  setwd(file.path(points.resultsDir, cropname))
  fnames <- list.files(pattern = glob2rx('*.csv'))
  for (j in seq_along(fnames)) {
    summary.fname <- gsub('results_points_rounded.csv', '', fnames[j])
    setwd(file.path(points.resultsDir, cropname))
    result <- read.csv(fnames[j], stringsAsFactors = FALSE)
    result <- result[ ,-2] #don't need the PAW data twice in different units
    result <- result[ ,!(names(result) %in% c('unique_model_code', 'mukey', 'compnames', 'cokeys', 'PRISMcellnumber', 'CIMIScellnumber', 'Model.Year'))] #don't need summary stats for these columns
    varnames <- colnames(result)
    varnames <- varnames[-which(varnames=='cellcounts30m2')]
    summary_result <- as.data.frame(matrix(data=NA, nrow=length(varnames), ncol=12))
    colnames(summary_result) <- c('varname', 'Min', 'Qu0.005', 'Qu0.025', 'Qu0.25', 'Median', 'Mean', 'Qu0.75', 'Qu0.975', 'Qu0.995', 'Max', 'StDev')
    row.names(summary_result) <- varnames
    for (i in seq_along(result)) {
      if (colnames(result)[i]=='cellcounts30m2') {
        next
      }
      varname <- colnames(result)[i]
      data.expanded <- rep(result[,i], times=result$cellcounts30m2)
      stats.data <- data.frame(varname=varname, Min=min(data.expanded, na.rm=TRUE), Qu0.005=quantile(data.expanded, 0.005, na.rm = TRUE), Qu0.025=quantile(data.expanded, 0.025, na.rm = TRUE), Qu0.25=quantile(data.expanded, 0.25, na.rm = TRUE), Median=median(data.expanded, na.rm = TRUE), Mean=mean(data.expanded, na.rm = TRUE), Qu0.75=quantile(data.expanded, 0.75, na.rm = TRUE), Qu0.975=quantile(data.expanded, 0.975, na.rm = TRUE), Qu0.995=quantile(data.expanded, 0.995, na.rm = TRUE), Max=max(data.expanded, na.rm = TRUE), StDev=sd(data.expanded, na.rm = TRUE), row.names = NULL)
      stats.data[,2:ncol(stats.data)] <-round(stats.data[,2:ncol(stats.data)], 1)
      stats.data$varname <- as.character(stats.data$varname)
      summary_result[varname,] <- stats.data #indexing by row.name here
    }
    if (!dir.exists(file.path(points.resultsDir, cropname, 'allyrs_stats'))) {
      dir.create(file.path(points.resultsDir, cropname, 'allyrs_stats'))
    }
    setwd(file.path(points.resultsDir, cropname, 'allyrs_stats'))
    write.csv(summary_result, paste0(summary.fname, '_summarystats.csv'), row.names = FALSE)
  }
}
collect.stats('walnut.mature')
collect.stats('pistachios')
collect.stats('almond.mature')
collect.stats('grapes.table')
collect.stats('grapes.wine')
#these results don't inlcude the P.winter or ETo.winter columns
collect.stats('alfalfa.CV')
collect.stats('alfalfa.intermountain')
#this result also does not include the GW.capture.net column
collect.stats('alfalfa.imperial')


test <- read.csv()

#make a hist with the expanded data
hist <- hist(data.expanded, main=NULL, xlab = varname)

#make a box plot of green water availability by year [not finished]
gw_bp <- boxplot(GW.ET.growing ~ Model.Year, data=almond_points_allyrs, plot=FALSE)
years <- 2004:2016
for (i in 1:ncol(gw_bp$stats)) {
  df <- almond_points_allyrs[which(almond_points_allyrs$Model.Year==years[i]),]
  gw_bp$stats[,i] <- CustomBP(df$GW.ET.growing)
  
}

#make a Blue Water boxplot by year
bw_bp <- boxplot(Irr.app.total ~ Model.Year, data=almond_points_allyrs, plot=FALSE)
for (i in 1:ncol(bw_bp$stats)) {
  df <- almond_points_allyrs[which(almond_points_allyrs$Model.Year==years[i]),]
  bw_bp$stats[,i] <- CustomBP(df$Irr.app.total)
}
setwd(file.path(SepResultsDir, 'figures'))
png(paste('almond2.0m50ADbluewaterdemand.png', sep = ''), family = 'Book Antiqua', width = 7, height = 5, units = 'in', res = 600)
par(mai=c(0.9, 0.9, 0.2, 0.2))
bxp(bw_bp, outline = FALSE, boxfill='lightblue', las=2, ylab='', xlab='')
mtext(text='Year', side=1, line=3.5)
mtext(text='Irrigation applied (mm)', side=2, line=3.5)
dev.off()

#make an ET.growing boxplot
et_bp <- boxplot(ET.growing ~ Model.Year, data=almond_points_allyrs, plot=FALSE)
for (i in 1:ncol(et_bp$stats)) {
  df <- almond_points_allyrs[which(almond_points_allyrs$Model.Year==years[i]),]
  et_bp$stats[,i] <- CustomBP(df$ET.growing)
}
setwd(file.path(SepResultsDir, 'figures'))
png(paste('almond2.0m50AD.ET.growing.png', sep = ''), family = 'Book Antiqua', width = 7, height = 5, units = 'in', res = 600)
par(mai=c(0.9, 0.9, 0.2, 0.2))
bxp(et_bp, outline = FALSE, boxfill='orange', las=2, ylab='', xlab='')
mtext(text='Year', side=1, line=3.5)
mtext(text='Growing season ET (mm)', side=2, line=3.5)
dev.off()

#make an annual P boxplot
P_bp <- boxplot(annual.P ~ Model.Year, data=almond_points_allyrs, plot=FALSE)
for (i in 1:ncol(P_bp$stats)) {
  df <- almond_points_allyrs[which(almond_points_allyrs$Model.Year==years[i]),]
  P_bp$stats[,i] <- CustomBP(df$annual.P)
}
setwd(file.path(SepResultsDir, 'figures'))
png(paste('almond.annual.P.png', sep = ''), family = 'Book Antiqua', width = 7, height = 5, units = 'in', res = 600)
par(mai=c(0.9, 0.9, 0.2, 0.2))
bxp(P_bp, outline = FALSE, boxfill='blue', las=2, ylab='', xlab='')
mtext(text='Year', side=1, line=3.5)
mtext(text='Annual precipitation (mm)', side=2, line=3.5)
dev.off()

#single year combined histogram and boxplot of blue water
nf <- layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,3))
par(mar=c(3.1, 3.1, 1.1, 2.1))
boxplot(almond_points_allyrs$Irr.app.total[which(almond_points_allyrs$Model.Year==2016)], horizontal=TRUE,  outline=TRUE, frame=F, col = "lightblue")
hist(almond_points_allyrs$Irr.app.total[which(almond_points_allyrs$Model.Year==2016)], col='lightblue')

#plot of BW vs. GW
setwd(file.path(SepResultsDir, 'figures'))
png(paste('almond2.0m50AD_BWvsGW.png', sep = ''), family = 'Book Antiqua', width = 5, height = 5, units = 'in', res = 600)
par(mai=c(0.9, 0.9, 0.2, 0.2))
smoothScatter(x=almond_points_allyrs$GW.ET.growing, y=almond_points_allyrs$Irr.app.total, xlab="" , ylab="")
mtext(text='Growing season green water availability', side=1, line=3.5)
mtext(text='Blue water demand (mm)', side=2, line=3.5)
dev.off()
#stats for BW vs. GW
lm(GW.ET.growing ~ Irr.app.total, data=almond_points_allyrs)

#plot of paw vs. mean annual GW (not all years)
setwd(file.path(SepResultsDir, 'figures'))
png(paste('almond2.0m50AD_PAWvsmean.GW.png', sep = ''), family = 'Book Antiqua', width = 5, height = 5, units = 'in', res = 600)
par(mai=c(0.9, 0.9, 0.2, 0.2))
smoothScatter(x=almond_points$paw_mm_2.0m, y=almond_points$GW.ET.growing, xlab="" , ylab="", xlim=c(0, 600))
mtext(text='Soil plant available water storage (mm)', side=1, line=2)
mtext(text='Mean growing season green water availability (mm)', side=2, line=2)
dev.off()

#plot of paw vs. mean annual GW (not all years)
setwd(file.path(SepResultsDir, 'figures'))
png(paste('almond2.0m50AD_Precip.vs.mean.GW.png', sep = ''), family = 'Book Antiqua', width = 5, height = 5, units = 'in', res = 600)
par(mai=c(0.9, 0.9, 0.2, 0.2))
smoothScatter(x=almond_points_allyrs$annual.P, y=almond_points_allyrs$GW.ET.growing, xlab="" , ylab="")
mtext(text='Annual precipitation (mm)', side=1, line=2)
mtext(text='Growing season green water availability (mm)', side=2, line=2)
dev.off()

#model GW vs. P + PAW
summary(lm(GW.ET.growing ~ annual.P + I(annual.P^2) + paw_mm_2.0m + I(paw_mm_2.0m^2) + paw_mm_2.0m*annual.P + I(paw_mm_2.0m*annual.P^2), data=almond_points_allyrs))
summary(lm(GW.ET.growing ~ annual.P + I(annual.P^2) + paw_mm_2.0m + I(paw_mm_2.0m^2) + paw_mm_2.0m*annual.P + I(paw_mm_2.0m*annual.P^2) + ET.growing + I(ET.growing^2), data=almond_points_allyrs))

#create results rasters for maps
RasterBuild(almond2.0m_AD50, "GW.ET.growing", 'almondGW.ET.growing.Sep2017runs.tif', mean, na.rm=TRUE)




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
setwd(file.path(resultsDir, 'rasters/Sep2017'))
almond_gw_et_raster <- raster('almondGW.ET.growing.Sep2017runs.tif')



#some green water modeling
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


#get cell numbers of interest to see if this works faster
cell_numbers_of_interest <- Which(!is.na(raster.model.codes), cells = TRUE)
unique_model_codes <- raster.model.codes[cell_numbers_of_interest]
cellnums_to_modelcode <- cbind(cell_numbers_of_interest, unique_model_codes)
length(cellnums_to_modelcode)
setwd('C:/Users/smdevine/Desktop/Allowable_Depletion/model_scaffold/run_model/Aug2017')
write.csv(cellnums_to_modelcode, 'cellnumbers_to_modelcodes.csv', row.names = FALSE)
cell_numbers_of_interest <- read.csv('cellnumbers_to_modelcodes.csv', stringsAsFactors = FALSE)


#code for working with irrigation dates
#now, aggregate mean date to 1st irrigation for green water ET by mukey & model.code

almond2.0m_AD50$Irr.1[which(almond2.0m_AD50$Irr.1=='1900-01-01')] <- NA
almond2.0m_AD50$Irr.1 <- as.Date(almond2.0m_AD50$Irr.1, format='%Y-%m-%d')
almond2.0m_AD50$Irr.1.doy <- as.integer(format.Date(almond2.0m_AD50$Irr.1, '%j'))
first.irr.mean <- tapply(almond2.0m_AD50$Irr.1.doy, almond2.0m_AD50$unique_model_code_final, mean, na.rm=TRUE)
mukeys <- tapply(almond2.0m_AD50$mukey, almond2.0m_AD50$unique_model_code_final, unique)
comppct_r <- tapply(almond2.0m_AD50$comppct_r, almond2.0m_AD50$unique_model_code_final, unique)
modelcode <- tapply(almond2.0m_AD50$unique_model_code, almond2.0m_AD50$unique_model_code_final, unique)
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
