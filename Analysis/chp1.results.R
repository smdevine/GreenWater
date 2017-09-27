
SepResultsDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/results/Sep2017'
modelscaffoldDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/model_scaffold/run_model/Sep2017'
setwd(modelscaffoldDir)
cell_numbers_of_interest <- read.csv('cellnumbers_to_modelcodes.csv', stringsAsFactors = FALSE)
raster.model.codes <- raster('model.codes.Aug2017.tif')
prism.data <- read.csv('PRISM.precip.data.updated9.13.17.csv', stringsAsFactors = FALSE)
cimis.data <- read.csv('SpatialCIMIS.ETo.updated9.13.17.csv', stringsAsFactors = FALSE)
resultsDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/results'
scenario.resultsDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/results/scenario results/Sep2017' #this is for most recent runs starting Sept 15 2017; August runs results are in a directory up
setwd(scenario.resultsDir)
list.files()
almond2.0m_AD50 <- read.csv('almond.mature2.0mAD50_FAO56results.csv', stringsAsFactors = FALSE)
almond2.0m_AD50$full_matrix_rownum <- NULL
dim(almond2.0m_AD50) #1206045 rows
walnut2.0m_AD50 <- read.csv('walnut.mature2.0mAD50_FAO56results.csv', stringsAsFactors = FALSE)
walnut2.0m_AD50$full_matrix_rownum <- NULL
dim(walnut2.0m_AD50)
pistachio2.0m_AD50 <- read.csv('pistachios2.0mAD50_FAO56results.csv', stringsAsFactors = FALSE)
dim(pistachio2.0m_AD50)
grapes.wine2.0m_0.5minRDI <- read.csv('grapes.wine2.0mRDI.min0.5_FAO56results.csv', stringsAsFactors = FALSE)
mean(grapes.wine2.0m_0.5minRDI$GW.ET.growing , na.rm = TRUE) #134.931 mm
hist(grapes.wine2.0m_0.5minRDI$GW.ET.growing)
sum(grapes.wine2.0m_0.5minRDI$GW.ET.growing < 0, na.rm = TRUE) #38,591 scenario years less than 0
#grapes.wine2.0m_0.5minRDI$H2O.stress <- NULL
dim(grapes.wine2.0m_0.2minRDI)
allcrops2.0m_AD50 <- rbind(almond2.0m_AD50, walnut2.0m_AD50, pistachio2.0m_AD50, grapes.wine2.0m_0.5minRDI)
dim(allcrops2.0m_AD50) #3,432,480 rows
length(unique(allcrops2.0m_AD50$unique_model_code)) #200,090 unique model codes
#almond scenario analyis

#read-in all points of interest
setwd(file.path(resultsDir, 'data.frames/Aug2017'))
model_points <- read.csv('mukeys_cropcodes_climatecodes_AEA.csv')
model_points$mukey_cropcode <- NULL
model_points$model_code <- NULL
sum(model_points$crop_code==75, na.rm=TRUE) #equal to 547,945.9 ha or 1,353,427 acres, an overestimate for almonds
sum(model_points$crop_code==76, na.rm=TRUE)
almond_points <- model_points[which(model_points$crop_code==75),]
dim(almond_points)
length(unique(almond_points$unique_model_code))

#summarize P and ETo data by water.year
#get the mean water year ETo by cell number
cimis.data$water.year <- cimis.data$year
cimis.data$water.year[which(cimis.data$month >= 10)] <- cimis.data$water.year[which(cimis.data$month >= 10)] + 1
cimis.by.year <- split(cimis.data, cimis.data$water.year)
for (j in 1:length(cimis.by.year)) { #get the unnecessary columns out now
  cimis.by.year[[j]] <- cimis.by.year[[j]][,6:(ncol(cimis.by.year[[j]])-1)]
}
cimis.annual.sums <- do.call(rbind, lapply(cimis.by.year, sapply, sum)) #sapply was necessary so that each "cell" of the results matrix was not returned as a list object
cimis.annual.sums <- t(cimis.annual.sums)
cimis.annual.sums <- as.data.frame(cimis.annual.sums)
cimis.annual.sums$cell_name <- rownames(cimis.annual.sums)
cimis.annual.sums$PRISMcellnumber <- as.integer(gsub('cell_', '', cimis.annual.sums$cell_name))
colnames(cimis.annual.sums)
cimis.annual.sums$mean.annual.P <- apply(cimis.annual.sums[,1:13], 1, mean)


#get the Oct 1 P - bloom.date P for relating to GW.ET
#get the bloom.date P - Sep 30 P

#get the mean water year P by cell number, excluding 2017
prism.data$water.year <- prism.data$year
prism.data$water.year[which(prism.data$month >= 10)] <- prism.data$water.year[which(prism.data$month >= 10)] + 1
prism.by.year <- split(prism.data, prism.data$water.year)
for (j in 1:length(prism.by.year)) { #get the unnecessary columns out now
  prism.by.year[[j]] <- prism.by.year[[j]][,6:(ncol(prism.by.year[[j]])-1)]
}
prism.annual.sums <- do.call(rbind, lapply(prism.by.year, sapply, sum)) #sapply was necessary so that each "cell" of the results matrix was not returned as a list object
prism.annual.sums <- t(prism.annual.sums)
prism.annual.sums <- as.data.frame(prism.annual.sums)
prism.annual.sums$cell_name <- rownames(prism.annual.sums)
prism.annual.sums$PRISMcellnumber <- as.integer(gsub('cell_', '', prism.annual.sums$cell_name))
colnames(prism.annual.sums)
prism.annual.sums$mean.annual.P <- apply(prism.annual.sums[,1:13], 1, mean)

#this is to obtain mean values across years, except for paw, which is static; almond_points_allyrs has the data by year
almond_gw_et <- AggregateResults(almond2.0m_AD50, 'GW.ET.growing', mean, na.rm=TRUE) #this produces 69,080 rows; concordance with above!
almond_paw <- AggregateResults(almond2.0m_AD50, 'z2.0m_cmH2O_modified_comp', unique)
almond_paw$paw_mm_2.0m <- almond_paw$z2.0m_cmH2O_modified_comp*10
almond_irr_app <- AggregateResults(almond2.0m_AD50, 'Irr.app.total', mean, na.rm=TRUE)
almond_et <- AggregateResults(almond2.0m_AD50, 'ET.growing', mean, na.rm=TRUE)
almond_e <- AggregateResults(almond2.0m_AD50, 'E.growing', mean, na.rm=TRUE)
almond_points <- SetPointValues(almond_points, almond_gw_et, 'GW.ET.growing')
almond_points <- SetPointValues(almond_points, almond_e, 'E.growing')
almond_points <- SetPointValues(almond_points, almond_et, 'ET.growing')
almond_points <- SetPointValues(almond_points, almond_paw, 'paw_mm_2.0m')
almond_points$paw_mm <- 10*almond_points$z2.0m_cmH2O_modified_comp
summary(lm(almond_points$GW.ET.growing ~ almond_points$paw_mm_2.0m))

#plots for chapter
#get almond data for all years into almond points.  It works for a dataframe of points with or without multiple model years
almond_GW_ET_allyrs <- do.call(rbind, lapply(split(almond2.0m_AD50, almond2.0m_AD50$Model.Year), MUAggregate, varname='GW.ET.growing'))
head(almond_GW_ET_allyrs)
dim(almond_GW_ET_allyrs) #898,040 rows acrss 13 model years; thus there are 69,080 unique model codes with data
length(unique(almond2.0m_AD50$unique_model_code)) #out of 71,404 total unique model codes for almonds
almond_Irr_app_allyrs <- do.call(rbind, lapply(split(almond2.0m_AD50, almond2.0m_AD50$Model.Year), MUAggregate, varname='Irr.app.total'))
tapply(almond_Irr_app_allyrs$Irr.app.total, almond_Irr_app_allyrs$Model.Year, mean, na.rm=TRUE)
almond_ET_allyrs <- do.call(rbind, lapply(split(almond2.0m_AD50, almond2.0m_AD50$Model.Year), MUAggregate, varname='ET.growing'))
almond_points_allyrs <- do.call(rbind, lapply(split(almond_GW_ET_allyrs, almond_GW_ET_allyrs$Model.Year), SetPointValues.AllYrs, varname='GW.ET.growing', points_df=almond_points))
dim(almond_points_allyrs)
gc()
almond_points_allyrs <- do.call(rbind, lapply(split(almond_Irr_app_allyrs, almond_Irr_app_allyrs$Model.Year), SetPointValues.AllYrs, varname='Irr.app.total', points_df=almond_points_allyrs))
dim(almond_points_allyrs)
gc()
almond_points_allyrs <- do.call(rbind, lapply(split(almond_ET_allyrs, almond_ET_allyrs$Model.Year), SetPointValues.AllYrs, varname='ET.growing', points_df=almond_points_allyrs))
#add paw data with SetPointValues function
almond_points_allyrs <- SetPointValues(almond_points_allyrs, almond_paw, 'paw_mm_2.0m')
dim(almond_points_allyrs)
gc()
almond_points_allyrs <- do.call(rbind, lapply(split(almond_points_allyrs, almond_points_allyrs$Model.Year), SetPointPrecipValues.AllYrs))
gc()

#make a box plot of green water availability by year


  
  
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


#get walnut data into results
walnut_points <- model_points[which(model_points$crop_code==76),]
dim(walnut2.0m_AD50)
dim(walnut_points)
walnut_GW_ET_allyrs <- do.call(rbind, lapply(split(walnut2.0m_AD50, walnut2.0m_AD50$Model.Year), MUAggregate, varname='GW.ET.growing'))
head(walnut_GW_ET_allyrs)
dim(walnut_GW_ET_allyrs) #458,731 rows acrss 13 model years; thus there are 35,287 unique model codes with data
length(unique(walnut2.0m_AD50$unique_model_code)) #out of 71,404 total unique model codes for walnuts
walnut_Irr_app_allyrs <- do.call(rbind, lapply(split(walnut2.0m_AD50, walnut2.0m_AD50$Model.Year), MUAggregate, varname='Irr.app.total'))
tapply(walnut_Irr_app_allyrs$Irr.app.total, walnut_Irr_app_allyrs$Model.Year, mean, na.rm=TRUE)
walnut_ET_allyrs <- do.call(rbind, lapply(split(walnut2.0m_AD50, walnut2.0m_AD50$Model.Year), MUAggregate, varname='ET.growing'))
walnut_points_allyrs <- do.call(rbind, lapply(split(walnut_GW_ET_allyrs, walnut_GW_ET_allyrs$Model.Year), SetPointValues.AllYrs, varname='GW.ET.growing', points_df=walnut_points))
dim(walnut_points_allyrs)
gc()
walnut_points_allyrs <- do.call(rbind, lapply(split(walnut_Irr_app_allyrs, walnut_Irr_app_allyrs$Model.Year), SetPointValues.AllYrs, varname='Irr.app.total', points_df=walnut_points_allyrs))
dim(walnut_points_allyrs)
gc()
walnut_points_allyrs <- do.call(rbind, lapply(split(walnut_ET_allyrs, walnut_ET_allyrs$Model.Year), SetPointValues.AllYrs, varname='ET.growing', points_df=walnut_points_allyrs))
#add paw data with SetPointValues function
walnut_points_allyrs <- SetPointValues(walnut_points_allyrs, walnut_paw, 'paw_mm_2.0m')
dim(walnut_points_allyrs)
gc()
walnut_points_allyrs <- do.call(rbind, lapply(split(walnut_points_allyrs, walnut_points_allyrs$Model.Year), SetPointPrecipValues.AllYrs))
gc()


