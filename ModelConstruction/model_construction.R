library(raster)
#library(rgdal)
#options(digits = 10, scipen = 999)
options(stringsAsFactors = FALSE)
#rasterOptions(progress = 'window')
mainDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion'
cropscape <- file.path(mainDir, 'CA_cropscape2015')
soildataDir <- file.path(mainDir, 'soils_data')
mu_dir <- file.path(mainDir, 'soils_data/spatial')
comp_data_dir <- file.path(mainDir, 'soils_data/results/summer2017model/CA_all')
results <- file.path(mainDir, 'results')
cropscape_results <- file.path(results, 'rasters/cropscape')
soil_results <- file.path(results, 'rasters/soils')
californiaDir <- 'C:/Users/smdevine/Desktop/SpatialData/CA_counties/government_units'
spatialCIMIS <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/SpatialCIMIS'
PRISMDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/PRISMdaily'
model_scaffoldDir <- file.path(mainDir, 'model_scaffold')
model_results <- file.path(results, 'model_results')

#define crop codes
setwd(cropscape)
cropscape_legend <- read.csv('cropscape_legend.txt', stringsAsFactors = FALSE)
alfalfa_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Alfalfa']
grape_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Grapes']
almond_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Almonds']
walnut_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Walnuts']

setwd(model_scaffoldDir)
#list.files()
model_scaffold <- read.csv("model_scaffold_codes_nocoords_6.9.17.csv")
#head(model_scaffold)
#dim(model_scaffold) #242,714 rows
#lapply(model_scaffold, class)
#length(unique(model_scaffold$mukey)) #5,225 unique mukeys
#get number of cokeys per mukey (can rewrite as function to end up with n set of model matrices for each scenario of allowable depletion assumptions and rooting depth)
setwd(comp_data_dir)
#list.files()
soil_comp_data <- read.csv("CA_all_comps_summary_dbmodified_FINAL2017-07-26.csv")
#head(soil_comp_data)
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
soil_comp_data <- soil_comp_data[ ,c(1:5, 22:26, 32:39)]
# missing_mukeys <- model_scaffold2$mukey[!(model_scaffold2$mukey %in% soil_comp_data$mukey)]
# mukeys <- model_scaffold2$mukey[model_scaffold2$mukey %in% soil_comp_data$mukey]
#setwd(model_scaffoldDir)
#write.csv(soil_comp_data, 'soil_comp_data_sorted.csv', row.names=F)
for (i in seq_len(maxcomps)) {
  soilcomp_rownums <- match(model_scaffold2$mukey, soil_comp_data$mukey)
  temp <- soil_comp_data[soilcomp_rownums, ]
  temp$mukey <- NULL
  temp2 <- cbind(model_scaffold2, temp)
  setwd(file.path(model_scaffoldDir, 'soil_climate_crop/by_component'))
  write.csv(temp2, paste0('model_scaffold_comp', as.character(i), '.csv'), row.names=F) #save the file for modeling purposes later
  model_scaffold2 <- model_scaffold2[-which(model_scaffold2$n_compkeys==i), ] #now get rid of model codes with i number of cokeys.  they don't need to be included in additional model scaffolds
  soilcomp_rownums <- unique(soilcomp_rownums)
  soil_comp_data <- soil_comp_data[-soilcomp_rownums, ] #get rid of the cokeys already covered
}

setwd(file.path(model_scaffoldDir, 'soil_climate_crop/by_component'))
fnames <- list.files(pattern = glob2rx('*csv'))
master.file <- do.call(rbind, lapply(fnames, read.csv))
dim(master.file)
#1,177,027 unique soil components, climate, and crop
j <- which(master.file$comppct_r >= 15)
length(j) #but only 277,477 are major components
sum(master.file$majcompflag=='Yes')
model_scaffold_majcomps_almonds <- master.file[master.file$comppct_r >= 15 & master.file$crop_code==almond_code,]
dim(model_scaffold_majcomps_almonds)
setwd(file.path(model_scaffoldDir, 'run_model/July2017'))
write.csv(model_scaffold_majcomps_almonds, 'model_scaffold_majcomps_almonds.csv', row.names = F)
model_scaffold_majcomps <- master.file[master.file$comppct_r >= 15, ]
dim(model_scaffold_majcomps)
setwd(file.path(model_scaffoldDir, 'run_model/July2017'))
write.csv(model_scaffold_majcomps_almonds, 'model_scaffold_majcomps.csv', row.names = F)
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


#old version of this function before revised SSURGO July 2017 aggregation work
for (i in 1:maxcomps) {
  soilcomp_rownums <- match(model_scaffold2$mukey, soil_comp_data$mukey)
  model_scaffold2$cokey_model <- soil_comp_data$cokey[soilcomp_rownums]
  model_scaffold2$comppct_r <- soil_comp_data$comppct_r[soilcomp_rownums]
  alfalfa_rows <- which(model_scaffold2$crop_code==alfalfa_code)
  almond_rows <- which(model_scaffold2$crop_code==almond_code)
  grape_rows <- which(model_scaffold2$crop_code==grape_code)
  walnut_rows <-which(model_scaffold2$crop_code==walnut_code)
  soilcomp_alf_rownums <- match(model_scaffold2$mukey[alfalfa_rows], soil_comp_data$mukey)
  soilcomp_alm_rownums <- match(model_scaffold2$mukey[almond_rows], soil_comp_data$mukey)
  soilcomp_grp_rownums <- match(model_scaffold2$mukey[grape_rows], soil_comp_data$mukey)
  soilcomp_wln_rownums <- match(model_scaffold2$mukey[walnut_rows], soil_comp_data$mukey)
  model_scaffold2$allowable_depletion <- NA
  model_scaffold2$allowable_depletion[alfalfa_rows] <- round(soil_comp_data$AD_alfalfa_cmH2O[soilcomp_alf_rownums], 3) #3 decimal places is more than precise enough
  model_scaffold2$allowable_depletion[almond_rows] <- round(soil_comp_data$AD_almonds_cmH2O[soilcomp_alm_rownums], 3)
  model_scaffold2$allowable_depletion[grape_rows] <- round(soil_comp_data$AD_grapes_cmH2O[soilcomp_grp_rownums], 3)
  model_scaffold2$allowable_depletion[walnut_rows] <- round(soil_comp_data$AD_walnuts_cmH2O[soilcomp_wln_rownums], 3)
  #model_scaffold2 <- model_scaffold2[-which(is.na(model_scaffold2$allowable_depletion)),] #get rid of NA rows; no reason to model NAs; this creates problems
  setwd(file.path(model_scaffoldDir, 'paw_check/mean_root_depth'))
  write.csv(model_scaffold2, paste0('model_scaffold_comp', as.character(i), '.csv'), row.names=F) #save the file for modeling purposes later
  model_scaffold2 <- model_scaffold2[-which(model_scaffold2$n_compkeys==i), ] #now get rid of model codes with i number of cokeys.  they don't need to be included in additional model scaffolds
  soilcomp_rownums <- unique(soilcomp_rownums)
  soil_comp_data <- soil_comp_data[-soilcomp_rownums, ] #get rid of the cokeys already covered
}