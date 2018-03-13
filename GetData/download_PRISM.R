#script to download free daily 4km resolution PRISM precip data.  There are three versions of this PRISM data: (1) "stable," meaning that which is not expected to be further refined, which has an approximate six month lag; (2) "provisional", which is 1-6 months old; and (3) "early", which is data from the current month, up to yesterday's data
#script also includes section to extract raster PRISM precip data for cells of interest relative to this modeling project into a matrix of cells of interest (columns) x days (rows), matching the same format as that used to extract spatial CIMIS raster data (see 'spatialCIMIS.R')
library(prism)
library(raster)
PRISMdir <- "C:/Users/smdevine/Desktop/Allowable_Depletion/PRISMdaily"
cellsofinterestDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/model_scaffold/run_model/Mar2018'
for (i in 2018:2018) {
  if (file.exists(file.path(PRISMdir, as.character(i))) == FALSE) {
    dir.create(file.path(PRISMdir, as.character(i)))
  }
  options(prism.path = paste0(PRISMdir, '/', as.character(i)))
  get_prism_dailys(type='ppt', minDate = paste0(as.character(i),'-01-01'), maxDate = paste0(as.character(i), '-03-08'), keepZip = FALSE) #control annual data window here with minDate and maxDate arguments
}

cellsofinterest <- read.csv(file.path(cellsofinterestDir, "PRISM_cells_unique.csv"), stringsAsFactors = FALSE)
cellsofinterest <- cellsofinterest[order(cellsofinterest$PRISM_cells), ]
cellsofinterest_names <- paste0('cell_', as.character(cellsofinterest))
startyear <- '2003'
endyear <- '2018'
startdate <- strptime(paste0("10/01/", startyear), '%m/%d/%Y')
enddate <- strptime(paste0("03/09/", endyear), '%m/%d/%Y')
datesequence <- seq.Date(from=as.Date(startdate), to=as.Date(enddate), by='day')
prism_data <- as.data.frame(matrix(nrow=length(datesequence), ncol=(length(cellsofinterest)+5)))
colnames(prism_data) <- c('dates', 'month', 'day', 'year', 'DOY', cellsofinterest_names)
prism_data$dates <- format.Date(datesequence, '%m_%d_%Y')
prism_data$month <- as.integer(format.Date(datesequence, '%m'))
prism_data$day <- as.integer(format.Date(datesequence, '%d'))
prism_data$year <- as.integer(format.Date(datesequence, '%Y'))
prism_data$DOY <- as.integer(format.Date(datesequence, '%j'))
for (i in 1:length(datesequence)) {
  day <- format.Date(datesequence[i], '%d')
  mnth <- format.Date(datesequence[i], '%m')
  yr <- format.Date(datesequence[i], '%Y')
  setwd(file.path(PRISMdir, yr))
  if (file.exists(paste0('PRISM_ppt_stable_4kmD2_', yr, mnth, day, '_bil'))) {
    setwd(file.path(PRISMdir, yr, paste0('PRISM_ppt_stable_4kmD2_', yr, mnth, day, '_bil')))
    PRISM <- raster(paste0('PRISM_ppt_stable_4kmD2_', yr, mnth, day, '_bil.bil'))
    prism_data[i, 6:ncol(prism_data)] <- extract(PRISM, cellsofinterest)
    print(i)
    next
  }
  else if (file.exists(paste0('PRISM_ppt_provisional_4kmD2_', yr, mnth, day, '_bil'))) {
    setwd(file.path(PRISMdir, yr, paste0('PRISM_ppt_provisional_4kmD2_', yr, mnth, day, '_bil')))
    PRISM <- raster(paste0('PRISM_ppt_provisional_4kmD2_', yr, mnth, day, '_bil.bil'))
    prism_data[i, 6:ncol(prism_data)] <- extract(PRISM, cellsofinterest)
    print(i)
    next
  }
  else if (file.exists(paste0('PRISM_ppt_early_4kmD2_', yr, mnth, day, '_bil'))) {
    setwd(file.path(PRISMdir, yr, paste0('PRISM_ppt_early_4kmD2_', yr, mnth, day, '_bil')))
    PRISM <- raster(paste0('PRISM_ppt_early_4kmD2_', yr, mnth, day, '_bil.bil'))
    prism_data[i, 6:ncol(prism_data)] <- extract(PRISM, cellsofinterest)
    print(i)
    next
  }
  else(print(paste('No PRISM precip file exists for', datesequence[i])))
}
#check data
summary(prism_data$cell_267008)
which(is.na(prism_data$cell_267008))
#write results to file
setwd(cellsofinterestDir)
prism_data <- prism_data[-nrow(prism_data),]
write.csv(prism_data, file.path(cellsofinterestDir, 'PRISM_precip_data.csv'), row.names = FALSE)
