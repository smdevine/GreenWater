library(prism)
library(raster)
PRISMdir <- "D:/PRISM/PRISM_daily"
cellsofinterestDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/results/model_scaffold'
for (i in 2009:2016) {
  if (file.exists(file.path(PRISMdir, as.character(i))) == FALSE) {
    dir.create(file.path(PRISMdir, as.character(i)))
  }
  options(prism.path = paste0(PRISMdir, as.character(i)))
  get_prism_dailys(type='ppt', minDate = paste0(as.character(i),'-01-01'), maxDate = paste0(as.character(i), '-12-31'), keepZip = FALSE)
}
setwd(file.path(PRISMdir, '2003'))

setwd(cellsofinterestDir)
cellsofinterest <- read.csv("PRISM_cells_unique.csv")
cellsofinterest <- cellsofinterest[order(cellsofinterest$PRISM_cells), ]
startyear <- '2003'
endyear <- '2016' #only available through 11/30/2016 on 6/9/17 download date
startdate <- strptime(paste0("10/1/", startyear), '%m/%d/%Y')
enddate <- strptime(paste0("11/30/", endyear), '%m/%d/%Y')
datesequence <- seq.Date(from=as.Date(startdate), to=as.Date(enddate), by='day')
datesequence_colnames <- format.Date(datesequence, '%b_%d_%Y')
prism_data <- as.data.frame(matrix(nrow=length(cellsofinterest), ncol=(length(datesequence)+1)))
colnames(prism_data) <- c('cell_number', datesequence_colnames)
prism_data$cell_number <- cellsofinterest
i <- 1
for (i in 2:length(datesequence)) {
  day <- format.Date(datesequence[i], '%d')
  mnth <- format.Date(datesequence[i], '%m')
  yr <- format.Date(datesequence[i], '%Y')
  setwd(file.path(PRISMdir, yr, paste0('PRISM_ppt_stable_4kmD2_', yr, mnth, day, '_bil')))
  PRISM <- raster(paste0('PRISM_ppt_stable_4kmD2_', yr, mnth, day, '_bil.bil'))
  prism_data[ ,i+1] <- extract(PRISM, cellsofinterest)
  print(i)
}
setwd(cellsofinterestDir)
write.csv(prism_data, 'PRISM_data.csv', row.names = F)
