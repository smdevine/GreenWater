#this is pre-processing to produce file for Rosetta predictions in Python
mainDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion'
SSURGOdir <- file.path(mainDir, 'soils_data')
results <- file.path(mainDir, 'soils_data/results')
setwd(SSURGOdir)
ssurgo_tables <- list.files(SSURGOdir, pattern = glob2rx('*.csv'))
#comp = 2, horizon = 3, and mu_area = 5, mu_table = 4, VG_parameters = 6
ssurgo_horizon <-read.csv(ssurgo_tables[3], stringsAsFactors = FALSE, na.strings=c(""," ", 'NA'))
ssurgo_comp <-read.csv(ssurgo_tables[2], stringsAsFactors = FALSE, na.strings=c(""," ", 'NA'))
ssurgo_mu <- read.csv(ssurgo_tables[4], stringsAsFactors = FALSE, na.strings=c(""," ", 'NA'))
ssurgo_horizon$silttotal_r <- 100 - ssurgo_horizon$claytotal_r - ssurgo_horizon$sandtotal_r
#find entries where sand + clay > 100 and edit (2 horizons)
i <- which(ssurgo_horizon$silttotal_r<0)
ssurgo_horizon$cokey[i]
ssurgo_comp[which(ssurgo_comp$cokey==10679827), ]
ssurgo_mu[which(ssurgo_mu$mukey==464024), ]
ssurgo_mu[which(ssurgo_mu$muname=='Fluvents'), ] #only one instance of this
ssurgo_horizon$silttotal_r[i] <- 0
ssurgo_horizon$claytotal_r[i] <- 100 - ssurgo_horizon$sandtotal_r[i]
#find entries where wthirdbar_r < wfifteenbar_r and convert these to NA (41 horizons)
i <- which(ssurgo_horizon$wthirdbar_r <= ssurgo_horizon$wfifteenbar_r)
ssurgo_horizon$wfifteenbar_r[i] <- NA
ssurgo_horizon$wthirdbar_r[i] <- NA
#find entries where clay + sand was equal to 0
i <- which(ssurgo_horizon$silttotal_r==100)
ssurgo_horizon <- ssurgo_horizon[-i, ]
#find entries where (wthirdbar_r - wfifteenbar_r) > awc_r (5518 horizons)
i <- which((ssurgo_horizon$wthirdbar_r - ssurgo_horizon$wfifteenbar_r)/100 > ssurgo_horizon$awc_r)
ssurgo_horizon$awc_check <- ssurgo_horizon$awc_r - (ssurgo_horizon$wthirdbar_r - ssurgo_horizon$wfifteenbar_r)/100

#get rid of horizons without texture data
i <- which(is.na(ssurgo_horizon$claytotal_r) | is.na(ssurgo_horizon$sandtotal_r))
ssurgo_horizon <- ssurgo_horizon[-i, ]
setwd(results)
write.csv(ssurgo_horizon, 'chorizon.csv', row.names = FALSE)
