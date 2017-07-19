#key to the SDA_query function is to get the correct type of join and
#to be explicit about which table the desired attribute is coming from if there
#is more than one instance of that attribute (eg. mukey or cokey)
#note the abbreviation of table names following a table name within the SDA_query
#function is not necessary 
library(soilDB)
options(stringsAsFactors = FALSE)
mainDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion'
soilsDir <- file.path(mainDir, 'soils_data')
setwd(soilsDir)
mu_area <- read.csv("farmland_mu_area.csv") #these are the soil mapunits of interest
areasymbols.needed <- unique(mu_area$areasymbol) #could have done the SQL style queries by mapunit
areasymbols.needed <- areasymbols.needed[order(areasymbols.needed)]

query_mapunit <- function() {
  SDA_query(paste0("SELECT l.areasymbol, mu.mukey, mu.muname, muacres, aws0150wta, aws0100wta, aws050wta 
    FROM legend l
      INNER JOIN mapunit mu ON mu.lkey = l.lkey
        LEFT OUTER JOIN muaggatt muag ON muag.mukey = mu.mukey
    WHERE l.areasymbol LIKE 'CA%'"))
}
mu_data <- query_mapunit()
dim(mu_data)
head(mu_data)
setwd(file.path(soilsDir, 'soilDB_query'))
write.csv(mu_data, paste0('CA_all_mu_data_', Sys.Date(), '.csv'), row.names = FALSE)

query_component <- function() {
  SDA_query(paste0("SELECT mu.mukey, comp.cokey, compname, comppct_r, majcompflag, reskind, resdept_r, resdepb_r
    FROM legend l
      INNER JOIN mapunit mu ON mu.lkey = l.lkey
        LEFT OUTER JOIN component comp ON comp.mukey = mu.mukey
          LEFT OUTER JOIN corestrictions cores ON cores.cokey = comp.cokey
    WHERE l.areasymbol LIKE 'CA%'"))
}

comp_data <- query_component()
setwd(file.path(soilsDir, 'soilDB_query'))
write.csv(comp_data, paste0('CA_all_comp_data_', Sys.Date(), '.csv'), row.names = FALSE)
dim(comp_data)
length(unique(comp_data$cokey))
length(unique(comp_data$mukey))
length(which(is.na(comp_data$cokey)))

query_horizon <- function(x) { #this will not return cokey NAs
  print(x)
  SDA_query(paste0("SELECT mu.mukey, comp.cokey, ch.chkey, hzname, hzdept_r, hzdepb_r, awc_r, ec_r, claytotal_r, silttotal_r, sandtotal_r, dbthirdbar_r, wthirdbar_r, wfifteenbar_r, ksat_r, fragvol_r, fragsize_r
    FROM legend l
      INNER JOIN mapunit mu ON mu.lkey = l.lkey
        INNER JOIN component comp ON comp.mukey = mu.mukey
          LEFT OUTER JOIN chorizon ch on ch.cokey = comp.cokey
            LEFT OUTER JOIN chfrags chf on chf.chkey = ch.chkey
    WHERE l.areasymbol = '", x, "'"))
}

horizon_data <- do.call(rbind, lapply(areasymbols.needed, query_horizon))
dim(horizon_data)
length(unique(horizon_data$cokey))
setwd(file.path(soilsDir, 'soilDB_query'))
write.csv(horizon_data, paste0('CA_all_horizon_data_', Sys.Date(), '.csv'), row.names = FALSE)

#mapunit level data needed
#mukey muname aws0150wta aws0100wta aws050wta

#component level data needed
#mukey	cokey	compname	comppct_r	majcompflag	reskind	resdept_r	resdepb_r

#horizon level data needed
#cokey	hzname	hzdept_r	hzdepb_r	awc_r	ec_r	claytotal_r	sandtotal_r	dbthirdbar_r	wthirdbar_r	wfifteenbar_r	fragvol_r_sum	chkey

#see ReturningSoilTextureRelatedAttributes.pdf for a comprehensize SQL query example