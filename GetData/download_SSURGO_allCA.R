#key to the SDA_query function is to get the correct type of join and
#to be explicit about which table the desired attribute is coming from if there
#is more than one instance of that attribute (eg. mukey or cokey)
#note the abbreviation of table names following a table name within the SDA_query
#function is not necessary 
library(soilDB)
options(stringsAsFactors = FALSE)
mainDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion'
modelscaffoldDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/model_scaffold/run_model/Mar2018'
soilsDir <- file.path(mainDir, 'soils_data/soilDB_query/Mar2018') #was CA_all for 2017 download
AOI <- read.csv(file.path(modelscaffoldDir, "soil.areasymbols.csv")) #these are the soil mapunits of interest and was produced by model_scaffold.v2.R on 3/6/18
areasymbols.needed <- AOI$areasymbol
areasymbols.needed <- areasymbols.needed[order(areasymbols.needed)]
areasymbols.needed <- gsub('ca', 'CA', areasymbols.needed)

query_tab.metadata <- function() {
  SDA_query(paste0("SELECT areasymbol, tabularversion, tabularverest
    FROM satabularver
    WHERE areasymbol LIKE 'CA%'"))
}
tab_metadata <- query_tab.metadata()
head(tab_metadata)

query_spatial.metadata <- function() {
  SDA_query("SELECT areasymbol, spatialversion, spatialverest
    FROM saspatialver
    WHERE areasymbol LIKE 'CA%'") #this is spatial version 2
}
spatial_metadata <- query_spatial.metadata()
head(spatial_metadata)
write.csv(spatial_metadata[spatial_metadata$areasymbol %in% areasymbols.needed,], file.path(soilsDir, 'spatial_metadata.AOI.csv'), row.names = FALSE)

query_mapunit <- function(x) {
  SDA_query(paste0("SELECT l.areasymbol, l.tabularversion, mu.mukey, mu.muname, muacres, aws0150wta, aws0100wta, aws050wta 
    FROM legend l
      INNER JOIN mapunit mu ON mu.lkey = l.lkey
        LEFT OUTER JOIN muaggatt muag ON muag.mukey = mu.mukey
    WHERE l.areasymbol = '", x, "'"))
}

mu_data <- do.call(rbind, lapply(areasymbols.needed, query_mapunit))
#this was faster
# query_mapunit <- function(x) {
#   SDA_query(paste0("SELECT l.areasymbol, l.tabularversion, mu.mukey, mu.muname, muacres, aws0150wta, aws0100wta, aws050wta 
#     FROM legend l
#       INNER JOIN mapunit mu ON mu.lkey = l.lkey
#         LEFT OUTER JOIN muaggatt muag ON muag.mukey = mu.mukey
#     WHERE l.areasymbol LIKE 'CA%'"))
# }
#mu_data <- query_mapunit()
#mu_data_final <- mu_data_final[mu_data_final$areasymbol %in% areasymbols.needed,]
dim(mu_data)
head(mu_data)
mu_data_final <- merge(mu_data, tab_metadata, by='areasymbol')
mu_data_final <- merge(mu_data_final, spatial_metadata, by='areasymbol')
dim(mu_data_final)
head(mu_data_final)
write.csv(mu_data_final, file.path(soilsDir, paste0('CA_all_mu_data_', Sys.Date(), '.csv')), row.names = FALSE)

query_component <- function(x) {
  SDA_query(paste0("SELECT mu.mukey, comp.cokey, compname, comppct_r, majcompflag, reskind, resdept_r, resdepb_r
    FROM legend l
      INNER JOIN mapunit mu ON mu.lkey = l.lkey
        LEFT OUTER JOIN component comp ON comp.mukey = mu.mukey
          LEFT OUTER JOIN corestrictions cores ON cores.cokey = comp.cokey
    WHERE l.areasymbol = '", x, "'"))
}

comp_data <- do.call(rbind, lapply(areasymbols.needed, query_component))
dim(comp_data)
write.csv(comp_data, file.path(soilsDir, paste0('CA_all_comp_data_', Sys.Date(), '.csv')), row.names = FALSE)

query_horizon <- function(x) { #this will not return cokey NAs
  print(x)
  SDA_query(paste0("SELECT mu.mukey, comp.cokey, ch.chkey, hzname, hzdept_r, hzdepb_r, awc_r, ec_r, claytotal_r, silttotal_r, sandtotal_r, dbthirdbar_r, wthirdbar_r, wfifteenbar_r, ksat_r, fragvol_r, fragsize_r, sandvc_r, sandco_r, sandmed_r, sandfine_r, sandvf_r
    FROM legend l
      INNER JOIN mapunit mu ON mu.lkey = l.lkey
        INNER JOIN component comp ON comp.mukey = mu.mukey
          LEFT OUTER JOIN chorizon ch on ch.cokey = comp.cokey
            LEFT OUTER JOIN chfrags chf on chf.chkey = ch.chkey
    WHERE l.areasymbol = '", x, "'"))
}

horizon_data <- do.call(rbind, lapply(areasymbols.needed, query_horizon))
dim(horizon_data)
length(unique(horizon_data$cokey)) #75978
length(unique(comp_data$cokey)) #75979
write.csv(horizon_data, file.path(soilsDir, paste0('CA_all_horizon_data_', Sys.Date(), '.csv')), row.names = FALSE)

#mapunit level data needed
#mukey muname aws0150wta aws0100wta aws050wta

#component level data needed
#mukey	cokey	compname	comppct_r	majcompflag	reskind	resdept_r	resdepb_r

#horizon level data needed
#cokey	hzname	hzdept_r	hzdepb_r	awc_r	ec_r	claytotal_r	sandtotal_r	dbthirdbar_r	wthirdbar_r	wfifteenbar_r	fragvol_r_sum	chkey

#see ReturningSoilTextureRelatedAttributes.pdf for a comprehensize SQL query example