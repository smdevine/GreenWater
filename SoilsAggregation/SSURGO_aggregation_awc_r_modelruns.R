#script modified and simplified from SSURGO_processing_awc_r.R to obtain required model paramters
#for summer 2017 model effort
library(plyr)
library(dplyr)
#library(nnet)
#library(extrafont)
#library(extrafontdb)
#loadfonts()
options(stringsAsFactors = FALSE)
mainDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion'
SoilsDataDir <- file.path(mainDir, 'soils_data/soilDB_query')
sum_modified <- function(x) {
  if(all(is.na(x))) {
    return(NA)
  }
  else {sum(x, na.rm = TRUE)}
}
range_modified <- function(x) {
  if(all(is.na(x))) {
    return(NA)
  }
  else {range(x, na.rm = TRUE)}
}
min_modified <- function(x) {
  if(all(is.na(x))) {
    return(NA)
  }
  else {min(x, na.rm = TRUE)}
}
max_modified <- function(x) {
  if(all(is.na(x))) {
    return(NA)
  }
  else {max(x, na.rm = TRUE)}
}
results <- file.path(mainDir, 'soils_data/results/summer2017model')
#fc_def <- 'awc_r'
#figure_label <- 'SSURGO defined FC'

#one-time operation to clean SSURGO horizon data which had duplicate chkeys 
setwd(SoilsDataDir)
ssurgo_horizon <- read.csv('CA_horizon_data_2017-07-13.csv')
ssurgo_horizon <- ssurgo_horizon[!is.na(ssurgo_horizon$chkey), ] #get rid of chkeys that have no data first
fragvol_tot <- as.data.frame(tapply(ssurgo_horizon$fragvol_r, ssurgo_horizon$chkey, sum, na.rm=TRUE))
colnames(fragvol_tot) <- 'fragvol_r_sum'
fragvol_tot$chkey <- rownames(fragvol_tot)
rownames(fragvol_tot) <- NULL
ssurgo_horizon <- ssurgo_horizon[!duplicated(ssurgo_horizon$chkey), ] #get rid of duplicate chkeys, which only have unique fragvol_r and fragsize_r values
ssurgo_horizon <- ssurgo_horizon[ ,-which(colnames(ssurgo_horizon) == "fragvol_r" | colnames(ssurgo_horizon)=="fragsize_r")]
ssurgo_horizon <- merge(ssurgo_horizon, fragvol_tot, by='chkey')
write.csv(ssurgo_horizon, 'CA_horizon_data_2017-07-13_clean.csv', row.names = FALSE)

#a real hack job originally developed Oct 2017 to deal with duplicate cokeys as 
#a result of reskind query, since multiple reskinds require multiple cokey rows 
#with duplicate data
#in order to apply ssurgo_horizon sum routine, need to first identify which 
#cokeys have lithic or paralithic contacts or both, so that these horizons 
#are not given plant available values
#purpose is to get the soil comp data into the correct dimensions for ease
#and to distinguish between modifiable and unmodifiable soil profiles
ssurgo_comp <- read.csv("CA_comp_data_2017-07-13.csv")
ssurgo_comp$majcompflag[ssurgo_comp$majcompflag=='No '] <- 'No' #strip out the space after 'No '
ssurgo_comp <- ssurgo_comp[!is.na(ssurgo_comp$cokey), ] #there were 6 mukeys with no identified cokeys
ssurgo_comp$reskind[is.na(ssurgo_comp$reskind) & ssurgo_comp$majcompflag=='Yes'] <- 'None'
i <- !is.na(ssurgo_comp$reskind) & (ssurgo_comp$reskind=='Paralithic bedrock' | ssurgo_comp$reskind=='Lithic bedrock' | ssurgo_comp$reskind=='Densic bedrock')
ssurgo_comp_rock <- ssurgo_comp[i, ]
ssurgo_comp_no_rock <- ssurgo_comp[!i, ] #56,126 of 56,451 are unique cokeys, meaning 78 have more than one reskind that is not a paralithic or lithic restriction
cokeys_rock <- ssurgo_comp$cokey[i]
cokeys_rock_unique <- unique(cokeys_rock) #15,779 of 16,215 are unique cokeys, meaning 436 cokeys have both paralithic and lithic and that 15779 are cokeys with either a paralithic or lithic reskind in SSURGO
k <- ssurgo_comp_no_rock$cokey %in% cokeys_rock_unique #this is find cokeys with paralithic or lithic reskinds that have other kinds of restrictions
ssurgo_comp_rock <- rbind(ssurgo_comp_rock, ssurgo_comp_no_rock[k, ])
ssurgo_comp_no_rock <- ssurgo_comp_no_rock[!k, ]
#repeat
cokeys_rock <- ssurgo_comp_rock$cokey
j <- match(cokeys_rock_unique, cokeys_rock) #find the index of the unique cokeys
cokeys_rock_reskind_one <- cokeys_rock[j] #select the unique cokeys
cokeys_rock_reskind_mult <- cokeys_rock[-j] #create vector of cokeys with more than one reskind
cokeys_rock_reskind_mult <- unique(cokeys_rock_reskind_mult)
cokeys_rock_reskind_mult <- sort(cokeys_rock_reskind_mult)
j <- match(cokeys_rock_reskind_mult, cokeys_rock_reskind_one) #find the index of the cokeys with more than one paralithic or lithic reskind
cokeys_rock_reskind_one <- cokeys_rock_reskind_one[-j] #take out the 2nd instance of the cokeys with both paralithic and lithic contacts
j <- match(cokeys_rock_reskind_one, ssurgo_comp_rock$cokey)
ssurgo_comp_rock_reskind_one <- ssurgo_comp_rock[j, ] 
ssurgo_comp_rock_reskind_mult <- ssurgo_comp_rock[-j, ]
ssurgo_comp_rock_reskind_mult <- ssurgo_comp_rock_reskind_mult[order(ssurgo_comp_rock_reskind_mult$cokey, ssurgo_comp_rock_reskind_mult$resdept_r), ]
k <- match(cokeys_rock_reskind_mult, ssurgo_comp_rock_reskind_mult$cokey) #now find the first instance of each cokey, which will be the shallowest of the two reskinds, according to the 'order' operation above
ssurgo_comp_rock_reskind1_mult <- ssurgo_comp_rock_reskind_mult[k, ] #select the shallowest reskind for each cokey
colnames(ssurgo_comp_rock_reskind1_mult)[6:8] <- c('reskind1', 'res1dept_r', 'res1depb_r')
ssurgo_comp_rock_reskind_mult <- ssurgo_comp_rock_reskind_mult[-k, ] #get rid of the shallowest reskind for each cokey
k <- match(cokeys_rock_reskind_mult, ssurgo_comp_rock_reskind_mult$cokey) #select the 2nd deepest reskind for each cokey
ssurgo_comp_rock_reskind2_mult <- ssurgo_comp_rock_reskind_mult[k, ] #select the 2nd deepest reskind for each cokey
ssurgo_comp_rock_reskind2_mult <- ssurgo_comp_rock_reskind2_mult[ , c("cokey", "reskind", "resdept_r", "resdepb_r")] 
colnames(ssurgo_comp_rock_reskind2_mult)[2:4] <- c('reskind2', 'res2dept_r', 'res2depb_r')
ssurgo_comp_rock_reskind3_mult <- ssurgo_comp_rock_reskind_mult[-k, ] #get rid of the 2nd deepest reskind for each cokey (this leaves two cokeys remaining)
ssurgo_comp_rock_reskind3_mult <- ssurgo_comp_rock_reskind3_mult[ , c("cokey", "reskind", "resdept_r", "resdepb_r")] 
colnames(ssurgo_comp_rock_reskind3_mult)[2:4] <- c('reskind3', 'res3dept_r', 'res3depb_r')
ssurgo_comp_rock_reskind_mult <- merge(ssurgo_comp_rock_reskind1_mult, ssurgo_comp_rock_reskind2_mult, by='cokey', all = TRUE)
ssurgo_comp_rock_reskind_mult <- merge(ssurgo_comp_rock_reskind_mult, ssurgo_comp_rock_reskind3_mult, by='cokey', all = TRUE)
colnames(ssurgo_comp_rock_reskind_one)[6:8] <- c('reskind1', 'res1dept_r', 'res1depb_r') 
ssurgo_comp_rock_reskind_one$reskind2 <- 'None' 
ssurgo_comp_rock_reskind_one$res2dept_r <- NA
ssurgo_comp_rock_reskind_one$res2depb_r <- NA
ssurgo_comp_rock_reskind_one$reskind3 <- 'None' 
ssurgo_comp_rock_reskind_one$res3dept_r <- NA
ssurgo_comp_rock_reskind_one$res3depb_r <- NA
ssurgo_comp_rock <- rbind(ssurgo_comp_rock_reskind_one, ssurgo_comp_rock_reskind_mult) #now merge the cokeys back to together so that each row represents a unique cokey with data for up to three reskinds per cokey (confirmed there are 15,779 rows now)

#now, do the same for the cokeys without a lithic or paralithic reskind
cokeys_no_rock <- ssurgo_comp_no_rock$cokey #35,700 total cokeys
cokeys_no_rock_unique <- unique(cokeys_no_rock) #35,629 are unique cokey.  Turns out that ? cokeys have two reskinds; ? have three reskinds.  Thus, ? are cokeys with one reskind in SSURGO
j <- match(cokeys_no_rock_unique, cokeys_no_rock) #find the index of the unique cokeys
cokeys_no_rock_reskind_one <- cokeys_no_rock[j] #select the unique cokeys
cokeys_no_rock_reskind_mult <- cokeys_no_rock[-j] #create vector of cokeys with more than one reskind; there are two duplicates here, meaning two cokeys had three reskinds?
cokeys_no_rock_reskind_mult <- unique(cokeys_no_rock_reskind_mult)
cokeys_no_rock_reskind_mult <- sort(cokeys_no_rock_reskind_mult)
j <- match(cokeys_no_rock_reskind_mult, cokeys_no_rock_reskind_one) #find the index of the cokeys with more than one reskind
cokeys_no_rock_reskind_one <- cokeys_no_rock_reskind_one[-j] #take these out
j <- match(cokeys_no_rock_reskind_one, ssurgo_comp_no_rock$cokey)
ssurgo_comp_no_rock_reskind_one <- ssurgo_comp_no_rock[j, ]
ssurgo_comp_no_rock_reskind_mult <- ssurgo_comp_no_rock[-j, ]
ssurgo_comp_no_rock_reskind_mult <- ssurgo_comp_no_rock_reskind_mult[order(ssurgo_comp_no_rock_reskind_mult$cokey, ssurgo_comp_no_rock_reskind_mult$resdept_r), ]
k <- match(cokeys_no_rock_reskind_mult, ssurgo_comp_no_rock_reskind_mult$cokey) #now find the first instance of each cokey, which will be the shallowest of multiple reskinds, according to the 'order' operation above
ssurgo_comp_no_rock_reskind1_mult <- ssurgo_comp_no_rock_reskind_mult[k, ] #select the shallowest reskind for each cokey
colnames(ssurgo_comp_no_rock_reskind1_mult)[6:8] <- c('reskind1', 'res1dept_r', 'res1depb_r')
ssurgo_comp_no_rock_reskind_mult <- ssurgo_comp_no_rock_reskind_mult[-k, ] #get rid of the shallowest reskind for each cokey
k <- match(cokeys_no_rock_reskind_mult, ssurgo_comp_no_rock_reskind_mult$cokey) #select the 2nd deepest reskind for each cokey
ssurgo_comp_no_rock_reskind2_mult <- ssurgo_comp_no_rock_reskind_mult[k, ] #select the 2nd deepest reskind for each cokey
ssurgo_comp_no_rock_reskind2_mult <- ssurgo_comp_no_rock_reskind2_mult[ , c("cokey", "reskind", "resdept_r", "resdepb_r")] 
colnames(ssurgo_comp_no_rock_reskind2_mult)[2:4] <- c('reskind2', 'res2dept_r', 'res2depb_r')
ssurgo_comp_no_rock_reskind3_mult <- ssurgo_comp_no_rock_reskind_mult[-k, ] #get rid of the 2nd deepest reskind for each cokey (this leaves two cokeys remaining)
ssurgo_comp_no_rock_reskind3_mult <- ssurgo_comp_no_rock_reskind3_mult[ , c("cokey", "reskind", "resdept_r", "resdepb_r")] 
colnames(ssurgo_comp_no_rock_reskind3_mult)[2:4] <- c('reskind3', 'res3dept_r', 'res3depb_r')
ssurgo_comp_no_rock_reskind_mult <- merge(ssurgo_comp_no_rock_reskind1_mult, ssurgo_comp_no_rock_reskind2_mult, by='cokey')
ssurgo_comp_no_rock_reskind_mult <- merge(ssurgo_comp_no_rock_reskind_mult, ssurgo_comp_no_rock_reskind3_mult, by='cokey', all = TRUE)
colnames(ssurgo_comp_no_rock_reskind_one)[6:8] <- c('reskind1', 'res1dept_r', 'res1depb_r') 
ssurgo_comp_no_rock_reskind_one$reskind2 <- 'None' 
ssurgo_comp_no_rock_reskind_one$res2dept_r <- NA
ssurgo_comp_no_rock_reskind_one$res2depb_r <- NA
ssurgo_comp_no_rock_reskind_one$reskind3 <- 'None' 
ssurgo_comp_no_rock_reskind_one$res3dept_r <- NA
ssurgo_comp_no_rock_reskind_one$res3depb_r <- NA
ssurgo_comp_no_rock <- rbind(ssurgo_comp_no_rock_reskind_one, ssurgo_comp_no_rock_reskind_mult) #now merge the cokeys back to together so that each row represents a unique cokey with data for up to two reskinds per cokey, if necessary
write.csv(ssurgo_comp_no_rock, 'CA_comp_data_no_rock_2017-07-13.csv', row.names = FALSE)
write.csv(ssurgo_comp_rock, 'CA_comp_data_rock_2017-07-13.csv', row.names = FALSE)

#one-time operation to split soil horizon data into a set with a rock contact and those with no rock contact
#split ssurgo_horizons into two data.frames
ssurgo_horizon <- read.csv('CA_horizon_data_2017-07-13_clean.csv')
ssurgo_horizon$hzthickness <- ssurgo_horizon$hzdepb_r - ssurgo_horizon$hzdept_r
i <- ssurgo_horizon$cokey %in% cokeys_rock_unique
ssurgo_horizon_Cr_R <- ssurgo_horizon[i, ]
ssurgo_horizon_no_Cr_R <- ssurgo_horizon[!i, ]
write.csv(ssurgo_horizon_Cr_R, 'ssurgo_horizon_Cr_R_2017-07-13.csv', row.names = FALSE)
write.csv(ssurgo_horizon_no_Cr_R, 'ssurgo_horizon_no_Cr_R_2017-07-13.csv', row.names = FALSE)

aggregate_SSURGO <- function(results, fc_def, figure_label) {
  setwd(SoilsDataDir)
  list.files(SoilsDataDir, pattern = glob2rx('*.csv'))
  ssurgo_horizon_no_Cr_R <- read.csv('ssurgo_horizon_no_Cr_R_2017-07-13.csv')
  ssurgo_horizon_Cr_R <- read.csv('ssurgo_horizon_no_Cr_R_2017-07-13.csv')
  ssurgo_comp_no_rock <- read.csv('CA_comp_data_no_rock_2017-07-13.csv')
  ssurgo_comp_rock <- read.csv('CA_comp_data_rock_2017-07-13.csv')
  ssurgo_mu <- read.csv("CA_mu_data_2017-07-13.csv")
  #txt2 <- paste(length(unique(ssurgo_horizon$cokey)), 'unique cokeys in soil horizon data table.')
  #txt3 <- paste(length(unique(ssurgo_comp$cokey)), 'unique cokeys in soil component data table.')
  #txt4 <- paste(nrow(ssurgo_comp), 'rows in the component data table.')
  #txt5 <- paste(paste(unique(ssurgo_comp$reskind), collapse=', '), 'are the unique kinds of restrictions.')
  #txt6 <- paste(length(unique(ssurgo_comp$mukey)), 'unique mukeys in soil component data table')
  #txt7 <- paste(length(unique(ssurgo_mu$mukey)), 'unique mukeys in soil mapunit data table')

  #add area info to component table
  ssurgo_comp_no_rock <- merge(ssurgo_comp_no_rock, ssurgo_mu[ ,c('mukey', 'muacres')], by='mukey')
  ssurgo_comp_no_rock$compacres <- ssurgo_comp$muacres*(ssurgo_comp$comppct_r/100) #correct area based upon component percentage; however there are 1802 NAs 
  #txt17 <- paste('Major comonents total:', round(sum(ssurgo_comp$hectares[which(ssurgo_comp$majcompflag=='Yes')]), digits = 0), 'hectares.') #add up major comp acreage: 6,331,342 ha, which is 85.4% of total
  #txt18 <- paste('Minor components total:', round(sum(ssurgo_comp$hectares[which(ssurgo_comp$majcompflag=='No')], na.rm = TRUE), digits = 0), 'hectares.') #add up minor comp acreage (there are 3 cokeys with missing comppct_r data): 1,085,786 ha, which is 14.6% of total

#first, sum up awc by cokey for those with paralithic or lithic reskinds with attention to possible rooting depths
  sum_PAW_cmH2O <- function(rooting_depth, PAW, df) {
    for (i in 1:nrow(df)) {
      if (i==1) {
        y <- vector(mode = 'numeric', length = nrow(df))
      }
      if (is.na(df$hzthickness[i]) & is.na(df[[PAW]][i])) {
        y[i] <- NA
        next
      }
      if (df$hzdepb_r[i] <= rooting_depth) {
        y[i] <- df[[PAW]][i]*df$hzthickness[i]
        next
      }
      if (df$hzdept_r[i] < rooting_depth) {
        y[i] <- (rooting_depth - df$hzdept_r[i])*df[[PAW]][i]
        next
      } else {
        y[i] <- NA
      }
    }
    return(y)
  }
  sum_PAW_soilthickness <- function(rooting_depth, PAW, df) {
    for (i in 1:nrow(df)) {
      if (i==1) {
        y <- vector()
      }
      if (df$hzdepb_r[i] <= rooting_depth & !is.na(df[[PAW]][i])) { 
        y <- c(y, df$hzthickness[i])
        next
      }
      if (df$hzdept_r[i] < rooting_depth & !is.na(df[[PAW]][i])) {
        y <- c(y, (rooting_depth - df$hzdept_r[i]))
        next
      }
      else {y <- c(y, NA)}
    }
    return(y)
  }
  ssurgo_horizon_Cr_R$z1m_cmH2O <- sum_PAW_cmH2O(100, 'awc_r', ssurgo_horizon_Cr_R)
  ssurgo_horizon_Cr_R$z2m_cmH2O <- sum_PAW_cmH2O(200, 'awc_r', ssurgo_horizon_Cr_R)
  ssurgo_horizon_Cr_R$z4m_cmH2O <- sum_PAW_cmH2O(400, 'awc_r', ssurgo_horizon_Cr_R)
  ssurgo_horizon_Cr_R$aws0150ck_cmH2O <- sum_PAW_cmH2O(150, 'awc_r', ssurgo_horizon_Cr_R)

#left off here 7/13/17  
#now sum up AD H2O and soil thickness used in the calculation by cokey
  comps_R_Cr <- as.data.frame(tapply(ssurgo_horizon_Cr_R$AD_alfalfa_cmH2O, ssurgo_horizon_Cr_R$cokey, sum_modified))
  colnames(comps_R_Cr) <- 'AD_alfalfa_cmH2O'
  comps_R_Cr$cokey <- rownames(comps_R_Cr)
  comps_R_Cr$cokey <- as.integer(comps_R_Cr$cokey)
  rownames(comps_R_Cr) <- NULL
  comps_R_Cr <- comps_R_Cr[ ,c(2, 1)]
  comps_R_Cr$AD_alfalfa_soilthickness <- tapply(ssurgo_horizon_Cr_R$AD_alfalfa_soilthickness, ssurgo_horizon_Cr_R$cokey, sum_modified)
  comps_R_Cr$aws0150ck_cmH2O <- tapply(ssurgo_horizon_Cr_R$aws0150ck_cmH2O, ssurgo_horizon_Cr_R$cokey, sum_modified)
  comps_R_Cr$aws0150ck_soilthickness <- tapply(ssurgo_horizon_Cr_R$aws0150ck_soilthickness, ssurgo_horizon_Cr_R$cokey, sum_modified)
  comps_R_Cr$AD_grapes_cmH2O <- tapply(ssurgo_horizon_Cr_R$AD_grapes_cmH2O, ssurgo_horizon_Cr_R$cokey, sum_modified)
  comps_R_Cr$AD_grapes_soilthickness <- tapply(ssurgo_horizon_Cr_R$AD_grapes_soilthickness, ssurgo_horizon_Cr_R$cokey, sum_modified)
  comps_R_Cr$AD_almonds_cmH2O <- tapply(ssurgo_horizon_Cr_R$AD_almonds_cmH2O, ssurgo_horizon_Cr_R$cokey, sum_modified)
  comps_R_Cr$AD_almonds_soilthickness <- tapply(ssurgo_horizon_Cr_R$AD_almonds_soilthickness, ssurgo_horizon_Cr_R$cokey, sum_modified)
  comps_R_Cr$AD_walnuts_cmH2O <- tapply(ssurgo_horizon_Cr_R$AD_walnuts_cmH2O, ssurgo_horizon_Cr_R$cokey, sum_modified)
  comps_R_Cr$AD_walnuts_soilthickness <- tapply(ssurgo_horizon_Cr_R$AD_walnuts_soilthickness, ssurgo_horizon_Cr_R$cokey, sum_modified)
  setwd(results)
  write.csv(comps_R_Cr, 'comps_rock_AD_calcs.csv', row.names = FALSE)
  
#second, for alfalfa, sum up awc by cokey for those without paralithic or lithic contacts but without modification of the soil profile.
  ssurgo_horizon_no_Cr_R$AD_alfalfa_cmH2O <- sum_AD_cmH2O(alfalfa_root_z, 'AD_alfalfa', ssurgo_horizon_no_Cr_R)
  ssurgo_horizon_no_Cr_R$AD_alfalfa_soilthickness <- sum_AD_soilthickness(alfalfa_root_z, 'AD_alfalfa', ssurgo_horizon_no_Cr_R)
  ssurgo_horizon_no_Cr_R$aws0150ck_cmH2O <- sum_AD_cmH2O(aws0150ck, 'paw_est', ssurgo_horizon_no_Cr_R)
  ssurgo_horizon_no_Cr_R$aws050ck_soilthickness <- sum_AD_soilthickness(aws0150ck, 'paw_est', ssurgo_horizon_no_Cr_R)
  comps_no_Cr_R <- as.data.frame(tapply(ssurgo_horizon_no_Cr_R$AD_alfalfa_cmH2O, ssurgo_horizon_no_Cr_R$cokey, sum_modified))
  colnames(comps_no_Cr_R) <- 'AD_alfalfa_cmH2O_unmodified'
  comps_no_Cr_R$cokey <- rownames(comps_no_Cr_R)
  comps_no_Cr_R$cokey <- as.integer(comps_no_Cr_R$cokey)
  rownames(comps_no_Cr_R) <- NULL
  comps_no_Cr_R <- comps_no_Cr_R[ ,c(2, 1)]
  comps_no_Cr_R$AD_alfalfa_soilthickness <- tapply(ssurgo_horizon_no_Cr_R$AD_alfalfa_soilthickness, ssurgo_horizon_no_Cr_R$cokey, sum_modified)
  comps_no_Cr_R$aws0150ck_cmH2O_unmodified <- tapply(ssurgo_horizon_no_Cr_R$aws0150ck_cmH2O, ssurgo_horizon_no_Cr_R$cokey, sum_modified)
  comps_no_Cr_R$aws0150ck_soilthickness_unmodified <- tapply(ssurgo_horizon_no_Cr_R$aws050ck_soilthickness, ssurgo_horizon_no_Cr_R$cokey, sum_modified)
  
#third, for walnuts, almonds, and grapes, sum up awc by cokey for those without paralithic or lithic contacts by adding missing AD data for the portion of the profile of the rooting zone without it, assuming profile wtd avgs for the missing data.  AD_'crop'_soilthickness reflects cm of rooting zone with data as a QC check. also do this for the 0-150 cm aws check.
  ssurgo_horizon_no_Cr_R$AD_grapes_soilthickness <- sum_AD_soilthickness(grape_root_z, 'AD_grapes', ssurgo_horizon_no_Cr_R)
  ssurgo_horizon_no_Cr_R$AD_almonds_soilthickness <- sum_AD_soilthickness(almonds_root_z, 'AD_deciduous', ssurgo_horizon_no_Cr_R)
  ssurgo_horizon_no_Cr_R$AD_walnuts_soilthickness <- sum_AD_soilthickness(walnuts_root_z, 'AD_deciduous', ssurgo_horizon_no_Cr_R)
  ssurgo_horizon_no_Cr_R$AD_grapes_cmH2O_unmodified <- sum_AD_cmH2O(grape_root_z , 'AD_grapes', ssurgo_horizon_no_Cr_R)
  ssurgo_horizon_no_Cr_R$AD_almonds_cmH2O_unmodified <- sum_AD_cmH2O(almonds_root_z, 'AD_deciduous', ssurgo_horizon_no_Cr_R)
  ssurgo_horizon_no_Cr_R$AD_walnuts_cmH2O_unmodified <- sum_AD_cmH2O(walnuts_root_z, 'AD_deciduous', ssurgo_horizon_no_Cr_R)
  comps_no_Cr_R$AD_grapes_soilthickness_unmodified <- tapply(ssurgo_horizon_no_Cr_R$AD_grapes_soilthickness, ssurgo_horizon_no_Cr_R$cokey, sum_modified)
  comps_no_Cr_R$AD_almonds_soilthickness_unmodified <- tapply(ssurgo_horizon_no_Cr_R$AD_almonds_soilthickness, ssurgo_horizon_no_Cr_R$cokey, sum_modified)  
  comps_no_Cr_R$AD_walnuts_soilthickness_unmodified <- tapply(ssurgo_horizon_no_Cr_R$AD_walnuts_soilthickness, ssurgo_horizon_no_Cr_R$cokey, sum_modified)
  comps_no_Cr_R$AD_grapes_cmH2O_unmodified <- tapply(ssurgo_horizon_no_Cr_R$AD_grapes_cmH2O_unmodified, ssurgo_horizon_no_Cr_R$cokey, sum_modified)
  comps_no_Cr_R$AD_almonds_cmH2O_unmodfied <- tapply(ssurgo_horizon_no_Cr_R$AD_almonds_cmH2O_unmodified, ssurgo_horizon_no_Cr_R$cokey, sum_modified)
  comps_no_Cr_R$AD_walnuts_cmH2O_unmodified <- tapply(ssurgo_horizon_no_Cr_R$AD_walnuts_cmH2O_unmodified, ssurgo_horizon_no_Cr_R$cokey, sum_modified)
  comps_no_Cr_R$AD_grapes_rootzone_wtdavg <- comps_no_Cr_R$AD_grapes_cmH2O_unmodified/comps_no_Cr_R$AD_grapes_soilthickness_unmodified
  comps_no_Cr_R$AD_walnuts_rootzone_wtdavg <- comps_no_Cr_R$AD_walnuts_cmH2O_unmodified/comps_no_Cr_R$AD_walnuts_soilthickness_unmodified
  comps_no_Cr_R$AD_almonds_rootzone_wtdavg <- comps_no_Cr_R$AD_almonds_cmH2O_unmodfied/comps_no_Cr_R$AD_almonds_soilthickness_unmodified
  comps_no_Cr_R$aws0150ck_rootzone_wtdavg <- comps_no_Cr_R$aws0150ck_cmH2O_unmodified/comps_no_Cr_R$aws0150ck_soilthickness

#function to create a modified AD value by cokey
  AD_sum_modified <- function(df, rootdepth, soilthickness, rootzone_wtdavg, AD) {
    for (i in 1:nrow(df)) {
      if (i==1) {
        y <- vector()
      }
      if (is.na(df[[AD]][i])) { #this means there was no horizon data for the given cokey
        y <- c(y, NA)
        next
      }
      if (rootdepth == df[[soilthickness]][i]) {
        y <- c(y, df[[AD]][i])
        next
      }
      if (rootdepth > df[[soilthickness]][i]) {
        y <- c(y, (df[[AD]][i]+(rootdepth - df[[soilthickness]][i])*df[[rootzone_wtdavg]][i]))
        next
      }
    }
    return(y)
  }
  comps_no_Cr_R$aws0150ck_cmH2O_modified <- AD_sum_modified(comps_no_Cr_R, aws0150ck, 'aws0150ck_soilthickness_unmodified', 'aws0150ck_rootzone_wtdavg', 'aws0150ck_cmH2O_unmodified')
  comps_no_Cr_R$aws0150ck_soilthickness_modified <- aws0150ck
  comps_no_Cr_R$AD_grapes_cmH2O_modified <- AD_sum_modified(comps_no_Cr_R, grape_root_z, 'AD_grapes_soilthickness_unmodified', 'AD_grapes_rootzone_wtdavg', 'AD_grapes_cmH2O_unmodified')
  comps_no_Cr_R$AD_grapes_soilthickness_modified <- grape_root_z
  comps_no_Cr_R$AD_almonds_cmH2O_modfied <- AD_sum_modified(comps_no_Cr_R, almonds_root_z, 'AD_almonds_soilthickness_unmodified', 'AD_almonds_rootzone_wtdavg', 'AD_almonds_cmH2O_unmodfied')
  comps_no_Cr_R$AD_almonds_soilthickness_modified <- almonds_root_z
  comps_no_Cr_R$AD_walnuts_cmH2O_modified <- AD_sum_modified(comps_no_Cr_R, walnuts_root_z, 'AD_walnuts_soilthickness_unmodified', 'AD_walnuts_rootzone_wtdavg', 'AD_walnuts_cmH2O_unmodified')
  comps_no_Cr_R$AD_walnuts_soilthickness_modified <- walnuts_root_z
  setwd(results)
  write.csv(comps_no_Cr_R, 'comps_no_Cr_R_AD_calcs.csv', row.names=FALSE)
#now, simplifiy this data.frame to match comps_R_Cr.  currently, this selects ummodified data for alfalfa and modified data for the 0-150cm check, grapes, almonds, and walnuts. colnames are changed to match comps_R_Cr
  comps_no_Cr_R_condensed <- comps_no_Cr_R[ ,c(1, 2, 3, 16:23)]
  colnames(comps_no_Cr_R_condensed) <- colnames(comps_R_Cr)
#merge and rbind data.frames to fill in data for minor components based off of component name averages
  ssurgo_comp_no_rock <- merge(ssurgo_comp_no_rock, comps_no_Cr_R_condensed, by='cokey', all = TRUE)
  ssurgo_comp_rock <- merge(ssurgo_comp_rock, comps_R_Cr, by='cokey', all = TRUE)
  #df <- ssurgo_comp_rock
  #varname <- "AD_alfalfa_cmH2O"
  area_weighted_average <- function(df, varname) { #modified function from 'SSURGO_analysis_v2.R' in 'Forest Dieoff and Hydrology/R Scripts' directory.  Uses data from the same areasymbol first and then goes beyond the areasymbol next.  Could do the same to fill in reskind data, if necessary
    has_data <- df[which(!is.na(df[[varname]])), ]
    data_area <- as.data.frame(tapply(has_data$hectares, list(has_data$compname, has_data$areasymbol), sum))
    data_calc_term <- as.data.frame(tapply(has_data[[varname]]*has_data$hectares, list(has_data$compname, has_data$areasymbol), sum))
    data_by_compname <- data_calc_term/data_area
    #replace NA AWC values with component name average AWC values
    i <- which(is.na(df[[varname]]))
    for (j in 1:length(i)){
      look_up_index <- match(df$compname[i[j]], rownames(data_by_compname))
      if (is.na(look_up_index)) {
        next
      }
      compname_areasymbol <- df$areasymbol[i[j]] #find name of the compname_reskind
      if (is.na(compname_areasymbol)) {
        next(print(paste(rownames(data_by_compname)[look_up_index], 'is missing areasymbol data to determine which', varname, 'to paste.')))
      }
      if (is.null(data_by_compname[[compname_areasymbol]][look_up_index])) {
        next(print('Null data during process'))
      }
      df[[varname]][i[j]] <- data_by_compname[[compname_areasymbol]][look_up_index]
    }
    data_area <- as.data.frame(tapply(has_data$hectares, has_data$compname, sum))
    data_calc_term <- as.data.frame(tapply(has_data[[varname]]*has_data$hectares, has_data$compname, sum))
    data_by_compname <- data_calc_term/data_area
    #replace NA AWC values with component name average AWC values
    i <- which(is.na(df[[varname]]))
    for (j in 1:length(i)){
      look_up_index <- match(df$compname[i[j]], rownames(data_by_compname))
      if (is.na(look_up_index)) {
        next
      }
      #compname_areasymbol <- df$areasymbol[i[j]] #find name of the compname_reskind
      #if (is.na(compname_areasymbol)) {
        #next(print(paste(rownames(data_by_compname)[look_up_index], 'is missing areasymbol data to determine which', varname, 'to paste.')))
      #}
      df[[varname]][i[j]] <- data_by_compname[ ,1][look_up_index]
    }
    invisible(df)
  }
#for ssurgo_comp_rock
  ssurgo_comp_rock <- area_weighted_average(ssurgo_comp_rock, 'AD_alfalfa_cmH2O')
  ssurgo_comp_rock <- area_weighted_average(ssurgo_comp_rock, 'AD_alfalfa_soilthickness')
  ssurgo_comp_rock <- area_weighted_average(ssurgo_comp_rock, 'aws0150ck_cmH2O')
  ssurgo_comp_rock <- area_weighted_average(ssurgo_comp_rock, 'aws0150ck_soilthickness')
  ssurgo_comp_rock <- area_weighted_average(ssurgo_comp_rock, 'AD_grapes_cmH2O')
  ssurgo_comp_rock <- area_weighted_average(ssurgo_comp_rock, 'AD_grapes_soilthickness')
  ssurgo_comp_rock <- area_weighted_average(ssurgo_comp_rock, 'AD_almonds_cmH2O')
  ssurgo_comp_rock <- area_weighted_average(ssurgo_comp_rock, 'AD_almonds_soilthickness')
  ssurgo_comp_rock <- area_weighted_average(ssurgo_comp_rock, 'AD_walnuts_cmH2O')
  ssurgo_comp_rock <- area_weighted_average(ssurgo_comp_rock, 'AD_walnuts_soilthickness')
#for ssurgo_comp_no_rock
  ssurgo_comp_no_rock <- area_weighted_average(ssurgo_comp_no_rock, 'AD_alfalfa_cmH2O')
  ssurgo_comp_no_rock <- area_weighted_average(ssurgo_comp_no_rock, 'AD_alfalfa_soilthickness')
  ssurgo_comp_no_rock <- area_weighted_average(ssurgo_comp_no_rock, 'aws0150ck_cmH2O')
  ssurgo_comp_no_rock <- area_weighted_average(ssurgo_comp_no_rock, 'aws0150ck_soilthickness')
  ssurgo_comp_no_rock <- area_weighted_average(ssurgo_comp_no_rock, 'AD_grapes_cmH2O')
  ssurgo_comp_no_rock <- area_weighted_average(ssurgo_comp_no_rock, 'AD_grapes_soilthickness')
  ssurgo_comp_no_rock <- area_weighted_average(ssurgo_comp_no_rock, 'AD_almonds_cmH2O')
  ssurgo_comp_no_rock <- area_weighted_average(ssurgo_comp_no_rock, 'AD_almonds_soilthickness')
  ssurgo_comp_no_rock <- area_weighted_average(ssurgo_comp_no_rock, 'AD_walnuts_cmH2O')
  ssurgo_comp_no_rock <- area_weighted_average(ssurgo_comp_no_rock, 'AD_walnuts_soilthickness')
#now bind all data together
  ssurgo_comp_all <- rbind(ssurgo_comp_rock, ssurgo_comp_no_rock)

#read in ssurgo_comp_all file
  #setwd(results)
  #ssurgo_comp_all <- read.csv('comps_all_final_summary.csv', stringsAsFactors = FALSE)  
#then apply area_weighted_average again, so that minor components that did not have an identified Cr or R are fixed; this could be fixed above in the reskind synthesis work but is more of a challenge because of multiple reskinds.  See SSURGO_analysis_v2.R for an example of how to do it.
  ssurgo_comp_all <- area_weighted_average(ssurgo_comp_all, 'AD_alfalfa_cmH2O')
  ssurgo_comp_all <- area_weighted_average(ssurgo_comp_all, 'AD_alfalfa_soilthickness')
  ssurgo_comp_all <- area_weighted_average(ssurgo_comp_all, 'aws0150ck_cmH2O')
  ssurgo_comp_all <- area_weighted_average(ssurgo_comp_all, 'aws0150ck_soilthickness')
  ssurgo_comp_all <- area_weighted_average(ssurgo_comp_all, 'AD_grapes_cmH2O')
  ssurgo_comp_all <- area_weighted_average(ssurgo_comp_all, 'AD_grapes_soilthickness')
  ssurgo_comp_all <- area_weighted_average(ssurgo_comp_all, 'AD_almonds_cmH2O')
  ssurgo_comp_all <- area_weighted_average(ssurgo_comp_all, 'AD_almonds_soilthickness')
  ssurgo_comp_all <- area_weighted_average(ssurgo_comp_all, 'AD_walnuts_cmH2O')
  ssurgo_comp_all <- area_weighted_average(ssurgo_comp_all, 'AD_walnuts_soilthickness')

#temp fix for rock outcrop (could be fixed earlier in script). only comprises 44,398 ha of AOI
  i <- which(ssurgo_comp_all$compname=='Rock outcrop')
  ssurgo_comp_all[i, 18:27] <- 0
  ssurgo_comp_all <- ssurgo_comp_all[order(ssurgo_comp_all$mukey, ssurgo_comp_all$comppct_r), ]
  setwd(results)
  write.csv(ssurgo_comp_all, 'comps_all_final_summary.csv', row.names = FALSE) # can read this in to shorten script

#map unit aggregation for % of components with data, max, component wtd avg, and min
  ssurgo_comp_all$aws_dummy <- ssurgo_comp_all$aws0150ck_cmH2O #copy aws check data, assuming all AD columns are populated where this is populated 
  ssurgo_comp_all$aws_dummy[ssurgo_comp_all$aws_dummy >= 0] <- 1
  mu_summary <- as.data.frame(tapply(ssurgo_comp_all$comppct_r*ssurgo_comp_all$aws_dummy, ssurgo_comp_all$mukey, sum, na.rm=TRUE)) #sum up the component %s by mukey if there is aws content data for a particular component
  colnames(mu_summary) <- 'aws_ppct_tot'
  mu_summary$mukey <- rownames(mu_summary)
  mu_summary <- mu_summary[ ,c(2, 1)]
  mu_aggregation <- function(df_comp, df_mu, varname) {
    df_mu[[paste(varname, '_min', sep='')]] <- tapply(df_comp[[varname]], df_comp$mukey, min_modified)
    df_mu[[paste(varname, '_max', sep='')]] <- tapply(df_comp[[varname]], df_comp$mukey, max_modified)
    df_comp$data_dummy <- df_comp[[varname]]
    df_comp$data_dummy[df_comp$data_dummy >= 0] <- 1
    df_mu$data_ppct_tot <- tapply(df_comp$comppct_r*df_comp$data_dummy, df_comp$mukey, sum_modified)
    df_mu$calc_term <- tapply(df_comp$comppct_r*df_comp[[varname]], df_comp$mukey, sum_modified)
    df_mu[[paste(varname, '_wtdavg', sep='')]] <- df_mu$calc_term/df_mu$data_ppct_tot
    df_mu$data_ppct_tot <- NULL
    df_mu$calc_term <- NULL
    invisible(df_mu)
  }
  mu_summary <- mu_aggregation(ssurgo_comp_all, mu_summary, 'AD_alfalfa_cmH2O')
  mu_summary <- mu_aggregation(ssurgo_comp_all, mu_summary, 'aws0150ck_cmH2O')
  mu_summary <- mu_aggregation(ssurgo_comp_all, mu_summary, 'AD_grapes_cmH2O')
  mu_summary <- mu_aggregation(ssurgo_comp_all, mu_summary, 'AD_almonds_cmH2O')
  mu_summary <- mu_aggregation(ssurgo_comp_all, mu_summary, 'AD_walnuts_cmH2O')
  final_result <- merge(ssurgo_mu, mu_summary, by='mukey', all=TRUE)
  setwd(results)
  write.csv(final_result, 'mu_aggregated_data_CA_ag.csv', row.names = FALSE) #represents 7,334,505 ha

#temp modification to read in mu_summary from here
  results <- file.path(mainDir, 'soils_data/results/paw_check')
  fc_def <- 'awc_r'
  figure_label <- 'with inclusions'
  setwd(results)
  final_result <- read.csv('mu_aggregated_data_CA_ag.csv', stringsAsFactors = FALSE)
  final_result_qc <- final_result[which(final_result$aws_ppct_tot > 75), ] #represents 7,011,392 ha
  lm.aws.comparison <- lm(aws0150wta ~ aws0150ck_cmH2O_wtdavg, data=final_result_qc)
  pdf(paste('paw_comparison_FC', fc_def, '.pdf', sep = ''), family = 'Book Antiqua', width = 7.3, height = 6.3)
  par(mar= c(4.5, 5, 1, 1) + 0.1) # default is c(5,4,4,2) + 0.1 (bottom, left, top, right margins in line widths)
  plot(final_result$aws0150ck_cmH2O_wtdavg, final_result$aws0150wta, type='p', xlab=bquote('modified SSURGO 0-150 cm PAW (cm H'[2]*'O)' ~ .(figure_label)), ylab=expression('SSURGO muggatt 0-150 cm PAW (cm H'[2]*'O)'), cex.lab=1.3, cex.axis=1.3, col='blue1')
  abline(0, 1, col=2, lty=2, lwd=2)
  dev.off()
  lm_result <- capture.output(summary(lm.aws.comparison))
  fileConn <- file(paste(fc_def, '_lm_paw_comparison.txt', sep = ''))
  writeLines(lm_result, fileConn)
  close(fileConn)
}

aggregate_SSURGO(file.path(mainDir, 'soils_data/results/paw_check'), 'ssurgo_fc', 'SSURGO defined FC')

#example of new figure specs
# pdf(paste('paw_comparison_FC', fc_def, '.pdf', sep = ''), family = 'Book Antiqua', width = 7, height = 6.3)
# par(mar= c(4.5, 5, 1, 1) + 0.1) # default is c(5,4,4,2) + 0.1 (bottom, left, top, right margins in line widths)
# plot(final_result$aws0150ck_cmH2O_wtdavg, final_result$aws0150wta, type='p', xlab=bquote('modified SSURGO 0-150 cm PAW (cm H'[2]*'O)' ~ .(figure_label)), ylab=expression('SSURGO muggatt 0-150 cm PAW (cm H'[2]*'O)'), cex.lab=1.3, cex.axis=1.3)
# abline(0, 1, col=2, lty=2, lwd=2)
# dev.off()


#summarize data for most common soil components
  wdir <- file.path(mainDir, 'soils_data/results/dtree2_FC_results')
  setwd(wdir)
  results_fnames <- list.files(pattern = glob2rx('*.csv'))
  ssurgo_comp_all <- read.csv(results_fnames[1], stringsAsFactors = FALSE)
  soil_compname_ha <- as.data.frame(tapply(ssurgo_comp_all$hectares, ssurgo_comp_all$compname, sum))
  colnames(soil_compname_ha) <- 'total_ha'
  i <- order(soil_compname_ha$total_ha, decreasing = TRUE)
  soil_compname_ha <- soil_compname_ha[i, ]
  maj_comps <- head(soil_compname_ha, 22)
  maj_comps <- names(maj_comps)
  maj_comps <- maj_comps[-which(maj_comps=='Urban land' | maj_comps=='Water')]
  y <- vector()
  for (i in 1:nrow(ssurgo_comp_all)) {
    if (ssurgo_comp_all$compname[i] %in% maj_comps & ssurgo_comp_all$majcompflag[i]=='Yes') {
      y <- c(y, i)
    }
  }
  ssurgo_comp_top20 <- ssurgo_comp_all[y, ]
  colnames(ssurgo_comp_top20)[20] <- 'paw_0150cm'
  top20_stats <- as.data.frame(tapply(ssurgo_comp_top20$AD_alfalfa_cmH2O, ssurgo_comp_top20$compname, function(X) length(unique(X))))
  colnames(top20_stats) <- 'unique_AD_values_in_DB'
#function to summarize data
  stats_calc <- function(summary_df, df, varname, func_name, func) {
    summary_df[[paste(varname, '_', func_name, sep = '')]] <- tapply(df[[varname]], df$compname, func)
    return(summary_df)
  }
  varlist <- c('paw_0150cm', 'AD_alfalfa_cmH2O', 'AD_grapes_cmH2O', 'AD_almonds_cmH2O', 'AD_walnuts_cmH2O')
  func_names <- c('mean', 'max', 'min')
  func_list <- c(mean, max, min)
#this needs debugging
  for (j in 1:length(func_list)) {
    for (i in 1:length(varlist)) {
      top20_stats <- stats_calc(top20_stats, ssurgo_comp_top20, varlist[i], func_names[j], func_list[j][[1]])
    }
  }
  setwd(wdir)
  write.csv(top20_stats, 'top20_comps_summary.csv')

#compare comp estimates by FC method
  wdir <- file.path(mainDir, 'soils_data/results')
  result_dirs <- list.dirs(wdir)
  for (i in 2:length(result_dirs)) {
    method_name <- basename(result_dirs[i])
    if (i == 2) {
      i=2
      setwd(result_dirs[i])
      comps <- read.csv("comps_all_final_summary.csv", stringsAsFactors = FALSE)
      comps <- comps[ ,c('cokey', 'compname', 'majcompflag', 'aws0150ck_cmH2O')]
      print(nrow(comps))
      colnames(comps)[3] <- paste('aws0150_', method_name, sep = '')
      next
    }
    setwd(result_dirs[i])
    df <- read.csv("comps_all_final_summary.csv", stringsAsFactors = FALSE)
    df <- df[ ,c('cokey', 'aws0150ck_cmH2O')]
    print(nrow(df))
    colnames(df)[2] <- paste('aws0150_', method_name, sep = '')
    comps <- merge(comps, df, by='cokey')
  }
  nrow(comps)
  setwd(wdir)
  write.csv(comps, 'comps_all_FC_methods.csv', row.names = FALSE)
  
  #ssurgo_comp_no_rock_reskind_mult <- ssurgo_comp_no_rock_reskind_mult[order(ssurgo_comp_no_rock_reskind_mult$cokey, ssurgo_comp_no_rock_reskind_mult$resdept_r), ]
  
# colnames to compute missing data for: AD_alfalfa_cmH2O, AD_alfalfa_soilthickness, aws0150ck_cmH2O, aws0150ck_soilthickness, AD_grapes_cmH2O, AD_grapes_soilthickness, AD_almonds_cmH2O, AD_almonds_soilthickness, AD_walnuts_cmH2O, AD_walnuts_soilthickness
  
#code to select deepest horizon                                              
  comp_bottom_hz <- as.data.frame(ssurgo_horizon %>% group_by(cokey) %>% slice(which.max(hzdept_r))) #select the deepest horizons by cokey
  
  
  #ssurgo_comp <- ssurgo_comp[-i, ] #take out cokeys with paralithic or lithic reskind 
#now find which cokeys in ssurgo_comp have other reskinds besides paralithic or lithic
  # cokeys_rock_dups <- vector()
  # for (i in 1:nrow(cokeys_rock)) {
  #   if (cokeys_rock$cokey[i] %in% ssurgo_comp$cokey) { #if the cokeys with paralithic or lithic contact have a second reskind, save the indices of these cokeys in ssurgo_comp to a vector
  #     cokeys_rock_dups <- c(cokeys_rock_dups, match(cokeys_rock$cokey[i], ssurgo_comp$cokey))
  #   }
  # }
  # cokeys_rock <- rbind(cokeys_rock, ssurgo_comp[cokeys_rock_dups, ])
  # ssurgo_comp <- ssurgo_comp[-cokeys_rock_dups, ]
  # cokeys_rock_dups <- vector()
  # for (i in 1:nrow(cokeys_rock)) {
  #   if (cokeys_rock$cokey[i] %in% ssurgo_comp$cokey) { #if the cokeys with paralithic or lithic contact have a second reskind, save the indices of these cokeys in ssurgo_comp to a vector
  #     cokeys_rock_dups <- c(cokeys_rock_dups, match(cokeys_rock$cokey[i], ssurgo_comp$cokey))
  #   }
  # }
  # cokeys_rock <- rbind(cokeys_rock, ssurgo_comp[cokeys_rock_dups, ]) #bind this second set of restricitons to cokeys_rock
  # ssurgo_comp <- ssurgo_comp[-cokeys_rock_dups, ] #then remove these rows from the cokeys without a paralithic or lithic contact
# get the keys for ssurgo_comp

#find bottom horizon depth and compare 0 - bottom horizon depth vs. sum(hz_thickness) by cokey 
  
#count instances of various kinds of restrictions 
tapply(ssurgo_comp$cokey, ssurgo_comp$reskind, length)
# Abrupt textural change                      Cemented horizon 
# 307                                         24 
# Densic bedrock                              Densic material 
# 2                                           39 
# Duripan                                     Lithic bedrock 
# 925                                         2399 
# Manufactured layer                          Natric 
# 45                                          47 
# Paralithic bedrock                          Petrocalcic 
# 3073                                        16 
# Salic                                       Strongly contrasting textural stratification 
# 18                                          97 
# Sulfuric 
# 4

#code to identify duplicate components
duplicated_cokeys <- ssurgo_comp$cokey[duplicated(ssurgo_comp$cokey)]
length(duplicated_cokeys) #269 duplicated cokeys out of 41,275 cokeys
# cokey_number <- 2
# print(duplicated_cokeys[cokey_number])
# ssurgo_comp[ssurgo_comp$cokey==duplicated_cokeys[cokey_number], ]
# ssurgo_horizon[ssurgo_horizon$cokey==duplicated_cokeys[cokey_number], ]
# for (i in 1:length(duplicated_cokeys)) {
#   dups <- ssurgo_comp[ssurgo_comp$cokey==duplicated_cokeys[i], ]
#   if (dups$resdept_r[1] == dups$resdept_r[2]) {
#     print(dups)
#     print(ssurgo_horizon[ssurgo_horizon$cokey==duplicated_cokeys[i], ])
#   }
# }
unique_restrictions <- vector()
for (i in 1:length(duplicated_cokeys)) {
  dups <- ssurgo_comp[ssurgo_comp$cokey==duplicated_cokeys[i], ]
  restrictions <- paste(dups$reskind[1], dups$reskind[2], collapse = ' ')
  unique_restrictions[i] <- restrictions
}
unique(unique_restrictions) #31 unique combinations of reskinds for the same cokey

  
  



