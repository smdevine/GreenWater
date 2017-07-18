#script modified and simplified from SSURGO_processing_awc_r.R to obtain 
  # required model paramters
#for summer 2017 model effort
#library(plyr)
#library(dplyr)
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
ssurgo_horizon <- ssurgo_horizon[!is.na(ssurgo_horizon$chkey), ] #get rid of 
  # chkeys that have no data first, which is 27,358 out of 71,488
fragvol_tot <- as.data.frame(tapply(ssurgo_horizon$fragvol_r, ssurgo_horizon$chkey, sum, na.rm=TRUE))
colnames(fragvol_tot) <- 'fragvol_r_sum'
fragvol_tot$chkey <- rownames(fragvol_tot)
rownames(fragvol_tot) <- NULL
ssurgo_horizon <- ssurgo_horizon[!duplicated(ssurgo_horizon$chkey), ] #get rid of duplicate chkeys, which only have unique fragvol_r and fragsize_r values
ssurgo_horizon <- ssurgo_horizon[ ,-which(colnames(ssurgo_horizon) == "fragvol_r" | colnames(ssurgo_horizon)=="fragsize_r")]
ssurgo_horizon <- merge(ssurgo_horizon, fragvol_tot, by='chkey')
write.csv(ssurgo_horizon, 'CA_horizon_data_2017-07-13_clean.csv', row.names = FALSE)

# a real hack job developed Oct 2017 to deal with duplicate cokeys as a result 
  # of reskind query, since multiple reskinds require multiple cokey rows with 
  # duplicate data in order to apply ssurgo_horizon sum routine, need to first 
  # identify which cokeys have lithic or paralithic contacts or both, so that 
  # these horizons are not given plant available values purpose is to get the 
  # soil comp data into the correct dimensions for ease and to distinguish 
  # between modifiable and unmodifiable soil profiles
ssurgo_comp <- read.csv("CA_comp_data_2017-07-13.csv")
ssurgo_comp$majcompflag[ssurgo_comp$majcompflag=='No '] <- 'No' #strip out the space after 'No '
ssurgo_comp <- ssurgo_comp[!is.na(ssurgo_comp$cokey), ] #there were 6 mukeys with no identified cokeys
ssurgo_comp$reskind[is.na(ssurgo_comp$reskind) & ssurgo_comp$majcompflag=='Yes'] <- 'None'
i <- !is.na(ssurgo_comp$reskind) & (ssurgo_comp$reskind=='Paralithic bedrock' | ssurgo_comp$reskind=='Lithic bedrock' | ssurgo_comp$reskind=='Densic bedrock')
ssurgo_comp_rock <- ssurgo_comp[i, ]
ssurgo_comp_no_rock <- ssurgo_comp[!i, ] #56,126 of 56,451 are unique cokeys, meaning 78 have more than one reskind that is not a paralithic or lithic restriction
cokeys_rock <- ssurgo_comp$cokey[i]
cokeys_rock_unique <- unique(cokeys_rock) #15,779 of 16,215 are unique cokeys, meaning 436 cokeys have both paralithic and lithic and that 15,779 are cokeys with either a paralithic or lithic reskind in SSURGO
k <- ssurgo_comp_no_rock$cokey %in% cokeys_rock_unique #this is find cokeys with paralithic or lithic reskinds that have other kinds of restrictions
ssurgo_comp_rock <- rbind(ssurgo_comp_rock, ssurgo_comp_no_rock[k, ])
ssurgo_comp_no_rock <- ssurgo_comp_no_rock[!k, ]
if (sum(ssurgo_comp_no_rock$cokey %in% cokeys_rock_unique) > 0) {
  print('There are still cokeys in ssurgo_comp_no_rock that have at least one paralithic or lithic contact.')
}  else {print('All good')}
cokeys_rock <- ssurgo_comp_rock$cokey #redefine cokeys_rock
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
cokeys_no_rock <- ssurgo_comp_no_rock$cokey #55,966 total cokeys
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
write.csv(ssurgo_comp_no_rock, 'CA_comp_data_no_rock_2017-07-17.csv', row.names = FALSE)
write.csv(ssurgo_comp_rock, 'CA_comp_data_rock_2017-07-17.csv', row.names = FALSE)

#one-time operation to split soil horizon data into a set with a rock contact and those with no rock contact
#split ssurgo_horizons into two data.frames
ssurgo_horizon <- read.csv('CA_horizon_data_2017-07-13_clean.csv')
ssurgo_horizon$hzthickness <- ssurgo_horizon$hzdepb_r - ssurgo_horizon$hzdept_r
i <- ssurgo_horizon$cokey %in% cokeys_rock_unique
ssurgo_horizon_Cr_R <- ssurgo_horizon[i, ]
ssurgo_horizon_no_Cr_R <- ssurgo_horizon[!i, ]
if (sum(ssurgo_horizon_no_Cr_R$cokey %in% cokeys_rock_unique) > 0) {'Hay problemas aqui'}
write.csv(ssurgo_horizon_Cr_R, 'ssurgo_horizon_Cr_R_2017-07-17.csv', row.names = FALSE)
write.csv(ssurgo_horizon_no_Cr_R, 'ssurgo_horizon_no_Cr_R_2017-07-17.csv', row.names = FALSE)

aggregate_SSURGO <- function(results, fc_def, figure_label) {
  setwd(SoilsDataDir)
  list.files(SoilsDataDir, pattern = glob2rx('*.csv'))
  ssurgo_horizon_no_Cr_R <- read.csv('ssurgo_horizon_no_Cr_R_2017-07-17.csv')
  ssurgo_horizon_Cr_R <- read.csv('ssurgo_horizon_Cr_R_2017-07-17.csv')
  ssurgo_comp_no_rock <- read.csv('CA_comp_data_no_rock_2017-07-17.csv')
  ssurgo_comp_rock <- read.csv('CA_comp_data_rock_2017-07-17.csv')
  ssurgo_mu <- read.csv("CA_mu_data_2017-07-13.csv")
  
#add area info to component table
  ssurgo_comp_no_rock <- merge(ssurgo_comp_no_rock, ssurgo_mu, by='mukey')
  ssurgo_comp_no_rock$compacres <- ssurgo_comp_no_rock$muacres*(ssurgo_comp_no_rock$comppct_r/100) #correct area based upon component percentage
  ssurgo_comp_rock <- merge(ssurgo_comp_rock, ssurgo_mu, by='mukey')
  ssurgo_comp_rock$compacres <- ssurgo_comp_rock$muacres*(ssurgo_comp_rock$comppct_r/100)

#sum_modified(ssurgo_horizon_Cr_R$awc_r==0) #2499 out of 52491
#sum_modified(ssurgo_horizon_no_Cr_R$awc_r==0) #963 out of 42327
#sum(is.na(ssurgo_horizon_Cr_R$awc_r)) #13680 out of 52491
#sum(is.na(ssurgo_horizon_no_Cr_R)) #18391 out of 42327
#unique(ssurgo_horizon$hzname[ssurgo_horizon_Cr_R$awc_r==0])

#first, sum up awc by cokey for those with paralithic or lithic reskinds with attention to possible rooting depths; there are 1464 NAs for awc_r out of 42,327
  sum_PAW_cmH2O <- function(df, rooting_depth, paw) {
    ifelse(is.na(df$hzthickness) & is.na(df[[paw]]), NA,
      ifelse(df$hzdepb_r <= rooting_depth, df[[paw]]*df$hzthickness,
        ifelse(df$hzdept_r < rooting_depth, 
          (rooting_depth - df$hzdept_r)*df[[paw]], NA)))
  }
  #function to calculate soil thickness of soils for purpose of calculating 
  #modified soil profile AWC value, not counting thickness of horizons with awc
  #equal to zero
  sum_PAW_soilthickness <- function(df, rooting_depth, paw) { 
    ifelse(df$hzdepb_r <= rooting_depth & (!is.na(df[[paw]]) & df[[paw]] != 0), 
           df$hzthickness,
           ifelse(df$hzdept_r < rooting_depth & (!is.na(df[[paw]]) & df[[paw]] != 0),
                  rooting_depth - df$hzdept_r, 0))
  }
  
  ssurgo_horizon_Cr_R$z0.5m_cmH2O_unmodified <- sum_PAW_cmH2O(ssurgo_horizon_Cr_R, 50, 'awc_r')
  ssurgo_horizon_Cr_R$z1.0m_cmH2O_unmodified <- sum_PAW_cmH2O(ssurgo_horizon_Cr_R, 100, 'awc_r')
  ssurgo_horizon_Cr_R$z1.5m_cmH2O_unmodified <- sum_PAW_cmH2O(ssurgo_horizon_Cr_R, 150, 'awc_r')
  ssurgo_horizon_Cr_R$z2.0m_cmH2O_unmodified <- sum_PAW_cmH2O(ssurgo_horizon_Cr_R, 200, 'awc_r')
  ssurgo_horizon_Cr_R$z4.0m_cmH2O_unmodified <- sum_PAW_cmH2O(ssurgo_horizon_Cr_R, 400, 'awc_r')
  ssurgo_horizon_Cr_R$z0.5m_soilthickness_unmodified <- sum_PAW_soilthickness(ssurgo_horizon_Cr_R, 50, 'awc_r')
  ssurgo_horizon_Cr_R$z1.0m_soilthickness_unmodified <- sum_PAW_soilthickness(ssurgo_horizon_Cr_R, 100, 'awc_r')
  ssurgo_horizon_Cr_R$z1.5m_soilthickness_unmodified <- sum_PAW_soilthickness(ssurgo_horizon_Cr_R, 150, 'awc_r')
  ssurgo_horizon_Cr_R$z2.0m_soilthickness_unmodified <- sum_PAW_soilthickness(ssurgo_horizon_Cr_R, 200, 'awc_r')
  ssurgo_horizon_Cr_R$z4.0m_soilthickness_unmodified <- sum_PAW_soilthickness(ssurgo_horizon_Cr_R, 400, 'awc_r')
  ssurgo_horizon_Cr_R$z0.5m_cmH2O_modified <- ssurgo_horizon_Cr_R$z0.5m_cmH2O_unmodified
  ssurgo_horizon_Cr_R$z1.0m_cmH2O_modified <- ssurgo_horizon_Cr_R$z1.0m_cmH2O_unmodified
  ssurgo_horizon_Cr_R$z1.5m_cmH2O_modified <- ssurgo_horizon_Cr_R$z1.5m_cmH2O_unmodified
  ssurgo_horizon_Cr_R$z2.0m_cmH2O_modified <- ssurgo_horizon_Cr_R$z2.0m_cmH2O_unmodified
  ssurgo_horizon_Cr_R$z4.0m_cmH2O_modified <- ssurgo_horizon_Cr_R$z4.0m_cmH2O_unmodified

#now sum up AD H2O and soil thickness used in the calculation by cokey
  sum_PAW_cmH2O_bycokey <- function(df, var) {
    y <- aggregate(x=df[[var]], by=list(df$cokey), FUN=sum_modified)
    colnames(y) <- c('cokey', paste0(var, '_comp'))
    return(y)
  }
  
#second, specifically for alfalfa, sum up awc by cokey as above but for those soils
#without paralithic or lithic contacts but assuming no modification of the soil profile.
#data could be extended with exception of restrictive horizons
  ssurgo_horizon_no_Cr_R$z0.5m_cmH2O_unmodified <- sum_PAW_cmH2O(ssurgo_horizon_no_Cr_R, 50, 'awc_r')
  ssurgo_horizon_no_Cr_R$z1.0m_cmH2O_unmodified <- sum_PAW_cmH2O(ssurgo_horizon_no_Cr_R, 100, 'awc_r')
  ssurgo_horizon_no_Cr_R$z1.5m_cmH2O_unmodified <- sum_PAW_cmH2O(ssurgo_horizon_no_Cr_R, 150, 'awc_r')
  ssurgo_horizon_no_Cr_R$z2.0m_cmH2O_unmodified <- sum_PAW_cmH2O(ssurgo_horizon_no_Cr_R, 200, 'awc_r')
  ssurgo_horizon_no_Cr_R$z4.0m_cmH2O_unmodified <- sum_PAW_cmH2O(ssurgo_horizon_no_Cr_R, 400, 'awc_r')
  ssurgo_horizon_no_Cr_R$z0.5m_soilthickness_unmodified <- sum_PAW_soilthickness(ssurgo_horizon_no_Cr_R, 50, 'awc_r')
  ssurgo_horizon_no_Cr_R$z1.0m_soilthickness_unmodified <- sum_PAW_soilthickness(ssurgo_horizon_no_Cr_R, 100, 'awc_r')
  ssurgo_horizon_no_Cr_R$z1.5m_soilthickness_unmodified <- sum_PAW_soilthickness(ssurgo_horizon_no_Cr_R, 150, 'awc_r')
  ssurgo_horizon_no_Cr_R$z2.0m_soilthickness_unmodified <- sum_PAW_soilthickness(ssurgo_horizon_no_Cr_R, 200, 'awc_r')
  ssurgo_horizon_no_Cr_R$z4.0m_soilthickness_unmodified <- sum_PAW_soilthickness(ssurgo_horizon_no_Cr_R, 400, 'awc_r')

  #see https://stackoverflow.com/questions/8091303/simultaneously-merge-multiple-data-frames-in-a-list
  comps.R.Cr <- Reduce(function(...) merge(..., all=T), list(sum_PAW_cmH2O_bycokey(ssurgo_horizon_Cr_R, 'z0.5m_cmH2O_unmodified'), sum_PAW_cmH2O_bycokey(ssurgo_horizon_Cr_R, 'z1.0m_cmH2O_unmodified'), sum_PAW_cmH2O_bycokey(ssurgo_horizon_Cr_R, 'z1.5m_cmH2O_unmodified'), sum_PAW_cmH2O_bycokey(ssurgo_horizon_Cr_R, 'z2.0m_cmH2O_unmodified'), sum_PAW_cmH2O_bycokey(ssurgo_horizon_Cr_R, 'z4.0m_cmH2O_unmodified'), sum_PAW_cmH2O_bycokey(ssurgo_horizon_Cr_R, 'z0.5m_soilthickness_unmodified'), sum_PAW_cmH2O_bycokey(ssurgo_horizon_Cr_R, 'z1.0m_soilthickness_unmodified'), sum_PAW_cmH2O_bycokey(ssurgo_horizon_Cr_R, 'z1.5m_soilthickness_unmodified'), sum_PAW_cmH2O_bycokey(ssurgo_horizon_Cr_R, 'z2.0m_soilthickness_unmodified'), sum_PAW_cmH2O_bycokey(ssurgo_horizon_Cr_R, 'z4.0m_soilthickness_unmodified'), sum_PAW_cmH2O_bycokey(ssurgo_horizon_Cr_R, 'z0.5m_cmH2O_modified'), sum_PAW_cmH2O_bycokey(ssurgo_horizon_Cr_R, 'z1.0m_cmH2O_modified'), sum_PAW_cmH2O_bycokey(ssurgo_horizon_Cr_R, 'z1.5m_cmH2O_modified'), sum_PAW_cmH2O_bycokey(ssurgo_horizon_Cr_R, 'z2.0m_cmH2O_modified'), sum_PAW_cmH2O_bycokey(ssurgo_horizon_Cr_R, 'z4.0m_cmH2O_modified'))) #modified columns are added using the same methodology as unmodified so as to match dim of comps.no.R.Cr
  #out of these, 235 have a sum of 0 and 766 are NA
  
  comps.no.R.Cr <- Reduce(function(...) merge(..., all=T), list(sum_PAW_cmH2O_bycokey(ssurgo_horizon_no_Cr_R, 'z0.5m_cmH2O_unmodified'), sum_PAW_cmH2O_bycokey(ssurgo_horizon_no_Cr_R, 'z1.0m_cmH2O_unmodified'), sum_PAW_cmH2O_bycokey(ssurgo_horizon_no_Cr_R, 'z1.5m_cmH2O_unmodified'), sum_PAW_cmH2O_bycokey(ssurgo_horizon_no_Cr_R, 'z2.0m_cmH2O_unmodified'), sum_PAW_cmH2O_bycokey(ssurgo_horizon_no_Cr_R, 'z4.0m_cmH2O_unmodified'), sum_PAW_cmH2O_bycokey(ssurgo_horizon_no_Cr_R, 'z0.5m_soilthickness_unmodified'), sum_PAW_cmH2O_bycokey(ssurgo_horizon_no_Cr_R, 'z1.0m_soilthickness_unmodified'), sum_PAW_cmH2O_bycokey(ssurgo_horizon_no_Cr_R, 'z1.5m_soilthickness_unmodified'), sum_PAW_cmH2O_bycokey(ssurgo_horizon_no_Cr_R, 'z2.0m_soilthickness_unmodified'), sum_PAW_cmH2O_bycokey(ssurgo_horizon_no_Cr_R, 'z4.0m_soilthickness_unmodified')))
#third, for walnuts, almonds, and grapes, sum up awc by cokey for those without paralithic or lithic contacts by adding missing AD data for the portion of the profile of the rooting zone without it, assuming profile wtd avgs of the portion of the profile that has data for the missing data.

#out of these, 169 have a sum of 0 and 453 are NA for awc and 623 are NA for soilthickness, because NAs for soil thickness include those that had 0 awc
  AD_sum_modified <- function(df, paw, rootdepth, soilthickness, percent_available) {df[[paw]] + df[[paw]]/df[[soilthickness]]*(rootdepth - df[[soilthickness]])*(percent_available/100)}
  comps.no.R.Cr$z0.5m_cmH2O_modified_comp <- AD_sum_modified(comps.no.R.Cr, 'z0.5m_cmH2O_unmodified_comp', 50, 'z0.5m_soilthickness_unmodified_comp', 100)
  comps.no.R.Cr$z1.0m_cmH2O_modified_comp <- AD_sum_modified(comps.no.R.Cr, 'z1.0m_cmH2O_unmodified_comp', 100, 'z1.0m_soilthickness_unmodified_comp', 100)
  comps.no.R.Cr$z1.5m_cmH2O_modified_comp <- AD_sum_modified(comps.no.R.Cr, 'z1.5m_cmH2O_unmodified_comp', 150, 'z1.5m_soilthickness_unmodified_comp', 100)
  comps.no.R.Cr$z2.0m_cmH2O_modified_comp <- AD_sum_modified(comps.no.R.Cr, 'z2.0m_cmH2O_unmodified_comp', 200, 'z2.0m_soilthickness_unmodified_comp', 100)
  comps.no.R.Cr$z4.0m_cmH2O_modified_comp <- AD_sum_modified(comps.no.R.Cr, 'z4.0m_cmH2O_unmodified_comp', 400, 'z4.0m_soilthickness_unmodified_comp', 100)  
  lapply(comps.no.R.Cr, summary)
  lapply(comps.R.Cr, summary)

#now, populate missing component data with data
#merge aggregated horizon level data with component level data and tag those without horizon level 0.5 m original data
  ssurgo_comp_no_rock <- merge(ssurgo_comp_no_rock, comps.no.R.Cr, by='cokey', all = TRUE)
  ssurgo_comp_rock <- merge(ssurgo_comp_rock, comps.R.Cr, by='cokey', all = TRUE)
  ssurgo_comp_rock$SSURGO_awc_data <- 'Yes'
  ssurgo_comp_rock$SSURGO_awc_data[is.na(ssurgo_comp_rock$z0.5m_cmH2O_unmodified_comp)] <- 'No'
  ssurgo_comp_no_rock$SSURGO_awc_data <- 'Yes'
  ssurgo_comp_no_rock$SSURGO_awc_data[is.na(ssurgo_comp_no_rock$z0.5m_cmH2O_unmodified_comp)] <- 'No'
  
  #concept is to replace NAs with available data for a given component name; 
  #first, from the same survey area
  #then, from the all the survey areas if not available within a given survey area
  #could be re-written without embedded for-loops to increase speed
  area_weighted_average <- function(df, varname) { #modified function from 'SSURGO_analysis_v2.R' in 'Forest Dieoff and Hydrology/R Scripts' directory.  Uses data from the same areasymbol first and then goes beyond the areasymbol next.  Could do the same to fill in reskind data, if necessary
    df <- ssurgo_comp_no_rock #temp arg
    varname <- 'z0.5m_cmH2O_unmodified_comp' #temp arg
    has_data <- df[!is.na(df[[varname]]) & !is.na(df$compacres), ]
    data_area <- as.data.frame(tapply(has_data$compacres, list(has_data$compname, has_data$areasymbol), sum))
    data_calc_term <- as.data.frame(tapply(has_data[[varname]]*has_data$compacres, list(has_data$compname, has_data$areasymbol), sum))
    data_by_compname <- data_calc_term/data_area
    #replace NA AWC values with component name average AWC values
    i <- which(is.na(df[[varname]]))
    for (j in 1:length(i)) {
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
    data_area <- as.data.frame(tapply(has_data$compacres, has_data$compname, sum))
    data_calc_term <- as.data.frame(tapply(has_data[[varname]]*has_data$compacres, has_data$compname, sum))
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
    return(df)
  }
  #run function for ssurgo_comp_no_rock
  ssurgo_no_rock <- area_weighted_average(ssurgo_comp_no_rock, "z0.5m_cmH2O_unmodified_comp")
  ssurgo_no_rock <- area_weighted_average(ssurgo_comp_no_rock, "z1.0m_cmH2O_unmodified_comp")
  ssurgo_no_rock <- area_weighted_average(ssurgo_comp_no_rock, "z1.5m_cmH2O_unmodified_comp")
  ssurgo_no_rock <- area_weighted_average(ssurgo_comp_no_rock, "z2.0m_cmH2O_unmodified_comp")
  ssurgo_no_rock <- area_weighted_average(ssurgo_comp_no_rock, "z4.0m_cmH2O_unmodified_comp")
  ssurgo_no_rock <- area_weighted_average(ssurgo_comp_no_rock, "z0.5m_cmH2O_modified_comp")
  ssurgo_no_rock <- area_weighted_average(ssurgo_comp_no_rock, "z1.0m_cmH2O_modified_comp")
  ssurgo_no_rock <- area_weighted_average(ssurgo_comp_no_rock, "z1.5m_cmH2O_modified_comp")
  ssurgo_no_rock <- area_weighted_average(ssurgo_comp_no_rock, "z2.0m_cmH2O_modified_comp")
  ssurgo_no_rock <- area_weighted_average(ssurgo_comp_no_rock, "z4.0m_cmH2O_modified_comp")
  
  #run function for ssurgo_comp_rock
  ssurgo_comp_rock <- area_weighted_average(ssurgo_comp_rock, "z0.5m_cmH2O_unmodified_comp")
  ssurgo_comp_rock <- area_weighted_average(ssurgo_comp_rock, "z1.0m_cmH2O_unmodified_comp")
  ssurgo_comp_rock <- area_weighted_average(ssurgo_comp_rock, "z1.5m_cmH2O_unmodified_comp")
  ssurgo_comp_rock <- area_weighted_average(ssurgo_comp_rock, "z2.0m_cmH2O_unmodified_comp")
  ssurgo_comp_rock <- area_weighted_average(ssurgo_comp_rock, "z4.0m_cmH2O_unmodified_comp")
  ssurgo_comp_rock <- area_weighted_average(ssurgo_comp_rock, "z0.5m_cmH2O_modified_comp")
  ssurgo_comp_rock <- area_weighted_average(ssurgo_comp_rock, "z1.0m_cmH2O_modified_comp")
  ssurgo_comp_rock <- area_weighted_average(ssurgo_comp_rock, "z1.5m_cmH2O_modified_comp")
  ssurgo_comp_rock <- area_weighted_average(ssurgo_comp_rock, "z2.0m_cmH2O_modified_comp")
  ssurgo_comp_rock <- area_weighted_average(ssurgo_comp_rock, "z4.0m_cmH2O_modified_comp")
  
  #merge and rbind data.frames to fill in data for minor components based off of component name averages
  
  ssurgo_comp_all <- rbind(ssurgo_comp_rock, ssurgo_comp_no_rock)
  #temp fix for rock outcrop (could be fixed earlier in script).
  summary(ssurgo_comp_all$z0.5m_cmH2O_modified_comp[ssurgo_comp_all$compname=='Rock outcrop'])
  summary(ssurgo_comp_all$z0.5m_cmH2O_unmodified_comp[ssurgo_comp_all$compname=='Rock outcrop'])
  #CHECK COLUMN REFERENCES HERE
  colnames(ssurgo_comp_all)
  ssurgo_comp_all[ssurgo_comp_all$compname=='Rock outcrop', 22:36] <- 0 #change all paw related variables to 0 where component name is Rock Outcrop
  ssurgo_comp_all <- ssurgo_comp_all[order(ssurgo_comp_all$mukey, ssurgo_comp_all$comppct_r, decreasing = c(FALSE, TRUE)), ]
  setwd(results)
  write.csv(ssurgo_comp_all, 'comps_all_summary_7.17.17_dbmodified.csv', row.names = FALSE) # can read this in to shorten script
  ssurgo_comp_all <- read.csv('comps_all_summary_7.17.17.csv')
  
  
  unique(ssurgo_comp_all$compname[ssurgo_comp_all$majcompflag=='Yes' & is.na(ssurgo_comp_all$z0.5m_cmH2O_unmodified_comp)])
  sum(is.na(ssurgo_mu$muacres)) #331 mukeys out of 15,086 don't have acreage data from the SDA_query in SSURGO_download.R
  sum(ssurgo_comp_all$majcompflag=='Yes' & is.na(ssurgo_comp_all$z0.5m_cmH2O_unmodified_comp)) #1,179 major soil components with no data; 1,135 after database modification
  sum(ssurgo_comp_all$majcompflag=='Yes' & is.na(ssurgo_comp_all$z0.5m_cmH2O_unmodified_comp) & is.na(ssurgo_comp_all$compacres)) #71 of these also have NA for compacres; 71 after database modification
  sum_modified(ssurgo_comp_all$compacres[ssurgo_comp_all$majcompflag=='Yes' & is.na(ssurgo_comp_all$z0.5m_cmH2O_unmodified_comp)]) #about 6.7 million acres for the 1,108 major soil components with no AWC data; 6.4 million acres after database modification 
  unique(ssurgo_comp_all$areasymbol[ssurgo_comp_all$majcompflag=='Yes' & is.na(ssurgo_comp_all$z0.5m_cmH2O_unmodified_comp)]) #present in all 90 soil survey areas
  unique(ssurgo_comp_all$compname[ssurgo_comp_all$majcompflag=='Yes' & is.na(ssurgo_comp_all$z0.5m_cmH2O_unmodified_comp)]) #143 unique soil component names are major components and have no data; however, there may be another instance of the same name with data; 137 after database modification
  sum(ssurgo_comp_all$SSURGO_awc_data=='No') #45,349 with no original data
  sum(ssurgo_comp_all$SSURGO_awc_data=='Yes') #26,139 with original data
  sum(is.na(ssurgo_comp_all$z0.5m_cmH2O_modified_comp)) #41,281 no data after database modification
  
#for ssurgo_comp_rock
  
  
  
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

#code to select deepest horizon                                              
  comp_bottom_hz <- as.data.frame(ssurgo_horizon %>% group_by(cokey) %>% slice(which.max(hzdept_r))) #select the deepest horizons by cokey
  
  


  



  
  



