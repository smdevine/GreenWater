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
ssurgo_horizon <- read.csv('CA_horizon_data_2017-07-13_clean.csv')
#see rules for soil textural classes.pdf in 'Allowable Depletion' folder
textural.class.calc <- function(sand, silt, clay) {
  ifelse(is.na(sand) | is.na(silt) | is.na(clay), NA,
  ifelse(sand + silt + clay > 101 |
    sand + silt + clay < 99, 'proportions do not sum to 100+-1%',
  ifelse(silt + 1.5 * clay < 15, 'sand',
  ifelse(silt + 1.5 * clay >= 15 & silt + 2 * clay < 30, 'loamy sand',
  ifelse((clay >= 7 & clay < 20 & sand > 52 & silt + 2 * clay >= 30) | 
    (clay < 7 & silt < 50 & silt + 2 * clay >= 30), 'sandy loam',
  ifelse(clay >= 7 & clay < 27 & silt >=28 & silt < 50 & sand <= 52, 'loam',
  ifelse((silt >= 50 & clay >= 12 & clay < 27) | 
    (silt >=50 & silt < 80 & clay < 12), 'silt loam',
  ifelse(silt >= 80 & clay < 12, 'silt',
  ifelse(clay >= 20 & clay < 35 & silt < 28 & sand > 45, 'sandy clay loam',
  ifelse(clay >= 27 & clay < 40 & sand > 20 & sand <= 45, 'clay loam',
  ifelse(clay >= 27 & clay < 40 & sand <= 20, 'silty clay loam',
  ifelse(clay >= 35 & sand > 45, 'sandy clay',
  ifelse(clay >= 40 & silt >= 40, 'silty clay',
  ifelse(clay >= 40 & sand <= 45 & silt < 40, 'clay',
    'undefined textural class'))))))))))))))
}
ssurgo_horizon$texture.sums <- ssurgo_horizon$sandtotal_r + ssurgo_horizon$silttotal_r + ssurgo_horizon$claytotal_r
summary(ssurgo_horizon$texture.sums)
sum(ssurgo_horizon$texture.sums != 100 & !is.na(ssurgo_horizon$texture.sums)) #801 don't add up to 100
table(ssurgo_horizon$texture.sums[ssurgo_horizon$texture.sums != 100 & !is.na(ssurgo_horizon$texture.sums)])
FixTexture <- function(df, varname) {
  ifelse(is.na(df$texture.sums), df[[varname]] <- df[[varname]],
  ifelse(df$texture.sums > 99 & df$texture.sums < 101, df[[varname]] <- df[[varname]],
  ifelse(df$texture.sums < 90 | df$texture.sums > 110, df[[varname]] <- df[[varname]],
    df[[varname]] <- df[[varname]] * 100 / df$texture.sums)))
}
ssurgo_horizon$sandtotal_r <- FixTexture(ssurgo_horizon, 'sandtotal_r')
ssurgo_horizon$silttotal_r <- FixTexture(ssurgo_horizon, 'silttotal_r')
ssurgo_horizon$claytotal_r <- FixTexture(ssurgo_horizon, 'claytotal_r')
ssurgo_horizon$texture.sums <- ssurgo_horizon$sandtotal_r + ssurgo_horizon$silttotal_r + ssurgo_horizon$claytotal_r
table(ssurgo_horizon$texture.sums)

test <- ssurgo_horizon[ssurgo_horizon$texture.sums != 100 & !is.na(ssurgo_horizon$texture.sums), ]
test2 <- test
test2$sandtotal_r <- FixTexture(test2, 'sandtotal_r')
test2$silttotal_r <- FixTexture(test2, 'silttotal_r')
test2$claytotal_r <- FixTexture(test2, 'claytotal_r')
test2$texture.sums <- test2$sandtotal_r + test2$silttotal_r + test2$claytotal_r
table(test$texture.sums)
table(test2$texture.sums)


# a real hack job developed Oct 2017 to deal with duplicate cokeys as a result 
  # of reskind query, since multiple reskinds require multiple cokey rows with 
  # duplicate data in order to apply ssurgo_horizon sum routine, need to first 
  # identify which cokeys have lithic or paralithic contacts or both, so that 
  # these horizons are not given plant available water values. purpose is to get 
  # the soil comp data into 1 row per cokey and to distinguish between 
  # modifiable and unmodifiable soil profiles.
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
    has_data <- df[!is.na(df[[varname]]) & !is.na(df$compacres), ]
    data_area <- as.data.frame(tapply(has_data$compacres, list(has_data$compname, has_data$areasymbol), sum))
    data_calc_term <- as.data.frame(tapply(has_data[[varname]]*has_data$compacres, list(has_data$compname, has_data$areasymbol), sum))
    data_by_compname <- data_calc_term/data_area
    #replace NA AWC values with component name average AWC values
    i <- which(is.na(df[[varname]]))
    for (j in 1:length(i)) {
      j <- 3 #temp arg
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
  ssurgo_comp_no_rock <- area_weighted_average(ssurgo_comp_no_rock, "z0.5m_cmH2O_unmodified_comp")
  ssurgo_comp_no_rock <- area_weighted_average(ssurgo_comp_no_rock, "z1.0m_cmH2O_unmodified_comp")
  ssurgo_comp_no_rock <- area_weighted_average(ssurgo_comp_no_rock, "z1.5m_cmH2O_unmodified_comp")
  ssurgo_comp_no_rock <- area_weighted_average(ssurgo_comp_no_rock, "z2.0m_cmH2O_unmodified_comp")
  ssurgo_comp_no_rock <- area_weighted_average(ssurgo_comp_no_rock, "z4.0m_cmH2O_unmodified_comp")
  ssurgo_comp_no_rock <- area_weighted_average(ssurgo_comp_no_rock, "z0.5m_cmH2O_modified_comp")
  ssurgo_comp_no_rock <- area_weighted_average(ssurgo_comp_no_rock, "z1.0m_cmH2O_modified_comp")
  ssurgo_comp_no_rock <- area_weighted_average(ssurgo_comp_no_rock, "z1.5m_cmH2O_modified_comp")
  ssurgo_comp_no_rock <- area_weighted_average(ssurgo_comp_no_rock, "z2.0m_cmH2O_modified_comp")
  ssurgo_comp_no_rock <- area_weighted_average(ssurgo_comp_no_rock, "z4.0m_cmH2O_modified_comp")
  
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
  write.csv(ssurgo_comp_all, 'comps_all_summary_7.18.17_dbmodified.csv', row.names = FALSE) # can read this in to shorten script
  
  #then apply area_weighted_average again, so that minor components that did not have an identified Cr or R are fixed; this could be fixed above in the reskind synthesis work but is more of a challenge because of multiple reskinds.  See SSURGO_analysis_v2.R for an example of how to do it.
  ssurgo_comp_all <- area_weighted_average(ssurgo_comp_all, "z0.5m_cmH2O_unmodified_comp")
  ssurgo_comp_all <- area_weighted_average(ssurgo_comp_all, "z1.0m_cmH2O_unmodified_comp")
  ssurgo_comp_all <- area_weighted_average(ssurgo_comp_all, "z1.5m_cmH2O_unmodified_comp")
  ssurgo_comp_all <- area_weighted_average(ssurgo_comp_all, "z2.0m_cmH2O_unmodified_comp")
  ssurgo_comp_all <- area_weighted_average(ssurgo_comp_all, "z4.0m_cmH2O_unmodified_comp")
  ssurgo_comp_all <- area_weighted_average(ssurgo_comp_all, "z0.5m_cmH2O_modified_comp")
  ssurgo_comp_all <- area_weighted_average(ssurgo_comp_all, "z1.0m_cmH2O_modified_comp")
  ssurgo_comp_all <- area_weighted_average(ssurgo_comp_all, "z1.5m_cmH2O_modified_comp")
  ssurgo_comp_all <- area_weighted_average(ssurgo_comp_all, "z2.0m_cmH2O_modified_comp")
  ssurgo_comp_all <- area_weighted_average(ssurgo_comp_all, "z4.0m_cmH2O_modified_comp")
  
  setwd(results)
  write.csv(ssurgo_comp_all, 'comps_all_summary_7.18.17_dbmodified_final.csv', row.names = FALSE)
  #get some metadata
  unique(ssurgo_comp_all$compname[ssurgo_comp_all$majcompflag=='Yes' & is.na(ssurgo_comp_all$z0.5m_cmH2O_unmodified_comp)])
  sum(is.na(ssurgo_mu$muacres)) #331 mukeys out of 15,086 don't have acreage data from the SDA_query in SSURGO_download.R
  sum(ssurgo_comp_all$majcompflag=='Yes' & is.na(ssurgo_comp_all$z0.5m_cmH2O_unmodified_comp)) #1,179 major soil components with no data; 350 after database modification; 314 after final db modification
  sum(ssurgo_comp_all$majcompflag=='Yes' & is.na(ssurgo_comp_all$z0.5m_cmH2O_unmodified_comp) & is.na(ssurgo_comp_all$compacres)) #71 of these also have NA for compacres; 14 after database modification
  sum_modified(ssurgo_comp_all$compacres[ssurgo_comp_all$majcompflag=='Yes' & is.na(ssurgo_comp_all$z0.5m_cmH2O_unmodified_comp)]) #about 6.7 million acres for the 1,108 major soil components with no AWC data; 5.1 million acres after database modification; 5.0 million acres after final modification
  unique(ssurgo_comp_all$areasymbol[ssurgo_comp_all$majcompflag=='Yes' & is.na(ssurgo_comp_all$z0.5m_cmH2O_unmodified_comp)]) #present in all 90 soil survey areas, also after db modification
  unique(ssurgo_comp_all$compname[ssurgo_comp_all$majcompflag=='Yes' & is.na(ssurgo_comp_all$z0.5m_cmH2O_unmodified_comp)]) #143 unique soil component names are major components and have no data; however, there may be another instance of the same name with data; 86 after database modification; 76 after final db modification
  sum(ssurgo_comp_all$SSURGO_awc_data=='No') #45,349 with no original data
  sum(ssurgo_comp_all$SSURGO_awc_data=='Yes') #26,139 with original data
  sum(is.na(ssurgo_comp_all$z0.5m_cmH2O_modified_comp)) #13,750 no data after database modification; 2,112 after final db modification (running 'area_weighted_avg' on ssurgo_all)

#code to select deepest horizon                                              
  sum(is.na(ssurgo_horizon_Cr_R$awc_r)) #13,680 are NA
  sum(is.na(ssurgo_horizon_no_Cr_R$awc_r)) #1,464 are NA
  i <- aggregate(ssurgo_horizon_Cr_R$hzdepb_r, list(ssurgo_horizon_Cr_R$cokey), which.min)

  
  surface_weighting <- function(df, depth) {
    df <- df[,c('cokey', 'hzdept_r', 'hzdepb_r', 'awc_r', 'wfifteenbar_r', 'fragvol_r_sum', 'claytotal_r', 'silttotal_r', 'sandtotal_r')]
    #print(class(df))
    if (nrow(df) == 1) {
      return(df)
    }
    else if (all(is.na(df$awc_r)) & all(is.na(df$wfifteenbar_r)) & all(is.na(df$sandtotal_r))) {
      return(df[1,])
    }
    else if (nrow(df) > 1) {
      if (TRUE %in% is.na(df$awc_r)) { #this leaves the possibility that there is an issue with some other relevant variable down the road
        df2 <- df[!is.na(df$awc_r), ]
        if (nrow(df2) == 1) {
          return(df2)
        }
        else if (nrow(df2)==0) {
            return(df[1,])
        } else {
            weighting_factor <- (depth - df2$hzdept_r - ifelse(df2$hzdepb_r <= 10, depth - df2$hzdepb_r, 0))/10
            df2[ ,4:ncol(df)] <- df2[ ,4:ncol(df2)]*weighting_factor
            df3 <- t(apply(df2[ ,4:ncol(df2)], 2, sum))
            df3 <- cbind(df[1,1], 0, 10, df3)
            colnames(df3)[1:3] <- c('cokey', 'hzdept_r', 'hzdepb_r')
            return(df3)
        }
      } else {
          weighting_factor <- (depth - df$hzdept_r - ifelse(df$hzdepb_r <= 10, depth - df$hzdepb_r, 0))/10
          df[,4:ncol(df)] <- df[,4:ncol(df)]*weighting_factor
          df2 <- t(apply(df[,4:ncol(df)], 2, sum))
          df <- cbind(df[1,1], 0, 10, df2)
          colnames(df)[1:3] <- c('cokey', 'hzdept_r', 'hzdepb_r')
          return(df)
      }
    }
  }
  

  
  depth <- 10 #depth is in cm soil
  surface.horizons.Cr.R <- ssurgo_horizon_Cr_R[ssurgo_horizon_Cr_R$hzdept_r < 10, ]
  surface.data.Cr.R <- do.call(rbind, lapply(split(surface.horizons.Cr.R, surface.horizons.Cr.R$cokey), surface_weighting, depth=depth))
  surface.data.Cr.R$TEW <- (surface.data.Cr.R$awc_r + 0.5*surface.data.Cr.R$wfifteenbar_r/100)*depth*10 #this converts it to mm H2O per X cm soil; typically 10-15 cm soil
  surface.horizons.no.Cr.R <- ssurgo_horizon_no_Cr_R[ssurgo_horizon_no_Cr_R$hzdept_r < 10, ]
  surface.data.no.Cr.R <- do.call(rbind, lapply(split(surface.horizons.no.Cr.R, surface.horizons.no.Cr.R$cokey), surface_weighting, depth=depth))
  surface.data.no.Cr.R$TEW <- (surface.data.no.Cr.R$awc_r + 0.5*surface.data.no.Cr.R$wfifteenbar_r/100)*depth*10
  summary(surface.data.Cr.R)
  summary(surface.data.no.Cr.R)
  surface.data.Cr.R$textural.class <- textural.class.calc(surface.data.Cr.R$sandtotal_r, surface.data.Cr.R$silttotal_r, surface.data.Cr.R$claytotal_r)
  surface.data.no.Cr.R$textural.class <- textural.class.calc(surface.data.no.Cr.R$sandtotal_r, surface.data.no.Cr.R$silttotal_r, surface.data.no.Cr.R$claytotal_r)
  surface.data.all <- rbind(surface.data.Cr.R, surface.data.no.Cr.R)
  surface.data.all$texture.class.sum <- surface.data.all$sandtotal_r + surface.data.all$silttotal_r + surface.data.all$claytotal_r
  summary(surface.data.all$texture.class.sum)

#extra functions
  textural.class.calc <- function(sand, silt, clay) {
    #if (is.na(sand) | is.na(silt) | is.na(clay)) {
    #print('Texture data is missing')
    #return(NA)
    #}
    if(silt + sand + clay != 100) {
      print('Sand, silt, and clay do not add up to 100%')
      return(NA)
    }
    else if (silt + 1.5 * clay < 15) {
      return('sand')
    }
    else if (silt + 1.5 * clay >= 15 & silt + 2 * clay < 30) {
      return('loamy sand')
    }
    else if ((clay >= 7 & clay < 20 & sand > 52 & silt + 2 * clay >= 30) | (clay < 7 & silt < 50 & silt + 2 * clay >= 30)) {
      return('sandy loam')
    }
    else if (clay >= 7 & clay < 27 & silt >=28 & silt < 50 & sand <= 52) {
      return('loam')
    }
    else if ((silt >= 50 & clay >= 12 & clay < 27) | (silt >=50 & silt < 80 & clay < 12)) {
      return('silt loam')
    }
    else if (silt >= 80 & clay < 12) {
      return('silt')
    }
    else if (clay >= 20 & clay < 35 & silt < 28 & sand > 45) {
      return('sandy clay loam')
    } 
    else if (clay >= 27 & clay < 40 & sand > 20 & sand <= 45) {
      return('clay loam')
    }
    else if (clay >= 27 & clay < 40 & sand <= 20) {
      return('silty clay loam') 
    } 
    else if (clay >= 35 & sand > 45) {
      return('sandy clay')
    } 
    else if (clay >= 40 & silt >= 40) {
      return('silty clay')
    } 
    else if (clay >= 40 & sand <= 45 & silt < 40) {
      return('clay')
    } else {
      return('undefined textural class')
    }
  }
  

  
  



