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
SoilsDataDir <- file.path(mainDir, 'soils_data/soilDB_query/CA_all')
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
results <- file.path(mainDir, 'soils_data/results/summer2017model/CA_all')

#fc_def <- 'awc_r'
#figure_label <- 'SSURGO defined FC'

#one-time operation to clean SSURGO horizon data which had duplicate chkeys 
setwd(SoilsDataDir)
list.files()
ssurgo_horizon <- read.csv('CA_all_horizon_data_2017-07-24.csv')
ssurgo_horizon <- ssurgo_horizon[!is.na(ssurgo_horizon$chkey), ] #get rid of 
  # chkeys that have no data first
sum(is.na(ssurgo_horizon$cokey))
fragvol_tot <- as.data.frame(tapply(ssurgo_horizon$fragvol_r, ssurgo_horizon$chkey, sum, na.rm=TRUE))
colnames(fragvol_tot) <- 'fragvol_r_sum'
fragvol_tot$chkey <- rownames(fragvol_tot)
rownames(fragvol_tot) <- NULL
ssurgo_horizon <- ssurgo_horizon[!duplicated(ssurgo_horizon$chkey), ] #get rid of duplicate chkeys, which only have unique fragvol_r and fragsize_r values
ssurgo_horizon <- ssurgo_horizon[ ,-which(colnames(ssurgo_horizon) == "fragvol_r" | colnames(ssurgo_horizon)=="fragsize_r")]
ssurgo_horizon <- merge(ssurgo_horizon, fragvol_tot, by='chkey')
write.csv(ssurgo_horizon, paste0('CA_all_horizon_data_', Sys.Date(), '_clean.csv'), row.names = FALSE)
setwd(SoilsDataDir)
ssurgo_horizon <- read.csv('CA_all_horizon_data_2017-07-24_clean.csv')
#see rules for soil textural classes.pdf in 'Allowable Depletion' folder
length(unique(ssurgo_horizon$cokey)) #37,642 cokeys; from comp table, 30k are majcomps
#stats before modification
sum(is.na(ssurgo_horizon$awc_r)) #19816
sum(is.na(ssurgo_horizon$wfifteenbar_r)) #24656
sum(is.na(ssurgo_horizon$claytotal_r)) #23859
sum(is.na(ssurgo_horizon$silttotal_r)) #25203
sum(is.na(ssurgo_horizon$sandtotal_r)) #25181
sum(is.na(ssurgo_horizon$textural.class)) #25237
sum(ssurgo_horizon$textural.class=='proportions do not sum to 100+-1', na.rm = TRUE) #543
sum(is.na(ssurgo_horizon$wpmd)) #25780 (relies on sand fraction data also)

textural.class.calc <- function(sand, silt, clay) {
  ifelse(is.na(sand) | is.na(silt) | is.na(clay), NA,
  ifelse(sand + silt + clay > 101 |
    sand + silt + clay < 99, 'proportions do not sum to 100+-1',
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
table(ssurgo_horizon$texture.sums)
sum(ssurgo_horizon$texture.sums != 100 & !is.na(ssurgo_horizon$texture.sums)) #801 don't add up to 100; 1749 statewide
FixTexture <- function(df, varname) {
  ifelse(is.na(df$texture.sums), df[[varname]] <- df[[varname]],
  ifelse(df$texture.sums > 99 & df$texture.sums < 101, df[[varname]] <- df[[varname]],
  ifelse(df$texture.sums < 85 | df$texture.sums > 115, df[[varname]] <- df[[varname]],
    df[[varname]] <- df[[varname]] * 100 / df$texture.sums)))
}
#run this after fixing sand, silt, and clay percentages first
FixSandTexture <- function(df, varname) {
  ifelse(is.na(df$texture.sums), df[[varname]] <- df[[varname]],
    ifelse(df$texture.sums > 99 & df$texture.sums < 101, df[[varname]] <- df[[varname]] * (df$sandtotal_r / df$sandfractions_sum), df[[varname]] <- df[[varname]]))
}
#need to fix new sand fractions with this function
ssurgo_horizon$sandtotal_r <- FixTexture(ssurgo_horizon, 'sandtotal_r')
ssurgo_horizon$silttotal_r <- FixTexture(ssurgo_horizon, 'silttotal_r')
ssurgo_horizon$claytotal_r <- FixTexture(ssurgo_horizon, 'claytotal_r')
ssurgo_horizon$texture.sums <- ssurgo_horizon$sandtotal_r + ssurgo_horizon$silttotal_r + ssurgo_horizon$claytotal_r
table(ssurgo_horizon$texture.sums)
ssurgo_horizon$sandfractions_sum <- ssurgo_horizon$sandvc_r + ssurgo_horizon$sandco_r + ssurgo_horizon$sandmed_r + ssurgo_horizon$sandfine_r + ssurgo_horizon$sandvf_r
sand_diffs <- ssurgo_horizon$sandfractions_sum - ssurgo_horizon$sandtotal_r
table(sand_diffs)
ssurgo_horizon$sandvc_r <- FixSandTexture(ssurgo_horizon, 'sandvc_r')
ssurgo_horizon$sandco_r <- FixSandTexture(ssurgo_horizon, 'sandco_r')
ssurgo_horizon$sandmed_r <- FixSandTexture(ssurgo_horizon, 'sandmed_r')
ssurgo_horizon$sandfine_r <- FixSandTexture(ssurgo_horizon, 'sandfine_r')
ssurgo_horizon$sandvf_r <- FixSandTexture(ssurgo_horizon, 'sandvf_r')
ssurgo_horizon$sandfractions_sum <- ssurgo_horizon$sandvc_r + ssurgo_horizon$sandco_r + ssurgo_horizon$sandmed_r + ssurgo_horizon$sandfine_r + ssurgo_horizon$sandvf_r
sand_diffs <- ssurgo_horizon$sandfractions_sum - ssurgo_horizon$sandtotal_r
table(sand_diffs) #confirmed that sand proportions are now fixed in accordance with sandtotal_r
ssurgo_horizon$textural.class <- textural.class.calc(ssurgo_horizon$sandtotal_r, ssurgo_horizon$silttotal_r, ssurgo_horizon$claytotal_r)

ssurgo_horizon$wpmd.calc.sum <- ssurgo_horizon$silttotal_r + ssurgo_horizon$claytotal_r + ssurgo_horizon$sandvc_r + ssurgo_horizon$sandco_r + ssurgo_horizon$sandmed_r + ssurgo_horizon$sandfine_r + ssurgo_horizon$sandvf_r
table(ssurgo_horizon$wpmd.calc.sum)
ssurgo_horizon$wpmd <- ifelse(ssurgo_horizon$wpmd.calc.sum > 99 & ssurgo_horizon$wpmd.calc.sum < 101, (ssurgo_horizon$sandvc_r*1.5 + ssurgo_horizon$sandco_r*0.75 + ssurgo_horizon$sandmed_r*0.375 + ssurgo_horizon$sandfine_r*0.175 + ssurgo_horizon$sandvf_r*0.075 + ssurgo_horizon$silttotal_r*0.026 + ssurgo_horizon$claytotal_r*0.001)/ssurgo_horizon$wpmd.calc.sum, NA)
ssurgo_horizon$wpmd.calc2.sum <- ssurgo_horizon$silttotal_r + ssurgo_horizon$claytotal_r + ssurgo_horizon$sandtotal_r
ssurgo_horizon$wpmd <- ifelse(is.na(ssurgo_horizon$wpmd) & ssurgo_horizon$wpmd.calc2.sum > 99 & ssurgo_horizon$wpmd.calc2.sum < 101, (ssurgo_horizon$sandtotal_r*1.025 + ssurgo_horizon$silttotal_r*0.026 + ssurgo_horizon$claytotal_r*0.001)/ssurgo_horizon$wpmd.calc2.sum, ssurgo_horizon$wpmd) #can get 130 wpmd calcs by ignoring bad sand fraction data and going with just the 3 class calc

#look at new variable wpmd (weighted particle mean diameter) by textural class
tapply(ssurgo_horizon$wpmd, ssurgo_horizon$textural.class, mean, na.rm=TRUE)
summary(as.factor(ssurgo_horizon$textural.class))
setwd(results)
write.csv(table(as.factor(ssurgo_horizon$textural.class)), paste0('CA_all_horizon_textural_class', Sys.Date(), '.csv'), row.names = FALSE)

#here on 7/24/2017; need to revise function to handle depth flexibly
unique(ssurgo_horizon$textural.class[ssurgo_horizon$wpmd > 0.15])
unique(ssurgo_horizon$textural.class[ssurgo_horizon$wpmd > 0.5])
sum(ssurgo_horizon$wpmd > 0.18, na.rm = TRUE)
median.wpmd <- median(ssurgo_horizon$wpmd, na.rm=TRUE)
sd.wpdmd <- sd(ssurgo_horizon$wpmd, na.rm = TRUE)
fudge_factor <- 1/exp(1) #- 0.1*(1/exp(1)); this factor scales the estimate of the surface depth exposed to evaporation from a max of 14 to 15.
ssurgo_horizon$surface.depth <- 100 * ifelse(is.na(ssurgo_horizon$wpmd), NA, ifelse(ssurgo_horizon$wpmd <= median.wpmd, 0.125 - 0.0125 * pmax((ssurgo_horizon$wpmd - median.wpmd) / (fudge_factor * sd.wpdmd), -2), 0.125 - 0.0125 * pmin((ssurgo_horizon$wpmd - median.wpmd) / sd.wpdmd, 2)))
summary(ssurgo_horizon$surface.depth)
hist(ssurgo_horizon$surface.depth)
unique(ssurgo_horizon$textural.class[ssurgo_horizon$surface.depth==15])
ssurgo_horizon[which(ssurgo_horizon$textural.class == 'loamy sand' & ssurgo_horizon$surface.depth==15), ]
setwd(results)
write.csv(ssurgo_horizon, paste0('CA_all_horizon_data_', Sys.Date(), '_cleantexture.csv'), row.names = FALSE)
#now, determine surface characteristics for a flexible depth (Ze), determined from surface wpmd
df <- test
surface_weighting <- function(df) {
  df <- df[,c('cokey', 'hzdept_r', 'hzdepb_r', 'surface.depth', 'awc_r', 'wfifteenbar_r', 'fragvol_r_sum', 'claytotal_r', 'silttotal_r', 'sandtotal_r')]
  df <- df[order(df$hzdept_r, decreasing = FALSE), ]
  if (nrow(df) == 1) {
    return(df[1,c(1, 4:ncol(df))])
  }
  else if (all(is.na(df$awc_r)) & all(is.na(df$wfifteenbar_r))) {
    return(df[1,c(1, 4:ncol(df))])
  }
  else if (TRUE %in% is.na(df$awc_r) | TRUE %in% is.na(df$surface.depth)) { #this leaves the possibility that there is an issue with some other relevant variable down the road
    df2 <- df[!is.na(df$awc_r) & !is.na(df$surface.depth), ]
    if (nrow(df2) == 1) {
      return(df2[1,c(1, 4:ncol(df2))])
    }
    else if (nrow(df2)==0) {
      return(df[1,c(1, 4:ncol(df))])
    } else {
        df2 <- df2[order(df2$hzdept_r, decreasing = FALSE), ]
        if (df2$surface.depth[1] < df2$hzdepb_r[1]) { #if the surface horizon's bottom depth exceeds the surface depth, just return that record
          return(df2[1,c(1, 4:ncol(df))])
        } else { #get the horizons which include a portion of the surface depth
            select.rows <- which(df2$hzdept_r < df2$surface.depth[1])
          #continue here
            df2 <- df2[select.rows, ]
            df2$hzthickness <- pmin(df2$hzdepb_r, df2$surface.depth[1]) - df2$hzdept_r
            if (sum(df2$hzthickness) == df2$surface.depth[1]) {
              depth.v1 <- df2$surface.depth[1]
              weighting_factor <- (depth.v1 - df2$hzdept_r - ifelse(df2$hzdepb_r <= depth.v1, depth.v1 - df2$hzdepb_r, 0))/depth.v1
              depth.v2 <- sum(df2$surface.depth*weighting_factor)
              weighting_factor <- (depth.v2 - df2$hzdept_r - ifelse(df2$hzdepb_r <= depth.v2, depth.v2 - df2$hzdepb_r, 0))/depth.v2
              depth.v3 <- sum(df2$surface.depth*weighting_factor)
              weighting_factor <- (depth.v3 - df2$hzdept_r - ifelse(df2$hzdepb_r <= depth.v3, depth.v3 - df2$hzdepb_r, 0))/depth.v3
              df2$hzthickness <- NULL
              df2[ ,4:ncol(df2)] <- df2[ ,4:ncol(df2)]*weighting_factor
              df3 <- t(apply(df2[ ,4:ncol(df2)], 2, sum))
              df3 <- cbind(df2[1,1], df3)
              colnames(df3)[1] <- c('cokey')
              return(df3)
            } else {
              depth.v1 <- df2$surface.depth[1]
              weighting_factor <- (depth.v1 - df2$hzdept_r - ifelse(df2$hzdepb_r <= depth.v1, depth.v1 - df2$hzdepb_r, 0))/sum(df2$hzthickness)
              depth.v2 <- sum(df2$surface.depth*weighting_factor)
              df2$hzthickness <- pmin(df2$hzdepb_r, depth.v2) - df2$hzdept_r 
              weighting_factor <- (depth.v2 - df2$hzdept_r - ifelse(df2$hzdepb_r <= depth.v2, depth.v2 - df2$hzdepb_r, 0))/sum(df2$hzthickness)
              depth.v3 <- sum(df2$surface.depth*weighting_factor)
              df2$hzthickness <- pmin(df2$hzdepb_r, depth.v3) - df2$hzdept_r 
              weighting_factor <- (depth.v3 - df2$hzdept_r - ifelse(df2$hzdepb_r <= depth.v3, depth.v3 - df2$hzdepb_r, 0))/sum(df2$hzthickness)
              df2$hzthickness <- NULL
              df2[ ,4:ncol(df2)] <- df2[ ,4:ncol(df2)]*weighting_factor
              df3 <- t(apply(df2[ ,4:ncol(df2)], 2, sum))
              df3 <- cbind(df2[1,1], df3)
              colnames(df3)[1] <- c('cokey')
              return(df3)
            }
          }
        }
    } else {
        if (df$surface.depth[1] < df$hzdepb_r[1]) { #if the surface horizon's bottom depth exceeds the surface depth, just return that record
          return(df[1,c(1, 4:ncol(df))])
        } else { #get the horizons which include a portion of the surface depth
            
            select.rows <- which(df$hzdept_r < df$surface.depth[1])
          #continue here
            df <- df[select.rows, ]
            depth.v1 <- df$surface.depth[1]
            weighting_factor <- (depth.v1 - df$hzdept_r - ifelse(df$hzdepb_r <= depth.v1, depth.v1 - df$hzdepb_r, 0))/depth.v1 #problem is here if horizon thickness are less than the surface.depth needed because of missing data in other horizons
            depth.v2 <- sum(df$surface.depth*weighting_factor)
            weighting_factor <- (depth.v2 - df$hzdept_r - ifelse(df$hzdepb_r <= depth.v2, depth.v2 - df$hzdepb_r, 0))/depth.v2
            depth.v3 <- sum(df$surface.depth*weighting_factor)
            weighting_factor <- (depth.v3 - df$hzdept_r - ifelse(df$hzdepb_r <= depth.v3, depth.v3 - df$hzdepb_r, 0))/depth.v3
            df[ ,4:ncol(df)] <- df[ ,4:ncol(df)] * weighting_factor
            df2 <- t(apply(df[ ,4:ncol(df)], 2, sum))
            df <- cbind(df[1,1], df2)
            colnames(df)[1] <- c('cokey')
            return(df)
          }
      }
    }
surface.horizons <- ssurgo_horizon[ssurgo_horizon$hzdept_r < 15, ]
surface.horizons <- surface.horizons[order(surface.horizons$cokey), ]
surface.data$fragvol_r_sum[surface.data$fragvol_r_sum > 100] <- 100
sum(is.na(surface.horizons$awc_r)) #3033
sum(is.na(surface.horizons$claytotal_r)) #4590
sum(is.na(surface.horizons$surface.depth)) #5128
sum(is.na(surface.horizons$wfifteenbar_r)) #4091
sum(is.na(surface.horizons$textural.class)) #5011
sum(surface.horizons$textural.class=='proportions do not sum to 100+-1', na.rm = TRUE) #117
sum(is.na(surface.horizons$awc_r) & !is.na(surface.horizons$surface.depth))
#61 have surface.depth data but no awc data; these will be ignored
sum(!is.na(surface.horizons$awc_r) & !is.na(surface.horizons$surface.depth)) #51769
sum(!is.na(surface.horizons$awc_r)) #53925
sum(!is.na(surface.horizons$surface.depth)) #51830
summary(as.factor(surface.horizons$textural.class))
sum(surface.horizons$surface.depth < 10, na.rm = TRUE)

test <- surface.horizons[which(surface.horizons$cokey==13063747), ] #need to get rid of NA cokeys
df <- test #then manually step into surface_weighting function from here
#run function to produce new dataset about surface horizons
surface.data <- do.call(rbind, lapply(split(surface.horizons, surface.horizons$cokey), surface_weighting))
surface.data$TEW <- (surface.data$awc_r + 0.5*surface.data$wfifteenbar_r/100)*surface.data$surface.depth*10 #this converts it to mm H2O per X cm soil; typically 10-15 cm soi

REWcalc <- function(sand, clay, fragvol, Ze) { #assumed equations were published for 10 cm soil slice
  ifelse(sand >= 80, (Ze / 10) * (20 - sand * 0.15) * (100 - fragvol) / 100, 
    ifelse(clay >= 50, (Ze / 10) * (11 - 0.06 * clay) * (100 - fragvol)/100, 
           (Ze / 10) * (8 + 0.08 * clay) * (100 - fragvol) / 100))
}
surface.data$REW <- REWcalc(surface.data$sandtotal_r, surface.data$claytotal_r, surface.data$fragvol_r_sum, surface.data$surface.depth) #see publication "Estimating Evaporation from Bare Soil and the Crop Coefficient for the Initial Period Using Common Soils Information"
surface.data$textural.class <- textural.class.calc(surface.data$sandtotal_r, surface.data$silttotal_r, surface.data$claytotal_r)
dim(surface.data)
head(surface.data)
sum(surface.data$surface.depth < 10, na.rm=TRUE) #72 surface depths are now > 15 & 730 are < 10
summary(surface.data$awc_r)
summary(surface.data$wfifteenbar_r)
summary(surface.data$sandtotal_r)
summary(surface.data$claytotal_r)
sum(surface.data$fragvol_r_sum > 100)
summary(surface.data$fragvol_r_sum)
summary(surface.data$TEW)
hist(surface.data$TEW)
summary(surface.data$REW)
summary(surface.data$REW/surface.data$TEW)
sum(surface.data$REW/surface.data$TEW > 0.6 & surface.data$REW/surface.data$TEW < 0.8, na.rm = TRUE) #2842 > 0.7; 1454 > 0.8; 301 > 1
hist(surface.data$REW/surface.data$TEW)
sum(!is.na(surface.data$TEW) & is.na(surface.data$REW))
sum(is.na(surface.data$TEW) & !is.na(surface.data$REW)) #68 of these
summary(as.factor(surface.data$textural.class)[surface.data$REW/surface.data$TEW > 1])
length(which(surface.data$REW/surface.data$TEW > 0.9))#678 of these #as opposed to sum(surface.data$REW/surface.data$TEW > 0.9, na.rm = TRUE)
#fix TEW where REW/TEW > 0.9
surface.data$TEW[which(surface.data$REW/surface.data$TEW >= 0.9)] <- surface.data$TEW[which(surface.data$REW/surface.data$TEW >= 0.9)] * (surface.data$REW[which(surface.data$REW/surface.data$TEW >= 0.9)] / (0.9 * surface.data$TEW[which(surface.data$REW/surface.data$TEW >= 0.9)]))
sum(surface.data$TEW < 2, na.rm = TRUE) #no zeroes present, only 8 < 1; 55 < 22
setwd(results)
write.csv(surface.data, paste0('CA_all_surface_data_synthesis_', Sys.Date(), '_.csv'), row.names=FALSE)


# a real hack job developed Oct 2017 to deal with duplicate cokeys as a result 
  # of reskind query, since multiple reskinds require multiple cokey rows with 
  # duplicate data in order to apply ssurgo_horizon sum routine, need to first 
  # identify which cokeys have lithic or paralithic contacts or both, so that 
  # these horizons are not given plant available water values. purpose is to get 
  # the soil comp data into 1 row per cokey and to distinguish between 
  # modifiable and unmodifiable soil profiles.
ssurgo_comp <- read.csv("CA_all_comp_data_2017-07-19.csv")
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
setwd(results)
write.csv(ssurgo_comp_no_rock, paste0('CA_all_comp_data_no_rock', Sys.Date(), '.csv'), row.names = FALSE)
write.csv(ssurgo_comp_rock, paste0('CA_all_comp_data_rock', Sys.Date(), '.csv'), row.names = FALSE)
#finished here 7/20/17

#one-time operation to split soil horizon data into a set with a rock contact and those with no rock contact
#split ssurgo_horizons into two data.frames
#ssurgo_horizon <- read.csv('CA_all_horizon_data_2017-07-19_clean.csv')
ssurgo_horizon$hzthickness <- ssurgo_horizon$hzdepb_r - ssurgo_horizon$hzdept_r
i <- ssurgo_horizon$cokey %in% cokeys_rock_unique
ssurgo_horizon_Cr_R <- ssurgo_horizon[i, ]
ssurgo_horizon_no_Cr_R <- ssurgo_horizon[!i, ]
if (sum(ssurgo_horizon_no_Cr_R$cokey %in% cokeys_rock_unique) > 0) {'Hay problemas aqui'}
setwd(results)
write.csv(ssurgo_horizon_Cr_R, paste0('ssurgo_horizon_Cr_R', Sys.Date(), '.csv'), row.names = FALSE)
write.csv(ssurgo_horizon_no_Cr_R, paste0('ssurgo_horizon_no_Cr_R', Sys.Date(), '.csv'), row.names = FALSE)

#split surface data into a set with a rock contact and those with no rock contact
i <- surface.data$cokey %in% cokeys_rock_unique
surface.data_Cr_R <- surface.data[i, ]
surface.data_no_Cr_R <- surface.data[!i, ]
if (sum(surface.data_no_Cr_R$cokey %in% cokeys_rock_unique) > 0) {'Hay problemas aqui'}
setwd(results)
write.csv(surface.data_Cr_R, paste0('CA.all.surface.data_Cr_R', Sys.Date(), '.csv'), row.names = FALSE)
write.csv(surface.data_no_Cr_R, paste0('CA.all.surface.data_no_Cr_R', Sys.Date(), '.csv'), row.names = FALSE)

aggregate_SSURGO <- function(results, fc_def, figure_label) {
  list.files(SoilsDataDir, pattern = glob2rx('*.csv'))
  list.files(results, pattern = glob2rx('*.csv'))
  setwd(results)
  ssurgo_horizon_no_Cr_R <- read.csv('ssurgo_horizon_no_Cr_R2017-07-26.csv')
  ssurgo_horizon_Cr_R <- read.csv('ssurgo_horizon_Cr_R2017-07-26.csv')
  ssurgo_comp_no_rock <- read.csv('CA_all_comp_data_no_rock2017-07-26.csv')
  ssurgo_comp_rock <- read.csv('CA_all_comp_data_rock2017-07-26.csv')
  setwd(SoilsDataDir)
  ssurgo_mu <- read.csv("CA_all_mu_data_2017-07-19.csv")
  
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
  ssurgo_comp_no_rock <- merge(ssurgo_comp_no_rock, surface.data_no_Cr_R[ ,c('cokey', 'TEW', 'REW', 'surface.depth')], by='cokey', all = TRUE)
  ssurgo_comp_rock <- merge(ssurgo_comp_rock, comps.R.Cr, by='cokey', all = TRUE)
  ssurgo_comp_rock <- merge(ssurgo_comp_rock, surface.data_Cr_R[ ,c('cokey', 'TEW', 'REW', 'surface.depth')], by='cokey', all = TRUE)
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
  ssurgo_comp_no_rock <- area_weighted_average(ssurgo_comp_no_rock, "TEW")
  ssurgo_comp_no_rock <- area_weighted_average(ssurgo_comp_no_rock, "REW")
  ssurgo_comp_no_rock <- area_weighted_average(ssurgo_comp_no_rock, "surface.depth")
  
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
  ssurgo_comp_rock <- area_weighted_average(ssurgo_comp_rock, "TEW")
  ssurgo_comp_rock <- area_weighted_average(ssurgo_comp_rock, "REW")
  ssurgo_comp_rock <- area_weighted_average(ssurgo_comp_rock, "surface.depth")
  #merge and rbind data.frames to fill in data for minor components based off of component name averages
  
  ssurgo_comp_all <- rbind(ssurgo_comp_rock, ssurgo_comp_no_rock)
  #temp fix for rock outcrop (could be fixed earlier in script).
  summary(ssurgo_comp_all$z0.5m_cmH2O_modified_comp[ssurgo_comp_all$compname=='Rock outcrop'])
  summary(ssurgo_comp_all$z0.5m_cmH2O_unmodified_comp[ssurgo_comp_all$compname=='Rock outcrop'])
  #CHECK COLUMN REFERENCES HERE
  colnames(ssurgo_comp_all)
  ssurgo_comp_all[ssurgo_comp_all$compname=='Rock outcrop', 22:39] <- 0 #change all paw related variables to 0 where component name is Rock Outcrop
  ssurgo_comp_all <- ssurgo_comp_all[order(ssurgo_comp_all$mukey, ssurgo_comp_all$comppct_r, decreasing = c(FALSE, TRUE)), ]
  setwd(results)
  write.csv(ssurgo_comp_all, paste0('CA_all_comps_summary_dbmodified', Sys.Date(), '.csv'), row.names = FALSE) # can read this in to shorten script
  #ssurgo_comp_all <- read.csv('CA_all_comps_summary_7.19.17')
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
  ssurgo_comp_all <- area_weighted_average(ssurgo_comp_all, "TEW")
  ssurgo_comp_all <- area_weighted_average(ssurgo_comp_all, "REW")
  ssurgo_comp_all <- area_weighted_average(ssurgo_comp_all, "surface.depth")
  
  setwd(results)
  write.csv(ssurgo_comp_all, paste0('CA_all_comps_summary_dbmodified_FINAL', Sys.Date(), '.csv'), row.names = FALSE)
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
  

  
  



