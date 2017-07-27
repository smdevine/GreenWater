#metadata creation if so desired
#txt2 <- paste(length(unique(ssurgo_horizon$cokey)), 'unique cokeys in soil horizon data table.')
#txt3 <- paste(length(unique(ssurgo_comp$cokey)), 'unique cokeys in soil component data table.')
#txt4 <- paste(nrow(ssurgo_comp), 'rows in the component data table.')
#txt5 <- paste(paste(unique(ssurgo_comp$reskind), collapse=', '), 'are the unique kinds of restrictions.')
#txt6 <- paste(length(unique(ssurgo_comp$mukey)), 'unique mukeys in soil component data table')
#txt7 <- paste(length(unique(ssurgo_mu$mukey)), 'unique mukeys in soil mapunit data table')
#txt17 <- paste('Major comonents total:', round(sum(ssurgo_comp$hectares[which(ssurgo_comp$majcompflag=='Yes')]), digits = 0), 'hectares.') #add up major comp acreage: 6,331,342 ha, which is 85.4% of total
#txt18 <- paste('Minor components total:', round(sum(ssurgo_comp$hectares[which(ssurgo_comp$majcompflag=='No')], na.rm = TRUE), digits = 0), 'hectares.') #add up minor comp acreage (there are 3 cokeys with missing comppct_r data): 1,085,786 ha, which is 14.6% of total

#extra SSURGO aggregation functions from Fall 2016

#field capacity as defined by SSURGO, loosely
field_cap <- function(clay, sand, theta_0.33bar, theta_0.1bar) { #function to determine whether one should use theta at 0.33 or 0.1 bars for field capacity
  if (is.na(clay)) {
    return(NA)
  }
  if (is.na(theta_0.33bar)) {
    return(NA)
  }
  if (clay < 20 & sand > 50) { #these are the cutoffs to be a sand, loamy sand, or sandy loam
    return(theta_0.1bar)
  }
  else(return(theta_0.33bar))
}
field_cap_vector <- vector()
for (i in 1:nrow(ssurgo_horizon)) {
  x <- field_cap(ssurgo_horizon$claytotal_r[i], ssurgo_horizon$sandtotal_r[i], ssurgo_horizon$theta_0.33bar[i], ssurgo_horizon$theta_0.1bar[i])
  field_cap_vector <- c(field_cap_vector, x)
}
ssurgo_horizon$field_cap <- fc_correction_vector

# #this is from the regression tree based correction of 0.33 bar data to FC estimates by Nemes et al. 2014
fc_correction <- function(theta0.33, clay) { 
  if (is.na(theta0.33)) {
    return(NA)
  }
  if (is.na(clay)) {
    return(NA)
  }
  if (theta0.33 <= .262) {
    if (clay <= 26.6) {
      return(1.1/100+theta0.33)
    }
    else {return(8.3/100+theta0.33)}
  }
  if (clay <= 17.8) {
    if (theta0.33 <= 33.0) {
      return(-3.5/100+theta0.33)
    }
    else {return(-10.6/100+theta0.33)}
  }
  if (theta0.33 > .376) {
    return(-3.7/100+theta0.33)
  }
  if (theta0.33 > .298) {
    return(-1.3/100+theta0.33)
  }
  if (clay <= 25.8) {
    return(-1.7/100+theta0.33)
  }
  else {return(4.5/100+theta0.33)}
}
fc_correction_vector <- vector()
for (i in 1:nrow(ssurgo_horizon)) {
  x <- fc_correction(ssurgo_horizon$fc_0.33bar[i], ssurgo_horizon$claytotal_r[i])
  fc_correction_vector <- c(fc_correction_vector, x)
}

allowable_depletion <- function(df, percent) { #see bottom of script for another way to write this kind of a fucntion
  for (i in 1:nrow(df)) {
    if (i==1) {
      y <- vector()
    }
    if (is.na(df[['awc_r']][i])) {
      y <- c(y, NA)
      next
    }
    y <- c(y, (df[['awc_r']][i])*percent/100)
  }
  invisible(y)
}
ssurgo_horizon$AD_grapes_tension <- allowable_depletion_tension('theta_h_grapes', ssurgo_horizon)
ssurgo_horizon$AD_grapes <- allowable_depletion(ssurgo_horizon, 35)
ssurgo_horizon$AD_deciduous_tension <- allowable_depletion_tension('theta_h_deciduous', ssurgo_horizon)
ssurgo_horizon$AD_deciduous <- allowable_depletion(ssurgo_horizon, 50)
ssurgo_horizon$AD_alfalfa_tension <- allowable_depletion_tension('theta_h_alfalfa', ssurgo_horizon)
ssurgo_horizon$AD_alfalfa <- allowable_depletion(ssurgo_horizon, 55)
#ssurgo_horizon$AD_citrus <- (ssurgo_horizon$field_cap - ssurgo_horizon$theta_h_citrus)*(1-ssurgo_horizon$fragvol_r_sum/100)

#root depths from Appendix Table D-3 of Scheduling Irrigations: When and How Much Water to Apply and are mid-points of the published range for each crop
grape_root_z <- 2.25*12*2.54
alfalfa_root_z <- 5*12*2.54
almonds_root_z <- 3*12*2.54
walnuts_root_z <- 6.75*12*2.54

#vectorized version of this function was written in SSURGO_aggregation_awc_r_modelruns.R
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

#find unique restrictive layers in ssurgo component level data
unique_restrictions <- vector()
for (i in 1:length(duplicated_cokeys)) {
  dups <- ssurgo_comp[ssurgo_comp$cokey==duplicated_cokeys[i], ]
  restrictions <- paste(dups$reskind[1], dups$reskind[2], collapse = ' ')
  unique_restrictions[i] <- restrictions
}
unique(unique_restrictions) #31 unique combinations of reskinds for the same cokey

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

#count instances of various kinds of restrictions 
tapply(ssurgo_comp$cokey, ssurgo_comp$reskind, length)