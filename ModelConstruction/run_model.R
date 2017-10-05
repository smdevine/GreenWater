library(foreach)
library(doSNOW)
modelscaffoldDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/model_scaffold/run_model/Sep2017' #location of input data
setwd(modelscaffoldDir)
cropscape_legend <- read.csv('cropscape_legend.txt', stringsAsFactors = FALSE)
alfalfa_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Alfalfa'] #75380 total
grape_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Grapes']
almond_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Almonds']
walnut_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Walnuts']
pistachio_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Pistachios']

#re-run 12 almond scenarios
#start time 9/1/17 12:34 PM
cl <- makeCluster(6, type = 'SOCK') #change the number to your desired number of CPU cores  
clusterExport(cl, list=c("resultsDir", "rounding_digits", "FAO56DualCropCalc", "crop.parameters.df", "model.scaffold", "U2.df", "P.df", "ETo.df", "RHmin.df", "irrigation.parameters", "cropscape_legend"))
registerDoSNOW(cl)
foreach(i=1:12) %dopar% {
  root_depth <- c('1.0m', '1.5m', '2.0m', '4.0m', '1.0m', '1.5m', '2.0m', '4.0m', '1.0m', '1.5m', '2.0m', '4.0m')
  AD_percentage <- c(30, 30, 30, 30, 50, 50, 50, 50, 80, 80, 80, 80)
  FAO56DualCropCalc('almond.mature', 75, AD_percentage[i], root_depth[i], 'Microspray, orchards', crop.parameters.df, model.scaffold, U2.df, P.df, ETo.df, RHmin.df, results_file = 'new', row_start = 1, RDI.min = NA)
}
stopCluster(cl)

#re-run 6 walnut scenarios
#start time ???
cl <- makeCluster(6, type = 'SOCK') #change the number to your desired number of CPU cores  
clusterExport(cl, list=c("resultsDir", "rounding_digits", "FAO56DualCropCalc", "crop.parameters.df", "model.scaffold", "U2.df", "P.df", "ETo.df", "RHmin.df", "irrigation.parameters", "cropscape_legend"))
registerDoSNOW(cl)
foreach(i=1:6) %dopar% {
  root_depth <- c('2.0m', '4.0m', '1.0m', '1.5m', '2.0m', '4.0m')
  AD_percentage <- c(50, 50, 80, 80, 80, 80)
  FAO56DualCropCalc('walnut.mature', 76, AD_percentage[i], root_depth[i], 'Microspray, orchards', crop.parameters.df, model.scaffold, U2.df, P.df, ETo.df, RHmin.df, results_file = 'new', row_start = 1, RDI.min = NA)
}
stopCluster(cl)

#re-run 12 pistachio scenarios to collect H2Ostress data
#start time 9/1/17 12:34 PM
cl <- makeCluster(6, type = 'SOCK') #change the number to your desired number of CPU cores  
clusterExport(cl, list=c("resultsDir", "rounding_digits", "FAO56DualCropCalc", "crop.parameters.df", "model.scaffold", "U2.df", "P.df", "ETo.df", "RHmin.df", "irrigation.parameters", "cropscape_legend"))
registerDoSNOW(cl)
foreach(i=1:12) %dopar% {
  root_depth <- c('1.0m', '1.5m', '2.0m', '4.0m', '1.0m', '1.5m', '2.0m', '4.0m', '1.0m', '1.5m', '2.0m', '4.0m')
  AD_percentage <- c(30, 30, 30, 30, 50, 50, 50, 50, 80, 80, 80, 80)
  FAO56DualCropCalc('pistachios', 204, AD_percentage[i], root_depth[i], 'Microspray, orchards', crop.parameters.df, model.scaffold, U2.df, P.df, ETo.df, RHmin.df, results_file = 'new', row_start = 1, RDI.min = NA)
}
stopCluster(cl)

#re-run 9 scenarios per crop x soil x climate combination
#re-started again 9:40 AM on 9/14/17
#re-starting at scenario 16 at 3:20 PM on 9/20/17
#root.depths <- c('1.0m', '2.0m', '3.0m')
#AD.percentage <- c(30, 50, 80)
#cropnames <- c('almond.mature', 'walnut.mature', 'pistachios', 'grapes.table', 'alfalfa.intermountain', 'alfalfa.CV', 'alfalfa.imperial')
modelgrid <- expand.grid(root.depths = c('1.0m', '2.0m', '3.0m'), AD.percentage = c(30, 50, 80), cropnames = c('almond.mature', 'walnut.mature', 'pistachios', 'grapes.table', 'alfalfa.intermountain', 'alfalfa.CV', 'alfalfa.imperial'))
modelgrid$root.depths <- as.character(modelgrid$root.depths)
modelgrid$cropnames <- as.character(modelgrid$cropnames)
modelgrid$AD.percentage <- as.integer(modelgrid$AD.percentage)
modelgrid$cropcode <- ifelse(modelgrid$cropnames=='almond.mature', almond_code, ifelse(modelgrid$cropnames=='walnut.mature', walnut_code, ifelse(modelgrid$cropnames=='pistachios', pistachio_code, ifelse(modelgrid$cropnames=='grapes.table', grape_code, ifelse(modelgrid$cropnames=='alfalfa.intermountain' | modelgrid$cropnames=='alfalfa.CV' | modelgrid$cropnames=='alfalfa.imperial', alfalfa_code, print('Done'))))))
modelgrid$irrtype <- ifelse(modelgrid$cropnames=='almond.mature', 'Microspray, orchards', ifelse(modelgrid$cropnames=='walnut.mature', 'Microspray, orchards', ifelse(modelgrid$cropnames=='pistachios', 'Microspray, orchards', ifelse(modelgrid$cropnames=='grapes.table', 'Drip', ifelse(modelgrid$cropnames=='alfalfa.intermountain' | modelgrid$cropnames=='alfalfa.CV' | modelgrid$cropnames=='alfalfa.imperial', 'Border', print('Done'))))))
modelgrid$alfalfa.zone <- ifelse(modelgrid$cropnames=='alfalfa.intermountain', 'Intermountain', ifelse(modelgrid$cropnames=='alfalfa.CV', 'Central Valley', ifelse(modelgrid$cropnames=='alfalfa.imperial', 'Imperial Valley', NA)))
modelgrid
cl <- makeCluster(6, type = 'SOCK') #change the number to your desired number of CPU cores  
clusterExport(cl, list=c("resultsDir", "rounding_digits", "FAO56DualCropCalc", "crop.parameters.df", "model.scaffold", "U2.df", "P.df", "ETo.df", "RHmin.df", "irrigation.parameters", "cropscape_legend"))
registerDoSNOW(cl)
foreach(i=16:63) %dopar% {
  FAO56DualCropCalc(modelgrid$cropnames[i], modelgrid$cropcode[i], modelgrid$AD.percentage[i], modelgrid$root.depths[i], modelgrid$irrtype[i], crop.parameters.df, model.scaffold, U2.df, P.df, ETo.df, RHmin.df, results_file = 'new', row_start = 1, RDI.min = NA, alfalfa.zone = modelgrid$alfalfa.zone[i])
  print(i)
}


#wine grape runs on Lenovo Yoga 3 laptop
FAO56DualCropCalc('grapes.wine', grape_code, 50, '3.0m', "Drip", crop.parameters.df, model.scaffold, U2.df, P.df, ETo.df, RHmin.df, results_file='grapes.wine3.0mRDI.min0.2_FAO56results.csv', row_start='20001', RDI.min = 0.2, alfalfa.zone = NA)
FAO56DualCropCalc('grapes.wine', grape_code, 50, '2.0m', "Drip", crop.parameters.df, model.scaffold, U2.df, P.df, ETo.df, RHmin.df, results_file='new', row_start=1, RDI.min = 0.5, alfalfa.zone = NA)

#almond re-run at row 60,001
FAO56DualCropCalc('almond.mature', almond_code, 50, '3.0m', "Microspray, orchards", crop.parameters.df, model.scaffold, U2.df, P.df, ETo.df, RHmin.df, results_file='almond.mature3.0mAD50_FAO56results.csv', row_start=60001, RDI.min = NA, alfalfa.zone = NA)

#wine grape re-run at row 50,001
FAO56DualCropCalc('grapes.wine', grape_code, 50, '2.0m', "Drip", crop.parameters.df, model.scaffold, U2.df, P.df, ETo.df, RHmin.df, results_file='grapes.wine2.0mRDI.min0.5_FAO56results.csv', row_start=50001, RDI.min = 0.5, alfalfa.zone = NA)

#almond 3.0 m, 50% AD re-run at row 60,001
FAO56DualCropCalc('almond.mature', almond_code, 50, '3.0m', "Microspray, orchards", crop.parameters.df, model.scaffold, U2.df, P.df, ETo.df, RHmin.df, results_file='almond.mature3.0mAD50_FAO56results.csv', row_start=60001, RDI.min = NA, alfalfa.zone = NA)

#almond 3.0 m, 80% AD re-run at row 70,001
FAO56DualCropCalc('almond.mature', almond_code, 80, '3.0m', "Microspray, orchards", crop.parameters.df, model.scaffold, U2.df, P.df, ETo.df, RHmin.df, results_file='almond.mature3.0mAD80_FAO56results.csv', row_start=70001, RDI.min = NA, alfalfa.zone = NA)

#almond 2.0 m, 80% AD re-run at row 70,001
FAO56DualCropCalc('almond.mature', almond_code, 80, '2.0m', "Microspray, orchards", crop.parameters.df, model.scaffold, U2.df, P.df, ETo.df, RHmin.df, results_file='almond.mature2.0mAD80_FAO56results.csv', row_start=70001, RDI.min = NA, alfalfa.zone = NA)

#alfalfa.intermountain 1.0 m, 30% AD re-run from scratch
FAO56DualCropCalc('alfalfa.intermountain', alfalfa_code, 30, '1.0m', "Border", crop.parameters.df, model.scaffold, U2.df, P.df, ETo.df, RHmin.df, results_file='new', row_start=1, RDI.min = NA, alfalfa.zone = 'Intermountain')

#wine grape test run according to geography (model scaffold had 'grape.zone' added)
FAO56DualCropCalc('grapes.wine', grape_code, 50, '2.0m', "Drip", crop.parameters.df, model.scaffold, U2.df, P.df, ETo.df, RHmin.df, results_file='new', row_start=1, RDI.min = 0.5, alfalfa.zone = NA, grape.zone = 'Central California Foothills and Coastal Mountains')

#re-run of grape scenarios
#computer outage required partial re-run on 10/2/17
modelgrid <- expand.grid(root.depths = c('1.0m', '2.0m', '3.0m'), AD.percentage = c(30, 50, 80), cropnames = c('grapes.wine', 'grapes.table'))
modelgrid$root.depths <- as.character(modelgrid$root.depths)
modelgrid$cropnames <- as.character(modelgrid$cropnames)
modelgrid$AD.percentage <- as.integer(modelgrid$AD.percentage)
modelgrid$cropcode <- ifelse(modelgrid$cropnames=='almond.mature', almond_code, ifelse(modelgrid$cropnames=='walnut.mature', walnut_code, ifelse(modelgrid$cropnames=='pistachios', pistachio_code, ifelse(modelgrid$cropnames=='grapes.table', grape_code, ifelse(modelgrid$cropnames=='grapes.wine', grape_code, ifelse(modelgrid$cropnames=='alfalfa.intermountain' | modelgrid$cropnames=='alfalfa.CV' | modelgrid$cropnames=='alfalfa.imperial', alfalfa_code, print('Done')))))))
modelgrid$irrtype <- ifelse(modelgrid$cropnames=='almond.mature', 'Microspray, orchards', ifelse(modelgrid$cropnames=='walnut.mature', 'Microspray, orchards', ifelse(modelgrid$cropnames=='pistachios', 'Microspray, orchards', ifelse(modelgrid$cropnames=='grapes.table', 'Drip', ifelse(modelgrid$cropnames=='grapes.wine', 'Drip', ifelse(modelgrid$cropnames=='alfalfa.intermountain' | modelgrid$cropnames=='alfalfa.CV' | modelgrid$cropnames=='alfalfa.imperial', 'Border', print('Done')))))))
modelgrid$alfalfa.zone <- ifelse(modelgrid$cropnames=='alfalfa.intermountain', 'Intermountain', ifelse(modelgrid$cropnames=='alfalfa.CV', 'Central Valley', ifelse(modelgrid$cropnames=='alfalfa.imperial', 'Imperial Valley', NA)))
modelgrid$grape.zone <- ifelse(modelgrid$cropnames=='grapes.wine', 'Central California Foothills and Coastal Mountains', ifelse(modelgrid$cropnames=='grapes.table', 'Central California Valley', NA))
modelgrid$AD.percentage[modelgrid$cropnames=='grapes.wine'] <- 50
modelgrid$RDI.min <- c(0.2, 0.2, 0.2, 0.5, 0.5, 0.5, 0.8, 0.8, 0.8, rep(NA, 9))
modelgrid
cl <- makeCluster(6, type = 'SOCK') #change the number to your desired number of CPU cores  
clusterExport(cl, list=c("resultsDir", "rounding_digits", "FAO56DualCropCalc", "crop.parameters.df", "model.scaffold", "U2.df", "P.df", "ETo.df", "RHmin.df", "irrigation.parameters", "cropscape_legend"))
registerDoSNOW(cl)
modelgrid <- modelgrid[c(8, 10:18),] #trim the grid for the re-run
modelgrid
foreach(i=1:10) %dopar% {
  FAO56DualCropCalc(modelgrid$cropnames[i], modelgrid$cropcode[i], modelgrid$AD.percentage[i], modelgrid$root.depths[i], modelgrid$irrtype[i], crop.parameters.df, model.scaffold, U2.df, P.df, ETo.df, RHmin.df, results_file = 'new', row_start = 1, RDI.min = modelgrid$RDI.min[i], alfalfa.zone = modelgrid$alfalfa.zone[i], grape.zone = modelgrid$grape.zone[i])
}

i <- 10
FAO56DualCropCalc(modelgrid$cropnames[i], modelgrid$cropcode[i], modelgrid$AD.percentage[i], modelgrid$root.depths[i], modelgrid$irrtype[i], crop.parameters.df, model.scaffold, U2.df, P.df, ETo.df, RHmin.df, results_file = 'new', row_start = 1, RDI.min = modelgrid$RDI.min[i], alfalfa.zone = modelgrid$alfalfa.zone[i], grape.zone = modelgrid$grape.zone[i])
