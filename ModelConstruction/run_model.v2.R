library(foreach)
library(doSNOW)
modelscaffoldDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/model_scaffold/run_model/Mar2018' #location of input data; re-worked early March 2018
cropscape_legend <- read.csv(file.path(modelscaffoldDir, 'cropscape_legend.txt'), stringsAsFactors = FALSE)
alfalfa_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Alfalfa']
grape_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Grapes']
almond_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Almonds']
walnut_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Walnuts']
pistachio_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Pistachios']
#function arguments:cropname, cropcode, AD.percentage, root_depth, irr.type, crop.parameters.df, model.scaffold, U2.df, P.df, ETo.df, RHmin.df, results_file, row_start, RDI.min, alfalfa.zone, grape.zone, stress.assumption
modelgrid <- expand.grid(root.depths = c('0.5m', '1.0m', '2.0m', '3.0m'), AD.percentage = as.integer(c(30, 50, 80)), cropnames = c('walnut.mature', 'almond.mature', 'pistachios', 'grapes.table', 'grapes.wine', 'alfalfa.intermountain', 'alfalfa.CV', 'alfalfa.imperial'), stringsAsFactors = FALSE)
modelgrid$RDI.min <- ifelse(modelgrid$cropnames=='grapes.wine' &  modelgrid$AD.percentage==30, 0.8, ifelse(modelgrid$cropnames=='grapes.wine' &  modelgrid$AD.percentage==50, 0.5, ifelse(modelgrid$cropnames=='grapes.wine' &  modelgrid$AD.percentage==80, 0.2, NA)))
modelgrid$cropcode <- ifelse(modelgrid$cropnames=='almond.mature', almond_code, ifelse(modelgrid$cropnames=='walnut.mature', walnut_code, ifelse(modelgrid$cropnames=='pistachios', pistachio_code, ifelse(modelgrid$cropnames=='grapes.table', grape_code, ifelse(modelgrid$cropnames=='grapes.wine', grape_code, ifelse(modelgrid$cropnames=='alfalfa.intermountain' | modelgrid$cropnames=='alfalfa.CV' | modelgrid$cropnames=='alfalfa.imperial', alfalfa_code, print('Done')))))))
modelgrid$irrtype <- ifelse(modelgrid$cropnames=='almond.mature', 'Microspray, orchards', ifelse(modelgrid$cropnames=='walnut.mature', 'Microspray, orchards', ifelse(modelgrid$cropnames=='pistachios', 'Microspray, orchards', ifelse(modelgrid$cropnames=='grapes.table', 'Drip', ifelse(modelgrid$cropnames=='grapes.wine', 'Drip', ifelse(modelgrid$cropnames=='alfalfa.intermountain' | modelgrid$cropnames=='alfalfa.CV' | modelgrid$cropnames=='alfalfa.imperial', 'Border', print('Done')))))))
modelgrid$alfalfa.zone <- ifelse(modelgrid$cropnames=='alfalfa.intermountain', 'Intermountain', ifelse(modelgrid$cropnames=='alfalfa.CV', 'Central Valley', ifelse(modelgrid$cropnames=='alfalfa.imperial', 'Imperial Valley', NA)))
modelgrid$grape.zone <- ifelse(modelgrid$cropnames=='grapes.wine', 'Coast, Foothills, and Mountains', ifelse(modelgrid$cropnames=='grapes.table', 'Central Valley', NA))
modelgrid
cl <- makeCluster(6, type = 'SOCK') #change the number to your desired number of CPU cores  
clusterExport(cl, list=c("resultsDir", "rounding_digits", "FAO56DualCropCalc", "crop.parameters.df", "model.scaffold", "U2.df", "P.df", "ETo.df", "RHmin.df", "irrigation.parameters", "cropscape_legend"))
registerDoSNOW(cl)
#re-started run at 10:20 AM on 4/18/18
foreach(i=1:96) %dopar% {
  FAO56DualCropCalc(modelgrid$cropnames[i], modelgrid$cropcode[i], modelgrid$AD.percentage[i], modelgrid$root.depths[i], modelgrid$irrtype[i], crop.parameters.df, model.scaffold, U2.df, P.df, ETo.df, RHmin.df, results_file = 'new', row_start = 1, RDI.min = modelgrid$RDI.min[i], alfalfa.zone = modelgrid$alfalfa.zone[i], grape.zone = modelgrid$grape.zone[i], stress.assumption=0.5, dailyWBsave = FALSE)
} ##function arguments as of 4/17/18:cropname, cropcode, AD.percentage, root_depth, irr.type, crop.parameters.df, model.scaffold, U2.df, P.df, ETo.df, RHmin.df, results_file, row_start, RDI.min, alfalfa.zone, grape.zone, stress.assumption
stopCluster(cl)

#sensitivy runs begun 4/26/18
#this is for an irrigation type sensitivity run on almonds and walnuts exploring 30% allowable depletion for 0.5 m rooting and 50% allowable depletion at the 3 different rooting depths
modelgrid <- expand.grid(root.depths = c('0.5m', '1.0m', '2.0m', '3.0m'), cropnames = c('walnut.mature', 'almond.mature'), stringsAsFactors = FALSE)
rep.number <- nrow(modelgrid)
modelgrid <- rbind(modelgrid, modelgrid)
modelgrid$AD.percentage <- ifelse(modelgrid$root.depths=='0.5m', 30, 50)
modelgrid$cropcode <- ifelse(modelgrid$cropnames=='almond.mature', almond_code, ifelse(modelgrid$cropnames=='walnut.mature', walnut_code, ifelse(modelgrid$cropnames=='pistachios', pistachio_code, ifelse(modelgrid$cropnames=='grapes.table', grape_code, ifelse(modelgrid$cropnames=='grapes.wine', grape_code, ifelse(modelgrid$cropnames=='alfalfa.intermountain' | modelgrid$cropnames=='alfalfa.CV' | modelgrid$cropnames=='alfalfa.imperial', alfalfa_code, print('Done')))))))
modelgrid$irrtype <- c(rep('Border', rep.number), rep('Drip', rep.number))
modelgrid$scenario.dir <- c(rep('Border.irrigation', rep.number), rep('Drip.irrigation', rep.number))
modelgrid
cl <- makeCluster(6, type = 'SOCK') #change the number to your desired number of CPU cores  
clusterExport(cl, list=c("resultsDir", "rounding_digits", "FAO56DualCropCalc", "crop.parameters.df", "model.scaffold", "U2.df", "P.df", "ETo.df", "RHmin.df", "irrigation.parameters", "cropscape_legend"))
registerDoSNOW(cl)
#re-started run at 10:20 AM on 4/18/18
foreach(i=1:nrow(modelgrid)) %dopar% {
  FAO56DualCropCalc(modelgrid$cropnames[i], modelgrid$cropcode[i], modelgrid$AD.percentage[i], modelgrid$root.depths[i], modelgrid$irrtype[i], crop.parameters.df, model.scaffold, U2.df, P.df, ETo.df, RHmin.df, results_file = 'new', row_start = 1, RDI.min = NA, alfalfa.zone = NA, grape.zone = NA, stress.assumption=0.5, dailyWBsave = FALSE, modelgrid$scenario.dir[i])
} ##function arguments as of 4/17/18:cropname, cropcode, AD.percentage, root_depth, irr.type, crop.parameters.df, model.scaffold, U2.df, P.df, ETo.df, RHmin.df, results_file, row_start, RDI.min, alfalfa.zone, grape.zone, stress.assumption
stopCluster(cl)
