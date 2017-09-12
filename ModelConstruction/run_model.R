library(foreach)
library(doSNOW)
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