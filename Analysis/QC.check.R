
#C:\Users\smdevine\Desktop\Allowable_Depletion\results\Dec2017.check\almond.mature_majcomps\scenario_2.0m50AD
clean.resultsDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/results/Dec2017.check/clean_results'
resultsDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/results/Dec2017.check/summaries'
almond.results <- read.csv(file.path(clean.resultsDir, 'almond.mature', 'almond.mature2.0mAD50_FAO56results_clean.csv'), stringsAsFactors = FALSE)
code <- 100012
almond.results[almond.results$unique_model_code==code,]
almond.results.aggregated <- read.csv(file.path(resultsDir, 'almond.mature', 'almond.mature2.0mAD50_FAO56results_points_rounded.csv'), stringsAsFactors = FALSE)
almond.results.aggregated[almond.results.aggregated$unique_model_code==code,]
almond.results$GW.ET.growing[almond.results$unique_model_code==code]
almond.results.aggregated$GW.ET.growing[almond.results.aggregated$unique_model_code==code]
colnames(almond.results.aggregated)

#go through the complete results and run a QC check
system.time(for(j in seq_along(cropnames)) {
  cropnames <- c('almond.mature', 'walnut.mature', 'pistachios', 'grapes.table', 'alfalfa.intermountain', 'alfalfa.CV', 'alfalfa.imperial')
  cropnames <- paste0(cropnames, '_majcomps')
  scenario_names <- list.dirs(file.path('C:/Users/smdevine/Desktop/Allowable_Depletion/results/Dec2017.check', cropnames[j]), full.names = FALSE, recursive = FALSE)
  for(k in seq_along(scenario_names)) {
    fnames <- list.files(file.path('C:/Users/smdevine/Desktop/Allowable_Depletion/results/Dec2017.check', cropnames[j], scenario_names[k]), full.names = TRUE)
    fnames <- fnames[1:(length(fnames)-2)] #last two files in each directory are overall results and metadata files
    results <- as.data.frame(matrix(data=NA, nrow=length(fnames), ncol=2))
    colnames(results) <- c('wb', 'rel.error')
    for(i in seq_along(fnames)) {
      df <- read.csv(fnames[i], stringsAsFactors = FALSE)
      results$wb[i] <- sum(df$P) + sum(df$Ir[1:(nrow(df) - 1)]) - sum(df$ETc.act) - sum(df$DPr) + (df$Dr.end[nrow(df)] - df$Dr.initial[1])
      results$rel.error[i] <- 100 * (results$wb[i] / sum(df$P, df$Ir[1:(nrow(df) - 1)]))
      write.csv(results, file =  file.path('C:/Users/smdevine/Desktop/Allowable_Depletion/results/Dec2017.check/QC.results', paste0('QCcheck_', cropnames[j], '_', scenario_names[k], '.csv')))
    }
  }
})
hist(results$rel.error)
which(results$rel.error > 0.2)
hist(results$wb)