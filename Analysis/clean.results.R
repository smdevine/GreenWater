resultsDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/results/Oct2017' #D:/Allowable_Depletion/results/Oct2017' #
modelscaffoldDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/model_scaffold/run_model/Oct2017'
clean.resultsDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/results/Oct2017/clean_results' #this is for most recent runs starting Oct 17, 2017
if (!dir.exists(file.path(resultsDir, 'clean_results'))) {
  dir.create(file.path(resultsDir, 'clean_results'))
}

# setwd(modelscaffoldDir)
# list.files()
# model_scaffold <- read.csv("model_scaffold_majcomps.v3.csv", stringsAsFactors = FALSE)
# 
# #add column that identifies whether or not unique_model_code had original SSURGO data
# sum(model_scaffold$SSURGO_awc_data=='Yes') #367,225
# sum(model_scaffold$SSURGO_awc_data=='No' & !is.na(model_scaffold$z1.5m_cmH2O_modified_comp) & model_scaffold$z1.5m_cmH2O_modified_comp !=0) #11,691
# model_scaffold_uncertain <- model_scaffold[which(model_scaffold$SSURGO_awc_data=='No' & !is.na(model_scaffold$z1.5m_cmH2O_modified_comp) & model_scaffold$z1.5m_cmH2O_modified_comp !=0),]
# model_scaffold_certain <- model_scaffold[-which(model_scaffold$SSURGO_awc_data=='No' & !is.na(model_scaffold$z1.5m_cmH2O_modified_comp) & model_scaffold$z1.5m_cmH2O_modified_comp !=0),]
# tapply(model_scaffold_uncertain$comppct_r, model_scaffold_uncertain$compname, summary)
# tapply(model_scaffold_uncertain$z1.5m_cmH2O_modified_comp, model_scaffold_uncertain$compname, summary)
# length(unique(model_scaffold_uncertain$compname)) #53 components
# compnames_uncertain_data <- unique(model_scaffold_uncertain$compname) #first draft of this
# sum(model_points$unique_model_code %in% model_scaffold_uncertain$unique_model_code) #563,808 cells would be affected by uncertain data; some of these cells would only have uncertain data, and some would have a mixture
# setwd(modelscaffoldDir)
# write.csv(compnames_uncertain_data, 'compnames_uncertain_data.csv', row.names=FALSE) #from here, additonal column was added and 53 compnames were given a yes, if not a real soil series, or a no, if it is a real soil series
# compnames_uncertain_data <- read.csv('compnames_uncertain_data.csv', stringsAsFactors = FALSE)
# compnames_uncertain_data <- compnames_uncertain_data[which(compnames_uncertain_data$remove.results=='yes'),]
# model_scaffold_uncertain <- model_scaffold_uncertain[which(model_scaffold_uncertain$compname %in% compnames_uncertain_data$compname),] #further refinement of this grid, still 10,275 rows
# cokeys_uncertain_data <- unique(model_scaffold_uncertain$cokey)
# setwd(modelscaffoldDir)
# write.csv(cokeys_uncertain_data, 'cokeys_uncertain_data.csv', row.names = FALSE) #these can be used to refine each results grid


#read in this file before running function
setwd(modelscaffoldDir)
cokeys_uncertain_data <- read.csv('cokeys_uncertain_data.csv', stringsAsFactors = FALSE)

CleanResults <- function(cropname) {
  parentDir <- file.path(resultsDir, paste0(cropname, '_majcomps'))
  if (!dir.exists(file.path(clean.resultsDir, cropname))) {
    dir.create(file.path(clean.resultsDir, cropname))
  }
  setwd(parentDir)
  dirnames <- list.files()
  for (i in seq_along(dirnames)) {
    setwd(file.path(parentDir, dirnames[i]))
    fname <- list.files(pattern = glob2rx('*FAO56results.csv'))
    df <- read.csv(fname, stringsAsFactors = FALSE)
    print(dirnames[i])
    print(ncol(df))
    print(nrow(df))
    pawvar <- colnames(df)[14]
    print(pawvar)
    df <- df[-which(df$cokey %in% cokeys_uncertain_data$x | is.na(df[[pawvar]]) | is.na(df$TEW) | is.na(df$REW) | df[[pawvar]] == 0 | df$TEW == 0), ] #this gets rid of all results that are "uncertain" or where model was not run as a result of soil variables being NA or equal to 0
    print(nrow(df))
    setwd(file.path(clean.resultsDir, cropname))
    fname_modified <- paste0(gsub('.csv', '', fname), '_clean.csv')
    write.csv(df, fname_modified, row.names = FALSE)
  }
}
#run function for walnuts
CleanResults('walnut.mature')
CleanResults('almond.mature')
CleanResults('pistachios')
CleanResults('grapes.table')
CleanResults('grapes.wine')
CleanResults('alfalfa.intermountain')
CleanResults('alfalfa.CV')
CleanResults('alfalfa.imperial')
#print-outs showed no concerns 10/5/17
#error discovered 10/13/17: df$cokey %in% cokeys_uncertain_data was not returning anything because cokeys_uncertain_data is a data.frame; everything re-run with df$cokey %in% cokeys_uncertain_data$x now part of the logical statement to remove specific records