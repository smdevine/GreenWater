resultsDir <- 'D:/Allowable_Depletion/results/Mar2018' #D:/Allowable_Depletion/results/Oct2017' #
modelscaffoldDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/model_scaffold/run_model/Mar2018'
clean.resultsDir <- 'D:/Allowable_Depletion/results/Mar2018/clean_results' #this is for most recent runs starting in March 2018
clean.QC.resultsDir <- 'D:/Allowable_Depletion/results/Mar2018/clean_QC.results'
#made revision on Dec 2017 to re-run results through but re-correcting GW.ET.growing for Irr.end.storage correction
#GW.ET.growing correction no longer needed as of March 2018, so 2nd function argument set to FALSE
if (!dir.exists(file.path(resultsDir, 'clean_results'))) {
  dir.create(file.path(resultsDir, 'clean_results'))
}

# setwd(modelscaffoldDir)
# list.files()
# model.scaffold <- read.csv("model_scaffold_majcomps.v3.csv", stringsAsFactors = FALSE)
# 
# #add column that identifies whether or not unique_model_code had original SSURGO data
dim(model.scaffold) #107561 rows
sum(model.scaffold$SSURGO_awc_data=='Yes') #106,834
sum(model.scaffold$SSURGO_awc_data=='No' & !is.na(model.scaffold$z1.5m_cmH2O_modified_comp) & model.scaffold$z1.5m_cmH2O_modified_comp !=0) #727 (same as no data)
sum(!is.na(model.scaffold$z1.5m_cmH2O_modified_comp)) #107561
sum(model.scaffold$z1.5m_cmH2O_modified_comp !=0) #107561
model.scaffold_uncertain <- model.scaffold[which(model.scaffold$SSURGO_awc_data=='No'),]
model.scaffold_certain <- model.scaffold[-which(model.scaffold$SSURGO_awc_data=='No'),]
tapply(model.scaffold_uncertain$comppct_r, model.scaffold_uncertain$compname, summary)
tapply(model.scaffold_uncertain$z1.5m_cmH2O_modified_comp, model.scaffold_uncertain$compname, summary)
length(unique(model.scaffold_uncertain$compname)) #23 components
compnames_uncertain_data <- unique(model.scaffold_uncertain$compname) #first draft of this
setwd(modelscaffoldDir)
write.csv(compnames_uncertain_data, file.path(modelscaffoldDir, 'compnames_uncertain_data.csv'), row.names=FALSE) #from here, additonal column was added and compnames were given a yes, if not a real soil series, or a no, if it is a real soil series
compnames_uncertain_data <- read.csv(file.path(modelscaffoldDir, 'compnames_uncertain_data.csv'), stringsAsFactors = FALSE)
compnames_uncertain_data <- compnames_uncertain_data[which(compnames_uncertain_data$remove.results=='Yes'),]
dim(model.scaffold_uncertain) #727 rows

model.scaffold_uncertain <- model.scaffold_uncertain[which(model.scaffold_uncertain$compname %in% compnames_uncertain_data$compname),] #further refinement of this grid, still 10,275 rows
dim(model.scaffold_uncertain) #now, 725 rows
cokeys_uncertain_data <- unique(model.scaffold_uncertain$cokey)
length(cokeys_uncertain_data)
write.csv(cokeys_uncertain_data, file.path(modelscaffoldDir, 'cokeys_uncertain_data.csv'), row.names = FALSE) #these can be used to refine each results grid

#read in this file before running function
cokeys_uncertain_data <- read.csv(file.path(modelscaffoldDir, 'cokeys_uncertain_data.csv'), stringsAsFactors = FALSE)

CleanResults <- function(cropname) {
  parentDir <- file.path(resultsDir, paste0(cropname, '_majcomps'))
  if (!dir.exists(file.path(clean.resultsDir, cropname))) {
    dir.create(file.path(clean.resultsDir, cropname))
  }
  dirnames <- list.files(parentDir)
  for (i in seq_along(dirnames)) {
    fname <- list.files(file.path(parentDir, dirnames[i]), pattern = glob2rx('*FAO56results.csv'))
    df <- read.csv(file.path(parentDir, dirnames[i], fname), stringsAsFactors = FALSE)
    print(dirnames[i])
    print(ncol(df))
    print(nrow(df))
    df <- df[-which(df$cokey %in% cokeys_uncertain_data$x | is.na(df$TEW) | is.na(df$REW) | df$TEW == 0), ] #this gets rid of all results that are "uncertain" or where model was not run as a result of soil variables being NA or equal to 0
    print(nrow(df))
    df <- df[ ,-c(1:4, 8:10)] #get rid of repetitive columns; these can be obtained from the qc_results file
    fname_modified <- paste0(gsub('.csv', '', fname), '_clean.csv')
    write.csv(df, file.path(clean.resultsDir, cropname, fname_modified), row.names = FALSE)
  }
}
#run function for walnuts
CleanResults('alfalfa.intermountain')
CleanResults('alfalfa.CV')
CleanResults('alfalfa.imperial')
CleanResults('walnut.mature')
CleanResults('almond.mature')
CleanResults('pistachios')
CleanResults('grapes.table')
CleanResults('grapes.wine')

CleanQCresults <- function(cropname) {
  parentDir <- file.path(resultsDir, paste0(cropname, '_majcomps'))
  if (!dir.exists(file.path(clean.QC.resultsDir, cropname))) {
    dir.create(file.path(clean.QC.resultsDir, cropname))
  }
  dirnames <- list.files(parentDir)
  for (i in seq_along(dirnames)) {
    fname <- list.files(file.path(parentDir, dirnames[i]), pattern = glob2rx('*_QC.results.csv'))
    df <- read.csv(file.path(parentDir, dirnames[i], fname), stringsAsFactors = FALSE)
    print(dirnames[i])
    print(ncol(df))
    print(nrow(df))
    df <- df[-which(df$cokey %in% cokeys_uncertain_data$x | is.na(df$TEW) | is.na(df$REW) | df$TEW == 0), ] #this gets rid of all results that are "uncertain" or where model was not run as a result of soil variables being NA or equal to 0
    print(nrow(df))
    fname_modified <- paste0(gsub('.csv', '', fname), '_clean.csv')
    write.csv(df, file.path(clean.QC.resultsDir, cropname, fname_modified), row.names = FALSE)
  }
}
CleanQCresults('alfalfa.intermountain')
CleanQCresults('alfalfa.CV')
CleanQCresults('alfalfa.imperial')
CleanQCresults('walnut.mature')
CleanQCresults('almond.mature')
CleanQCresults('pistachios')
CleanQCresults('grapes.table')
CleanQCresults('grapes.wine')
#print-outs showed no concerns 10/5/17
#error discovered 10/13/17: df$cokey %in% cokeys_uncertain_data was not returning anything because cokeys_uncertain_data is a data.frame; everything re-run with df$cokey %in% cokeys_uncertain_data$x now part of the logical statement to remove specific records
#re-run 12/20/17 to check for sensitivity to irr.end.storage correction in GW.ET.growing calc, which was discovered to have produced problems for Alfalfa Imperial calculation