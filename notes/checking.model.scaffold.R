#model scaffold exploration after reading in FAO56_dualcropcoeff.v2.R
modelscaffoldDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/model_scaffold/run_model/Mar2018' #location of input data; re-worked early March 2018
model.scaffold <- read.csv(file.path(modelscaffoldDir, 'model_scaffold_majcomps2018-03-15.csv'), stringsAsFactors = FALSE) #note this has several more columns than previous versions; will have affects downstream
nrow(model.scaffold) #107,561 unique combinations of crop, soil, and climate
length(unique(model.scaffold$compname)) #1143
length(unique(model.scaffold$muname)) #4345
length(unique(model.scaffold$mukey)) #4889
length(unique(model.scaffold$cokey)) #5887
length(unique(model.scaffold$PRISMcellnumber)) #4,262 prism cells of interest
length(unique(model.scaffold$CIMIScellnumber)) #12,524 CIMIS cells of interest

#check TEW and REW data
summary(model.scaffold$TEW)
sum(model.scaffold$TEW < 10)
sum(model.scaffold$TEW < 5)
unique(model.scaffold$compname[model.scaffold$TEW < 5])
unique(model.scaffold$cokey[model.scaffold$TEW < 5])
unique(model.scaffold$compname[model.scaffold$TEW < 10])
unique(model.scaffold$TEW[model.scaffold$TEW < 5])
sum(model.scaffold$TEW > model.scaffold$z0.5m_cmH2O_unmodified_comp*10)
sum(model.scaffold$TEW > model.scaffold$z1.0m_cmH2O_unmodified_comp*10)
sum(model.scaffold$TEW > model.scaffold$z0.5m_cmH2O_modified_comp*10)
sum(model.scaffold$TEW > model.scaffold$z1.0m_cmH2O_modified_comp*10)
unique(model.scaffold$compname[model.scaffold$TEW > model.scaffold$z0.5m_cmH2O_unmodified_comp*10])
model.scaffold.crop[model.scaffold.crop$TEW > model.scaffold.crop$z0.5m_cmH2O_modified_comp*10, ]
sum(model.scaffold.crop$unique_model_code==101362)
model.scaffold.crop[model.scaffold.crop$PRISMcellnumber==446907 & model.scaffold.crop$CIMIScellnumber==155748, ]
sum(model.scaffold$cropcode==69 & model.scaffold$PRISMcellnumber==510126 & model.scaffold$CIMIScellnumber==208777)
model.scaffold$grape.zone[model.scaffold$cropcode==69 & model.scaffold$PRISMcellnumber==510126 & model.scaffold$CIMIScellnumber==208777]

nrow(model.scaffold) - sum(!(model.scaffold$unique_model_code %in% model_shp_1.0mAD50$unq__))
length(unique(model_shp_1.0mAD50$unq__))
