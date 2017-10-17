#add alfalfa.zone and grape.zone to points
pointsDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/results/data.frames/Aug2017'
modelscaffoldDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/model_scaffold/run_model/Sep2017'
setwd(modelscaffoldDir)
list.files()
model_scaffold <- read.csv("model_scaffold_majcomps.v3.csv", stringsAsFactors = FALSE) #remember the model_scaffold contains duplicate unique_model_codes where there is more than one major soil component
cellnumbers_to_modelcodes <- read.csv("cellnumbers_to_modelcodes.csv", stringsAsFactors = FALSE) #dim is 21,679,100 x 2
#cellnumbers_to_modelcodes$unique_model_codes <- as.integer(cellnumbers_to_modelcodes$unique_model_codes)
cellnumbers_to_modelcodes$model_code_short <- cellnumbers_to_modelcodes$unique_model_codes - 100000 #this necessary because tabulate returning all zeroes when using the longer model_code integers
cell_counts <- tabulate(cellnumbers_to_modelcodes$model_code_short)
cell_counts <- data.frame(cell_counts30m2=cell_counts, unique_model_code=100001:441207)
#check validity
sum(cell_counts$cell_counts30m2) #21679100
cell_counts$cell_counts30m2[cell_counts$unique_model_code==124878]
sum(cellnumbers_to_modelcodes$unique_model_codes==124878)
cell_counts$cell_counts30m2[cell_counts$unique_model_code==374217]
sum(cellnumbers_to_modelcodes$unique_model_codes==374217)
sum(cell_counts$cell_counts30m2==1) #56,839 model codes have only 1 cell
sum(cell_counts$cell_counts30m2==2) #32,311 model codes have only 2 cells
sum(cell_counts$cell_counts30m2==3) #21,138 model codes have only 3 cells
setwd(modelscaffoldDir)
write.csv(cell_counts, 'cell_counts.csv', row.names=FALSE)

