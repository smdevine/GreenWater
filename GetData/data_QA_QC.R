modelscaffoldDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/model_scaffold/run_model/Sep2017'
setwd(modelscaffoldDir)
precip <- read.csv('PRISM_precip_data.csv') #this is a daily summary of precip from 10/1/2003-6/25/17 from 'free' daily PRISM 4km resolution for cells of interest in California, created in download_PRISM.R script (from 6/26/17 download)
which(is.na(precip)) #no NAs in PRISM as of 6/26/17

RHmin <- read.csv('SpatialCIMIS_minRH_rounded.csv', stringsAsFactors = F) #this is a daily summary of minimum relative humidity, estimated from download of spatial CIMIS Tdew and Tmax data, created in spatialCIMIS.R script
na_indices_RHmin <- lapply(RHmin[,6:ncol(RHmin)], function(x) {which(is.na(x))})
which(names(RHmin)=='cell_148533')
head(RHmin$cell_148533)
head(RHmin[,10018])
head(RHmin[,10019])
for (i in 1:length(na_indices_RHmin)) {
  if (length(na_indices_RHmin[[i]]) == 1) {
    next
  }
  else(print(i))
}
names(na_indices_RHmin)[10013] #"cell_148533" there is no data for this cell.  It's located east of Watsonville right on the beach at this lat, long: 36.90404, -121.844
na_indices_RHmin <- na_indices_RHmin[-10013]
na_indices_RHmin <- unlist(na_indices_RHmin)
unique(na_indices_RHmin) #row 2991 is only NA for all other cells
#change row 2991 to average of all those same DOYs for a given cell
doy_indices <- which(RHmin$DOY==RHmin$DOY[2991])
doy_indices <- doy_indices[-9] #get rid of 2991
doy342_means <- apply(RHmin[doy_indices, 6:ncol(RHmin)], 2, mean)
RHmin[2991,6:ncol(RHmin)] <- doy342_means
write.csv(RHmin, 'SpatialCIMIS_minRH_rounded_QCpass.csv', row.names = F)

U2 <- read.csv('SpatialCIMIS_U2_rounded.csv') #this is a daily summary of wind data from download of spatial CIMIS data, created in spatialCIMIS.R script
na_indices_U2 <- lapply(U2[,6:ncol(U2)], function(x) {which(is.na(x))})
for (i in 1:length(na_indices_U2)) {
  if (length(na_indices_U2[[i]]) == 0) { #could set to 1 also as above
    next
  }
  else(print(i))
} #i=9524 is all NA; no other NAs present
names(na_indices_U2)[10013] #"cell_148533" is NA; same for RHmin
#no changes made to 'SpatialCIMIS_U2_rounded.csv'

ETo <- read.csv('SpatialCIMIS_ETo_rounded.csv') #this is a daily summary of reference ET from download of spatial CIMIS data, created in spatialCIMIS.R script
na_indices_ETo <- lapply(ETo, function(x) {which(is.na(x))})
j <- c()
for (i in 1:length(na_indices_ETo)) {
  if (length(na_indices_ETo[[i]]) == 0) { #could set to 1 also as above
    next
  }
  else if (length(j) == 0) {
    j <- i
    next
  }
  else{j <- c(j,i)}
}
for (i in 1:length(j)) {
  print(na_indices_ETo[j][i])
} #10 cells have NA values, one of which is cell_148533
#for example,
ETo[2600:2620, j[1]]
#run loop to replace NAs
j <- j[-5] #get rid of index referring to column 10018; it's all NA
#in a loop, j[i] returns the column index
column_indices <- j
i <- 1
k <- 1
for (i in 1:length(column_indices)) {
  row_indices <- unlist(na_indices_ETo[column_indices[i]], use.names = F)
  for (k in 1:length(row_indices)){
     doy <- ETo$DOY[row_indices[k]]
     doy_indices <- which(ETo$DOY==doy)
     gap_fill <- mean(ETo[doy_indices, column_indices[i]], na.rm = TRUE)
     ETo[row_indices[k], column_indices[i]] <- gap_fill
  }
}
setwd(modelscaffoldDir)
write.csv(ETo, 'SpatialCIMIS_ETo_rounded_QCpass.csv', row.names = F)

#code to check data updates for problems

#precip <- read.csv('PRISM_precip_data.csv') #this is a daily summary of precip from 10/1/2003-6/25/17 from 'free' daily PRISM 4km resolution for cells of interest in California, created in download_PRISM.R script (from 6/26/17 download)
which(is.na(precip)) #no NAs in PRISM as of 6/26/17

setwd(file.path(modelscaffoldDir, 'SpCIMIS'))
RHmin <- read.csv('SpatialCIMIS.minRHupdate.rounded.csv', stringsAsFactors = F) #this is a daily summary of minimum relative humidity, estimated from download of spatial CIMIS Tdew and Tmax data, created in spatialCIMIS.R script
na_indices_RHmin <- lapply(RHmin[,6:ncol(RHmin)], function(x) {which(is.na(x))})
head(na_indices_RHmin)
which(names(RHmin)=='cell_148533')
head(RHmin$cell_148533)
head(RHmin[,10018])
for (i in 1:length(na_indices_RHmin)) {
  if (length(na_indices_RHmin[[i]]) == 1) {
    next
  }
  else(print(i))
}
#all ok except cell_148533

U2 <- read.csv('SpatialCIMIS.U2update.rounded.csv') #this is a daily summary of wind data from download of spatial CIMIS data, created in spatialCIMIS.R script
na_indices_U2 <- lapply(U2[,6:ncol(U2)], function(x) {which(is.na(x))})
for (i in 1:length(na_indices_U2)) {
  if (length(na_indices_U2[[i]]) == 0) { #could set to 1 also as above
    next
  }
  else(print(i))
} #i=9524 is all NA; no other NAs present
names(na_indices_U2)[10013] #"cell_148533" is NA; same for RHmin
#no changes made to 'SpatialCIMIS_U2_rounded.csv'

ETo <- read.csv('SpatialCIMIS.EToupdate.rounded.csv') #this is a daily summary of reference ET from download of spatial CIMIS data, created in spatialCIMIS.R script
na_indices_ETo <- lapply(ETo, function(x) {which(is.na(x))})
j <- c()
for (i in 1:length(na_indices_ETo)) {
  if (length(na_indices_ETo[[i]]) == 0) { #could set to 1 also as above
    next
  }
  else if (length(j) == 0) {
    j <- i
    next
  }
  else{j <- c(j,i)}
}
for (i in 1:length(j)) {
  print(na_indices_ETo[j][i])
} #only cell_148533 has NA
#proceed with all SpCiMIS update files as is (9/13/2017)

#tests for assurance of gap-filling (these worked 6/26/17)
ETo$cell_77141[2600:2620]
ETo$DOY[2601]
i <- which(ETo$DOY==317)
mean(ETo$cell_77141[i], na.rm = TRUE)
#1.324769 should replace NA at ETo$cell_77141[2601]
ETo$cell_105651[3049]
ETo$DOY[3049]
i <- which(ETo$DOY==35)
mean(ETo$cell_105651[i], na.rm = TRUE)
#1.157308 should replace NA at ETo$cell_105651[3049]

#these were the original NAs in spatial CIMIS ETo
#$cell_77141
#[1] 2601 2602 2603 2612 2613 2614 2615

#$cell_105651
#[1] 3037 3038 3039 3040 3041 3042 3043 3044 3045 3046 3047 3048 3049 3050 3051

#$cell_136867
#[1] 2956 2957 2958 2959 2960 2961 2962 2963 2964 2965 2966 2967 2968 2969 2970

#$cell_139918
#[1] 2742 2743 2744 2745 2746 2747 2748 2749 2750 2751 2752 2753 2754 2755 2756

#$cell_189441
#[1] 3660 3661 3662 3664 3665 3666 3667 3668 3669 3671 3672 3673 3674

#$cell_246668
#[1] 3082 3083 3084 3085 3086 3087 3088 3089 3090 3091 3092 3093 3094 3095 3096

#$cell_254393
#[1] 2726 2727 2728 2729 2730 2731 2732 2733 2734 2735 2736 2737 2738 2739 2740

#$cell_264587
#[1] 4975

#$cell_264588
#[1] 4243 4608
